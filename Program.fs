open System
open System.Threading
open FSharp.Control
open System.Net.Http
open System.Net.NetworkInformation
open System.Text
open System.Text.Json
open Domain
open System.Diagnostics
open System.Collections.Generic
open System.IO
open System.Threading.Tasks
open Spectre
open Spectre.Console
open FSharp.Json
open Anthropic
open ModelContextProtocol.Client
open ModelContextProtocol.Protocol.Transport
open Microsoft.Extensions.AI

// Define types for API request and response
type ClaudeContentBlock = { Type: string; Text: string }

type ClaudeResponse =
    { Id: string
      Content: ClaudeContentBlock[]
      Model: string
      StopReason: string
      StopSequence: string option
      Usage:
          {| InputTokens: int
             OutputTokens: int |} }

type ClaudeRequest =
    { Model: string
      [<JsonField("max_tokens")>]
      MaxTokens: int
      Messages: ChatMessage[]
      System: string
      Stream: bool }

module UI =
    let display (state: State) =

        let printBold (text: string) = AnsiConsole.Markup($"[bold]{text}[/]")

        match state.Message with
        | You _ ->
            printBold ">>>"
            printf " "
        | _ -> ()

        state

let await f = f |> Async.RunSynchronously

let withNewChat (msg: Domain.Message) (convo: Conversation) =
    match msg with
    | Quit -> convo
    | You msg -> [ yield! convo; You msg ]
    | Jarvis msg -> [ yield! convo; Jarvis msg ]

let ask llm state : ChatContent =
    let (payload, httpRequest) =
        match llm with
        | Ollama _ ->
            let model = "jarvis"
            let payload = LLM.createPayload state (Ollama model)
            let request = Ollama.httpRequest payload
            (payload, request)
        | Claude _ ->
            let model = "claude-sonnet-4-20250514"
            let payload = LLM.createPayload state (Claude model)
            let request = Claude.httpRequest payload
            (payload, request)

    let parseHandler =
        match llm with
        | Ollama _ -> Ollama.parse
        | Claude _ -> Claude.parse

    let jsonOptions = JsonSerializerOptions(PropertyNameCaseInsensitive = true)

    LLM.makeRequest httpRequest parseHandler payload
    |> (fun stream ->
        async {
            let startInfo = ProcessStartInfo()
            startInfo.FileName <- $"/Users/amirpanahi/Documents/projects/jarvis/prettified-output/main"
            startInfo.UseShellExecute <- false
            startInfo.RedirectStandardInput <- true

            use proc = Process.Start(startInfo)
            use stdin = proc.StandardInput

            let! res =
                stream
                |> AsyncSeq.foldAsync<ParseContext, Event>
                    (fun acc content ->
                        async {
                            // printfn "%A" acc
                            // printfn ""

                            // if acc.RestartRenderer then
                            //     proc.Start() |> ignore

                            match content with
                            | ReceivedResponse res ->
                                do! stdin.WriteAsync(content.Serialize None (Some res)) |> Async.AwaitTask
                                do! stdin.FlushAsync() |> Async.AwaitTask

                                return
                                    { acc with
                                        Response = ChatContent.Text(acc.Response.Serialize() + res) }
                            | RequiresTool tool ->
                                do! stdin.WriteAsync(content.Serialize (Some tool.name) None) |> Async.AwaitTask
                                do! stdin.FlushAsync() |> Async.AwaitTask

                                return { acc with Tool = Some tool, "" }
                            | ConstructingToolSchema partial ->
                                let toolName =
                                    fst acc.Tool |> Option.map (fun x -> x.name) |> Option.defaultValue "Unknown"

                                do! stdin.WriteAsync(content.Serialize (Some toolName) None) |> Async.AwaitTask
                                do! stdin.FlushAsync() |> Async.AwaitTask

                                // Safely append schema parts - trim to avoid malformed JSON
                                let cleanPartial = partial.Trim()
                                let toolUpdated = (fst acc.Tool, snd acc.Tool + cleanPartial)

                                return { acc with Tool = toolUpdated }
                            | CallTool ->
                                let latestTextOutput =
                                    acc.Response
                                    |> function
                                        | Text t -> t
                                        | _ -> ""

                                let (_tool, schema) = acc.Tool

                                match _tool with
                                | Some tool ->
                                    let client =
                                        state.McpServerTools
                                        |> Array.collect (fun x -> x |> Array.filter (fun y -> y.Name = tool.name))
                                        |> Array.tryHead

                                    match client with
                                    | Some x ->
                                        let schemaProcessed = if String.IsNullOrEmpty schema then "{}" else schema

                                        // Try to correctly parse the schema JSON
                                        let! outcome =
                                            async {
                                                try
                                                    let schemaDict =
                                                        schemaProcessed
                                                        |> JsonSerializer.Deserialize<Dictionary<string, obj>>

                                                    let functionArgs = AIFunctionArguments(schemaDict)

                                                    let! result =
                                                        x.InvokeAsync(functionArgs).AsTask() |> Async.AwaitTask

                                                    // Direct access to response content without double serialization
                                                    let output = result

                                                    // Extract just the text content safely
                                                    let outcome =
                                                        try
                                                            let output =
                                                                result
                                                                |> JsonSerializer.Serialize
                                                                |> JsonSerializer.Deserialize<
                                                                    ModelContextProtocol.Protocol.Types.CallToolResponse
                                                                    >

                                                            Some (output.Content.Item 0).Text
                                                        with _ ->
                                                            // Fallback if content structure is unexpected
                                                            printfn "Error processing tool response"
                                                            None

                                                    return outcome
                                                with ex ->
                                                    // Handle schema parsing errors
                                                    printfn "Error parsing tool schema: %s" ex.Message
                                                    return None
                                            }

                                        let toolResponseUser =
                                            { ``type`` = tool.``type``
                                              tool_use_id = tool.id
                                              content = outcome }

                                        let printToolCall =
                                            Event.ReceivedResponse $"\n> [{tool.name}] called w/ ```{schemaProcessed}```"
                                        do!
                                            stdin.WriteAsync(
                                                printToolCall.Serialize
                                                    None
                                                    None
                                            )
                                            |> Async.AwaitTask
                                        do!
                                            stdin.WriteAsync(
                                                content.Serialize
                                                    (Some tool.name)
                                                    None
                                            )
                                            |> Async.AwaitTask

                                        do! stdin.FlushAsync() |> Async.AwaitTask

                                        return
                                            { acc with
                                                Tool = None, ""
                                                Response =
                                                    JarvisToolResponse.Create schema latestTextOutput tool
                                                    |> (fun x -> (Some x, Some toolResponseUser))
                                                    |> ChatContent.Tool }
                                    | None -> return acc
                                | None -> return acc
                        })
                    { Response = ChatContent.Text ""
                      Tool = None, "" }

            stdin.Close()
            proc.WaitForExit()
            return res.Response
        })
    |> await

let withNewestPrompt state = List.last state.Conversation

let rec chat (state: State) (llm: LLM) =
    match state.Message with
    | You prompt ->
        state |> UI.display |> ignore

        match prompt with
        | Implicit response ->
            let newState =
                { state with
                    Message = "" |> ChatContent.Text |> Explicit |> Jarvis
                    Conversation = withNewChat (You(Explicit response)) state.Conversation } //end

            chat newState llm
        | Explicit response ->
            let input = System.Console.ReadLine()

            let newState =
                match input with
                | "/exit"
                | "/quit" -> { state with Message = Quit }
                // | "/retain" ->
                //     printfn "Generating summary of chat history..."

                //     try
                //         let content = state.Conversation |> Json.serialize

                //         if String.IsNullOrEmpty content then
                //             printfn "No conversation history to summarize."
                //             state
                //         else

                //             // Create direct API request to Anthropic
                //             let client = new HttpClient()

                //             // Prepare the payload
                //             let apiKey = Environment.GetEnvironmentVariable("ANTHROPIC_API_KEY")

                //             let systemPrompt =
                //                 "Write 5 concise bullet points rich in information that describe any design decisions, insights uncovered, errors and issues encountered from the chat history that would be useful for future reference. Things like: 'I need to bare in mind X' or 'Key points to consider are X' or 'As a result of X then Y'. Notes that will help me in the future keep on top of how things morphozises over time"

                //             // Create the request payload
                //             let payload =
                //                 { Model = "claude-3-5-sonnet-20241022"
                //                   MaxTokens = 1024
                //                   Messages = [| { role = "user"; content = content } |]
                //                   System = systemPrompt
                //                   Stream = false }

                //             let config = JsonConfig.create (jsonFieldNaming = Json.snakeCase)
                //             let payloadJson = Json.serializeEx config payload
                //             let content = new StringContent(payloadJson, Encoding.UTF8, "application/json")

                //             // Create request
                //             let request = new HttpRequestMessage()
                //             request.Method <- HttpMethod.Post
                //             request.RequestUri <- Uri("https://api.anthropic.com/v1/messages")
                //             request.Content <- content
                //             request.Headers.Add("x-api-key", apiKey)
                //             request.Headers.Add("anthropic-version", "2023-06-01")

                //             // Execute request
                //             let exec =
                //                 async {
                //                     try
                //                         // Print request payload for debugging
                //                         printfn "Debug - Request payload: %s" payloadJson

                //                         use! response = client.SendAsync(request) |> Async.AwaitTask

                //                         // Get response body even if status code indicates failure
                //                         let! responseBody = response.Content.ReadAsStringAsync() |> Async.AwaitTask

                //                         printfn
                //                             "Debug - Status code: %d %s"
                //                             (int response.StatusCode)
                //                             (response.StatusCode.ToString())

                //                         printfn "Debug - Raw response: %s" responseBody

                //                         // Check status code after logging response
                //                         response.EnsureSuccessStatusCode() |> ignore

                //                         // Parse the response
                //                         let deserializeOptions =
                //                             JsonSerializerOptions(PropertyNameCaseInsensitive = true)

                //                         let result =
                //                             JsonSerializer.Deserialize<ClaudeResponse>(responseBody, deserializeOptions)

                //                         return Ok result
                //                     with ex ->
                //                         return Error ex
                //                 }

                //             match exec |> await with
                //             | Ok result ->
                //                 try
                //                     let textContent = result.Content |> Array.tryFind (fun c -> c.Type = "text")

                //                     match textContent with
                //                     | Some content ->
                //                         let summary = content.Text
                //                         printfn "\nSummary of chat history:"
                //                         printfn "%s" summary

                //                         // Append to CLAUDE.md file
                //                         try
                //                             let contentToWrite = sprintf "\n(%s)\n" summary

                //                             let claudeFilePath =
                //                                 Path.Combine(Directory.GetCurrentDirectory(), "CLAUDE.md")

                //                             // Check if file exists, create if not
                //                             if not (File.Exists(claudeFilePath)) then
                //                                 File.WriteAllText(claudeFilePath, "# Jarvis Chat Summaries\n")

                //                             // Append the content
                //                             File.AppendAllText(claudeFilePath, contentToWrite)
                //                             printfn "Summary appended to CLAUDE.md"
                //                         with ex ->
                //                             printfn "Error writing to file: %s" ex.Message
                //                     | None -> printfn "No text content found in response."
                //                 with ex ->
                //                     printfn "Error parsing response: %s" ex.Message
                //             | Error ex -> printfn "API call failed: %s" ex.Message

                //             state
                //     with ex ->
                //         printfn "Error generating summary: %s" ex.Message
                //         state
                | "/end" ->
                    { state with
                        Message = "" |> ChatContent.Text |> Explicit |> Jarvis
                        Conversation = withNewChat (You(Explicit response)) state.Conversation } //end
                | str ->
                    //append text to accumulated message
                    let accumulatedMsg = $"{response.Serialize()}\n{str}"

                    { state with
                        Message = You(Explicit(ChatContent.Text accumulatedMsg)) }

            chat newState llm
    | Jarvis said ->
        state |> UI.display |> ignore

        //ask for jarvis input -> ollama rest api call
        let response = state |> ask llm

        // printfn "%A" state.Conversation

        chat
            (match response with
             | Text t ->
                 { state with
                     Conversation = withNewChat (t |> ChatContent.Text |> Explicit |> Jarvis) state.Conversation
                     Message = You(Explicit(ChatContent.Text "")) }
             | Tool(jarvis_tr, user_tr) ->

                 let newConvo =
                     state.Conversation
                     |> withNewChat (Jarvis(Implicit(ChatContent.Tool(jarvis_tr, user_tr)))) //tool invoked
                     |> withNewChat (You(Implicit(ChatContent.Tool(jarvis_tr, user_tr)))) //tool answer

                 { state with
                     Conversation = newConvo
                     Message = "" |> ChatContent.Text |> Explicit |> Jarvis }) //request jarvis again after tool use done to generate outcome
            llm
    | Quit -> ()

let main argv =
    task {
        let isInternetAvailable () =
            try
                use ping = new System.Net.NetworkInformation.Ping()
                let reply = ping.Send("8.8.8.8") // Ping Google DNS
                reply.Status = IPStatus.Success
            with _ ->
                false

        let! mcpServers =
            task {
                let! config = MCP.readConfig $"{Directory.GetCurrentDirectory()}/mcp-servers.json"

                match config with
                | Some serverList ->
                    let! mcpClients = MCP.createClients serverList
                    let! tools = MCP.listTools mcpClients
                    return Some tools
                | None -> return None

            }

        let initially =
            { Message = You(Explicit(ChatContent.Text ""))
              Conversation = List.Empty
              WithLogging = false
              McpServerTools =
                mcpServers
                |> Option.map (fun x -> x |> Array.map snd)
                |> Option.defaultValue [||] }

        match argv |> Array.toList with
        | dll :: rest ->
            match rest with
            | [ "tools" ] -> MCP.display mcpServers true
            | [ "-l"] ->
                let llm =
                    match isInternetAvailable () with
                    | true -> Claude ""
                    | false -> Ollama "" // Ollama can be used offline

                MCP.display mcpServers false
                chat {initially with WithLogging = true } llm
            | _ ->
                let llm =
                    match isInternetAvailable () with
                    | true -> Claude ""
                    | false -> Ollama "" // Ollama can be used offline

                MCP.display mcpServers false
                chat initially llm
        | _ -> ()

        return 0
    }

let argv = System.Environment.GetCommandLineArgs()
main argv |> Async.AwaitTask |> Async.RunSynchronously |> ignore

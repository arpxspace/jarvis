open System
open System.Threading
open FSharp.Control
open System.Net.Http
open System.Net.NetworkInformation
open System.Text
open System.Text.Json
open Domain
open System.Diagnostics
open System.IO
open System.Threading.Tasks
open Spectre
open Spectre.Console

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

let withNewChat (msg: Message) (convo: Conversation) =
    match msg with
    | Quit -> convo
    | You msg -> [ yield! convo; You msg ]
    | Jarvis msg -> [ yield! convo; Jarvis msg ]

let ask llm state : ChatContent =
    let (payload, httpRequest) =
        match llm with
        | Ollama ->
            let model = "jarvis"
            let payload = LLM.createPayload state.Conversation Ollama
            let request = Ollama.httpRequest payload
            (payload, request)
        | Claude ->
            let model = "claude-3-5-sonnet-20241022"
            let payload = LLM.createPayload state.Conversation Claude
            let request = Claude.httpRequest payload
            (payload, request)

    let parseHandler =
        match llm with
        | Ollama -> Ollama.parse
        | Claude -> Claude.parse

    let jsonOptions = JsonSerializerOptions(PropertyNameCaseInsensitive = true)

    LLM.makeRequest httpRequest parseHandler payload
    |> (fun stream ->
        async {
            let startInfo = ProcessStartInfo()
            startInfo.FileName <- "/Users/amirpanahi/Documents/projects/jarvis/prettified-output/main"
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

                                return
                                    { acc with
                                        Tool = ToolData.init tool.name tool }
                            | ConstructingToolSchema partial ->
                                match acc.Tool with
                                | Some tool ->
                                    do! stdin.WriteAsync(content.Serialize (Some tool.Name) None) |> Async.AwaitTask
                                    do! stdin.FlushAsync() |> Async.AwaitTask

                                    return
                                        { acc with
                                            Tool = Some(tool.UpdateSchema partial) }
                                | None -> return acc
                            | CallTool ->
                                let latestTextOutput =
                                    acc.Response
                                    |> function
                                    | Text t -> t
                                    | _ -> ""

                                match acc.Tool with
                                | Some(WriteNote(schema, tool)) ->
                                    let input = JsonSerializer.Deserialize<Tools.WriteNoteSchema>(schema, jsonOptions)
                                    let filepath = $"/Users/amirpanahi/notes/literature/{input.filename}"

                                    do! File.AppendAllTextAsync(filepath, input.note) |> Async.AwaitTask

                                    let notification = $"\n\n**Written to: {filepath}**"
                                    do! stdin.WriteAsync(notification) |> Async.AwaitTask
                                    do! stdin.FlushAsync() |> Async.AwaitTask


                                    let toolResponse: UserToolResponse =
                                        { ``type`` = "tool_result"
                                          tool_use_id = tool.id
                                          content = Some notification }

                                    return
                                        { acc with
                                            Tool = None
                                            Response =
                                                JarvisToolResponse.Create schema latestTextOutput tool
                                                |> (fun x -> (Some x, Some toolResponse))
                                                |> ChatContent.Tool }

                                | Some(RecordThinking(schema, tool))
                                | Some(RecordMistake(schema, tool)) ->
                                    //stop prettified output program
                                    proc.Kill()

                                    Thread.Sleep 1000

                                    printfn ""

                                    let subProcessInfo = ProcessStartInfo()

                                    subProcessInfo.FileName <-
                                        match acc.Tool.Value.Name with
                                        | "record-thinking" ->
                                            "/Users/amirpanahi/Documents/projects/think/bin/Debug/net9.0/think"
                                        | "record-mistake" ->
                                            "/Users/amirpanahi/Documents/projects/oops/bin/Debug/net9.0/oops"
                                        | _ -> failwith "should be a tool name that is recognised"

                                    subProcessInfo.UseShellExecute <- false

                                    use thinkProc = Process.Start(subProcessInfo)
                                    thinkProc.WaitForExit()

                                    let toolResponse: UserToolResponse =
                                        { ``type`` = "tool_result"
                                          tool_use_id = tool.id
                                          content = Some "This has now been resolved" }

                                    return
                                        { acc with
                                            Tool = None
                                            Response =
                                                JarvisToolResponse.Create schema latestTextOutput tool
                                                |> (fun x -> (Some x, Some toolResponse))
                                                |> ChatContent.Tool }

                                | None -> return acc
                        })
                    { Response = ChatContent.Text ""
                      Tool = None }

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
                | "exit"
                | "quit" -> { state with Message = Quit }
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

        chat
            (match response with
             | Text t ->
                 // printfn "%A" state.Conversation
                 { state with
                     Conversation = withNewChat (t |> ChatContent.Text |> Explicit |> Jarvis) state.Conversation
                     Message = You(Explicit(ChatContent.Text "")) }
             | Tool(Some jarvis_tr, Some user_tr) ->

                 let newConvo =
                     state.Conversation
                     |> withNewChat (Jarvis(Implicit(ChatContent.Tool(Some jarvis_tr, Some user_tr))))
                     |> withNewChat (You(Implicit(ChatContent.Tool(Some jarvis_tr, Some user_tr))))

                 { state with
                     Conversation = newConvo
                     Message = "" |> ChatContent.Text |> Explicit |> Jarvis } //request jarvis again after tool use done to generate outcome
             | Tool(None, None)
             | Tool(_, None)
             | Tool(None, _) ->
                 state)
            llm
    | Quit -> ()

[<EntryPoint>]
let main argv =
    let isInternetAvailable () =
        try
            use ping = new Ping()
            let reply = ping.Send("8.8.8.8") // Ping Google DNS
            reply.Status = IPStatus.Success
        with _ ->
            false

    let initially =
        { Message = You(Explicit(ChatContent.Text ""))
          Conversation = List.Empty }

    match argv with
    | [| llm_param |] ->
        let llm =
            match llm_param with
            | "claude" -> Claude
            | "ollama" -> Ollama
            | _ ->
                // Fallback
                match isInternetAvailable () with
                | true -> Claude
                | false -> Ollama // Ollama can be used offline

        chat initially llm
    | _ ->
        let llm =
            match isInternetAvailable () with
            | true -> Claude
            | false -> Ollama // Ollama can be used offline

        chat initially llm

    0

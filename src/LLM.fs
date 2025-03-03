module LLM

open Domain
open System
open System.Net.Http
open System.Text
open System.Text.Json
open System.Threading
open FSharp.Control

let createPayload (convo: Conversation) (init: LLM) =
    let serializedConvo =
        convo
        |> List.map (fun x ->
            match x with
            | You msg ->
                match msg with
                | Implicit x ->
                    { role = "user"; content = x.SerializeUser() } //serialize user res
                | Explicit x ->
                    { role = "user"; content = x.Serialize() }
            | Jarvis msg -> 
                match msg with
                | Implicit x ->
                    { role = "assistant"; content = x.SerializeJarvis() } //serialize jarvis res
                | Explicit x ->
                    { role = "assistant"; content = x.Serialize() }
            | _ -> { role = ""; content = "" })
        |> List.filter (fun msg -> not (String.IsNullOrEmpty msg.role))
        |> List.toArray

    let payload =
        match init with
        | Ollama ->
            let p: Ollama.Payload =
                { model = "jarvis"
                  messages = serializedConvo
                  stream = true }

            JsonSerializer.Serialize(p)
        | Claude ->
            let p: Claude.Payload =
                { model = "claude-3-7-sonnet-20250219"
                  messages = serializedConvo
                  system =
                    "you will roleplay an ai agent character similar to that from iron man with jarvis or from interstellar with TARS. as an ai agent your aim is to elevate your clients intuition. be concise when needed. be detailed when needed. use your judgement to know when to be which. dont be too interactive. have a bit of conviction. dont be too empathetic and conversational. When providing code examples only show 1 example at a time.veer clear from providing to much information in the form of lists."
                  stream = true
                  max_tokens = 1024
                  tools =
                    [| Claude.Tool.write_note
                       Claude.Tool.record_mistake
                       Claude.Tool.record_thinking |] }

            JsonSerializer.Serialize(p)
        | MCP ->
            // For MCP, the actual payload is handled separately in the MCP module
            "{}"

    // let doc = JsonDocument.Parse(payload)
    // let prettyOptions = JsonSerializerOptions(WriteIndented = true)
    // let formatted = JsonSerializer.Serialize(doc, prettyOptions)
    // printfn "%A" formatted
    new StringContent(payload)

let makeRequest httpRequest parse (payload: StringContent) =
    // Check if this is an MCP request
    if httpRequest.RequestUri.ToString().StartsWith("http://localhost:8080") then
        // Use MCP's custom implementation
        MCP.makeRequest httpRequest parse payload
    else
        // Standard HTTP client for Claude and Ollama
        let client = new HttpClient()

        asyncSeq {
            // Construct the HttpRequestMessage
            use request = httpRequest

            try
                use! response =
                    client.SendAsync(request, HttpCompletionOption.ResponseHeadersRead)
                    |> Async.AwaitTask

                response.EnsureSuccessStatusCode() |> ignore

                use! stream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
                use reader = new System.IO.StreamReader(stream)

                while not reader.EndOfStream do
                    let! line = reader.ReadLineAsync() |> Async.AwaitTask

                    // printfn "%A" line
                    // printfn ""

                    if not (String.IsNullOrWhiteSpace(line)) then
                        match parse line with
                        | Ok(Data x) -> yield x
                        | Ok(Ended x) -> yield! AsyncSeq.empty
                        | Error _ -> yield! AsyncSeq.empty

            with
            | :? HttpRequestException as ex ->
                printfn "HTTP Request Error: %s" ex.Message
                yield! AsyncSeq.empty
            | ex ->
                printfn "Unexpected Error: %s" ex.Message
                yield! AsyncSeq.empty
        }

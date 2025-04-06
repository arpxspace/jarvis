module LLM

open Domain
open System
open System.Net.Http
open System.Text
open System.Text.Json
open System.Threading
open FSharp.Control
open System.IO
open System.Text.Json.Serialization
open FSharp.Json

let serializedConvo convo =
    convo
    |> List.map (fun x ->
        match x with
        | You msg ->
            match msg with
            | Implicit x ->
                { role = "user"
                  content = x.SerializeUser() } //serialize user res
            | Explicit x ->
                { role = "user"
                  content = x.Serialize() }
        | Jarvis msg ->
            match msg with
            | Implicit x ->
                { role = "assistant"
                  content = x.SerializeJarvis() } //serialize jarvis res
            | Explicit x ->
                { role = "assistant"
                  content = x.Serialize() }
        | _ -> { role = ""; content = "" })
    |> List.filter (fun msg -> not (String.IsNullOrEmpty msg.role))
    |> List.toArray

let createPayload state (llm: LLM) =
    let convo = state.Conversation
    let mcpTools = state.McpServerTools |> Array.collect id
    // let tools = Array.concat [mcpTools; [|Claude.Tool.write_note|]]

    let contextInfo =
        let currDir = Directory.GetCurrentDirectory() + string Path.DirectorySeparatorChar
        Utils.readFileAsync $"{currDir}CLAUDE.md"
        |> Async.RunSynchronously

    let payload =
        match llm with
        | Ollama ->
            let p: Ollama.Payload =
                { model = "deepseek-r1:14b"
                  messages = serializedConvo convo
                  stream = true }

            Json.serialize p
        | Claude ->
            let p: Claude.Payload =
                { model = "claude-3-5-sonnet-20241022"
                  messages = serializedConvo convo
                  system =
                    "you will roleplay an ai agent character similar to that from iron man with jarvis or from interstellar with TARS. as an ai agent your aim is to elevate your clients intuition. be concise when needed. be detailed when needed. use your judgement to know when to be which. dont be too interactive. have a bit of conviction. dont be too empathetic and conversational. When providing code examples only show 1 example at a time.veer clear from providing to much information in the form of lists. if you need to showcase code, show mini code snippets that are relevant to the answer instead of the whole thing at once"
                    + "\nHere is some specific information you should know for this session\n"
                    + contextInfo
                  stream = true
                  max_tokens = 1024
                  tools = Some (mcpTools |> Array.map _.ProtocolTool)  }

            JsonSerializer.Serialize p

            //TODO: Left here (11:39am)

    // let doc = JsonDocument.Parse(payload)
    // let prettyOptions = JsonSerializerOptions(WriteIndented = true)
    // let formatted = JsonSerializer.Serialize(doc, prettyOptions)
    // printfn "%A" payload
    new StringContent(payload)

let makeRequest httpRequest parse (payload: StringContent) =
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

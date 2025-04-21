module LLM

open Domain
open System
open System.Net
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
    
// Log conversation to log.txt
let logConversation (convo: Conversation) (llm: LLM) =
    try
        let logFilePath = Path.Combine(Directory.GetCurrentDirectory(), "log.txt")
        let timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")
        
        let serialized = 
            serializedConvo convo
            |> Array.map (fun msg -> sprintf "%s: %s" msg.role msg.content)
            |> String.concat "\n"
        
        let logEntry = sprintf "[%s] %s Conversation:\n%s\n\n" timestamp (llm.ToString()) serialized
        File.AppendAllText(logFilePath, logEntry)
    with ex ->
        printfn "Error logging conversation: %s" ex.Message

let createPayload state (llm: LLM) =
    let convo = state.Conversation
    let mcpTools = state.McpServerTools |> Array.collect id
    
    // Log the current conversation before creating the payload
    logConversation convo llm

    let contextInfo =
        let currDir = Directory.GetCurrentDirectory() + string Path.DirectorySeparatorChar
        Utils.readFileAsync $"{currDir}CLAUDE.md" |> Async.RunSynchronously

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
                    "you will roleplay an ai agent character similar to that from iron man with jarvis or from interstellar with TARS. as an ai agent your aim is to elevate your clients intuition. be concise when needed. be detailed when needed. use your judgement to know when to be which. dont be too interactive. have a bit of conviction. dont be too empathetic and conversational. When providing code examples only show 1 example at a time.veer clear from providing to much information in the form of lists. if you need to showcase code, show mini code snippets that are relevant to the answer instead of the whole thing at once. Before answering determine which directories in the file system you have access to and if you can, scan the users notes directory to see if they lack the pre-requisite knowledge or if they have enough pre-requisite knowledge in order to infer the answer theyre looking for. IF THEY DO HAVE PRE-REQUISITE KNOWLEDGE ON THE MATTER it is your responsibility to act on the role as a socratic questioner to guide the user to figuring it out on their own accord. Do one question at a time - don't bombard the user with more than one socractic question. Let them gradually uncover it for themselves. Respond naturally to the user without forcing a question at the end of every response."
                    // + "\nHere is some specific information you should know for this session\n"
                    // + contextInfo
                  stream = true
                  max_tokens = 1024
                  tools =
                    Some(
                        mcpTools
                        |> Array.map (fun x ->
                            // Direct mapping instead of serialize/deserialize
                            { name = x.ProtocolTool.Name
                              description = x.ProtocolTool.Description
                              input_schema =
                                x.ProtocolTool.InputSchema
                                |> JsonSerializer.Serialize
                                |> JsonSerializer.Deserialize<Claude.ToolInput>
                                |> (fun x ->
                                    { x with
                                        properties =
                                            x.properties
                                            |> Map.map (fun k v ->
                                                if isNull v.description then
                                                    { v with description = "" }
                                                else
                                                    v)
                                        required = if isNull x.required then [||] else x.required }) })
                    ) }

            JsonSerializer.Serialize p

    // let doc = JsonDocument.Parse(payload)
    // let prettyOptions = JsonSerializerOptions(WriteIndented = true)
    // let formatted = JsonSerializer.Serialize(doc, prettyOptions)
    // printfn "%A" formatted
    // printfn ""
    new StringContent(payload)

let makeRequest httpRequest parse (payload: StringContent) =
    let client = new HttpClient()

    // Function to log raw responses to log.txt
    // let logRawResponse (line: string) =
    //     try
    //         let logFilePath = Path.Combine(Directory.GetCurrentDirectory(), "log.txt")
    //         let timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")
    //         let logEntry = sprintf "[%s] Raw Response: %s\n" timestamp line
    //         File.AppendAllText(logFilePath, logEntry)
    //     with ex ->
    //         printfn "Error logging response: %s" ex.Message

    asyncSeq {
        // Construct the HttpRequestMessage
        use request = httpRequest

        try
            use! response =
                client.SendAsync(request, HttpCompletionOption.ResponseHeadersRead)
                |> Async.AwaitTask

            // let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask

            // if response.StatusCode = HttpStatusCode.BadRequest then
            //     printfn "400 Error - Response Body: %s" content
            // // Parse error response if possible to extract validation messages

            response.EnsureSuccessStatusCode() |> ignore

            use! stream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
            use reader = new System.IO.StreamReader(stream)

            while not reader.EndOfStream do
                let! line = reader.ReadLineAsync() |> Async.AwaitTask

                // printfn "%A" line
                // printfn ""

                if not (String.IsNullOrWhiteSpace(line)) then
                    // logRawResponse line
                    match parse line with
                    | Ok(Data x) -> yield x
                    | Ok(Ended x) -> yield! AsyncSeq.empty
                    | Error _ -> yield! AsyncSeq.empty

        with
        | :? HttpRequestException as ex ->
            printfn "HTTP Request Error: %A" ex
            yield! AsyncSeq.empty
        | ex ->
            printfn "Unexpected Error: %s" ex.Message
            yield! AsyncSeq.empty
    }

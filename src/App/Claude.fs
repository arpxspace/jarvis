module Claude

open Domain
open System
open System.Net.Http
open System.Text
open System.Text.Json
open System.Threading
open FSharp.Control
open dotenv.net

DotEnv.Load() |> ignore

let envVars = DotEnv.Read()

// ABSTRACT START
type ChatMessage = { role: string; content: string }

type ChatRequest =
    { model: string
      messages: ChatMessage[]
      max_tokens: int
      stream: bool }

type ContentBlock = { ``type``: string; text: string }

type ContentBlockStart =
    { ``type``: string
      index: int
      content_block: ContentBlock }

type ContentBlockDelta =
    { ``type``: string
      index: int
      delta: ContentBlock }

type ContentBlockStop = { ``type``: string; index: int }

type MessageStop = { ``type``: string }
//ABSTRACT END

let jsonOptions = JsonSerializerOptions(PropertyNameCaseInsensitive = true)

let createPayload model (convo: Conversation) =
    let serializedConvo: ChatMessage[] =
        convo
        |> List.map (fun x ->
            match x with
            | You msg -> { role = "user"; content = msg }
            | Jarvis msg -> { role = "assistant"; content = msg }
            | _ -> { role = ""; content = "" })
        |> List.filter (fun msg -> not (String.IsNullOrEmpty msg.role))
        |> List.toArray

    let payload =
        { model = model
          messages = serializedConvo
          max_tokens = 500
          stream = true }

    let json = JsonSerializer.Serialize(payload)
    new StringContent(json)

let makeRequest (payload: StringContent) =
    let client = new HttpClient()

    asyncSeq {
        // Construct the HttpRequestMessage

        // ABSTRACT START
        use request = new HttpRequestMessage()
        request.Method <- HttpMethod.Post
        request.RequestUri <- Uri("https://api.anthropic.com/v1/messages")
        request.Content <- payload
        request.Headers.Add("x-api-key", envVars["CLAUDE"])
        request.Headers.Add("anthropic-version", "2023-06-01")
        payload.Headers.ContentType <- System.Net.Http.Headers.MediaTypeHeaderValue("application/json")
        // ABSTRACT END
        try
            // Send the POST request asynchronously with cancellation support
            use! response =
                client.SendAsync(request, HttpCompletionOption.ResponseHeadersRead)
                |> Async.AwaitTask

            // Ensure the response indicates success
            response.EnsureSuccessStatusCode() |> ignore

            // Get the response stream
            use! stream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
            use reader = new System.IO.StreamReader(stream)

            // Read the stream line by line asynchronously
            while not reader.EndOfStream do
                // Respect the cancellation request
                // Read a line asynchronously
                let! line = reader.ReadLineAsync() |> Async.AwaitTask

                if not (String.IsNullOrWhiteSpace(line)) then
                    //ABSTRACT START
                    let data = line.Substring 6
                    try
                        let chatType = JsonSerializer.Deserialize<{| ``type``: string |}>(data, jsonOptions)

                        match chatType.``type`` with
                        | "content_block_start"
                        | "content_block_delta" ->
                            let response = JsonSerializer.Deserialize<ContentBlockDelta>(data, jsonOptions)
                            // Yield the content for processing
                            yield response.delta.text
                        | "content_block_stop"
                        | "message_stop" ->
                            yield! AsyncSeq.empty
                        | _ -> ()

                    with
                    | :? JsonException as ex ->
                        // Handle JSON deserialization errors
                        // printfn "JSON Deserialization Error: %s" ex.Message
                        ()
                    | ex ->
                        // Handle other exceptions
                        // printfn "Error: %s" ex.Message
                        ()

                    //ABSTRACT END
        with
        | :? HttpRequestException as ex ->
            printfn "HTTP Request Error: %s" ex.Message
            exit 0
        | ex ->
            printfn "Unexpected Error: %s" ex.Message
            exit 0
    }

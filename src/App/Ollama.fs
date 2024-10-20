module Ollama

open Domain
open System
open System.Net.Http
open System.Text
open System.Text.Json
open System.Threading
open FSharp.Control

// Define the request payload types
type ChatMessage = { role: string; content: string }

type ChatRequest =
    { model: string
      messages: ChatMessage[]
      stream: bool }

type ChatResponse =
    { model: string
      message: ChatMessage
      _done: bool }

let jsonOptions =
    JsonSerializerOptions(
        PropertyNameCaseInsensitive = true
    // Add other options if needed
    )

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
          stream = true }

    let json = JsonSerializer.Serialize(payload)
    new StringContent(json)

let makeRequest (payload: StringContent) =
    let client = new HttpClient()


    asyncSeq {
        // Construct the HttpRequestMessage
        use request =
            new HttpRequestMessage(HttpMethod.Post, "http://localhost:11434/api/chat")

        request.Content <- payload

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
                    try
                        // Deserialize the JSON line into ChatResponse
                        let chatResponse = JsonSerializer.Deserialize<ChatResponse>(line, jsonOptions)

                        if not chatResponse._done then
                            // printf "%s" chatResponse.message.content
                            // Yield the content for processing
                            yield chatResponse.message.content
                        else
                            // If _done is true, terminate the stream
                            yield! AsyncSeq.empty
                    with
                    | :? JsonException as ex ->
                        // Handle JSON deserialization errors
                        printfn "JSON Deserialization Error: %s" ex.Message
                    | ex ->
                        // Handle other exceptions
                        printfn "Error: %s" ex.Message

        with
        | :? HttpRequestException as ex -> printfn "HTTP Request Error: %s" ex.Message
        | ex -> printfn "Unexpected Error: %s" ex.Message
    }

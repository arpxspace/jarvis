// StreamResponse.fsx
// This script sends an HTTP POST request and streams the response asynchronously.

// Install the necessary NuGet package for AsyncSeq
#r "nuget: FSharp.Control.AsyncSeq, 2.1.1"

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

// Define types to represent the streaming response
type ChatResponseMessage = { role: string; content: string }

type ChatResponse =
    { model: string
      message: ChatResponseMessage
      _done: bool }

// Create a single, reusable instance of HttpClient
let httpClient =
    let client = new HttpClient()
    // Optionally set default headers or other configurations here
    client

// Define JsonSerializer options for case-insensitive deserialization
let jsonOptions =
    JsonSerializerOptions(
        PropertyNameCaseInsensitive = true
    // Add other options if needed
    )

/// Streams the API response as an asynchronous sequence of strings.
/// Parameters:
/// - url: The API endpoint URL.
/// - payload: The ChatRequest payload.
/// - cancellationToken: Token to support cancellation.
/// Returns: An AsyncSeq<string> representing the streamed content.
let streamResponse (url: string) (payload: ChatRequest) =
    asyncSeq {
        // Serialize the payload to JSON
        let jsonPayload = JsonSerializer.Serialize(payload)
        use content = new StringContent(jsonPayload, Encoding.UTF8, "application/json")

        // Construct the HttpRequestMessage
        use request = new HttpRequestMessage(HttpMethod.Post, url)
        request.Content <- content

        try
            // Send the POST request asynchronously with cancellation support
            use! response =
                httpClient.SendAsync(request, HttpCompletionOption.ResponseHeadersRead)
                |> Async.AwaitTask

            // Ensure the response indicates success
            response.EnsureSuccessStatusCode() |> ignore

            // Inform about the successful connection
            printfn "Connected to %s" url

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

/// Processes and prints the streamed responses.
/// Parameters:
/// - stream: An AsyncSeq<string> representing the streamed content.
/// Returns: An asynchronous workflow.
let processStream (stream: AsyncSeq<string>) =
    async {
        try
            // Iterate over the asynchronous sequence
            do!
                stream
                |> AsyncSeq.iter (fun content ->
                    // Print each chunk of content as it's received
                    printf "%s" content)
            // Print a newline at the end for better formatting
            printfn ""
        with
        | :? OperationCanceledException -> printfn "Streaming was canceled."
        | ex -> printfn "An error occurred during streaming: %s" ex.Message
    }

// Define the API endpoint
let url = "http://localhost:11434/api/chat"

// Define the request payload
let payload: ChatRequest =
    { model = "jarvis"
      messages =
        [| { role = "user"
             content = "why is the sky blue?" } |]
      stream = true }

// Start the streaming and processing asynchronously
let streaming = streamResponse url payload |> processStream

// Run the asynchronous workflow
streaming |> Async.RunSynchronously

// Dispose of the HttpClient when the application exits
httpClient.Dispose()

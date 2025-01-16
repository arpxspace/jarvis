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

let jsonOptions = JsonSerializerOptions(PropertyNameCaseInsensitive = true)

let httpRequest payload = 
    new HttpRequestMessage(HttpMethod.Post, "http://localhost:11434/api/chat", Content=payload)

let parse (line:string) = 
    try
        // Deserialize the JSON line into ChatResponse
        let chatResponse = JsonSerializer.Deserialize<ChatResponse>(line, jsonOptions)

        if not chatResponse._done then
            // printf "%s" chatResponse.message.content
            // Yield the content for processing
            Ok (Data chatResponse.message.content)
        else
            // If _done is true, terminate the stream
            Ok (Ended AsyncSeq.empty)
    with
    | :? JsonException as ex ->
        // Handle JSON deserialization errors
        printfn "JSON Deserialization Error: %s" ex.Message
        exit 0
    | ex ->
        // Handle other exceptions
        printfn "Error: %s" ex.Message
        exit 0


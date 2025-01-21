module Ollama

open Domain
open System
open System.Net.Http
open System.Text
open System.Text.Json
open System.Threading
open FSharp.Control

type ChatRequest =
    { model: string
      messages: ChatMessage[]
      stream: bool }

type ChatResponse =
    { model: string
      message: ChatMessage
      _done: bool }

[<RequireQualifiedAccess>]
type Payload =
    { model: string
      messages: ChatMessage[]
      stream: bool }

let jsonOptions = JsonSerializerOptions(PropertyNameCaseInsensitive = true)

let httpRequest payload =
    new HttpRequestMessage(HttpMethod.Post, "http://localhost:11434/api/chat", Content = payload)

let parse (line: string) =
    try
        // Deserialize the JSON line into ChatResponse
        let chatResponse = JsonSerializer.Deserialize<ChatResponse>(line, jsonOptions)

        if not chatResponse._done then
            Ok(Data (ReceivedText chatResponse.message.content))
        else
            Ok(Ended AsyncSeq.empty)
    with
    | :? JsonException as ex ->
        // Handle JSON deserialization errors
        printfn "JSON Deserialization Error: %s" ex.Message
        exit 0
    | ex ->
        // Handle other exceptions
        printfn "Error: %s" ex.Message
        exit 0

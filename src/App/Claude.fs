module Claude

open Domain
open System
open System.Net.Http
open System.Text
open System.Text.Json
open System.Threading
open FSharp.Control
open System.IO
open System.Reflection

let key = Environment.GetEnvironmentVariable("CLAUDE")

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

let jsonOptions = JsonSerializerOptions(PropertyNameCaseInsensitive = true)

let httpRequest payload =
    let mutable request = new HttpRequestMessage()
    request.Method <- HttpMethod.Post
    request.RequestUri <- Uri("https://api.anthropic.com/v1/messages")
    request.Content <- payload
    request.Headers.Add("x-api-key", key)
    request.Headers.Add("anthropic-version", "2023-06-01")
    payload.Headers.ContentType <- System.Net.Http.Headers.MediaTypeHeaderValue("application/json")
    request

let parse (line: string) =
    let data = line.Substring 6

    try
        let chatType = JsonSerializer.Deserialize<{| ``type``: string |}>(data, jsonOptions)

        match chatType.``type`` with
        | "content_block_start"
        | "content_block_delta" ->
            let response = JsonSerializer.Deserialize<ContentBlockDelta>(data, jsonOptions)
            Ok(Data response.delta.text)
        | "content_block_stop"
        | "message_stop" -> Ok(Ended AsyncSeq.empty)
        | _ -> Error()

    with
    | :? JsonException as ex ->
        // Handle JSON deserialization errors
        // printfn "JSON Deserialization Error: %s" ex.Message
        Error ()
    | ex ->
        // Handle other exceptions
        // printfn "Error: %s" ex.Message
        Error ()

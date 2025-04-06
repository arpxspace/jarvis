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

let key = Environment.GetEnvironmentVariable("ANTHROPIC_API_KEY")

type PropertyMetadata =
    { ``type``: string
      description: string }

type ToolInput =
    { ``type``: string
      required: string array
      properties: Map<string, PropertyMetadata> }

type Tool =
    { name: string
      description: string
      input_schema: ToolInput }

module Tool =
    let write_note: Tool =
        let input_schema: ToolInput =
            { ``type`` = "object"
              required = [| "note"; "filename" |]
              properties =
                Map.ofList
                    [ "note",
                      { ``type`` = "string"
                        description = "The content of the note to be written which contains information ONLY from the current discussion. NO new information that wasnt explicitly mentioned in the existing conversation should be present in this content. Include any code snippets if necessary" }
                      "filename",
                      { ``type`` = "string"
                        description = "A unix friendly filename for the note" } ] }

        { name = "write_note"
          description =
            "IF the user explicitly specifies that they want you to write a note THEN Write to the filesystem an aggregated literature note of the conversation that has taken place in the style of a blog post. come up with a comprehensive and easy to digest literature note. write the note in the style of having thought through somethig. dont end it with any closings. Also never prompt the user if they would like you to write a note. let them proactively specify that if they wish"
          input_schema = input_schema }

    let think: Tool =
        let input_schema: ToolInput =
            { ``type`` = "object"
              required = [| "thought" |]
              properties =
                Map.ofList
                    [ "thought",
                      { ``type`` = "string"
                        description = "A thought to think about." } ] }

        { name = "think"
          description =
            "Use the tool to think about something. It will not obtain new information or change the database, but just append the thought to the log. Use it when complex reasoning or some cache memory is needed like specific information from the system prompt."
          input_schema = input_schema }

type InputJsonDelta =
    { ``type``: string // "input_json_delta"
      partial_json: string }

type TextDelta =
    { ``type``: string // "text_delta"
      text: string }

[<RequireQualifiedAccess>]
type DeltaContent =
    | InputJson of InputJsonDelta
    | Text of TextDelta

[<RequireQualifiedAccess>]
type ContentBlock =
    | Tool of ToolContentBlock
    | Text of TextDelta

type ContentBlockConverter() =
    inherit System.Text.Json.Serialization.JsonConverter<ContentBlock>()

    override _.Read(reader: byref<Utf8JsonReader>, _: Type, options: JsonSerializerOptions) =
        // Read the JSON object into a JsonDocument
        let doc = JsonDocument.ParseValue(&reader)
        let root = doc.RootElement

        // Check the "type" field to determine which case to deserialize
        match root.GetProperty("type").GetString() with
        | "tool_use" ->
            let delta = JsonSerializer.Deserialize<ToolContentBlock>(root.GetRawText(), options)
            ContentBlock.Tool delta
        | "text" ->
            let delta = JsonSerializer.Deserialize<TextDelta>(root.GetRawText(), options)
            ContentBlock.Text delta
        | unknown -> failwith $"Unknown delta type: {unknown}"

    override _.Write(writer: Utf8JsonWriter, value: ContentBlock, options: JsonSerializerOptions) =
        match value with
        | ContentBlock.Tool delta -> JsonSerializer.Serialize(writer, delta, options)
        | ContentBlock.Text delta -> JsonSerializer.Serialize(writer, delta, options)

type DeltaContentConverter() =
    inherit System.Text.Json.Serialization.JsonConverter<DeltaContent>()

    override _.Read(reader: byref<Utf8JsonReader>, _: Type, options: JsonSerializerOptions) =
        // Read the JSON object into a JsonDocument
        let doc = JsonDocument.ParseValue(&reader)
        let root = doc.RootElement

        // Check the "type" field to determine which case to deserialize
        match root.GetProperty("type").GetString() with
        | "input_json_delta" ->
            let delta = JsonSerializer.Deserialize<InputJsonDelta>(root.GetRawText(), options)
            DeltaContent.InputJson delta
        | "text_delta" ->
            let delta = JsonSerializer.Deserialize<TextDelta>(root.GetRawText(), options)
            DeltaContent.Text delta
        | unknown -> failwith $"Unknown delta type: {unknown}"

    override _.Write(writer: Utf8JsonWriter, value: DeltaContent, options: JsonSerializerOptions) =
        match value with
        | DeltaContent.InputJson delta -> JsonSerializer.Serialize(writer, delta, options)
        | DeltaContent.Text delta -> JsonSerializer.Serialize(writer, delta, options)


type ContentBlockStart =
    { ``type``: string
      index: int
      content_block: ContentBlock }

type ContentBlockDelta =
    { ``type``: string
      index: int
      delta: DeltaContent }

type ContentBlockStop = { ``type``: string; index: int }

type MessageStop = { ``type``: string }

[<RequireQualifiedAccess>]
type Payload =
    { model: string
      messages: ChatMessage[]
      system: string
      stream: bool
      max_tokens: int
      tools: ModelContextProtocol.Client.McpClientTool array option }

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
        // let doc = JsonDocument.Parse(data)
        // let prettyOptions = JsonSerializerOptions(WriteIndented = true)
        // let formatted = JsonSerializer.Serialize(doc, prettyOptions)
        // printfn "%s" formatted

        let chatType = JsonSerializer.Deserialize<{| ``type``: string |}>(data, jsonOptions)

        match chatType.``type`` with
        | "content_block_start" ->
            let jsonOptions = JsonSerializerOptions()
            jsonOptions.Converters.Add(ContentBlockConverter())
            let start = JsonSerializer.Deserialize<ContentBlockStart>(data, jsonOptions)

            // printfn "start %A" start
            let event = 
                match start.content_block with
                | ContentBlock.Tool tool -> 
                    RequiresTool tool
                | ContentBlock.Text data ->
                    ReceivedResponse ""

            Ok(Data event)
        | "content_block_delta" ->
            let jsonOptions = JsonSerializerOptions()
            jsonOptions.Converters.Add(DeltaContentConverter())

            let response = JsonSerializer.Deserialize<ContentBlockDelta>(data, jsonOptions)

            match response.delta with
            | DeltaContent.InputJson delta ->
                Ok(Data (ConstructingToolSchema delta.partial_json))
            | DeltaContent.Text delta ->
                Ok(Data (ReceivedResponse delta.text))
        | "content_block_stop" ->
            Ok(Data CallTool)

        | "message_stop" -> Ok(Ended AsyncSeq.empty)
        | _ -> Error()

    with
    | :? JsonException as ex -> Error()
    | ex -> Error()

module Domain
open FSharp.Control
open System
open System.Text.Json
open System.Text.Json.Serialization

type ChatMessage = { role: string; content: string }

type ToolData = 
    | WriteNote of schema: string
    | RecordThinking of schema: string
    | RecordMistake of schema: string
    with
        static member fromString str =
            match str with
            | "write-note" -> Some (WriteNote "")
            | "record-thinking" -> Some (RecordThinking "")
            | "record-mistake" -> Some (RecordMistake "")
            | _ -> None

        member this.Name =
            match this with
            | WriteNote _ -> "write-note"
            | RecordThinking _ -> "record-thinking"
            | RecordMistake _ -> "record-mistake"

        member this.UpdateSchema partial =
            match this with
            | WriteNote schema -> WriteNote (schema + partial)
            | RecordThinking schema -> RecordThinking (schema + partial)
            | RecordMistake schema -> RecordMistake (schema + partial)

type Event =
    | ReceivedText of string
    | RequiresTool of name: string
    | ConstructingToolSchema of partial_json: string
    | BlockFinished
    with
        /// This is for sending it to 'prettified-output' go program for rendering
        member this.Serialize tool text =
            match this with
            | ReceivedText text -> 
                {| Text = text; Tool = ""; Event = "received-text"|} |> JsonSerializer.Serialize  
            | RequiresTool tool ->
                {| Text = text |> Option.defaultValue ""; Tool = tool; Event = "requires-tool" |} |> JsonSerializer.Serialize
            | ConstructingToolSchema _ ->
                {| Text = text |> Option.defaultValue ""; Tool = tool |> Option.defaultValue ""; Event = "constructing-tool"|} |> JsonSerializer.Serialize
            | BlockFinished ->
                {| Text = ""; Tool = ""; Event = "block-finished"|} |> JsonSerializer.Serialize

[<RequireQualifiedAccess>]
type Context = {
    Text: string
    Tool: ToolData option
}

type ParseResponse = 
    | Data of Event
    | Ended of AsyncSeq<string>

type LLM =
    | Claude
    | Ollama

type Message =
    | You of msg: string
    | Jarvis of msg: string
    | Quit

type Conversation = Message list

type State =
    { Message: Message
      Conversation: Conversation }



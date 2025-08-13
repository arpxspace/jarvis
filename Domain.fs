module Domain
open FSharp.Control
open System
open System.Text.Json
open System.Text.Json.Serialization
open System.IO
open MCP

type UserToolResponse = {
    ``type``: string
    tool_use_id: string
    content: string option
}

type ToolContentBlock =
    { ``type``: string
      id: string
      name: string
      input: obj }

type JarvisToolResponseText = {
    ``type``: string;
    text: string
}

type JarvisToolResponse =
    | JarvisToolResponse of (JarvisToolResponseText * ToolContentBlock)
    with
        static member Create (schema: string) (text: string) (tool: ToolContentBlock) =
            let responseText = {
                ``type`` = "text"
                text = text
            }

            (responseText, tool) |> JarvisToolResponse
        member this.Value =
            match this with
            | JarvisToolResponse value -> value

        member this.Serialize()=
            match this with
            | JarvisToolResponse (text, tool) ->
                [|
                    text :> obj
                    tool :> obj
                |]

type ChatContent =
    | Tool of JarvisToolResponse option  * UserToolResponse option
    | Text of string
    with
        member this.Serialize () =
            match this with
            | Text str ->
                str
            | _ ->
                ""

        member this.SerializeJarvis () =
            match this with
            | Tool (jarvis, _) ->
                jarvis
                |> Option.map (fun x -> x.Serialize()) //need to inner serialize for jarvis res
                |> JsonSerializer.Serialize
            | _ ->
                ""

        member this.SerializeUser () =
            match this with
            | Tool (_, user) ->
                user |> JsonSerializer.Serialize
            | _ ->
                ""

type ChatMessage = { role: string; content: string }

type ToolData =
    | WriteNote of schema: string * ToolContentBlock
    | Think of schema: string * ToolContentBlock
    with
        static member init str tool =
            match str with
            | "write_note" -> Some (WriteNote ("", tool))
            | "think" -> Some (Think ("", tool))
            | _ -> None

        member this.Name =
            match this with
            | WriteNote _ -> "write_note"
            | Think _ -> "think"

        member this.UpdateSchema partial =
            match this with
            | WriteNote (schema,id) -> WriteNote ((schema + partial), id)
            | Think (schema,id) -> Think ((schema + partial), id)

        member this.Finalize () =
            match this with
            | WriteNote (schema, tool)
            | Think (schema, tool) ->
                {tool with input = JsonDocument.Parse(schema)}

type Event =
    | ReceivedResponse of string
    | RequiresTool of ToolContentBlock
    | ConstructingToolSchema of partial_json: string
    | CallTool
    with
        /// This is for sending it to 'prettified-output' go program for rendering
        member this.Serialize tool text =
            match this with
            | ReceivedResponse res ->
                {| Text = res; Tool = ""; Event = "received-text"|} |> JsonSerializer.Serialize
            | RequiresTool {name = tool; id = id} ->
                // Don't include text in the requires-tool event - show only that a tool is being called
                {| Text = ""; Tool = tool; Event = "requires-tool" |} |> JsonSerializer.Serialize
            | ConstructingToolSchema partial ->
                // Don't send the JSON schema as Text - just display that tool construction is in progress
                {| Text = ""; Tool = tool |> Option.defaultValue ""; Event = "constructing-tool"|} |> JsonSerializer.Serialize
            | CallTool ->
                {| Text = text |> Option.defaultValue ""; Tool = tool; Event = "block-finished"|} |> JsonSerializer.Serialize

[<RequireQualifiedAccess>]
type ParseContext = {
    Response: ChatContent
    Tool: ToolContentBlock option * string
}

type ParseStatus =
    | Data of Event
    | Ended of AsyncSeq<string>

//TODO: reference in future
type OllamaModel =
    | DeepSeek
    | LLaMA
with
    member this.Serialize () =
        match this with
        | DeepSeek -> "deepseek-r1"
        | LLaMA -> "jarvis"

type LLM =
    | Claude of string
    | Ollama of string

type MessageMode =
    | Implicit of ChatContent //tool use, mcp
    | Explicit of ChatContent //text generation

type Message =
    | You of MessageMode
    | Jarvis of MessageMode
    | Quit

type Conversation = Message list

type State =
    { Message: Message
      WithLogging: bool
      McpServerTools: ModelContextProtocol.Client.McpClientTool array array
      Conversation: Conversation }

module Utils =
    let readFileAsync (path: string) = async {
        try
            let! content = File.ReadAllTextAsync(path) |> Async.AwaitTask
            return content
        with
        | ex -> return "no content"
    }


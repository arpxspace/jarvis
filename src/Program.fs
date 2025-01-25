open System
open System.Threading
open FSharp.Control
open System.Net.Http
open System.Text
open System.Text.Json
open Domain
open System.Diagnostics
open System.IO
open System.Threading.Tasks
open Spectre
open Spectre.Console

module UI =
    let display (state: State) =

        let printBold (text: string) = AnsiConsole.Markup($"[bold]{text}[/]")

        match state.Message with
        | You _ ->
            printBold ">>>"
            printf " "
        | _ -> ()

        state

let await f = f |> Async.RunSynchronously

let withNewChat (msg: Message) (convo: Conversation) =
    match msg with
    | Quit -> convo
    | You msg -> [ yield! convo; You msg ]
    | Jarvis msg -> [ yield! convo; Jarvis msg ]

let ask llm state : string =
    let (payload, httpRequest) =
        match llm with
        | Ollama ->
            let model = "jarvis"
            let payload = LLM.createPayload state.Conversation Ollama
            let request = Ollama.httpRequest payload
            (payload, request)
        | Claude ->
            let model = "claude-3-5-sonnet-20241022"
            let payload = LLM.createPayload state.Conversation Claude
            let request = Claude.httpRequest payload
            (payload, request)

    let parseHandler =
        match llm with
        | Ollama -> Ollama.parse
        | Claude -> Claude.parse

    let jsonOptions = JsonSerializerOptions(PropertyNameCaseInsensitive = true)

    LLM.makeRequest httpRequest parseHandler { Tool = None; Text = "" } payload
    |> (fun stream ->
        async {
            let startInfo = ProcessStartInfo()
            startInfo.FileName <- "/Users/amirpanahi/Documents/projects/jarvis/prettified-output/main"
            startInfo.UseShellExecute <- false
            startInfo.RedirectStandardInput <- true

            use proc = Process.Start(startInfo)
            use stdin = proc.StandardInput

            let! res =
                stream
                |> AsyncSeq.foldAsync<Context, Event>
                    (fun acc content ->
                        async {
                            // printfn "%A" acc
                            // printfn ""

                            match content with
                            | ReceivedText text ->
                                do! stdin.WriteAsync (content.Serialize None (Some text)) |> Async.AwaitTask
                                do! stdin.FlushAsync() |> Async.AwaitTask
                                return { acc with Text = acc.Text + text }
                            | RequiresTool tool ->
                                do! stdin.WriteAsync (content.Serialize (Some tool) None) |> Async.AwaitTask
                                do! stdin.FlushAsync() |> Async.AwaitTask
                                return
                                    { acc with
                                        Tool = Some({ Name = tool; Schema = "" }) }
                            | ConstructingToolSchema partial ->
                                match acc.Tool with
                                | Some tool ->
                                    do! stdin.WriteAsync (content.Serialize (Some tool.Name) None) |> Async.AwaitTask
                                    do! stdin.FlushAsync() |> Async.AwaitTask
                                    return
                                        { acc with
                                            Tool = Some { tool with Schema = tool.Schema + partial } }
                                | None -> return acc
                            | BlockFinished ->
                                match acc.Tool with
                                | Some tool when tool.Name = "write_note" ->
                                    let input =
                                        JsonSerializer.Deserialize<Tools.WriteNoteSchema>(
                                            tool.Schema,
                                            jsonOptions
                                        )

                                    let filepath = $"/Users/amirpanahi/notes/literature/{input.filename}"
                                    File.AppendAllText(filepath, input.note)
                                    let text = $"\n\n**Written to: {filepath}**"
                                    do! stdin.WriteAsync (content.Serialize (Some tool.Name) (Some text)) |> Async.AwaitTask
                                    do! stdin.WriteAsync(text) |> Async.AwaitTask
                                    do! stdin.FlushAsync() |> Async.AwaitTask
                                    AnsiConsole.MarkupLine text

                                    return
                                        { acc with
                                            Tool = None
                                            Text = acc.Text + text }
                                | _ -> return acc
                        })
                    { Text = ""; Tool = None }

            stdin.Close()
            proc.WaitForExit()
            // printfn ""
            return res.Text
        })
    |> await

let withNewestPrompt state = List.last state.Conversation

let rec chat (state: State) (llm: LLM) =
    match state.Message with
    | You prompt ->
        state |> UI.display |> ignore

        let input = System.Console.ReadLine()

        let newState =
            match input with
            | "exit"
            | "quit" -> { state with Message = Quit }
            | "/end" ->
                { state with
                    Message = Jarvis ""
                    Conversation = withNewChat (You prompt) state.Conversation } //end
            | str ->
                { state with
                    Message = You(prompt + "\n" + str) }

        chat newState llm
    | Jarvis said ->
        state |> UI.display |> ignore

        //ask for jarvis input -> ollama rest api call
        let response = state |> ask llm

        chat
            { state with
                Conversation = withNewChat (Jarvis response) state.Conversation
                Message = You "" }
            llm
    | Quit -> ()

[<EntryPoint>]
let main argv =
    match argv with
    | [| llm_param |] ->
        let llm =
            match llm_param with
            | "claude" -> Claude
            | "ollama" -> Ollama
            | _ -> Ollama

        let initially =
            { Message = You ""
              Conversation = List.Empty }

        chat initially llm
    | _ -> printfn "Usage: jarvis <llm>"

    0

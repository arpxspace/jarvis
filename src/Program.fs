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

        let printBold (text: string) = printf "\u001b[1m%s\u001b[0m" text

        match state.Message with
        | You _ ->
            printBold ">>>"
            printf " "
        | _ -> ()

        state

let printBold (text: string) = printf "\u001b[1m%s\u001b[0m" text

let countWrappedLines (text: string) =
    let terminalWidth = Console.WindowWidth

    let folder (currentCol, totalLines) c =
        match c with
        | '\n' -> (0, totalLines + 1)
        | _ when currentCol + 1 >= terminalWidth -> (1, totalLines + 1)
        | _ -> (currentCol + 1, totalLines)

    text |> Seq.fold folder (0, 0) |> snd

let clearUpToLine n =
    printf "\u001b[%dA" n
    printf "\u001b[G"
    printf "\u001b[0J"

let withNewChat (msg: Message) (convo: Conversation) =
    match msg with
    | Quit -> convo
    | You msg -> [ yield! convo; You msg ]
    | Jarvis msg -> [ yield! convo; Jarvis msg ]

let renderMarkdown (res: string) =
    async {
        let startInfo = ProcessStartInfo()
        startInfo.FileName <- "glow"
        startInfo.Arguments <- "-" // Read from stdin
        startInfo.RedirectStandardInput <- true
        startInfo.UseShellExecute <- false

        use _process = new Process()
        _process.StartInfo <- startInfo
        _process.Start() |> ignore

        do! _process.StandardInput.WriteLineAsync(res) |> Async.AwaitTask
        _process.StandardInput.Close()

        do! _process.WaitForExitAsync() |> Async.AwaitTask
    }

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

    LLM.makeRequest httpRequest parseHandler payload
    |> (fun stream ->
        async {
            let startInfo = ProcessStartInfo()
            startInfo.FileName <- "../prettified-output/main"
            startInfo.UseShellExecute <- false
            startInfo.RedirectStandardInput <- true

            use proc = Process.Start(startInfo)
            use stdin = proc.StandardInput

            let! res =
                stream
                |> AsyncSeq.foldAsync (fun acc content ->
                    async {
                        do! stdin.WriteAsync(content) |> Async.AwaitTask
                        do! stdin.FlushAsync() |> Async.AwaitTask
                        return acc + content
                    }) ""

            stdin.Close()
            proc.WaitForExit()

            printfn ""
            return res
        })
    |> Async.RunSynchronously

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
        let response =
            state
            |> ask llm

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

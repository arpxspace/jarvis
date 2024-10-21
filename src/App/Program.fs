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

let mutable lastLineCount = 0

module UI =
    let spinner () =
        let mutable running = true

        while running do
            for c in "|/-\\" do
                Console.Write(sprintf "\r%c" c)
                Thread.Sleep(100)

            if Console.KeyAvailable && Console.ReadKey(true).Key = ConsoleKey.Escape then
                running <- false

    let saveStartingPoint () = printf "\u001b[s"

    let printInPlaceSmooth (text: string) =
        async {
            let startInfo = ProcessStartInfo()
            startInfo.FileName <- "glow"
            startInfo.Arguments <- "-" // Read from stdin
            startInfo.RedirectStandardInput <- true
            startInfo.RedirectStandardOutput <- true
            startInfo.UseShellExecute <- false
            use _process = new Process()
            _process.StartInfo <- startInfo
            _process.Start() |> ignore
            do! _process.StandardInput.WriteLineAsync(text) |> Async.AwaitTask
            _process.StandardInput.Close()
            let! output = _process.StandardOutput.ReadToEndAsync() |> Async.AwaitTask
            do! _process.WaitForExitAsync() |> Async.AwaitTask

            // Move cursor up by the number of lines we printed last time
            if lastLineCount > 0 then
                printf "\u001b[%dA" lastLineCount

            // Split output into lines
            let lines = output.Split('\n')
            lastLineCount <- lines.Length

            // Print each line, clearing to the end of line for each
            for line in lines do
                printf "%s\u001b[K\n" line

            // Move cursor up to the end of our output
            printf "\u001b[%dA" lines.Length
        }

    let printInPlace (text: string) =
        async {
            //move cursor back to starting point
            printf "\u001b[u"

            // //clear everything
            printf "\u001b[0J"

            let startInfo = ProcessStartInfo()
            startInfo.FileName <- "glow"
            startInfo.Arguments <- "-" // Read from stdin
            startInfo.RedirectStandardInput <- true
            startInfo.UseShellExecute <- false

            use _process = new Process()
            _process.StartInfo <- startInfo
            _process.Start() |> ignore

            do! _process.StandardInput.WriteLineAsync(text) |> Async.AwaitTask
            _process.StandardInput.Close()

            do! _process.WaitForExitAsync() |> Async.AwaitTask

        }

    let display (state: State) =

        let printBold (text: string) = printf "\u001b[1m%s\u001b[0m" text

        match state.Message with
        | Start
        | Jarvis _ ->
            printBold ">>"
            printf " "
        | You _ -> ()
        | Quit -> ()

        state

let withNewChat (msg: Message) (convo: Conversation) =
    match msg with
    | Start
    | Quit -> convo
    | You msg -> [ yield! convo; You msg ]
    | Jarvis msg -> [ yield! convo; Jarvis msg ]

let askJarvis prompt state : string =
    Ollama.createPayload "jarvis" state.Conversation
    |> Ollama.makeRequest
    |> (fun stream ->
        async {
            let mutable res = ""

            //save cursor at starting point
            UI.saveStartingPoint ()

            try
                do!
                    stream
                    |> AsyncSeq.iterAsync (fun content ->
                        async {
                            res <- res + content
                            do! UI.printInPlace res
                            do! Task.Delay(50) |> Async.AwaitTask
                        })

            with
            | :? OperationCanceledException -> printfn "Streaming was canceled."
            | ex -> printfn "An error occurred during streaming: %s" ex.Message

            return res
        })
    |> Async.RunSynchronously

let getStdInput () =
    //cover case of multi-line input
    ()

let rec chat (state: State) =
    match state.Message with
    | Start ->
        state |> UI.display |> ignore //print initial UI

        let input = System.Console.ReadLine() //ask for user input -> msg

        let newState =
            match input with
            | "exit"
            | "quit" -> { state with Message = Quit }
            | str when str.Length <> 0 -> { state with Message = You str }
            | _ -> { state with Message = Start }

        chat newState
    | You prompt ->
        state |> UI.display |> ignore //print UI with Jarvis placeholder

        //update conversation with msg
        let newState =
            { state with
                Conversation = withNewChat (You prompt) state.Conversation }

        //ask for jarvis input -> ollama rest api call
        chat
            { newState with
                Message = Jarvis(newState |> askJarvis prompt) }

    | Jarvis said ->
        state |> UI.display |> ignore //print initial UI

        //update conversation with jarvis msg
        let newState =
            { state with
                Conversation = withNewChat (Jarvis said) state.Conversation }

        //ask for user input -> msg
        let input = System.Console.ReadLine() //ask for user input -> msg

        let newNewState =
            match input with
            | "exit"
            | "quit" -> { state with Message = Quit }
            | str when str.Length <> 0 -> { newState with Message = You str }
            | _ -> { newState with Message = Start }

        chat newNewState
    | Quit ->
        //exit the program
        ()

[<EntryPoint>]
let main args =

    let initially =
        { Message = Start
          Conversation = List.Empty }

    chat initially

    0 // return an integer exit code

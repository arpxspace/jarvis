open System
open System.Diagnostics
open System.Text
open System.Threading.Tasks
open System.IO

// Function to run glow asynchronously and get the output
let runGlowAsync input =
    async {
        use p = new Process()
        p.StartInfo.FileName <- "glow"
        p.StartInfo.Arguments <- "--style auto"
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.RedirectStandardInput <- true
        p.StartInfo.RedirectStandardOutput <- true
        p.StartInfo.CreateNoWindow <- true
        p.Start() |> ignore

        do! p.StandardInput.WriteLineAsync(input: string) |> Async.AwaitTask
        p.StandardInput.Close()

        return! p.StandardOutput.ReadToEndAsync() |> Async.AwaitTask
    }

// Function to clear console and print content
let clearAndPrint content =
    Console.Clear()
    printfn "%s" content

// Main processing loop
let processInput () =
    async {
        let mutable inputBuffer = ""
        let inputStream = Console.OpenStandardInput()
        let buffer = Array.zeroCreate 1024
        let encoding = Encoding.UTF8

        while true do
            let! bytesRead = inputStream.ReadAsync(buffer, 0, buffer.Length) |> Async.AwaitTask

            if bytesRead = 0 then
                // End of input
                ()
            else
                let chunk = encoding.GetString(buffer, 0, bytesRead)
                inputBuffer <- inputBuffer + chunk

                // Process with glow and display asynchronously
                let! output = runGlowAsync inputBuffer
                clearAndPrint output
    }


// This function simulates a stream of markdown content
let simulateMarkdownStream () =
    async {
        let markdown =
            [ "# Welcome to the Markdown Stream\n\n"
              "This is a *live* demonstration of "
              "streaming **markdown** content.\n\n"
              "## Features:\n"
              "1. Real-time rendering\n"
              "2. Dynamic updates\n"
              "3. Glow integration\n\n"
              "Here's some `code`:\n"
              "```fsharp\n"
              "let hello = \"Hello, World!\"\n"
              "printfn \"%s\" hello\n"
              "```\n"
              "That's all for now!\n" ]

        use memStream = new MemoryStream()
        use writer = new StreamWriter(memStream, Encoding.UTF8)

        for chunk in markdown do
            do! writer.WriteAsync(chunk) |> Async.AwaitTask
            do! writer.FlushAsync() |> Async.AwaitTask
            memStream.Seek(0L, SeekOrigin.Begin) |> ignore

            do!
                Console
                    .OpenStandardInput()
                    .WriteAsync(memStream.GetBuffer(), 0, int memStream.Position)
                |> Async.AwaitTask

            memStream.SetLength(0L)
            do! Task.Delay(1000) |> Async.AwaitTask // Simulate delay between chunks

        Console.OpenStandardInput().Close() // Signal end of input
    }

// Run the simulation
Async.Start(simulateMarkdownStream ())

// Run the process
Async.RunSynchronously(processInput ())

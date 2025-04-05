module Tools

open System
open System.IO
open Spectre
open Spectre.Console

type WriteNoteSchema = {
    note: string
    filename: string
}

let writeNote note fileName =
    let filepath = $"/Users/amirpanahi/notes/literature/{fileName}"
    File.AppendAllText(filepath, note)
    AnsiConsole.WriteLine()
    AnsiConsole.MarkupLine($"  [green]COMMAND: Written to: {filepath} [/]")
    AnsiConsole.WriteLine()



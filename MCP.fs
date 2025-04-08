module MCP

open System.IO
open FSharp.Json
open ModelContextProtocol.Client
open ModelContextProtocol.Protocol.Transport
open System.Threading.Tasks
open System.Collections.Generic
open Spectre
open Spectre.Console

type Server = { command: string; args: string list }

type JarvisMcpServers = Map<string, Server>

let readConfig filepath =
    task {
        try
            let! content = File.ReadAllTextAsync(filepath)
            return content |> Json.deserialize<JarvisMcpServers> |> Some
        with ex ->
            return None
    }

let createClients (config: JarvisMcpServers) =
    config
    |> Map.toArray
    |> Array.map (fun (serverName, server) ->
        let serverConfig =
            ModelContextProtocol.McpServerConfig(
                Id = serverName,
                Name = serverName,
                TransportType = TransportTypes.StdIo,
                TransportOptions =
                    Dictionary<string, string>(
                        dict [ ("command", server.command); ("arguments", server.args |> String.concat " ") ]
                    )
            )

        task {
            let! client = McpClientFactory.CreateAsync(serverConfig)
            return (serverName, client)
        })
    |> Task.WhenAll

let listTools (mcpClients: (string * IMcpClient) array) =
    let (names, clients) = mcpClients |> Array.unzip

    task {
        let! elements = task { return! clients |> Array.map (_.ListToolsAsync()) |> Task.WhenAll }

        return elements |> Array.map Array.ofSeq |> Array.zip names 
    }

let display (tools: (string * McpClientTool array) array option) detailed =
    match tools with
    | Some toolArrays ->
        let (names, serverTools) = toolArrays |> Array.unzip 

        let totalTools = serverTools |> Array.sumBy Array.length

        // Add tools count indicator with simple string formatting
        if totalTools > 0 then
            AnsiConsole.MarkupLine($"[green]Found {totalTools} tool(s)[/]")
        else
            AnsiConsole.MarkupLine("[red]No tools available[/]")

        AnsiConsole.WriteLine()

        // Render tools in columns for each server
        if totalTools > 0 then
            let grid = Grid()

            // Configure grid with one column per server
            let serverCount = toolArrays.Length

            for _ in 1..serverCount do
                grid.AddColumn() |> ignore

            // Create server headers row
            let headers =
                Array.init serverCount (fun i -> $"[yellow]{Array.get names i}[/]")

            grid.AddRow(headers) |> ignore

            // Find maximum number of tools across all servers
            let maxTools = serverTools |> Array.map Array.length |> Array.max

            // Create rows for each tool and description
            for rowIndex in 0 .. (maxTools - 1) do
                let cells =
                    Array.init serverCount (fun serverIndex ->
                        let specificTools = serverTools.[serverIndex]

                        if rowIndex < specificTools.Length then
                            let tool = specificTools.[rowIndex]

                            if detailed then
                                $"[blue]{tool.Name}[/]\n[gray]{tool.Description}[/]"
                            else
                                $"{tool.Name}"
                        else
                            "" // Empty cell if this server has fewer tools
                    )

                grid.AddRow(cells) |> ignore

            // Set grid style
            grid.Expand <- true

            // Add a title
            AnsiConsole.MarkupLine("[bold]Available Tools by Server:[/]")
            AnsiConsole.WriteLine()

            // Write the grid to console
            AnsiConsole.Write(grid)

            AnsiConsole.WriteLine()
        else
            // No tools available
            AnsiConsole.MarkupLine("[red]No tools available in the current working directory[/]")

    | None ->
        // Error message
        AnsiConsole.MarkupLine("[red]⚠️ Error loading MCP tools ⚠️[/]")

        AnsiConsole.MarkupLine(
            "[yellow]Try ensuring your MCP configuration (mcp-servers.json) exists or is set up correctly.[/]"
        )

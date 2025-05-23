module MCP

open System.IO
open FSharp.Json
open ModelContextProtocol.Client
open ModelContextProtocol.Protocol.Transport
open System.Threading.Tasks
open System.Collections.Generic
open Spectre
open Spectre.Console

type Server =
    { disabled: bool option
      exclude: string array option
      command: string
      args: string list
      env: Map<string, string> option }

type JarvisMcpServers = Map<string, Server>

let readConfig filepath =
    task {
        try
            let! content = File.ReadAllTextAsync(filepath)
            return content |> Json.deserialize<JarvisMcpServers> |> Some
        with ex ->
            //read global config 
            try 
                let path = $"{System.Environment.GetFolderPath(System.Environment.SpecialFolder.UserProfile)}/.jarvis/mcp-servers.json"
                if not (File.Exists(path)) then
                    do! File.WriteAllTextAsync(path, "{}")
                let! content = File.ReadAllTextAsync(path)
                return content |> Json.deserialize<JarvisMcpServers> |> Some
            with ex ->
                printfn $"Error: {ex.Message}"
                return None
    }

let createClients (config: JarvisMcpServers) =
    config
    |> Map.toArray
    |> Array.filter (fun (_, server) -> not (server.disabled |> Option.defaultValue false))
    |> Array.map (fun (serverName, server) ->
        let clientTransport =
            StdioClientTransport(
                StdioClientTransportOptions(
                    Name = serverName,
                    Command = server.command,
                    Arguments = (server.args |> List.toArray),
                    EnvironmentVariables =
                        Dictionary<string, string>(
                            dict
                                [ match server.env with
                                  | Some envs ->
                                      for env in envs do
                                          (env.Key, env.Value)
                                  | None -> () ]
                        )
                )
            )

        task {
            let! client = McpClientFactory.CreateAsync(clientTransport)
            return (serverName, server.exclude |> Option.defaultValue [||], client)
        })
    |> Task.WhenAll

let listTools (mcpClients: (string * string array * IMcpClient) array) =
    let (names, excluded, clients) = mcpClients |> Array.unzip3

    task {
        let! elements = task { return! clients |> Array.map (_.ListToolsAsync()) |> Task.WhenAll }

        return
            elements
            |> Array.map Array.ofSeq
            |> Array.mapi (fun i tools -> tools |> Array.filter (fun y -> excluded[i] |> Array.contains y.Name |> not ))
            |> Array.zip names
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
            let headers = Array.init serverCount (fun i -> $"[yellow]{Array.get names i}[/]")

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

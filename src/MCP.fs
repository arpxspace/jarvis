module MCP

open Domain
open System
open System.Net.Http
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading
open FSharp.Control
open System.Net.WebSockets
open System.Threading.Tasks

// JSON-RPC types according to MCP specification
let JSONRPC_VERSION = "2.0"
let LATEST_PROTOCOL_VERSION = "2024-11-05"

type RequestId = string

[<RequireQualifiedAccess>]
type MCPMessage =
    | Request of JSONRPCRequest
    | Response of JSONRPCResponse
    | Notification of JSONRPCNotification
    | Error of JSONRPCError

and JSONRPCRequest = {
    jsonrpc: string
    id: RequestId
    method: string
    ``params``: JsonElement option
}

and JSONRPCResponse = {
    jsonrpc: string
    id: RequestId
    result: JsonElement
}

and JSONRPCNotification = {
    jsonrpc: string
    method: string
    ``params``: JsonElement option
}

and JSONRPCError = {
    jsonrpc: string
    id: RequestId
    error: {|
        code: int
        message: string
        data: JsonElement option
    |}
}

// Client capabilities
type ClientCapabilities = {
    roots: {| listChanged: bool |} option
    sampling: obj option
    experimental: Map<string, obj> option
}

// Server capabilities
type ServerCapabilities = {
    tools: {| listChanged: bool |} option
    resources: {| 
        subscribe: bool option
        listChanged: bool option 
    |} option
    prompts: {| listChanged: bool |} option
    logging: obj option
    experimental: Map<string, obj> option
}

// Implementation info
type Implementation = {
    name: string
    version: string
}

// Initialize result
type InitializeResult = {
    protocolVersion: string
    capabilities: ServerCapabilities
    serverInfo: Implementation
    instructions: string option
}

// Connection state
type ConnectionState =
    | Disconnected
    | Connecting
    | Connected of clientWebSocket: ClientWebSocket * serverCapabilities: ServerCapabilities
    | Failed of error: string

// Utilities to generate JSON-RPC messages
let createRequest (id: string) (method: string) (``params``: obj option) =
    let jsonParams = 
        match ``params`` with
        | Some p -> 
            let options = JsonSerializerOptions()
            options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
            Some (JsonSerializer.SerializeToElement(p, options))
        | None -> None
        
    {
        jsonrpc = JSONRPC_VERSION
        id = id
        method = method
        ``params`` = jsonParams
    }

let createNotification (method: string) (``params``: obj option) =
    let jsonParams = 
        match ``params`` with
        | Some p -> 
            let options = JsonSerializerOptions()
            options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
            Some (JsonSerializer.SerializeToElement(p, options))
        | None -> None
        
    {
        jsonrpc = JSONRPC_VERSION
        method = method
        ``params`` = jsonParams
    }

// WebSocket client for MCP
type MCPClient(serverUri: string) =
    let mutable state = Disconnected
    let mutable nextRequestId = 0
    let requestCallbacks = System.Collections.Generic.Dictionary<string, TaskCompletionSource<JsonElement>>()
    
    let getNextRequestId() =
        let id = nextRequestId
        nextRequestId <- nextRequestId + 1
        id.ToString()
        
    let serializeMessage (message: MCPMessage) =
        let options = JsonSerializerOptions()
        options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        
        match message with
        | MCPMessage.Request req -> JsonSerializer.Serialize(req, options)
        | MCPMessage.Response res -> JsonSerializer.Serialize(res, options)
        | MCPMessage.Notification notif -> JsonSerializer.Serialize(notif, options)
        | MCPMessage.Error err -> JsonSerializer.Serialize(err, options)
    
    let parseMessage (json: string) =
        try
            let doc = JsonDocument.Parse(json)
            let root = doc.RootElement
            
            if root.TryGetProperty("method", ref Unchecked.defaultof<JsonElement>) then
                if root.TryGetProperty("id", ref Unchecked.defaultof<JsonElement>) then
                    // It's a request
                    let req = JsonSerializer.Deserialize<JSONRPCRequest>(json)
                    MCPMessage.Request req
                else
                    // It's a notification
                    let notif = JsonSerializer.Deserialize<JSONRPCNotification>(json)
                    MCPMessage.Notification notif
            elif root.TryGetProperty("result", ref Unchecked.defaultof<JsonElement>) then
                // It's a response
                let res = JsonSerializer.Deserialize<JSONRPCResponse>(json)
                MCPMessage.Response res
            elif root.TryGetProperty("error", ref Unchecked.defaultof<JsonElement>) then
                // It's an error
                let err = JsonSerializer.Deserialize<JSONRPCError>(json)
                MCPMessage.Error err
            else
                failwith "Unknown message type"
        with ex ->
            failwith $"Failed to parse message: {ex.Message}"
    
    // Send a message over the WebSocket
    let sendMessage (message: MCPMessage) =
        async {
            match state with
            | Connected (webSocket, _) ->
                let json = serializeMessage message
                let bytes = Encoding.UTF8.GetBytes(json)
                let buffer = new ArraySegment<byte>(bytes)
                
                do! webSocket.SendAsync(buffer, WebSocketMessageType.Text, true, CancellationToken.None)
                    |> Async.AwaitTask
                return ()
            | _ ->
                return failwith "Not connected"
        }

    // Initialize the connection
    let initialize() =
        async {
            match state with
            | Connected (webSocket, _) ->
                let clientInfo = {
                    name = "Jarvis MCP Client"
                    version = "0.1.0"
                }
                
                let capabilities = {
                    roots = Some {| listChanged = true |}
                    sampling = Some {||}
                    experimental = None
                }
                
                let initializeParams = {|
                    protocolVersion = LATEST_PROTOCOL_VERSION
                    clientInfo = clientInfo
                    capabilities = capabilities
                |}
                
                let id = getNextRequestId()
                let request = createRequest id "initialize" (Some initializeParams)
                
                // Set up the completion source for the response
                let tcs = TaskCompletionSource<JsonElement>()
                requestCallbacks.[id] <- tcs
                
                // Send the request
                do! sendMessage (MCPMessage.Request request)
                
                // Wait for the response
                let! result = tcs.Task |> Async.AwaitTask
                
                // Parse the initialize result
                let initResult = JsonSerializer.Deserialize<InitializeResult>(result.ToString())
                
                // Update the state with capabilities
                state <- Connected(webSocket, initResult.capabilities)
                
                // Send initialized notification
                let notif = createNotification "notifications/initialized" None
                do! sendMessage (MCPMessage.Notification notif)
                
                return Ok initResult
            | _ ->
                return Error "Not connected"
        }
        
    // Notification handlers
    let mutable resourceUpdateHandlers = Map.empty<string, string -> unit>
    let mutable resourceListChangedHandler = Option<unit -> unit>.None
    let mutable toolListChangedHandler = Option<unit -> unit>.None
    let mutable promptListChangedHandler = Option<unit -> unit>.None
    let mutable logMessageHandler = Option<string * string * string -> unit>.None
    
    // Message receiver loop with improved WebSocket handling and notification support
    let rec receiveMessages (webSocket: ClientWebSocket) (cancellationToken: CancellationToken) =
        async {
            let buffer = Array.zeroCreate<byte> 16384 // 16KB buffer
            let receiveBuffer = new ArraySegment<byte>(buffer)
            
            try
                // Continue while the connection is open and not cancelled
                while webSocket.State = WebSocketState.Open && not cancellationToken.IsCancellationRequested do
                    // Use memory stream to accumulate message data
                    use memoryStream = new System.IO.MemoryStream()
                    let mutable isEndOfMessage = false
                    
                    // Receive complete message
                    while not isEndOfMessage && webSocket.State = WebSocketState.Open && not cancellationToken.IsCancellationRequested do
                        let! result = webSocket.ReceiveAsync(receiveBuffer, cancellationToken) |> Async.AwaitTask
                        
                        match result.MessageType with
                        | WebSocketMessageType.Close ->
                            // Handle close message
                            do! webSocket.CloseAsync(WebSocketCloseStatus.NormalClosure, "Closing", cancellationToken) |> Async.AwaitTask
                            state <- Disconnected
                            isEndOfMessage <- true
                        | WebSocketMessageType.Text ->
                            // Append chunk to memory stream
                            memoryStream.Write(buffer, 0, result.Count)
                            isEndOfMessage <- result.EndOfMessage
                        | _ ->
                            // Unsupported message type (like binary)
                            isEndOfMessage <- result.EndOfMessage
                    
                    // Process complete message if we have one
                    if memoryStream.Length > 0L && not cancellationToken.IsCancellationRequested then
                        memoryStream.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
                        use reader = new System.IO.StreamReader(memoryStream, Encoding.UTF8)
                        let message = reader.ReadToEnd()
                        
                        try
                            let parsedMessage = parseMessage message
                            
                            // Handle different message types
                            match parsedMessage with
                            | MCPMessage.Response response ->
                                // Complete the pending request
                                if requestCallbacks.ContainsKey(response.id) then
                                    let tcs = requestCallbacks.[response.id]
                                    requestCallbacks.Remove(response.id) |> ignore
                                    tcs.SetResult(response.result)
                                else
                                    eprintfn "Received response for unknown request ID: %s" response.id
                                    
                            | MCPMessage.Error error ->
                                // Complete the pending request with an error
                                if requestCallbacks.ContainsKey(error.id) then
                                    let tcs = requestCallbacks.[error.id]
                                    requestCallbacks.Remove(error.id) |> ignore
                                    tcs.SetException(Exception(sprintf "MCP Error %d: %s" error.error.code error.error.message))
                                else
                                    eprintfn "Received error for unknown request ID: %s" error.id
                                    
                            | MCPMessage.Notification notification ->
                                // Handle different notification types
                                match notification.method with
                                | "notifications/resources/updated" ->
                                    // Resource update notification
                                    match notification.``params`` with
                                    | Some ``params`` ->
                                        if ``params``.TryGetProperty("uri", ref Unchecked.defaultof<JsonElement>) then
                                            let uri = ``params``.GetProperty("uri").GetString()
                                            if resourceUpdateHandlers.ContainsKey(uri) then
                                                resourceUpdateHandlers.[uri] uri
                                            eprintfn "Resource updated: %s" uri
                                    | None -> ()
                                
                                | "notifications/resources/list_changed" ->
                                    // Resource list changed notification
                                    match resourceListChangedHandler with
                                    | Some handler -> handler()
                                    | None -> ()
                                    eprintfn "Resources list changed"
                                
                                | "notifications/tools/list_changed" ->
                                    // Tool list changed notification
                                    match toolListChangedHandler with
                                    | Some handler -> handler()
                                    | None -> ()
                                    eprintfn "Tools list changed"
                                
                                | "notifications/prompts/list_changed" ->
                                    // Prompt list changed notification
                                    match promptListChangedHandler with
                                    | Some handler -> handler()
                                    | None -> ()
                                    eprintfn "Prompts list changed"
                                
                                | "notifications/message" ->
                                    // Log message notification
                                    match notification.``params`` with
                                    | Some ``params`` ->
                                        if ``params``.TryGetProperty("level", ref Unchecked.defaultof<JsonElement>) then
                                            let level = ``params``.GetProperty("level").GetString()
                                            let logger = 
                                                if ``params``.TryGetProperty("logger", ref Unchecked.defaultof<JsonElement>) then
                                                    ``params``.GetProperty("logger").GetString()
                                                else ""
                                            let data = 
                                                if ``params``.TryGetProperty("data", ref Unchecked.defaultof<JsonElement>) then
                                                    ``params``.GetProperty("data").ToString()
                                                else ""
                                                
                                            match logMessageHandler with
                                            | Some handler -> handler(level, logger, data)
                                            | None -> ()
                                            
                                            eprintfn "[%s] %s: %s" level logger data
                                    | None -> ()
                                
                                | _ ->
                                    // Unknown notification
                                    eprintfn "Unknown notification: %s" notification.method
                                    
                            | MCPMessage.Request request ->
                                // Handle requests from server (ping, sampling, etc.)
                                match request.method with
                                | "ping" ->
                                    // Respond to ping with empty result
                                    let response = {
                                        jsonrpc = JSONRPC_VERSION
                                        id = request.id
                                        result = JsonSerializer.SerializeToElement({||})
                                    }
                                    do! sendMessage (MCPMessage.Response response)
                                
                                | _ ->
                                    // Unsupported request method
                                    eprintfn "Unsupported request method: %s" request.method
                                    let error = {
                                        jsonrpc = JSONRPC_VERSION
                                        id = request.id
                                        error = {|
                                            code = -32601 // Method not found
                                            message = $"Method not supported: {request.method}"
                                            data = None
                                        |}
                                    }
                                    do! sendMessage (MCPMessage.Error error)
                        
                        with ex ->
                            eprintfn "Error processing message: %s" ex.Message
            
            with ex ->
                state <- Failed ex.Message
                eprintfn "WebSocket error: %s" ex.Message
        }
    
    
    // Connect to the MCP server
    member this.Connect() =
        async {
            if state = Disconnected || (match state with | Failed _ -> true | _ -> false) then
                state <- Connecting
                
                try
                    let webSocket = new ClientWebSocket()
                    let uri = Uri(serverUri)
                    
                    do! webSocket.ConnectAsync(uri, CancellationToken.None) |> Async.AwaitTask
                    
                    state <- Connected(webSocket, {
                        tools = None
                        resources = None
                        prompts = None
                        logging = None
                        experimental = None
                    })
                    
                    // Start receiving messages
                    Async.Start(receiveMessages webSocket CancellationToken.None)
                    
                    // Initialize the connection
                    let! result = initialize()
                    
                    return result
                with ex ->
                    state <- Failed ex.Message
                    return Error ex.Message
            else
                return Error "Already connected or connecting"
        }
    
    // Disconnect from the MCP server
    member this.Disconnect() =
        async {
            match state with
            | Connected (webSocket, _) ->
                do! webSocket.CloseAsync(WebSocketCloseStatus.NormalClosure, "Closing", CancellationToken.None)
                    |> Async.AwaitTask
                state <- Disconnected
                return Ok()
            | _ ->
                return Error "Not connected"
        }
    
    // Send a request and wait for response
    member this.SendRequest<'T> (method: string) (requestParams: obj option) =
        async {
            match state with
            | Connected _ ->
                let id = getNextRequestId()
                let request = createRequest id method requestParams
                
                // Set up the completion source for the response
                let tcs = TaskCompletionSource<JsonElement>()
                requestCallbacks.[id] <- tcs
                
                // Send the request
                do! sendMessage (MCPMessage.Request request)
                
                // Wait for the response
                let! result = tcs.Task |> Async.AwaitTask
                
                // Deserialize the result
                return JsonSerializer.Deserialize<'T>(result.ToString())
            | _ ->
                return failwith "Not connected"
        }
    
    // Send a notification
    member this.SendNotification (method: string) (requestParams: obj option) =
        async {
            match state with
            | Connected _ ->
                let notification = createNotification method requestParams
                do! sendMessage (MCPMessage.Notification notification)
                return ()
            | _ ->
                return failwith "Not connected"
        }
    
    // List available tools
    member this.ListTools() =
        async {
            match state with
            | Connected (_, capabilities) ->
                match capabilities.tools with
                | Some _ ->
                    let! result = this.SendRequest<{| tools: {| name: string; description: string option |} array |}> "tools/list" None
                    return result.tools
                | None ->
                    return [||]
            | _ ->
                return [||]
        }
    
    // Call a tool with improved response handling per MCP spec
    member this.CallTool (name: string) (args: obj) =
        async {
            try
                // Create tool call parameters
                let requestParams = {| name = name; arguments = args |}
                
                // Send the request to the MCP server
                let! response = this.SendRequest<{| content: JsonElement array; isError: bool option |}> "tools/call" (Some requestParams)
                
                // Process the content array - extract text content
                let content = 
                    response.content
                    |> Array.choose (fun item ->
                        try
                            if item.TryGetProperty("type", ref Unchecked.defaultof<JsonElement>) && 
                               item.GetProperty("type").GetString() = "text" && 
                               item.TryGetProperty("text", ref Unchecked.defaultof<JsonElement>) then
                                Some (item.GetProperty("text").GetString())
                            else
                                None
                        with _ -> None)
                    |> String.concat "\n"
                
                let isError = defaultArg response.isError false
                
                return Ok (content, isError)
            with ex ->
                return Error $"Tool call failed: {ex.Message}"
        }
    
    // List available resources
    member this.ListResources() =
        async {
            match state with
            | Connected (_, capabilities) ->
                match capabilities.resources with
                | Some _ ->
                    let! result = this.SendRequest<{| resources: {| uri: string; name: string; description: string option |} array |}> "resources/list" None
                    return result.resources
                | None ->
                    return [||]
            | _ ->
                return [||]
        }
    
    // Read a resource with improved handling of different content types
    member this.ReadResource (uri: string) =
        async {
            try
                let requestParams = {| uri = uri |}
                
                // Send request to MCP server
                let! result = this.SendRequest<{| contents: JsonElement array |}> "resources/read" (Some requestParams)
                
                // Process each content item in the array
                let processedContents = 
                    result.contents
                    |> Array.map (fun content ->
                        try
                            // Check if this is a text resource
                            if content.TryGetProperty("text", ref Unchecked.defaultof<JsonElement>) then
                                let text = content.GetProperty("text").GetString()
                                let uri = 
                                    if content.TryGetProperty("uri", ref Unchecked.defaultof<JsonElement>) then
                                        Some (content.GetProperty("uri").GetString())
                                    else None
                                let mimeType = 
                                    if content.TryGetProperty("mimeType", ref Unchecked.defaultof<JsonElement>) then
                                        Some (content.GetProperty("mimeType").GetString())
                                    else None
                                
                                // Format with URI and MIME type if available
                                match uri, mimeType with
                                | Some u, Some m -> sprintf "[%s (%s)]\n%s" u m text
                                | Some u, None -> sprintf "[%s]\n%s" u text
                                | None, Some m -> sprintf "[%s]\n%s" m text
                                | None, None -> text
                            
                            // Check if this is a blob resource (binary data)
                            elif content.TryGetProperty("blob", ref Unchecked.defaultof<JsonElement>) then
                                let uri = 
                                    if content.TryGetProperty("uri", ref Unchecked.defaultof<JsonElement>) then
                                        content.GetProperty("uri").GetString()
                                    else "unknown"
                                let mimeType = 
                                    if content.TryGetProperty("mimeType", ref Unchecked.defaultof<JsonElement>) then
                                        content.GetProperty("mimeType").GetString()
                                    else "application/octet-stream"
                                
                                sprintf "[Binary data: %s (%s)]" uri mimeType
                            else
                                "Unknown resource format"
                        with ex ->
                            sprintf "Error processing resource content: %s" ex.Message
                    )
                
                return Ok processedContents
            with ex ->
                return Error $"Failed to read resource: {ex.Message}"
        }
    
    // List available prompts
    member this.ListPrompts() =
        async {
            match state with
            | Connected (_, capabilities) ->
                match capabilities.prompts with
                | Some _ ->
                    let! result = this.SendRequest<{| prompts: {| name: string; description: string option |} array |}> "prompts/list" None
                    return result.prompts
                | None ->
                    return [||]
            | _ ->
                return [||]
        }
    
    // Get a prompt with improved message handling
    member this.GetPrompt (name: string) (args: Map<string, string> option) =
        async {
            try
                // Create parameters object based on whether arguments are provided
                let requestParams = 
                    match args with
                    | Some arguments -> {| name = name; arguments = arguments |}
                    | None -> {| name = name; arguments = Map.empty |}
                
                // Send request to MCP server
                let! result = this.SendRequest<{| description: string option; messages: JsonElement array |}> "prompts/get" (Some requestParams)
                
                // Process the messages to extract structured content
                let processedMessages =
                    result.messages
                    |> Array.map (fun message ->
                        try
                            // Extract role
                            let role = 
                                if message.TryGetProperty("role", ref Unchecked.defaultof<JsonElement>) then
                                    message.GetProperty("role").GetString()
                                else "unknown"
                            
                            // Extract content based on type
                            let content =
                                if message.TryGetProperty("content", ref Unchecked.defaultof<JsonElement>) then
                                    let content = message.GetProperty("content")
                                    
                                    // Handle text content
                                    if content.ValueKind = JsonValueKind.Object && 
                                       content.TryGetProperty("type", ref Unchecked.defaultof<JsonElement>) then
                                        
                                        let contentType = content.GetProperty("type").GetString()
                                        
                                        match contentType with
                                        | "text" when content.TryGetProperty("text", ref Unchecked.defaultof<JsonElement>) ->
                                            content.GetProperty("text").GetString()
                                        | "resource" when content.TryGetProperty("resource", ref Unchecked.defaultof<JsonElement>) ->
                                            let resource = content.GetProperty("resource")
                                            if resource.TryGetProperty("uri", ref Unchecked.defaultof<JsonElement>) then
                                                sprintf "[Resource: %s]" (resource.GetProperty("uri").GetString())
                                            else
                                                "[Embedded resource]"
                                        | "image" ->
                                            "[Image content]"
                                        | _ ->
                                            "[Unsupported content type]"
                                    else
                                        "[Unrecognized content format]"
                                else
                                    "[No content]"
                            
                            sprintf "[%s]: %s" role content
                        with ex ->
                            sprintf "Error processing message: %s" ex.Message
                    )
                
                // Combine description and messages
                let description = defaultArg result.description ""
                
                return Ok (description, processedMessages)
            with ex ->
                return Error $"Failed to get prompt: {ex.Message}"
        }

// Map MCPClient into the format expected by the chat loop
let createPayload (convo: Conversation) (init: LLM) =
    let serializedConvo =
        convo
        |> List.map (fun x ->
            match x with
            | You msg ->
                match msg with
                | Implicit x ->
                    { role = "user"; content = x.SerializeUser() } // serialize user res
                | Explicit x ->
                    { role = "user"; content = x.Serialize() }
            | Jarvis msg -> 
                match msg with
                | Implicit x ->
                    { role = "assistant"; content = x.SerializeJarvis() } // serialize jarvis res
                | Explicit x ->
                    { role = "assistant"; content = x.Serialize() }
            | _ -> { role = ""; content = "" })
        |> List.filter (fun msg -> not (String.IsNullOrEmpty msg.role))
        |> List.toArray

    // MCP doesn't use the traditional payload format, so we'll just use a placeholder
    // The actual MCP handling happens in makeRequest
    new StringContent("{}")

let httpRequest payload =
    // MCP doesn't use traditional HTTP requests, it uses WebSockets
    // This is just a placeholder - the actual connection happens in makeRequest
    let mutable request = new HttpRequestMessage()
    request.Method <- HttpMethod.Post
    request.RequestUri <- Uri("http://localhost:8080") // Placeholder URI
    request.Content <- payload
    request

// Server configuration type
type ServerConfig = {
    name: string
    url: string
    description: string
}

// Load server configurations from JSON file
let loadServerConfigs() =
    try
        let filePath = System.IO.Path.Combine(System.IO.Directory.GetCurrentDirectory(), "src", "mcp-servers.json")
        if System.IO.File.Exists(filePath) then
            let json = System.IO.File.ReadAllText(filePath)
            let options = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
            let config = JsonSerializer.Deserialize<{| servers: ServerConfig[] |}>(json, options)
            config.servers
        else
            printfn "MCP servers configuration file not found: %s" filePath
            [| { name = "default"; url = "ws://localhost:8080"; description = "Default local MCP server" } |]
    with ex ->
        printfn "Error loading MCP server configurations: %s" ex.Message
        [| { name = "default"; url = "ws://localhost:8080"; description = "Default local MCP server" } |]

// Get a server configuration by name
let getServerConfig (name: string option) =
    let configs = loadServerConfigs()
    match name with
    | Some serverName -> 
        configs 
        |> Array.tryFind (fun config -> config.name = serverName)
        |> Option.defaultValue configs.[0]
    | None -> configs.[0]

    // Subscribe to resource updates
    member this.SubscribeToResource (uri: string) (handler: string -> unit) =
        async {
            match state with
            | Connected (_, capabilities) ->
                match capabilities.resources with
                | Some res when res.subscribe = Some true ->
                    try
                        // Register handler
                        resourceUpdateHandlers <- resourceUpdateHandlers.Add(uri, handler)
                        
                        // Send subscription request
                        let requestParams = {| uri = uri |}
                        let! _ = this.SendRequest<obj> "resources/subscribe" (Some requestParams)
                        return Ok()
                    with ex ->
                        return Error $"Failed to subscribe: {ex.Message}"
                | _ ->
                    return Error "Server does not support resource subscriptions"
            | _ ->
                return Error "Not connected"
        }
    
    // Unsubscribe from resource updates
    member this.UnsubscribeFromResource (uri: string) =
        async {
            match state with
            | Connected (_, capabilities) ->
                match capabilities.resources with
                | Some res when res.subscribe = Some true ->
                    try
                        // Remove handler
                        resourceUpdateHandlers <- resourceUpdateHandlers.Remove(uri)
                        
                        // Send unsubscription request
                        let requestParams = {| uri = uri |}
                        let! _ = this.SendRequest<obj> "resources/unsubscribe" (Some requestParams)
                        return Ok()
                    with ex ->
                        return Error $"Failed to unsubscribe: {ex.Message}"
                | _ ->
                    return Error "Server does not support resource subscriptions"
            | _ ->
                return Error "Not connected"
        }
    
    // Set handlers for list change notifications
    member this.SetResourceListChangedHandler (handler: unit -> unit) =
        resourceListChangedHandler <- Some handler
    
    member this.SetToolListChangedHandler (handler: unit -> unit) =
        toolListChangedHandler <- Some handler
    
    member this.SetPromptListChangedHandler (handler: unit -> unit) =
        promptListChangedHandler <- Some handler
    
    member this.SetLogMessageHandler (handler: string * string * string -> unit) =
        logMessageHandler <- Some handler

// Initialize a client and connect to the server with improved error handling
let createClient (serverNameOrUrl: string) =
    // Check if the input is a URL or a server name
    let serverUri = 
        if serverNameOrUrl.StartsWith("ws://") || serverNameOrUrl.StartsWith("wss://") then
            serverNameOrUrl
        else
            let config = getServerConfig (Some serverNameOrUrl)
            config.url
    
    try        
        let client = new MCPClient(serverUri)
        match client.Connect() |> Async.RunSynchronously with
        | Ok _ -> 
            eprintfn "Connected to MCP server: %s" serverUri
            client
        | Error errMsg -> 
            eprintfn "Failed to connect to MCP server at %s: %s" serverUri errMsg
            failwith $"MCP connection failed: {errMsg}"
    with ex ->
        eprintfn "Error initializing MCP client: %s" ex.Message
        failwith $"MCP initialization failed: {ex.Message}"

// Parse function to convert MCP responses to the format expected by the chat loop
let parse (line: string) =
    try
        // Placeholder implementation - actual parsing would depend on MCP response format
        Ok(Data (ReceivedResponse line))
    with
    | ex -> Error()

// Define tool result structure as per MCP spec
type ToolContent = {
    type_: string
    text: string option
}

type ToolResult = {
    content: ToolContent array
    isError: bool option
}

// Custom makeRequest implementation for MCP that handles the full lifecycle
let makeRequest (httpRequest: HttpRequestMessage) parse (payload: StringContent) =
    // Get MCP server name from environment variable or use default
    let serverName = 
        match Environment.GetEnvironmentVariable("MCP_SERVER_NAME") with
        | null -> "default"
        | name -> name
       
    // Use cancellation token to allow clean shutdown
    let cts = new CancellationTokenSource()
    
    // Store client for cleanup
    let mutable client = None
    
    asyncSeq {
        try
            // Get server from config or default to localhost
            let serverConfig = getServerConfig (Some serverName)
            
            // Initialize MCP client with proper error handling
            try
                let mcpClient = new MCPClient(serverConfig.url)
                
                // Set up notification handlers for better responsiveness
                mcpClient.SetResourceListChangedHandler(fun () -> 
                    eprintfn "Resource list changed notification received")
                    
                mcpClient.SetToolListChangedHandler(fun () -> 
                    eprintfn "Tool list changed notification received")
                    
                mcpClient.SetPromptListChangedHandler(fun () -> 
                    eprintfn "Prompt list changed notification received")
                    
                mcpClient.SetLogMessageHandler(fun (level, logger, data) -> 
                    eprintfn "[%s] %s: %s" level logger data)
                
                // Connect to the server
                match! mcpClient.Connect() with
                | Ok initResult ->
                    // Store client for cleanup
                    client <- Some mcpClient
                    
                    // Display connection info
                    yield ReceivedResponse $"Connected to MCP server '{serverConfig.name}'"
                    if serverConfig.description <> "" then
                        yield ReceivedResponse $"Server description: {serverConfig.description}"
                        
                    // Show server info
                    yield ReceivedResponse $"Server: {initResult.serverInfo.name} v{initResult.serverInfo.version}"
                    
                    if initResult.instructions.IsSome then
                        yield ReceivedResponse $"\nServer instructions:\n{initResult.instructions.Value}"
                
                    // Fetch available tools
                    let! tools = mcpClient.ListTools() |> Async.AwaitTask
                    
                    // Show available tools
                    if tools.Length > 0 then
                        yield ReceivedResponse "\nAvailable tools:"
                        for tool in tools do
                            let description = defaultArg tool.description "No description"
                            yield ReceivedResponse $"• {tool.name}: {description}"
                    else
                        yield ReceivedResponse "\nNo tools available from this server."
                
                    // Fetch available resources
                    try
                        let! resources = mcpClient.ListResources() |> Async.AwaitTask
                        if resources.Length > 0 then
                            yield ReceivedResponse "\nAvailable resources:"
                            for resource in resources do
                                let description = defaultArg resource.description ""
                                
                                if description.Length > 0 then
                                    yield ReceivedResponse $"• {resource.name} ({resource.uri}): {description}"
                                else
                                    yield ReceivedResponse $"• {resource.name} ({resource.uri})"
                                    
                            // Set up resource change handler
                            match initResult.capabilities.resources with
                            | Some res when res.subscribe = Some true ->
                                yield ReceivedResponse "\nResource subscriptions are supported."
                            | _ -> ()
                    with ex ->
                        yield ReceivedResponse $"\nFailed to load resources: {ex.Message}"
                    
                    // Fetch available prompts
                    try
                        let! prompts = mcpClient.ListPrompts() |> Async.AwaitTask
                        if prompts.Length > 0 then
                            yield ReceivedResponse "\nAvailable prompts:"
                            for prompt in prompts do
                                let description = defaultArg prompt.description ""
                                yield ReceivedResponse $"• {prompt.name}: {description}"
                    with ex ->
                        yield ReceivedResponse $"\nFailed to load prompts: {ex.Message}"
                    
                    // Ready for conversation
                    yield ReceivedResponse "\n\nHow can I help you today? You can ask me to use any of the available tools."
                    
                    // This is where the real integration with the chat system would happen
                    // In a complete implementation, we would:
                    // 1. Process user input to identify which tools to call
                    // 2. Extract parameters from the user message
                    // 3. Call the appropriate tools
                    // 4. Format and return the results
                    
                | Error errMsg ->
                    yield ReceivedResponse $"Failed to connect to MCP server: {errMsg}"
            with ex ->
                yield ReceivedResponse $"Error initializing MCP client: {ex.Message}"
                
        with ex ->
            yield ReceivedResponse $"Error in MCP connection: {ex.Message}"
            
        finally
            // Clean up resources
            match client with
            | Some c -> 
                try
                    let! _ = c.Disconnect() |> Async.AwaitTask
                    () // Ignore result
                with _ -> 
                    () // Ignore errors during disconnect
            | None -> ()
            
            // Cancel any pending operations
            cts.Cancel()
            cts.Dispose()
    }

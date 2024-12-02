module Domain
open FSharp.Control

type ChatMessage = { role: string; content: string }

type ParseResponse = 
    | Data of string
    | Ended of AsyncSeq<string>

type LLM =
    | Claude
    | Ollama

type OllamaPayload = {
    model: string
    messages: ChatMessage[]
    stream: bool
}

type ClaudePayload = {
    model: string
    messages: ChatMessage[]
    stream: bool
    max_tokens: int
}

type Message =
    | You of msg: string
    | Jarvis of msg: string
    | Quit

type Conversation = Message list

type State =
    { Message: Message
      Conversation: Conversation }

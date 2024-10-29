module Domain

type Message =
    | You of msg: string
    | Jarvis of msg: string
    | Quit

type Conversation = Message list

type State =
    { Message: Message
      Conversation: Conversation }

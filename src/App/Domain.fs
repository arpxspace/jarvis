module Domain

type Message =
    | Start
    | You of msg: string
    | Jarvis of msg: string
    | Quit

type Conversation = Message list

type State =
    { Message: Message
      Conversation: Conversation }

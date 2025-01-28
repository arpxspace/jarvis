# Jarvis

https://github.com/user-attachments/assets/a2138e8f-aa9b-488d-8d1d-db0fbf827ce0

A minimal TUI that started off as being a prettified version of the existing ollama TUI. I felt that it was hard to read and distinguish rich text from the markdown that was being produced from ollama and so i went on this journey to build an llm client on the terminal that better fitted my needs. Over time this project grew arms and legs to the point where now Jarvis can be seen as an AI agent within the terminal. Here are the following features/functinalities

* Multi-line input
* Can use any LLM Model (Claude, OpenAI) but currently defaults to Claude 3.5 Sonnet
* When offline automatically uses Ollama instead
* Prettified markdown output with clear formatting
* Status bar suggesting the steps the Agent is taking
* Custom tool use
  - `write-tool`: writes a literature note to the users specified directory path based on the current thread of messages
  - more to come ...
* *MCP* (TODO)
  - uses tool use from the `filesystem` MCP server

**TODO**
- [ ] MCP integration, starting with the `filesystem` server
- [ ] Ability for jarvis to depict whether to respond normally or perform socratic method if the user has prior sufficient knowledge
- [ ] Ability for jarvis to use a CoT model (`R1`) in cases where it is being presented with a complex problem
- [ ] RAG from vector databases

**Pre-requisites**:
- Claude API key
- Ollama installed


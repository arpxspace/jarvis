Cursor Movement
	Move cursor up: \033[<N>A (Move cursor up by N lines)
	Move cursor down: \033[<N>B
	Move cursor right: \033[<N>C
	Move cursor left: \033[<N>D
	Set cursor position: \033[<row>;<column>H

Clear from cursor to end of screen: \033[0J


perhaps we could
1. move cursor to start of jarvis output for this particular chat
	- move cursor up N lines where N is dynamically changing as the stream comes in
	- clear from cursor to end of screen 
	- reprint the current state of `res`


partition text into lines based on the size of each partition being the terminal width


what if you save the cursor position for jarvis' starting point of response. everytime jarvis resopnse comes back move cursor back to saved starting point and reset everything. then write the new concatenated content. repeat

Save Position (\033[s):

when to save?

Restore Position (\033[u):

printinplace string

- take string and split into 2d array

"hello its me \n and im good"

[["hello its me"], ["an im good"]]

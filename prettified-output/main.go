package main

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"strings"
	"sync"
	"time"
	"unicode"

	"github.com/charmbracelet/bubbles/spinner"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/glamour"
	"github.com/charmbracelet/lipgloss"
)

type InputData struct {
	Text  string `json:"Text"`
	Tool  string `json:"Tool"`
	Event string `json:"Event"`
}

// Your streaming text
type StreamContent string
type EventMsg string
type ToolMsg string

type MarkdownRenderer struct {
	content      strings.Builder
	viewport     viewport.Model
	renderer     *glamour.TermRenderer
	lipgloss     *lipgloss.Renderer
	output       string
	rendered     string
	height       int
	width        int
	contentMutex *sync.Mutex

	// Spinner stuff
	spinner      spinner.Model
	spinnerLabel string

	// Keep track of the current tool and event
	toolName   string
	eventLabel string
}

func NewMarkdownRenderer() (*MarkdownRenderer, error) {
	gr, err := glamour.NewTermRenderer(
		glamour.WithEnvironmentConfig(),
		glamour.WithWordWrap(80),
	)
	if err != nil {
		return nil, err
	}

	vp := viewport.New(0, 0)
	vp.GotoBottom()

	s := spinner.New()
	s.Spinner = spinner.Spinner{
		Frames: []string{"✶", "✸", "✹", "✺", "✹", "✷"},
		FPS:    35 * time.Millisecond,
	}
	s.Style = lipgloss.NewStyle().Foreground(lipgloss.Color("205"))

	return &MarkdownRenderer{
		renderer:     gr,
		viewport:     vp,
		lipgloss:     lipgloss.NewRenderer(os.Stdout),
		contentMutex: &sync.Mutex{},
		spinner:      s,
		// Default text for spinner
		spinnerLabel: "Processing...",
	}, nil
}

func (r *MarkdownRenderer) Init() tea.Cmd {
	return r.spinner.Tick
}

func (r *MarkdownRenderer) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmds []tea.Cmd

	switch m := msg.(type) {
	case tea.WindowSizeMsg:
		r.width, r.height = m.Width, m.Height
		r.viewport.Width = m.Width
		r.viewport.Height = m.Height

	case StreamContent:
		r.appendContent(string(m))
		// Update the viewport after appending content
		return r, r.updateViewportCmd

	case ToolMsg:
		// Store the tool name in the model
		r.toolName = string(m)

	case EventMsg:
		// Switch on the event to set spinner text or anything else
		switch string(m) {
		case "received-text":
			r.eventLabel = "Writing..."
		case "requires-tool":
			r.eventLabel = "Calling..."
		case "constructing-tool":
			r.eventLabel = fmt.Sprintf("Constructing info for tool %s...", r.toolName)
		case "block-finished":
			r.eventLabel = "Processing..."
		default:
			r.eventLabel = ""
		}
	}

	// Update the viewport if needed
	if r.viewportNeeded() {
		var vpCmd tea.Cmd
		r.viewport, vpCmd = r.viewport.Update(msg)
		cmds = append(cmds, vpCmd)
	}

	// Update spinner
	var spinCmd tea.Cmd
	r.spinner, spinCmd = r.spinner.Update(msg)
	cmds = append(cmds, spinCmd)

	return r, tea.Batch(cmds...)
}

// Render the view
func (r *MarkdownRenderer) View() string {
	// If you want to show both the current event and the tool name:
	// e.g. "Constructing tool [my-tool]"
	// Or you can show just the event label; it’s up to you
	combinedLabel := r.eventLabel
	if r.toolName != "" {
		combinedLabel += " [" + r.toolName + "]"
	}
	if combinedLabel == "" {
		combinedLabel = "Loading..."
	}

	spinnerView := fmt.Sprintf("%s %s",
		r.spinner.View(),
		lipgloss.NewStyle().Italic(true).Render(combinedLabel),
	)

	if r.viewportNeeded() {
		return r.viewport.View() + "\n" + spinnerView
	}
	return r.rendered + "\n" + spinnerView
}

func (r *MarkdownRenderer) appendContent(s string) {
	r.contentMutex.Lock()
	defer r.contentMutex.Unlock()

	r.content.WriteString(s)
	r.output = r.content.String()
}

func (r *MarkdownRenderer) updateViewportCmd() tea.Msg {
	r.contentMutex.Lock()
	defer r.contentMutex.Unlock()

	wasAtBottom := r.viewport.ScrollPercent() == 1.0
	oldHeight := r.height

	rendered, err := r.renderer.Render(r.output)
	if err != nil {
		// Return nil or handle error
		return nil
	}

	rendered = strings.TrimRightFunc(rendered, unicode.IsSpace)
	r.rendered = rendered + "\n"
	r.height = lipgloss.Height(r.rendered)

	if r.width > 0 {
		truncated := r.lipgloss.NewStyle().MaxWidth(r.width).Render(r.rendered)
		r.viewport.SetContent(truncated)
	} else {
		r.viewport.SetContent(r.rendered)
	}

	if oldHeight < r.height && wasAtBottom {
		r.viewport.GotoBottom()
	}
	return nil
}

func (r *MarkdownRenderer) viewportNeeded() bool {
	return r.height > r.viewport.Height
}

func RunRenderer(input io.Reader) error {
	renderer, err := NewMarkdownRenderer()
	if err != nil {
		return fmt.Errorf("failed to create renderer: %w", err)
	}

	p := tea.NewProgram(renderer)

	// Decode JSON objects in a goroutine as they arrive
	go func() {
		dec := json.NewDecoder(input)

		for {
			var data InputData
			// Read one JSON object from the stream
			// println(input)
			if err := dec.Decode(&data); err != nil {
				if err == io.EOF {
					// End of stream
					break
				}
				// Non-EOF error
				fmt.Fprintf(os.Stderr, "Error decoding JSON: %v\n", err)
				break
			}

			// Dispatch messages to Bubble Tea
			p.Send(ToolMsg(data.Tool))
			p.Send(EventMsg(data.Event))
			p.Send(StreamContent(data.Text))
		}

		// Once the stream ends or there's an error, quit Bubble Tea
		p.Send(tea.Quit())
	}()

	_, err = p.Run()
	return err
}

func main() {
	if err := RunRenderer(os.Stdin); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

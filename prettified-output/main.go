package main

import (
	"bufio"
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

// StreamContent represents new content being streamed in
type StreamContent string

// MarkdownRenderer manages the streaming rendering of markdown content
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

	// New: spinner
	spinner spinner.Model
}

// NewMarkdownRenderer sets up our renderer and spinner
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
	}, nil
}

func (r *MarkdownRenderer) Init() tea.Cmd {
	// Start the spinner
	return r.spinner.Tick
}

// Update handles tea.Msg events and updates the renderer state
func (r *MarkdownRenderer) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmds []tea.Cmd

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		r.width, r.height = msg.Width, msg.Height
		r.viewport.Width = msg.Width
		r.viewport.Height = msg.Height
		return r, nil

	case StreamContent:
		r.appendContent(string(msg))
		return r, r.updateViewportCmd
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

// View renders the current state
func (r *MarkdownRenderer) View() string {
	// Render either the viewport (if we need scrolling) plus spinner
	// or the rendered text plus spinner.
	var spinnerView = fmt.Sprintf(
		"%s %s",
		r.spinner.View(),
		lipgloss.NewStyle().Italic(true).Render("Loading..."),
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

	// Render the full content
	rendered, err := r.renderer.Render(r.output)
	if err != nil {
		return nil // handle error appropriately
	}

	// Clean up the rendered content
	rendered = strings.TrimRightFunc(rendered, unicode.IsSpace)
	rendered = strings.ReplaceAll(rendered, "\t", strings.Repeat(" ", 4))
	r.rendered = rendered + "\n"

	// Calculate new dimensions
	r.height = lipgloss.Height(r.rendered)

	// Handle viewport content
	if r.width > 0 {
		truncated := r.lipgloss.NewStyle().MaxWidth(r.width).Render(r.rendered)
		r.viewport.SetContent(truncated)
	} else {
		r.viewport.SetContent(r.rendered)
	}

	// Auto-scroll if we were at the bottom
	if oldHeight < r.height && wasAtBottom {
		r.viewport.GotoBottom()
	}

	return nil
}

func (r *MarkdownRenderer) viewportNeeded() bool {
	return r.height > r.viewport.Height
}

// RunRenderer starts the Bubble Tea program
func RunRenderer(input io.Reader) error {
	renderer, err := NewMarkdownRenderer()
	if err != nil {
		return fmt.Errorf("failed to create renderer: %w", err)
	}

	p := tea.NewProgram(renderer)

	// Start reading input in a goroutine
	go func() {
		reader := bufio.NewReader(input)
		buf := make([]byte, 1024)

		for {
			n, err := reader.Read(buf)
			if n > 0 {
				p.Send(StreamContent(buf[:n]))
			}
			if err == io.EOF {
				p.Send(tea.Quit()) // Signal Bubble Tea to quit on EOF
				break
			}
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
				p.Send(tea.Quit()) // Optionally signal quit on error as well
				break
			}
		}
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

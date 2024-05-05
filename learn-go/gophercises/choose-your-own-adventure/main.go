package main

import (
	"encoding/json"
	"fmt"
	"html/template"
	"log"
	"net/http"
	"os"
)

func init() {
	tpl = template.Must(template.New("").Parse(defaultHtmlTemplate))
}

var tpl *template.Template

var defaultHtmlTemplate = `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Choose your own adventure</title>
</head>
<body>
    <h1>{{.Title}}</h1>
    {{ range .Paragraphs }}
        <p>{{.}}</p>
    {{end}}
    <ul>
        {{range .Options}}
            <li><a href="{{.Arc}}">{{.Text}}</a></li>
        {{end}}
    </ul> </body>
</html>
`

type Story map[string]StoryArc

type StoryArc struct {
	Title      string   `json:"title"`
	Paragraphs []string `json:"story"`
	Options    []struct {
		Text string `json:"text"`
		Arc  string `json:"arc"`
	} `json:"options"`
}

type handler struct {
	s Story
}

func NewHandler(s Story) http.Handler {
	return handler{s}
}

func (h handler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	err := tpl.Execute(w, h.s["intro"])
	if err != nil {
		panic(err)
	}
}

func main() {
	data, err := os.ReadFile("gopher.json")
	if err != nil {
		log.Fatal(err)
	}
	var story Story
	err = json.Unmarshal(data, &story)
	if err != nil {
		log.Fatal("Error during Unmarshal(): ", err)
	}

	h := NewHandler(story)
	fmt.Println("Starting server")
	http.ListenAndServe("localhost:8080", h)
}

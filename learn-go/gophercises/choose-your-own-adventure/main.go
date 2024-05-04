package main

import (
	"fmt"
	"html/template"
	"io"
	"net/http"
	"os"
)

func getRoot(w http.ResponseWriter, r *http.Request)  {
	fmt.Println("Request received for root path")
	io.WriteString(w,"Hello World!!")
}

func main() {
	// http.HandleFunc("/",getRoot)
	// http.ListenAndServe("localhost:8080",nil)
	// fmt.Println("exited!!")
	tmplFile := "home.tmpl"
	tmpl, err := template.New(tmplFile).ParseFiles(tmplFile)
	if err != nil {
		panic(err)
	}
	err = tmpl.Execute(os.Stdout, "Ram")
	if err != nil {
		panic(err)
	}
}

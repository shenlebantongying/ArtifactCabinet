/*
Basic http in go with

* template
* persistent data

http://localhost:8080/?name=john
*/
package main

import (
	"fmt"
	"html/template"
	"math/rand"
	"net"
	"net/http"
)

type CounterHandler struct {
	Counter int
	Name    string
	Tmp     template.Template
}

func makeRadCounter() *CounterHandler {
	tmp := template.Must(template.ParseFiles("what.html"))
	return &CounterHandler{Counter: rand.Intn(50) + 50, Name: "what", Tmp: *tmp}
}

func (ct *CounterHandler) renderHtml(writer *http.ResponseWriter) {
	err := ct.Tmp.Execute(*writer, ct)
	if err != nil {
		fmt.Println("tmp failed")
		return
	}
}

func (ct *CounterHandler) ServeHTTP(writer http.ResponseWriter, request *http.Request) {

	ct.Counter++

	strings, ok := request.URL.Query()["name"]
	if ok {
		ct.Name = strings[0]
	}
	ct.renderHtml(&writer)
}

func main() {

	http.Handle("/", makeRadCounter())
	listener, err := net.Listen("tcp", ":0")

	if err != nil {
		fmt.Println("Cannot listen!")
	}
	fmt.Println("Listening -> ", listener.Addr().String())
	err = http.Serve(listener, nil)
	if err != nil {
		fmt.Println("Cannot Serve!")
	}

}

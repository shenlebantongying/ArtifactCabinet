package main

import "fmt"

// one direction linked_list
type chain struct {
	value int
	next  *chain
}

type LinkedList struct {
	head   *chain
	length int
}

func slbGet() LinkedList {
	return LinkedList{nil, 0}
}

func slbLen(l LinkedList) int {
	return l.length
}

func slbAppend(l *LinkedList, val int) {
	ptr := l.head

	n := chain{}
	n.value = val

	if l.length == 0 {
		l.head = &n
		l.length++
		return
	}

	for i := 0; i < l.length; i++ {
		if ptr.next == nil {
			l.length += 1
			ptr.next = &n
			return
		}
		ptr = ptr.next
	}
}

func slbInsert(l *LinkedList, pos int, val int) error {

	if pos >= l.length {
		return fmt.Errorf("too long")
	}

	l.length += 1

	cur := l.head
	for i := 0; i < pos; i++ {
		cur = cur.next
	}

	toBeInserted := chain{}
	toBeInserted.value = val

	toBeInserted.next = cur.next
	cur.next = &toBeInserted

	return nil
}

type handler func(int)

// pass function as parameter
func slbIterate(l *LinkedList, h handler) {
	ptr := l.head

	for {
		if ptr == nil {
			break
		}
		h(ptr.value)
		ptr = ptr.next

	}
}

func main() {
	var my = slbGet()

	slbAppend(&my, 8)
	slbAppend(&my, 7)
	slbAppend(&my, 6)
	slbAppend(&my, 5)

	_ = slbInsert(&my, 1, 3)

	println("Length", slbLen(my))
	slbIterate(&my, func(i int) {
		println(i)
	})
}

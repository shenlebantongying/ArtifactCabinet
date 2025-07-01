package main

import (
	"fmt"
	"os"
)

type Ctx struct {
	err  error
	data []int
}

func NewCtx() Ctx {
	return Ctx{nil, make([]int, 0)}
}

func (c *Ctx) AppendPositiveInt(i int) *Ctx {
	if c.err != nil || i < 0 {
		return c
	}

	c.data = append(c.data, i)

	return c
}

func (c *Ctx) Execute() error {
	if c.err != nil {
		return c.err
	}

	return nil
}

func main() {

	ctx := NewCtx()
	if err := ctx.AppendPositiveInt(2).AppendPositiveInt(3).Execute(); err != nil {
		os.Exit(1)
	}

	fmt.Println(ctx.data)

}

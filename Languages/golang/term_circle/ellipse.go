package main

import (
	"fmt"
	"math"
	"strings"
)

// Coordinate: Right-Down as positive
// We only have Int type
// Return always positive on the right
// x^/a^2 + y^2/b^2 = 1
// Always positive
// [Elp] -> Ellipse

type Elp struct {
	a float64 // x -> horizontal half radius
	b float64 // y -> vertical half radius
}

func p(s string) {
	fmt.Print(s)
}

func pl(s string) {
	fmt.Println(s)
}

// x -> length a
// y -> length b
// Get two Y coordinates
func ElpHorizontalCords(e Elp, y float64) (float64, float64) {

	y = y - e.b

	if y > ElpMaxVertical(e) {
		panic("Y exceed valid range")
	}

	corX := e.a * math.Sqrt(1.0-math.Pow(y, 2)/math.Pow(e.b, 2))
	return e.a - corX, e.a + corX
}

func ElpMaxVertical(e Elp) float64 {
	return 2 * e.b
}

func ElpPrintHorizontal(e Elp, y float64) {
	fillChar :=" "
	shapeChar:="▀"
	x1, x2 := ElpHorizontalCords(e, y)
	rx1, rx2 := int(math.Round(x1)), int(math.Round(x2))
	if rx1 == rx2 {
		fmt.Println(strings.Repeat(fillChar, rx1+1) + shapeChar + strings.Repeat(fillChar, rx1))
	} else {
		fmt.Println(strings.Repeat(fillChar, rx1) + shapeChar + strings.Repeat(fillChar, rx2-rx1) + shapeChar + strings.Repeat(fillChar, rx1))
	}
}

func main() {
	// 38;2 is magical number
	fmt.Println(Esc + "38;2;74;217;112m")
	e := Elp{a: 60, b: 10}
	for y := 0.0; y <= e.b*2.0; y++ {
		ElpPrintHorizontal(e, y)
	}
	fmt.Println(ColorReset)
}

package main

import "fmt"

// plan:
// make a list that suit structures like [+, 1, [- 3 2]]

// TODO: this is a possible structure for a lisp interepter

type TokType int

const (
	tokenAtom   TokType = iota
	tokenNumber TokType = iota
	tokenString TokType = iota
)

type Atom struct {
	typ  TokType
	text string
	num  int // nil for non-number
}

type Expr struct {
	atom  *Atom // nil when is a slice // <- parser's job
	sexpr []interface{}
}

func pprint(expr Expr) {
	if expr.atom == nil {
		for _, v := range expr.sexpr {
			switch v.(type) {
			case Atom:
				pprint_atom(v.(Atom))
			case Expr:
				pprint(v.(Expr))
			}
		}
	} else {
		pprint_atom(*expr.atom)
	}
}

func pprint_atom(atom Atom) {
	switch atom.typ {
	case tokenNumber:
		fmt.Println("Atom: Num", atom.num)
	default:
		fmt.Println("Atom: Str", atom.text)
	}
}

func main() {
	// example of atom
	var single_atom_expr = Expr{
		atom: &Atom{
			typ:  tokenNumber,
			text: "",
			num:  3,
		},
		sexpr: nil,
	}
	fmt.Println("Single atom expr")
	pprint(single_atom_expr)

	// example of sexp (aka, a expression with multiple parts)
	var an_sexpr_with_single_atom = Expr{
		atom: nil,
		sexpr: []interface{}{Atom{
			typ:  tokenNumber,
			text: "",
			num:  5,
		}},
	}

	fmt.Println("\nSexpr but with only one atom")
	pprint(an_sexpr_with_single_atom)

	var an_sexpr_with_two_atom = Expr{
		atom: nil,
		sexpr: []interface{}{
			Atom{typ: tokenNumber, text: "", num: 5},
			Atom{typ: tokenString, text: "This is nice", num: 0}},
	}

	fmt.Println("\nSexpr but with 2 atoms")
	pprint(an_sexpr_with_two_atom)
}

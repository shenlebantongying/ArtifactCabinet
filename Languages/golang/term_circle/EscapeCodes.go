package main

import "strconv"

// Notes:



const (
	Esc = "\033["
)

func i2s(i int)string{
	return strconv.Itoa(i)
}

// [cur movement]
//**********************************************************************************************************************

func curUp(n int) string{
	return Esc+strconv.Itoa(n)+"A"
}

func curDown(n int) string{
	return Esc+strconv.Itoa(n)+ "B"
}

func curForward(n int) string{
	return Esc+strconv.Itoa(n)+ "C"
}

func curBackward(n int) string{
	return Esc+strconv.Itoa(n)+ "D"
}

func curNextLine(n int) string{
	return Esc+strconv.Itoa(n)+ "E"
}

func curPreviousLine(n int) string{
	return Esc+strconv.Itoa(n)+ "F"
}

// [Imported]
//**********************************************************************************************************************

const (
	NewScreen = Esc + "2J" //"Clear" by scroll down


	CursorUp       = Esc + "A"
	CursorDown     = Esc + "B"
	CursorForward  = Esc + "C"
	CursorBackward = Esc + "D"
	CursorNextLine = Esc + "E"
	CursorPrevLine = Esc + "F"
	CursorLeft     = Esc + "G"
	CursorTop      = Esc + "d"
	CursorTopLeft  = Esc + "H"

	CursorBlinkEnable  = Esc + "?12h"
	CursorBlinkDisable = Esc + "?12I"
	CursorShow         = Esc + "?25h"
	CursorHide         = Esc + "?25l"

	ScrollUp   = Esc + "S"
	ScrollDown = Esc + "T"

	TextInsertChar = Esc + "@"
	TextDeleteChar = Esc + "P"
	TextEraseChar  = Esc + "X"
	TextInsertLine = Esc + "L"
	TextDeleteLine = Esc + "M"

	EraseRight  = Esc + "K"
	EraseLeft   = Esc + "1K"
	EraseLine   = Esc + "2K"
	EraseDown   = Esc + "J"
	EraseUp     = Esc + "1J"
	EraseScreen = Esc + "2J"

	TextColorBlack         = Esc + "30m"
	TextColorRed           = Esc + "31m"
	TextColorGreen         = Esc + "32m"
	TextColorYellow        = Esc + "33m"
	TextColorBlue          = Esc + "34m"
	TextColorMagenta       = Esc + "35m"
	TextColorCyan          = Esc + "36m"
	TextColorWhite         = Esc + "37m"
	TextColorBrightBlack   = Esc + "30;1m"
	TextColorBrightRed     = Esc + "31;1m"
	TextColorBrightGreen   = Esc + "32;1m"
	TextColorBrightYellow  = Esc + "33;1m"
	TextColorBrightBlue    = Esc + "34;1m"
	TextColorBrightMagenta = Esc + "35;1m"
	TextColorBrightCyan    = Esc + "36;1m"
	TextColorBrightWhite   = Esc + "37;1m"

	BackgroundColorBlack         = Esc + "40m"
	BackgroundColorRed           = Esc + "41m"
	BackgroundColorGreen         = Esc + "42m"
	BackgroundColorYellow        = Esc + "43m"
	BackgroundColorBlue          = Esc + "44m"
	BackgroundColorMagenta       = Esc + "45m"
	BackgroundColorCyan          = Esc + "46m"
	BackgroundColorWhite         = Esc + "47m"
	BackgroundColorBrightBlack   = Esc + "40;1m"
	BackgroundColorBrightRed     = Esc + "41;1m"
	BackgroundColorBrightGreen   = Esc + "42;1m"
	BackgroundColorBrightYellow  = Esc + "43;1m"
	BackgroundColorBrightBlue    = Esc + "44;1m"
	BackgroundColorBrightMagenta = Esc + "45;1m"
	BackgroundColorBrightCyan    = Esc + "46;1m"
	BackgroundColorBrightWhite   = Esc + "47;1m"

	ColorReset = Esc + "0m"

	ClearScreen = "\u001Bc"
)


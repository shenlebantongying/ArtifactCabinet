import curses

def main(stdscr):
    stdscr.addstr("nice")

    stdscr.refresh()
    stdscr.getkey()

curses.wrapper(main)
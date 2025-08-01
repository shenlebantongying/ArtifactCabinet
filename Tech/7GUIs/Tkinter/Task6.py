import tkinter as tk
import tkinter.ttk as ttk

# Read the tag section <https://tkdocs.com/tutorial/canvas.html>

root = tk.Tk()
root.title("Task1")
root.rowconfigure(0, weight=1)
root.columnconfigure(0, weight=1)

mainFrame = ttk.Frame(root)
mainFrame.grid(column=0, row=0, sticky="nswe")

canvas = tk.Canvas(mainFrame, width=500, height=500, background="white")
canvas.grid(column=9, row=0)


def printCoordinate(event):
    print(event.x, event.y)


canvas.bind("<Button-1>", printCoordinate)

root.mainloop()

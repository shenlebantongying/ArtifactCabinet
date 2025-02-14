import tkinter as tk
import tkinter.ttk as ttk

root = tk.Tk()
root.title("Task1")
root.rowconfigure(0, weight=1)
root.columnconfigure(0, weight=1)

mainFrame = ttk.Frame(root)
mainFrame.grid(column=0, row=0, sticky='nswe')

mainFrame.rowconfigure(0, weight=1)

mainFrame.columnconfigure(0, weight=1)
mainFrame.columnconfigure(1, weight=1)

lbl = ttk.Label(mainFrame, text="0")
lbl.grid(column=0, row=0, sticky='nswe', pady=10, padx=10)

tkCounterVar = tk.StringVar()
lbl['textvariable'] = tkCounterVar


def counter(i):
    n = i
    while True:
        yield n
        n += 1


x = counter(0)
tkCounterVar.set(next(x))


def update_label():
    tkCounterVar.set(next(x))


btn = ttk.Button(mainFrame, text="count", command=update_label)
btn.grid(column=1, row=0, sticky='nswe')

root.mainloop()

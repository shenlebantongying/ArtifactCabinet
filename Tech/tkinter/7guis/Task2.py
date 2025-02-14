import tkinter as tk
import tkinter.ttk as ttk

# Possible improvement:
# TODO: type checking, pervent str type
# TODO: Use add_trace()
# TODO: check trace_info

# TODO: This implementation is bad
# You should separate the Data about temperature and presentation of it, so that they will not infinitely update each other.
# Its called "bidirectional data flow". However, if they just share a space, the "flow" doesn't required at all.

root = tk.Tk()
root.title("Task1")
root.rowconfigure(0, weight=1)
root.columnconfigure(0, weight=1)

mainFrame = ttk.Frame(root)
mainFrame.grid(column=0, row=0, sticky='nswe')

mainFrame.rowconfigure(0, weight=1)

for i in [0, 2]:
    mainFrame.columnconfigure(i, weight=1)

C = tk.StringVar()
F = tk.StringVar()

Ce_entry = ttk.Entry(mainFrame, textvariable=C)
Fa_entry = ttk.Entry(mainFrame, textvariable=F)

Ce_label = ttk.Label(mainFrame, text="Celsius =")
Fa_label = ttk.Label(mainFrame, text="Fahrenheit")

Ce_entry.grid(column=0, row=0, sticky='nswe')
Ce_label.grid(column=1, row=0, sticky='nswe')
Fa_entry.grid(column=2, row=0, sticky='nswe')
Fa_label.grid(column=3, row=0, sticky='nswe')


def C_update(*args):
    Fa_entry.delete(0, 'end')
    Fa_entry.insert(0, float(C.get()) * (9 / 5) + 32)


def F_update(*args):
    Ce_entry.delete(0, 'end')
    Ce_entry.insert(0, (float(F.get()) - 32) * (5 / 9))


Ce_entry.bind('<KeyRelease>', C_update)
Fa_entry.bind('<KeyRelease>', F_update)

root.mainloop()

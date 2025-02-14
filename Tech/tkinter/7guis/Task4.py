import tkinter as tk
import tkinter.ttk as ttk
import time

# TODO: Sliderbar for Duration.

start_time = time.time()


def update_timer():
    itimer.set(round(time.time() - start_time))
    progress['value'] = itimer.get()
    root.after(100, update_timer)


def reset_timer():
    global start_time
    start_time = time.time()


#

root = tk.Tk()
root.title("Task4")
root.rowconfigure(0, weight=1)
root.columnconfigure(0, weight=1)

mainFrame = ttk.Frame(root)
mainFrame.grid(column=0, row=0, sticky='nswe')

timer_label = ttk.Label(mainFrame)

resetBtn = ttk.Button(mainFrame, text='reset', command=reset_timer)

itimer = tk.IntVar()
timer_label['textvariable'] = itimer

## progressbar

progress = ttk.Progressbar(mainFrame, orient=tk.HORIZONTAL, length=100, mode='determinate')

## sliderbar
# slider_label=ttk.Label(mainFrame,text="Duration")
# sliderbar = ttk.Scale(mainFrame, orient=tk.HORIZONTAL, length=100, from_=1.0, to=100.0)


#

elapsed_label = ttk.Label(mainFrame, text="Elapsed Time")
elapsed_label.grid(column=0, row=0, sticky='nswe')

progress.grid(column=1, row=0, sticky='nswe')

timer_label.grid(column=0, row=1, sticky='nswe')

# slider_label.grid(column=0, row=2, sticky='nswe')
# sliderbar.grid(column=1, row=2, sticky='nswe')

resetBtn.grid(column=0, row=3, columnspan=2, sticky='nswe')

update_timer()
root.mainloop()

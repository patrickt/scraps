# ======================================
# = Sample Curses-based Python program =
# ======================================

# Written by Patrick Thomson with help from Alex Levenson

import curses, sys, time, random, string
scr = curses.initscr()

curses.noecho()
curses.cbreak()

def random_char(c):
    if random.randint(0, 1):
        return ''
    else:
        choice = c
        while choice == c:
            choice = random.choice(string.ascii_letters)
        return choice

def add_typo(text):
    if(random.randint(1, 6) != 1):
        for ii in xrange(1, random.randint(1, 3)):
            position = random.randint(0, len(text) - 1)
            text = text[0:(position - 1)] + random_char(text[position]) + text[position:]
    return text

def exit_function():
    curses.nocbreak()
    scr.keypad(1)
    curses.echo()
    curses.endwin()
    
sys.exitfunc = exit_function

try:
    while True:
        if(random.randint(0,4) == 1):
            line = line.upper()
            speed = 0.1
        else:
            line = add_typo("All work and no play makes Jack a dull boy.") + "\n"
            speed = .25
        for ch in line:
            scr.addstr(ch)
            scr.refresh()
            time.sleep(random.random() * speed)
finally:
    exit_function()
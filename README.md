Kilo
===

Usage: kilo `<filename>`

Keys:

    CTRL-S: Save
    CTRL-Q: Quit
    CTRL-F: Find string in file (ESC to exit search, arrows to navigate)

Kilo does not depend on any library (not even curses). It uses fairly standard
VT100 (and similar terminals) escape sequences. The project is in alpha
stage and was written in just a few hours taking code from my other two
projects, load81 and linenoise.

People are encouraged to use it as a starting point to write other editors
or command line interfaces that are more advanced than the usual REPL
style CLI.

Kilo was written by Salvatore Sanfilippo aka antirez and is released
under the BSD 2 clause license.

## kilo/BusyBox

My point is to make kilo into a very small nano-like editor for BusyBox,
don't know if I'll get there though

DONE:
- (atleast slightly) optimized drawing and memory operations
  (did i read it draws the whole screen always with a buffer that is realloc'd on every
   append to it...)
- line cutting and pasting (CTRL-K and U from nano)
- CTRL-W to find
- nano-compatible keybindings (CTRL-X to save and quit, CTRL-O to save)
- improved save routine (doesnt need heap)
- attempt to save on crashy exits 
- improved out of memory handling

TODO:
- shell script syntax highlighting (shell scripts are what you're mostly editing on BusyBox systems anyways)
- BusyBoxy feature configuration:
  - syntax highlighting
    - C
    - shell
  - line cut and paste
  - search


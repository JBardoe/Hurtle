# Hurtle

This is a Haskell parser for a version of the Turtle programming language.
The program can be used to either parse and execute turtle programs or the user can use the 
interactive mode to control the turtle directly with the keyboard.

Some correct and incorrect examples of programs can be found in the examples folder.


## Parser

Users can choose to parse and execute valid programs written in other files. The parser will read 
the file then render the final image. Programs should be written in .hogo files and when prompted 
for the file name, users should enter the entire file path.

Valid commands include:
- forward x        moves the turtle forward by x
- left x           turns the turtle left by x degrees
- right x          turns the turtle right by x degrees
- penup            stops the turtle drawing for the following movements
- pendown          restarts the turtle drawing
- home             moves the turtle back to the center
- clearscreen      clears all drawing on the screen
- repeat x []      repeats the section of code in the brackets x times
- colour x y z     changes the colour of the pen to RGB values xyz (where xyz >=0 and <=1)

Functions can also be defined using "function foo []" and called using "foo()".

Separate commands should be written on separate lines.

The parser is not case-sensitive and will ignore blank lines.

Comments can be written using a semi-colon and can be on their own line or after other commands 
on the same line.

Closing square brackets should be written on their own line.


## Interactive

The turtle can also choose the control the turtle directly. This opens a window and allows the use
of keyboard controls.

Controls include:
- Up Arrow - Move forward 50
- Down Arrow - Move back 50
- Left Arrow - Turn 45 degrees left
- Right Arrow - Turn 45 degrees right
- u - penup
- d - pendown
- c - clearscreen
- h - home
    

    

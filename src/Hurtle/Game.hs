module Hurtle.Game where

{- This module defines the functions used for the interactive form of the viewer. -}

import Hatch
import Hurtle.Types
import Hurtle.RunHogo
import Graphics.Gloss.Interface.Pure.Game

{-
    Main function for using the interactive form of hurtle. It is offered as an option in Main.hs as an alternative to the file parser. 
    The function prints to the terminal all the instructions for using the hurtle (i.e. up arrow for forward), and then uses the play function 
    from Graphics.Gloss.Interface.Pure.Game to render the interactive window. This function and library was chosen as it allows easy user interaction
    with the window and interfaces well with the other structures in the program. For example, TurtleState works as a world state, and allows
    applyCommand to update the world state as needed. The arguments to play are:
    1. Definition of the window it is rendered in
    2. FPS
    3. The function to render the world state which reuses renderTurtleState from the RunHogo module saving on repeated code.
    4. The handler for user input which is defined below
    5. A function to update the state every second which is not used in this case to it is just set to the identity function  
-}
playTurtle :: IO ()
playTurtle = do 
    putStrLn "\n\nWelcome\nUp Arrow -> Move Forward\nDown Arrow -> Move Backwards\nLeft/Right Arrow -> Turn\nu -> Lift Pen Up\nd -> Put Pen Down\nc -> Clear Screen\nh -> Go Home"
    play (InWindow "CS141 Hurtle Programming" (1280, 960) (10, 10)) (makeColor 0.9 0.9 0.9 1) 30 initialState (render . layout . renderTurtleState) userAction (const id)


{-
    Function for handling user input and updating the world state. 
    The function works by pattern matching on user keyboard input and then essentially translating that into hurtle commands. For example, the up arrow is linked 
    to the GoForward command. This allows the it to then call the applyCommand function defined in the RunHogo module to update the TurtleState with the new command
    which therefore updates the window. This reuse of functions makes the code much more readable. Default values are chosen here with 50 as the line length and 45 
    as the angle in order to avoid the user having to type in numbers every time and allowing them to control the hurtle with single key presses.
-}
userAction :: Event -> TurtleState -> TurtleState
userAction (EventKey (SpecialKey KeyUp) Down _ _) state = applyCommand state $ GoForward 50
userAction (EventKey (SpecialKey KeyLeft) Down _ _) state = applyCommand state $ TurnLeft 45
userAction (EventKey (SpecialKey KeyRight) Down _ _) state = applyCommand state $ TurnRight 45
userAction (EventKey (SpecialKey KeyDown) Down _ _) state = applyCommand state $ GoBackward 50
userAction (EventKey (Char 'u') Down _ _) state = applyCommand state PenUp 
userAction (EventKey (Char 'd') Down _ _) state = applyCommand state PenDown 
userAction (EventKey (Char 'c') Down _ _) state = applyCommand state ClearScreen 
userAction (EventKey (Char 'h') Down _ _) state = applyCommand state GoHome
userAction _ state = state


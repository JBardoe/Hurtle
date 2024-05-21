module Hurtle.RunHogo where

{- This module defines the functions for getting the user input and turning the Hogo code into an output image. -}

import Hurtle.Types
import Hatch
import Data.Fixed
import Data.Foldable
import qualified Data.Set as Set
import Text.Megaparsec
import Hurtle.Parser

{-
    Main function for the file parsing functionality of hurtle. It is offered as an option in Main.hs as an alternative to the interactive view.
    It is declared in this module (opposed to main) in order to keep it alongside the functions that build up its output. It starts by getting the 
    file name from the user and reading it in. It then runs the Hogo parser on the file to get the HogoProgram. This returns an Either value 
    with whether the parser succeeded or not so it pattern matches on the left and right constructors to get either the error or program. 
    If it is an error, it is outputted using the errorBundlePretty function from Text-Megaparsec to make it more human readable. 
    Otherwise it uses the runAnimation function from Hatch to render the final TurtleState after the program. As the time does not matter, 
    it is ignored in the lambda and the image is simply rendered. The image is obtained by executing the program on the default TurtleState 
    defined in Types and then calling another function that converts a TurtleState to an image. This separation of concerns makes the function 
    far more readable.
-}
parseFile :: IO ()
parseFile = do
    putStrLn "Enter File Name: "
    name <- getLine
    file <- readFile name 
    let program = parse parseHogo name file
    case program of
        Left err -> putStrLn $ errorBundlePretty err
        Right x -> runAnimation $ \_ -> renderTurtleState $ executeProgram x initialState

{-
    Applies one Hogo command to a turtle state and returns the result. It does this by pattern matching on the HogoCode and destructuring the 
    TurtleState before creating a new TurtleState with the updated values.
-}
applyCommand :: TurtleState -> HogoCode -> TurtleState
--PenDown command simply sets the pen down attribute to true
applyCommand (TS pos ang _ penColour drawn methods) PenDown = TS pos ang True penColour drawn methods

--PenUp command simply sets the pen down attribute to false
applyCommand (TS pos ang _ penColour drawn methods) PenUp = TS pos ang False penColour drawn methods

--ClearScreen command resets the drawn lines to an empty list
applyCommand (TS pos ang pen penColour _ methods) ClearScreen = TS pos ang pen penColour [] methods

{- GoForward command first calculates the coordinates of the endpoint using the current postion, angle and line length. 
   It then checks whether the pen is currently down. If it is, it adds the new line to the drawn lines with the old and new coordinates
   alongside the colour of the line. Otherwise the list of drawn lines remains unchanged. Finally, it returns a turtle state with an updated
   hurtle position and drawn lines.
-}
applyCommand (TS (x,y) ang pen penColour drawn methods) (GoForward n) =
    let
        newPos = (x+ (n * sin (toRadians ang)), y + (n * cos (toRadians ang)))
        newLines = if pen then (penColour, (x,y), newPos):drawn else drawn
    in
        TS newPos ang pen penColour newLines methods

{- GoBackward command first flips the angle around and then uses the same logic as GoForward to calculate the new position and line. 
   It returns this new turtle state in the same way as GoForward.
-}
applyCommand (TS (x,y) ang pen penColour drawn methods) (GoBackward n) =
    let
        movingAngle = (ang + 180) `mod'` 360
        newPos = (x+ (n * sin (toRadians movingAngle)), y + (n * cos (toRadians movingAngle)))
        newLines = if pen then (penColour, (x,y), newPos):drawn else drawn
    in
        TS newPos ang pen penColour newLines methods

--TurnLeft command subtracts the amount given from the current angle to turn anti-clockwise and then performs modular division by 360 to make it valid
applyCommand (TS pos ang pen penColour drawn methods) (TurnLeft n) = TS pos ((ang - n) `mod'` 360) pen penColour drawn methods

--TurnRight command adds the amount given to the current angle to turn clockwise and then performs modular division by 360 to make it valid
applyCommand (TS pos ang pen penColour drawn methods) (TurnRight n) = TS pos ((ang + n) `mod'` 360) pen penColour drawn methods

--GoHome command simply resets the hurtle coordinates to (0,0)
applyCommand (TS _ ang pen penColour drawn methods) GoHome = TS (0,0) ang pen penColour drawn methods

--PenColour command updates the colour attribute of the state to the new values
applyCommand (TS pos ang pen _ drawn methods) (PenColour r g b) = TS pos ang pen (r,g,b) drawn methods

{- Repeat command is performed by using a helper function to recursively execute the repeated code n times. It is declared in a where statement to
   obscure it from the top level and works by decrementing a counter each time it applies the program to the state and then stops once the counter
   is 0. 
-}
applyCommand currentState (Repeat n program) = repetition n currentState program
    where
        repetition :: Int -> TurtleState -> HogoProgram -> TurtleState
        repetition 0 state _ = state
        repetition x state code = repetition (x-1) (executeProgram program state) code 

--Function definition works by inserting a new function into the set of recognised functions
applyCommand (TS pos ang pen penColour drawn methods) (FunctionDef name program) = TS pos ang pen penColour drawn (Set.insert (name, program) methods)

{- Function calling works using the the find function to try to find the function in the existing set by using a lambda to match the name
   This returns a maybe value which is pattern matched upon. If the function was found, it is executed using the executeProgram function on the 
   currentState. Otherwise an error is raised that the function cannot be found.
-}
applyCommand (TS pos ang pen penColour drawn methods) (FunctionCall name) =
    let
        function = find (\x -> fst x == name) methods
        state = case function of 
            Just (_,code) -> executeProgram code (TS pos ang pen penColour drawn methods)
            Nothing -> error $ "Function " ++ name ++ " not found."
    in
        state 

{- Converts degrees to radians to fit with the gloss library -}
toRadians :: Float -> Float 
toRadians = (*) (pi/180)  

{-
    Executes a whole HogoProgram through recursively applying the applyCommand function. 
    This function uses foldl to neatly and efficiently call the applyCommand function on an updating turtle state result over a HogoProgram which 
    is a list of commands. Foldl is used here because it causes it to start from the currentState fed in and then apply the function several 
    times so it happens in the correct order.
-}
executeProgram :: HogoProgram -> TurtleState -> TurtleState
executeProgram program currentState = foldl applyCommand currentState program  

{-
    Converts a turtle state to an image to be rendered.
    This function works using two helper functions as it allows the turtle and lines to be drawn separately while hiding those functions from 
    the top level. The printLines function explicitely recurses over the list of lines and draws each one using the line function from Hatch 
    (which has been updated in this application to include a colour). The base case shows a blank image. The printTurtle function uses offset
    to place the turtle at the right coordinates, rotates the turtle to the correct angle, and shrinks it down to better match the size of the 
    lines. 
-}
renderTurtleState :: TurtleState -> Image
renderTurtleState (TS (x,y) ang _ _ drawn _) = printLines drawn <@> printTurtle
    where
        printLines :: [((Float, Float, Float), (Float, Float), (Float, Float))] -> Image
        printLines [] = blank
        printLines ((lineColour,(x1,y1),(x2,y2)):xys) = line lineColour x1 y1 x2 y2 <@> printLines xys

        printTurtle :: Image
        printTurtle = offset x y (scale 0.4 $ rotate ang ant)

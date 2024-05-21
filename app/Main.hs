import Hurtle.RunHogo
import Hurtle.Game

{-
    Main function for running the hurtle viewer. It starts by asking which function and accepting input. It then calls either the file parser
    or the interactive window depending on their input. These have been placed in separate files to keep them alongside their associated functions.
    It also keeps the main file simpler and cuts down on the number of imports. If the user inputs an unrecognised input, the function calls 
    itself to keep prompting them until they input a correct one thus stopping the program from crashing.
-}
main :: IO ()
main = do
    putStrLn "Would you like to parse a file (F) or control the turtle directly (T)?"
    option <- getLine
    case option of 
        "F" -> parseFile 
        "T" -> playTurtle
        _ -> main



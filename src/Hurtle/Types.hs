module Hurtle.Types where

import Text.Megaparsec
import Data.Void
import Data.Set
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Type Definitions

-- | A Hogo program is a list of HogoCode instructions.
type HogoProgram = [HogoCode]

data HogoCode
  -- | Movement Commands
  = GoForward Float
  | GoBackward Float
  | TurnLeft Float
  | TurnRight Float
  | GoHome
  -- | Pen Commands
  | PenUp
  | PenDown
  | ClearScreen
  | PenColour Float Float Float --Changing the colour of the pen 
  -- | Control Flow
  | Repeat Int HogoProgram
  | FunctionDef String HogoProgram --Declaration of a function
  | FunctionCall String --Calling a function
  deriving (Show,Read,Eq,Ord)

-- | This is an alias for the Megaparsec parser type; the "Void" tells it that we don't have any custom error type, and the "string" tells it that we're parsing strings.
type Parser = Parsec Void String

{-
    The TurtleState datatype defined the state of the current image at any one time. It is also used as the world state in Game.hs. Wrapping these
    values together into the TurtleState datatype 
-}
data TurtleState = TS {
    position :: (Float, Float), --Current coordinates of the hurtle
    angle :: Float, --Angle clockwise from straight upwards (in degrees)
    penDown :: Bool, --Whether the hurtle is drawing or not
    colour :: (Float, Float, Float), --Colour of the pen in RGB values
    linesDrawn :: [((Float, Float, Float), (Float, Float), (Float,Float))], --Lines already drawn (RGB colour followed by the start and end coordinates)
    functions :: Set (String, HogoProgram) --Functions already defined stored as a set so only one function of a name can exist
}
    deriving (Eq, Show)

{-
    Initial turtle state.
    It is declared here so it can be used in both runHogo and Game without having to be redefined. Initially all stores are set to empty, the pen 
    colour is black and the hurtle is in the middle of the screen.
-}
initialState :: TurtleState
initialState  = TS (0,0) 0 True (0,0,0) [] Set.empty
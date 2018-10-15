{-
Title : Chatbot with visual elements
Participants : Kennedy Ellison, Leslie Goloh
Date  : 5/1/2017 -}


{-# LANGUAGE TypeOperators, TypeInType, GADTs, TypeFamilies,
             UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables,
             TypeApplications, StandaloneDeriving #-}

import System.IO
import Graphics.Gloss
import System.Exit
import Control.Monad
import Control.Concurrent
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
import Data.Monoid


data Response where
   PrintText:: String->Response
   ShowPicture:: Picture ->Response
   QuitProgram:: Response


--Variable names used

width, height, offset :: Int
width = 300

height = 300

offset = 100

posA, posB, new_radius, radius :: Float
posA = 0

posB = 0

new_radius = 0

radius = 0

window :: Display
window = InWindow "Haskell Paint" (width, height) (offset, offset)

background :: Color
background = light(light blue)

drawing :: Picture
drawing = Blank

{-Contains the Game State,
   That is, the line and features of the circle
   and the list of all pictures drawn in prev_pics -}
   
data Zodiac = Game { z_line   :: Path              
                   , z_circle :: Float                             
                   , circ_radius :: Float
                   , positionA :: Float
                   , positionB :: Float   --position of the circle
                   , circ_col :: Color
                   , prev_pics:: [Picture]
                   }                                    

--Initial State
initState :: Zodiac
initState  =  Game { z_line   = []      
                   , z_circle = radius
                   , circ_radius = new_radius
                   , positionA = posA
                   , positionB = posB
                   , circ_col = white
                   , prev_pics= []
                   }

{-Shows the game state as a picture.-} 
gen_picture :: Zodiac -> Picture
gen_picture game  = pictures (lines : circ :  all_pic)
              where
                 lines   = line(z_line game)
                 circ    = translate (positionA game) (positionB game)$ color (circ_col game) (circleSolid (z_circle game))
                 all_pic = (prev_pics game)

handle_inputs :: Event -> Zodiac -> Zodiac
handle_inputs ev z
              |EventKey (Char 'e') _ _ _ <- ev                         --pressing ‘e’ enlarges the circle
                  = z {z_circle = (z_circle z) + 5}
              |EventKey (Char 'c') _ _ _ <- ev                         --creates a new circle and adds it to the list
                  = z {z_circle = 10
                      ,prev_pics = (translate (positionA z) (positionB z)
                      $ (circleSolid (z_circle z))) : (prev_pics z)}
              |EventKey (SpecialKey KeyRight) Down _ _  <- ev          --moves circle maker to the right
                  = z {positionA = (positionA z) + 5}
              |EventKey (SpecialKey KeyDown) Down _ _ <- ev            --moves the circle maker down
                  = z {positionB = (positionB z) - 5}
              |EventKey (SpecialKey KeyUp) Down _ _ <- ev              --moves the circle maker up
                  = z {positionB = (positionB z) + 5}
              |EventKey (SpecialKey KeyLeft) Down _ _ <- ev            --moves the circle maker left
                  = z {positionA = (positionA z) - 5}
              |EventKey (MouseButton LeftButton) Down  _ pt@(x, y)  <- ev       
                  = z {z_line = pt:(z_line z)}                         --create a new line/continue existing line
              |EventKey(Char 'q')_ _ _ <- ev                           --forcefully ends the game. 
                  = error "Game Over"
handle_inputs _ z = z

stepGame :: Float -> Zodiac -> Zodiac                                      
stepGame _ z = z { z_circle = radius'
                 }
        where
           radius = z_circle z
           new_radius = circ_radius z
           radius' = radius + new_radius




{-The functions recCircles, sadResponse, boredResponse,
  and happyResponse are all used to generate the images -}
  
 {-recCircles recursively draws circles of a certain radius and thickness
  it returns text and all the other circles when the radius is zero-}
recCircles:: Float->Float-> Picture
recCircles rad thick
  |rad<= 0=pictures ((Translate ((-x/1.5)) ((y/10))
    $ Text "I'm glad you're feeling")
    :(Translate ((-x/5.5)) ((y/35))  --move the next text
    $ Text "great!"):[])
  |otherwise= pictures((Color yellow $ thickCircle((rad/2)*5) thick )
    :(Color green $ thickCircle((rad/2)*15) thick)
    :(recCircles (rad-25) (thick)):[])
 where
     --window size
     x=1000
     y=1500 
 

--uses a list of picture to display a smiley face and some text
sadResponse =
 ShowPicture 
        (Translate (-220) (-20)   -- shift the text in the window
        $ Scale 0.25 0.25         -- display it a quarter the original size
        $ Pictures ((Color yellow
        $ Translate (x/1.25) ((y/5)) (circleSolid 540))
        :(Color black $Translate(x/1.25) ((y/5.25)) (thickArc 210 330 150 20))
        : (Translate (0) (-(y/2)) (Text "I'm sorry you aren't feeling your best!"))
        :(Color black $ Translate (x/1.65) ((y/4)) (circleSolid 50))
        :(Color black $ Translate (x/1.05) ((y/4)) (circleSolid 50)):[]))
    where
       --window size
       x=1000
       y=1500
    
boredResponse = PrintText("You're bored? How about a game?")

happyResponse=ShowPicture
       (Translate(0) (0)
        $ Scale 0.5 0.5
        $ Pictures((recCircles ((x/5)*(y/15)) (20)):[]))
     where
       --window size
       x=1000
       y=1500
  
endResponse      = PrintText ("Goodbye")

--response if nothing is matched
continueResponse = PrintText ("Tell me more about how you are feeling")

respResponse = PrintText ("Please tell me how you are feeling")

--list of words which specific reactions
positiveWords      = ["good", "happy", "nice"]
negativeWords      = ["sad",  "bad" , "disappointed", "down"]
boredWords         = ["bored", "nothing"]
responsiveWords    = ["hello","hi"]

--responses to be chosen
chooseResponse:: String-> Response
chooseResponse "bye"                     = endResponse
chooseResponse input
  | any (flip elem positiveWords)   ws   = happyResponse
  | any (flip elem boredWords)      ws      = boredResponse
  | any (flip elem negativeWords)   ws   = sadResponse
  | any (flip elem responsiveWords) ws   = respResponse
  | otherwise                            = continueResponse 
  where
    ws = words input 

{-renderResponse displays responses with IO.
 The data type for response is at the beginning of the progrma. -}
renderResponse:: Response -> IO()
--if user says bye, exit the program
renderResponse (PrintText t) 
  | t=="Goodbye" = do
   putStrLn t
   exitSuccess
--if user is bored, play the game
  |t== "You're bored? How about a game?"= do
   play  window
             background
             50
             initState
             gen_picture
             handle_inputs
             stepGame
  | otherwise = putStrLn t
--if response is a ShowPicture, then display the picture
renderResponse (ShowPicture p) = void(forkIO(display  (InWindow
                "Window"           -- window title
                (1000, 1500)       -- window size
                (10, 10))         
                white  p))

loop = do
  putStr "> "
  hFlush stdout
  input <- getLine
  let resp = chooseResponse input
  renderResponse resp
  loop

main =do
 putStrLn "Hi there! Tell me how you are feeling"
 loop

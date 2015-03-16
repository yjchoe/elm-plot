module Example where

{-| Interactive 2D Plotting in Elm
    Yo Joong Choe 
-}

import List
import List ((::))
import String
import Color
import Graphics.Collage as C
import Graphics.Collage (defaultLine)
import Graphics.Element as E
import Diagrams as D
import Text as T
import Window
import Mouse
import Signal (Signal)

import Plot (..)

main : Signal E.Element
main = 
    let xs  = D.generate -0.3 0.7 0.05
        ys  = List.map (\x -> -x*x + 0.5) xs
        sty = { defaultPlot | figsize    <- (600, 450)
                            , title      <- "Interactive Plotting in Elm!"
                            , xlabel     <- "x-axis"
                            , ylabel     <- "y-axis"
                            , xlim       <- (-0.3, 0.7)
                            , ylim       <- (0, 0.5)
                            , pointStyle <- { defaultPoint | color <- Color.blue
                                                           , shape <- Circle } }
    in
        plot sty xs ys

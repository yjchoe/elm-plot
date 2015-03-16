module Plot where

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
import Signal
import Signal (Signal, (<~), (~))

{-| Shapes and Styles -}

type Shape = Circle | Square | X -- TODO: Triangle, +, EmptyCircle, ...

type alias PointStyle = { color  : Color.Color
                        , shape  : Shape
                        , radius : Float 
                        }

type alias PlotStyle = { figsize    : (Float, Float)
                       , title      : String
                       , xlabel     : String
                       , ylabel     : String
                       , xlim       : (Float, Float)
                       , ylim       : (Float, Float)
                       , xnumticks  : Int
                       , ynumticks  : Int 
                       , pointStyle : PointStyle
                       }

defaultPoint : PointStyle
defaultPoint = { color  = Color.blue
               , shape  = Circle
               , radius = 3 
               }

defaultPlot : PlotStyle
defaultPlot = { figsize    = (600, 450) 
              , title      = "Plot"
              , xlabel     = "x"
              , ylabel     = "y"
              , xlim       = (0, 1)
              , ylim       = (-0.2, 0.8)
              , xnumticks  = 6
              , ynumticks  = 6
              , pointStyle = defaultPoint
              }

defaultText : T.Style
defaultText = 
    let ts = T.defaultStyle in
    { ts | typeface <- ["candara", "serif"]
         , height <- Just 14
    }

{-| Axes -}

ticks : (Float, Float) -> Int -> List Float -- TODO: implement ticks
ticks (lo, hi) n = 
    let range = hi - lo
        scale = (floor <| logBase 10 range) + 1 |> toFloat
        k     = (ceiling <| range * 10^scale) // n
        start = ceiling <| lo * 10^scale
    in
        List.map (\i -> (start + i*k)) [0..n]
            |> List.map (\i -> 10^(-scale) * toFloat i)

drawAxes : (Float,Float) -> ((Float,Float) -> (Float,Float)) -> PlotStyle -> D.Diagram a
drawAxes (plotWidth, plotHeight) scaleMap sty = 
    let rect   = D.rect plotWidth plotHeight <| D.justStroke defaultLine
        xticks = let tick = D.path [(0, -1.5), (0, 1.5)] defaultLine in
                 List.map (\val -> D.moveX (scaleMap (val,0) |> fst) tick) 
                    <| ticks sty.xlim sty.xnumticks
        yticks = let tick = D.path [(-1.5, 0), (1.5, 0)] defaultLine in
                 List.map (\val -> D.moveY (scaleMap (0,val) |> snd) tick)
                    <| ticks sty.ylim sty.ynumticks
        (xlo, xhi) = sty.xlim
        (ylo, yhi) = sty.ylim
        xlim   = D.group [D.text (toString xlo) defaultText
                            |> D.moveX (scaleMap (xlo, 0) |> fst),
                          D.text (toString xhi) defaultText
                            |> D.moveX (scaleMap (xhi, 0) |> fst),
                          if xlo < 0 && 0 < xhi -- print 0 if within limits
                            then D.text (toString 0) defaultText
                                    |> D.moveX (scaleMap (0,0) |> fst)
                            else D.empty]
                    |> D.moveY (1.1*(scaleMap (0, ylo) |> snd))
        ylim   = D.group [D.text (toString ylo) defaultText
                            |> D.moveY (scaleMap (0, ylo) |> snd),
                          D.text (toString yhi) defaultText
                            |> D.moveY (scaleMap (0, yhi) |> snd),
                          if ylo < 0 && 0 < yhi -- print 0 if within limits
                            then D.text (toString 0) defaultText
                                    |> D.moveY (scaleMap (0,0) |> snd)
                            else D.empty]
                    |> D.moveX (1.1*(scaleMap (xlo, 0) |> fst))
    in 
        D.group <| [rect, xlim, ylim]

{-| Points -}

type alias Point = { x : Float, y : Float, style : PointStyle }

ptWithStyle : PointStyle -> Float -> Float -> Point
ptWithStyle sty x0 y0 = { x = x0, y = y0, style = sty }

pt : Float -> Float -> Point
pt x0 y0 = ptWithStyle defaultPoint x0 y0

drawPoint_ : PointStyle -> D.Diagram a
drawPoint_ sty =
    let c = sty.color
        r = sty.radius in
    case sty.shape of
        Circle -> D.circle r << D.justFill <| C.Solid c
        Square -> let d = 2 * r in 
                  D.rect d d << D.justFill <| C.Solid c
        X      -> let ls = { defaultLine | color <- c } in
                  D.group [D.path [(-r,-r), (r,r)] ls,
                           D.path [(-r,r), (r,-r)] ls]

drawPoint : ((Float,Float) -> (Float,Float)) -> Point -> D.Diagram a
drawPoint scaleMap {x, y, style} = 
    drawPoint_ style |> D.move (scaleMap (x,y))


{-| Interactive Component: Mouse Hovering -}

dist : (Float,Float) -> (Float,Float) -> Float
dist (x,y) (x',y') = (x-x')^2 + (y-y')^2 |> sqrt

epsilon = 0.025

hover : (Float,Float) -> List (Float,Float) -> Maybe (Float,Float)
hover pt0 pts =
    let closer (d, pt) (d', pt') = if d <= d' then (d,pt) else (d',pt')
        dist2pt0 = List.map (\pt -> (dist pt0 pt, pt)) pts
        (d, closestPoint) = List.foldl1 closer dist2pt0
    in
        if | d <= epsilon -> Just closestPoint
           | otherwise    -> Nothing

wrapFloat : Float -> Float
wrapFloat f = (toFloat <| round <| 10000*f ) / 10000

drawHover : ((Float,Float) -> (Float,Float)) -> Point -> D.Diagram a
drawHover scaleMap {x, y, style} = 
    D.group [ D.text (toString (wrapFloat x, wrapFloat y)) defaultText 
                |> D.move (scaleMap (x,y))
            , drawPoint_ { style | color <- Color.red } 
                |> D.move (scaleMap (x,y))
            ]

{-| The Plot -}

plotWithStyle : (Float,Float) -> PlotStyle -> List Float -> List Float -> D.Diagram a
plotWithStyle mousePos sty xs ys = 
    let (x0, x1) = sty.xlim
        (y0, y1) = sty.ylim
        (w,h)    = sty.figsize
        scaleMap = \(x, y) -> -- TODO: factor out to scaleX and scaleY
                    (w * ((x-x0)/(x1-x0) - 1/2), h * ((y-y0)/(y1-y0) - 1/2))
        unscale  = \(x',y') ->
                    ((x1-x0) * x' / w + (x0+x1)/2,
                     (y1-y0) * y' / h + (y0+y1)/2)
        axes     = drawAxes sty.figsize scaleMap sty
        points   = List.map2 (ptWithStyle sty.pointStyle) xs ys
                    |> List.map (drawPoint scaleMap)
        title    = D.text sty.title { defaultText | height <- Just 24
                                                  , bold   <- True }
                    |> D.moveY ( 1.25*h/2) -- TODO: rewrite in terms of scaleMap
        xlabel   = D.text sty.xlabel defaultText
                    |> D.moveY (-1.25*h/2)
        ylabel   = D.text sty.ylabel defaultText
                    |> D.moveX (-1.25*w/2)
                    --|> D.rotate (degrees 90)  -- not yet supported by Diagrams
        -- interactive component
        hov      = case hover (unscale mousePos) (List.map2 (,) xs ys) of
                    Just (x,y) -> drawHover scaleMap (ptWithStyle sty.pointStyle x y)
                    Nothing    -> D.empty
    in
        D.group <| (hov :: axes :: points) ++ [title, xlabel, ylabel]

signalPlot : PlotStyle -> List Float -> List Float -> Signal (D.Diagram a)
signalPlot sty xs ys = 
    (\mousePos -> plotWithStyle mousePos sty xs ys) <~ D.collageMousePos

{-| The callable plot function -}

plot : PlotStyle -> List Float -> List Float -> Signal E.Element
plot sty xs ys =
    D.fullWindowView <~ Window.dimensions ~ signalPlot sty xs ys

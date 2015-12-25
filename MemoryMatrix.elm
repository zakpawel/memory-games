module MemoryMatrix (update, init, isInitAction, view, Action, Model, input) where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Html.Attributes as A
import Html exposing (Html)
import Html.Events exposing (on)
import Random exposing (Seed)
import Signal exposing (Mailbox)
import Task exposing (Task)
import Debug exposing (..)
import Array exposing (Array)
import List
import String
import Time exposing (fps, Time)
import TaskTutorial exposing (getCurrentTime, print)
import Effects exposing (Effects, Never)
import Json.Decode exposing (Decoder, map, object2, int, bool, float, string, (:=), list)
import Helpers exposing (Error)



type alias MouseEvent =
  { screenX   : Int
  , screenY   : Int
  , clientX   : Int
  , clientY   : Int
  , ctrlKey   : Bool
  , shiftKey  : Bool
  , altKey    : Bool
  , metaKey   : Bool
  , button    : Int
  }

andMap : Decoder (a -> b) -> Decoder a -> Decoder b
andMap = object2 (<|)

infixl 0 `map`
infixl 0 `andMap`

mouseEvent : Decoder MouseEvent
mouseEvent =
  MouseEvent
    `map`    "screenX"  := int
    `andMap` "screenY"  := int
    `andMap` "clientX"  := int
    `andMap` "clientY"  := int
    `andMap` "ctrlKey"  := bool
    `andMap` "shiftKey" := bool
    `andMap` "altKey"   := bool
    `andMap` "metaKey"  := bool
    `andMap` "button"   := int

{--}
generatePoints : (Int,Int) -> Seed -> (List (Int,Int), Seed)
generatePoints (nx,ny) seed = 
    let ax = nx - 1
        ay = ny - 1
        xs = [0..ax]
        ys = [0..ay]
        xsRepeated = List.map (List.repeat ny) xs
        all = List.concatMap (List.map2 (,) ys) xsRepeated
    in shuffle all seed

shuffle : List a -> Seed -> (List a, Seed)
shuffle l seed =
    let n = List.length l
        listGen = (Random.list n (Random.int 0 n))
        (l',seed') = Random.generate
                        listGen   
                        seed
        zipped = List.map2 (,) l l'
    in  zipped |> List.sortBy snd >> List.map fst >> (flip (,)) seed'


split : Int -> List a -> (List a, List a)
split n l =
    let split' n l r =
        if n > 0 then
            case l of
                (x :: xs) -> split' (n-1) xs (x :: r)
                _  -> (r,l)
        else (r,l)
    in split' n l []

type alias Model =
    { seed: Random.Seed
    , nrows: Int
    , ncols: Int
    , targetRatio: Float
    , rects: Array (Array Rect)
    , stage: Stage
    , ntiles: Int
    , personalBest: Maybe Int
    }

type alias Rect = 
    { position: (Int,Int)
    , clicked: Bool
    , target: Bool
    }

type Stage = Welcome
           | ResetGrid Countdown
           | Remember Countdown 
           | Playing 
           | Mistake Rect Countdown
           | Success Countdown
           | Exit 
           -- todo LevelUp?

isInitAction : Action -> Bool
isInitAction action =
    case action of
        PersonalBestLoaded _ -> True
        CurrentTime _ -> True
        _ -> False

type Action
    = StartGame
    | RectClick Rect
    | TimeDelta Time
    | NoOp
    | CurrentTime Time
    | PersonalBestLoaded (Result Error (Maybe String))
    | PersonalBestSaved

type Countdown = Timelapse Time | Finished

makeRect : (Int,Int) -> Bool -> Bool -> Rect
makeRect pos clicked target =
    { position=pos, clicked=clicked, target=target }

emptyRect : (Int,Int) -> Rect
emptyRect pos = makeRect pos False False

targetRect : (Int,Int) -> Rect
targetRect pos = makeRect pos False True

input : Signal Action
input = Signal.map TimeDelta (fps 3 )

needsTime : Model -> Bool
needsTime model =
    case model.stage of
        Remember (Timelapse x) -> True
        _ -> False

init : (Model, Effects Action)
init =
    ( { seed = Random.initialSeed 20
      , nrows = 5
      , ncols = 5
      , targetRatio = 0.7
      , rects = Array.empty
      , stage = Welcome
      , ntiles = 3
      , personalBest = Nothing
      } |> randomGrid
    , Effects.batch
        [ Effects.task (Task.map CurrentTime getCurrentTime)
        , Effects.task getPersonalBest
        ]
    --, Effects.none  
    )

getPersonalBest : Task Never Action
getPersonalBest =
    Task.toResult (Helpers.get "MemoryMatrixPersonalBest") |> Task.map PersonalBestLoaded

setPersonalBest : Int -> Task Never Action
setPersonalBest pb =
    Task.toResult (Helpers.set "MemoryMatrixPersonalBest" (toString pb)) |> Task.map (always PersonalBestSaved)

countdown : Time -> Countdown -> Countdown
countdown t counter =
    case counter of
        Timelapse tt -> if tt > 0 then Timelapse <| tt - t else Finished
        _ -> counter

handleTime : Time -> Countdown -> (Countdown -> Stage) -> (Model -> Model) -> Model -> Model
handleTime delta count stageCtor onFinish model =
    let newCount = countdown delta count
    in 
        if newCount == Finished
            then onFinish model
            else { model | stage = stageCtor newCount }


update : Action -> Model -> (Model, Effects Action)
update input model =
    let model2 =
        case input of
            PersonalBestLoaded m ->
                case m of
                    Ok (Just pbs) ->
                        case (String.toInt pbs) of
                            Err s -> model
                            Ok pbi -> { model | personalBest = Just pbi }
                    _ -> model
            CurrentTime t -> { model | seed = Random.initialSeed <| floor t }
            NoOp -> model
            TimeDelta t ->
                case model.stage of
                    Remember cnt ->
                        handleTime t cnt Remember (\m -> { m | stage = Playing }) model
                    Success cnt ->
                        handleTime t cnt Success (advanceGame >> randomGrid) model
                    Mistake rect cnt ->
                        handleTime t cnt (Mistake rect) randomGrid model
                    ResetGrid cnt ->
                        handleTime t cnt ResetGrid (\m -> { m | stage = Remember (Timelapse 1500)}) model
                    _ -> model
            StartGame -> model |> advanceGame >> randomGrid
            RectClick r -> case model.stage of
                Playing -> revealRect r model
                _ -> model
            PersonalBestSaved -> model
        updt pb = 
            ( { model2 | personalBest = Just pb }
            , Effects.task (setPersonalBest pb)
            )
        (m, eff) =
            case model.stage of
                Success _ ->
                    case model.personalBest of
                        Just pb ->
                            if pb < model.ntiles then
                                updt model.ntiles                                
                            else (model2, Effects.none)
                        _ -> updt model.ntiles

                _ -> (model2, Effects.none)
    in (m, eff)


advanceGame : Model -> Model
advanceGame model = { model | ntiles = model.ntiles + 1 }

computeGridDims : Model -> (Int,Int)
computeGridDims model =
    let ncells = (toFloat model.ntiles) / model.targetRatio
        side = sqrt ncells |> ceiling
    in (side,side)

randomGrid : Model -> Model
randomGrid model =
    let dims = computeGridDims model
        (nrows,ncols) = dims
        (l,seed') = generatePoints dims model.seed
        (h,t) = split model.ntiles l
        rs = pointsToGrid nrows ncols h
    in
        { model 
        | rects = rs
        , seed = seed'
        , stage = ResetGrid (Timelapse 500)
        }

resetGrid : Model -> Model
resetGrid model = { model | stage = ResetGrid (Timelapse 500) }

type alias Grid = Array (Array Rect)

clickRect : Rect -> Array (Array Rect) -> Array (Array Rect)
clickRect r a =
    updateGrid r.position { r | clicked = True } a

revealRect : Rect -> Model -> Model
revealRect rect model =
    let grid = clickRect rect model.rects
        clckdRect = { rect | clicked = True }
        model' = { model | rects = grid }
    in if  not rect.target then { model' | stage = Mistake clckdRect (Timelapse 1000) }
       else gridSolved model'

gridSolved : Model -> Model
gridSolved model =
    let solved = foldGrid (\r result -> if r.target then r.clicked && result else result) True model.rects
    in if solved then { model | stage = Success (Timelapse 500) } else model

gridGet : (Int,Int) -> Grid -> Maybe Rect
gridGet (x,y) grid =
    let mcol = Array.get x grid
    in case mcol of
        Nothing -> Nothing
        Just col -> Array.get y col

foldGrid : (Rect -> b -> b) -> b -> Grid -> b
foldGrid fn init grid =
    Array.foldl (\col acc -> Array.foldl fn acc col) init grid

updateGrid : (Int,Int) -> Rect -> Array (Array Rect) -> Array (Array Rect)
updateGrid pos rect grid =
    updateGridUsing pos (always <| Just rect) grid

updateGridUsing : (Int,Int) -> (Maybe Rect -> Maybe Rect) -> Grid -> Grid
updateGridUsing (x,y) fn grid =
    let mcol = Array.get x grid
    in case mcol of
        Nothing -> grid
        Just col ->
            let existRect = Array.get y col
            in case fn existRect of
                Nothing -> grid
                Just rect -> let newcol = Array.set y rect col in Array.set x newcol grid

pointsToGrid : Int -> Int -> List (Int,Int) -> Grid
pointsToGrid nrows ncols targets =
    let grid = Array.initialize nrows (\x -> Array.initialize ncols (\y -> emptyRect (x,y)))
    in List.foldl (\r g -> updateGrid r (targetRect r) g) grid targets


renderGrid : Signal.Address Action -> Array (Array Rect) -> List Svg
renderGrid address = renderGridUsing <| renderRect address Nothing


renderGridUsing : (Rect -> Svg) -> Array (Array Rect) -> List Html
renderGridUsing renderFn grid =
    let renderGrid' g l =

        let table = Html.div [ A.class "table" ]
            tableRow = Html.div [ A.class "row" ]
            tableCell el = Html.div [ A.class "cell" ] [ el ]
        in

        table <|
            Array.foldl (\row l ->
                (::) (tableRow <|
                        Array.foldl (\r l ->
                        (r |> renderFn |> tableCell) :: l ) [] row) l)
                l grid

    in [ renderGrid' grid [] ]

renderWelcome : Signal.Address Action -> Html
renderWelcome address =
    Html.div [] [ Html.text "welcome" 
                , Html.div [] [ Html.button [ Html.Events.onClick address StartGame ] [ Html.text "Start game" ] ]
                ]

renderExposed : Signal.Address Action -> Array (Array Rect) -> List Svg
renderExposed address = renderGridUsing 
    (\r -> renderRect address Nothing <|
            if r.target then { r | clicked = True }
            else r)

renderGridReset : Signal.Address Action -> Grid -> List Svg
renderGridReset address =
    renderGridUsing <| renderRect address <| Just "#DADADA"

colorRect : Rect -> String
colorRect r =
    if r.clicked then
        if r.target then "#80D945" else "#FF653A"
    else "white"

renderRect : Signal.Address Action -> Maybe String -> Rect -> Svg
renderRect address mcolor r =
    let color = case mcolor of
                    Nothing -> colorRect r
                    Just color -> color
        (xc,yc) = r.position
        w = 50
        h = 50
        border = 1
        position = ("position", "static")
        ws = ("width", (toString w) ++ "px")
        hs = ("height", (toString h) ++ "px")
        left = ("left", (toString <| xc*(w+border)) ++ "px")
        top = ("top", (toString <| yc*(h+border)) ++ "px")
        bgcolor = ("background-color", color)
        style = [position, ws, hs, top, left, bgcolor]
        clicked = if r.clicked then "clicked" else ""
    in
        Html.div
             [ class <| "square " ++ clicked
             , onClick <| Signal.message address <| RectClick r
             , A.style style
             ]
             []

embedSvg : Maybe String -> Model -> List Html -> Html
embedSvg mclass model rects =
    let w = "400"
        h = "400"
        classname = case mclass of
                        Just class -> class
                        Nothing -> ""
        loadCssNode = Html.node "script" [] [Html.text loadCss]
        pb =
            case model.personalBest of
                Just p -> p
                _ -> 0
        float a = [("float", a)]
        personalBest =
            Html.div [] 
                [ Html.div [ A.style (float "left") ] [ Html.text <| "tiles: " ++ (toString model.ntiles) ]
                , Html.div [ A.style (float "right") ] [ Html.text <| "to beat: " ++ (toString pb) ]
                ]
    in Html.div
        [ A.class "container" ]
        [ Html.div
            [ A.class <| String.join " " [ classname, "sub-container" ] ]
            (loadCssNode :: personalBest :: rects)
        ]
        

loadCss = "
    var head   = document.getElementsByTagName('head')[0];
    var link   = document.createElement('link');
    link.rel   = 'stylesheet';
    link.type  = 'text/css';
    link.href  = 'http://localhost:8000/style.css';
    link.media = 'all';
    head.appendChild(link);"

view : Signal.Address Action -> Model -> Html
view address model =
    case model.stage of
        Welcome    -> renderWelcome address
        ResetGrid _  -> embedSvg (Just "reset") model <| renderGridReset address model.rects
        Remember _ -> embedSvg Nothing model <| renderExposed address model.rects
        Playing    -> embedSvg Nothing model <| renderGrid address model.rects
        Mistake _ _  -> embedSvg Nothing model <| renderExposed address model.rects
        Success _  -> embedSvg Nothing model <| renderExposed address model.rects
        _ -> Html.div [] [ Html.text "exit" ]
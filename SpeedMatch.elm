module SpeedMatch (Model, Action, isInitAction, view, init, update, input, renderPersonalBest) where

import Keyboard
import Char exposing (KeyCode)
import Random exposing (Seed)
import Set exposing (Set)
import Time exposing (Time)
import Html exposing (div, toElement, fromElement, text, Html)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList)
import Svg as S
import Svg.Attributes as A
import Helpers as Helpers exposing (get, set, Error)
import Task exposing (Task)
import String
import Debug exposing (watch)
import Effects exposing (Effects, Never)


type Card = Triangle | Circle | Square

type Stage = Welcome | Playing | GameOver | GameOverGetReady

type alias Model =
    { seed : Seed
    , shape : Card
    , prevShape : Maybe Card
    , nShape : Int
    , pointsPerGuess : Int
    , pointsOverall : Int
    , correctSpree : Int
    , totalTime : Time
    , timeLeft : Time
    , stage : Stage
    , lastCorrect : Maybe Bool
    , personalBest : Maybe Int
    }

totalTime : Time
totalTime = 50 * Time.second

init : (Model, Effects Action)
init =
    ( { seed = Random.initialSeed 20
      , shape = Triangle
      , prevShape = Nothing
      , nShape = 0
      , pointsPerGuess = 10
      , pointsOverall = 0
      , correctSpree = 0
      , totalTime = totalTime
      , timeLeft = totalTime
      , stage = Welcome
      , lastCorrect = Nothing
      , personalBest = Nothing
      }
    , Effects.batch
        [ Effects.task getPersonalBest
        , Effects.task getRandomNumber
        ]
    )

setPersonalBest : Int -> Task Never (Result Error (Maybe String))
setPersonalBest =
    toString >> 
    Helpers.set "SpeedMatchPersonalBest" >>
    Task.toResult

getPersonalBest : Task Never Action
getPersonalBest = Task.toResult (Helpers.get "SpeedMatchPersonalBest") |> Task.map Load

getRandomNumber : Task Never Action
getRandomNumber =
    Helpers.randomFloat ()
        |> Task.map (\f -> CurrentTime (Just f))

isInitAction : Action -> Bool
isInitAction action =
    case action of
        Load _ -> True
        CurrentTime _ -> True
        _ -> False

type Action = KeyPress Arrow
            | CurrentTime (Maybe Time)
            | TimeDelta Time
            | Action (Model -> Model)
            | Load (Result Error (Maybe String))
            | SaveScore
            | NoAction

type Arrow = Left | Right | Up | NoKey

input : Signal Action
input = Signal.mergeMany
        [ (Signal.map KeyPress arrows)
        , (Signal.map TimeDelta (Time.fps 60))
        ]

arrows : Signal Arrow
arrows =
    Keyboard.keysDown
        |> Signal.foldp foldPresses {beingPressed=Set.empty,wentDown=Set.empty}
        |> Signal.filterMap keyPressed NoKey

maybeToBool : Maybe a -> Bool
maybeToBool m = case m of
            Just _ -> True
            _ -> False


every : List Bool -> Bool
every = List.foldl (&&) True

barrier : a -> (Bool, List (a -> Bool)) -> (Bool, List (a -> Bool))
barrier =
    let f input (b, preds) =
        if List.isEmpty preds
        then (True, [])
        else
            List.map (\p -> (p input, p)) preds
            |> List.foldl (\(b, p) (rb, rp) ->
                            let newrp =
                                    if not b
                                    then p :: rp
                                    else rp
                            in (b || rb, newrp)) (False,[])
    in f

noInputUntilEvent : List (Action -> Bool) -> Signal Bool
noInputUntilEvent filters =
    Signal.map fst <|
        Signal.foldp (barrier) (False, filters) input

emptyPB : Model -> Model
emptyPB model = { model | personalBest = Just 0 }


update : Action -> Model -> (Model, Effects Action)
update input model =
    let newModel =
            case input of
                CurrentTime mn -> case mn of
                                    Just n -> n
                                                |> (*) 1000000
                                                |> floor
                                                |> (\n -> { model | seed = Random.initialSeed n })
                                    _ -> model
                Load (Ok (Just pbs)) ->
                    case (String.toInt pbs) of
                        Err s -> model
                        Ok pbi -> { model | personalBest = Just pbi }
                Load _ -> model
                Action fn -> fn model
                _ -> case model.stage of
                    Welcome -> case input of
                        KeyPress _ -> restartGame model
                        _          -> model
                    GameOver -> case input of
                        KeyPress _ -> { model | stage = GameOverGetReady }
                        _ -> model
                    GameOverGetReady -> case input of
                        KeyPress _ -> restartGame model
                        _          -> model
                    Playing ->
                        case input of
                            TimeDelta t ->
                                let timeLeft' = model.timeLeft - t
                                in if timeLeft' > 0
                                    then { model | timeLeft = timeLeft' }
                                    else { model | stage = GameOver }
                            KeyPress a -> 
                                case a of
                                    Up -> restartGame model
                                    _ ->
                                        model
                                            |> score a
                                            |> nextShape
                            _ -> model
        ret =
            let
                noNewScore = (newModel, Effects.none)
            in
                case model.stage of
                    GameOver ->
                        let 
                            saveEff = setPersonalBest model.pointsOverall
                                        |> Effects.task
                                        |> Effects.map (always SaveScore)
                            withNewScore = ( { newModel | personalBest = Just model.pointsOverall }
                                           , saveEff
                                           )
                        in
                            case model.personalBest of
                                Just pb ->
                                    if model.pointsOverall > pb then
                                        withNewScore
                                    else noNewScore
                                Nothing -> 
                                    withNewScore

                    _ -> noNewScore
    in ret

checkGuessCorrection : Arrow -> Model -> Bool
checkGuessCorrection arrow model =
    case model.prevShape of
        Nothing -> True
        Just prevShape ->
        let shapeEquals = model.shape == prevShape
        in 
            if arrow == Left && not shapeEquals then
                True
            else if arrow == Right && shapeEquals then
                True
            else
                False

adjustPointsAward : Bool -> Model -> Model
adjustPointsAward guessCorrect model =
    if guessCorrect
    then { model | pointsPerGuess = min 150 (model.correctSpree * 2 + model.pointsPerGuess) }
    else { model | pointsPerGuess = 10 }

storeCorrect : Bool -> Model -> Model
storeCorrect correct model =
    { model | lastCorrect = Just correct }

break : Arrow -> Model -> Model
break arrow model =
    let a = (watch "arrow" arrow)
    in model

score : Arrow -> Model -> Model
score arrow model =
    let guessCorrect = checkGuessCorrection arrow model
        in model
            |> awardPoints guessCorrect
            |> adjustPointsAward guessCorrect
            |> adjustSpree guessCorrect
            |> storeCorrect guessCorrect
            

awardPoints : Bool -> Model -> Model
awardPoints guessCorrect model =
    if guessCorrect
    then { model | pointsOverall = model.pointsOverall + model.pointsPerGuess }
    else model



adjustSpree : Bool -> Model -> Model
adjustSpree guessCorrect model =
    if guessCorrect
    then { model | correctSpree = model.correctSpree + 1 }
    else { model | correctSpree = 0 }



nextShape : Model -> Model
nextShape m =
    let gen = Random.int 1 3
        (i,seed') = Random.generate gen m.seed
        newShape = toCard i
    in { m
         | seed = seed'
         , prevShape = Just m.shape
         , shape = newShape
         , nShape = m.nShape + 1 }

toCard : Int -> Card
toCard i =
    case i of
        1 -> Triangle
        2 -> Circle
        3 -> Square
        _ -> Square

renderCheckmark : Model -> S.Svg
renderCheckmark model =
    let class = if model.nShape % 2 == 1
                then "fadeinout2"
                else "fadeinout1"
    in S.polygon [ A.class class
              , A.points "10,50 50,80 90,20 80,15 50,70"
              , A.fill "green" ] []

renderIncorrect : Model -> S.Svg
renderIncorrect model =
    let class = if model.nShape % 2 == 1
                then "fadeinout2"
                else "fadeinout1"
    in S.polygon [ A.class class
              , A.points "25,30 45,50 25,70 30,75 50,55 70,75 75,70 55,50 75,30 70,25 50,45 30,25"
              , A.fill "red" ] []


--(List S.Attribute -> List S.Svg -> S.Svg) -> (List S.Attribute -> List S.Svg -> S.Svg)
augmentRender : (List a -> List b -> b) -> List a -> List b -> (List a -> List b -> b)
augmentRender fn attrs shapes =
    let rfn a s = fn (List.append attrs a) (List.append shapes s)
    in rfn

mkClassAttr : (String -> String) -> List S.Attribute
mkClassAttr f = [ A.class <| f "" ]

appendClass : String -> String -> String
appendClass c1 c2 = c1 ++ " " ++ c2


renderDoubleShape : (List S.Attribute -> List S.Svg -> S.Svg) -> List S.Attribute -> Model -> (RenderType -> S.Svg)
renderDoubleShape fn attrs model =
    let shape    = appendClass "shape"
        fadein   = appendClass "fadein"
        fadeout  = appendClass "fadeout"
        hide     = appendClass "hide"
        correct  = model.lastCorrect
        cfadein  = mkClassAttr <| shape >> fadein
        cfadeout = mkClassAttr <| shape >> fadeout >>
                        (if model.nShape == 0
                            then hide
                            else identity)
        check = case correct of
                    Just b ->
                        if model.nShape == 1
                            then []
                            else (if b
                                    then renderCheckmark model
                                    else renderIncorrect model) :: []
                    Nothing -> []
        first = []
        second = []
        rfn rt = case rt of
            Render1 -> (augmentRender S.g [] [ fn cfadein first, fn cfadeout second ]) [] check
            Render2 -> (augmentRender S.g [] [ fn cfadeout first, fn cfadein second ]) [] check
            Dont    -> S.g [] [ fn cfadeout first, fn cfadeout second ]
    in rfn

renderShapes : Model -> List S.Svg
renderShapes model =
    let shape = model.shape
        rt = if model.nShape % 2 == 0
            then Render2
            else Render1
        triangle = renderDoubleShape renderTriangle [] model
        circle  = renderDoubleShape renderCircle [] model
        square  = renderDoubleShape renderSquare [] model
    in case shape of
        Triangle -> [ triangle rt, circle Dont, square Dont ]
        Square -> [ triangle Dont, circle Dont, square rt ]
        Circle -> [ triangle Dont, circle rt, square Dont ]


renderSquare : List S.Attribute -> List S.Svg -> S.Svg
renderSquare attrs s =
    (augmentRender
        S.rect [ A.fill "#AE19FF"
               , A.x "0", A.y "0"
               , A.width "100", A.height "100"
               , A.rx "5", A.ry "5" ] [])
        attrs s

renderCircle : List S.Attribute -> List S.Svg -> S.Svg
renderCircle attrs s =
    (augmentRender
        S.circle [ A.fill "#3C45FF"
                 , A.r  "50"
                 , A.cx "50"
                 , A.cy "50" ] [])
    attrs s

renderTriangle : List S.Attribute -> List S.Svg -> S.Svg
renderTriangle attrs s =
    (augmentRender
        S.polygon [ A.fill "#FFDB3C"
                  , A.points "0,100 50,0 100,100" ] [])
    attrs s

renderSummary : Model -> Html
renderSummary model =
    div [ class "summary-container" ]
        [ div [ class "summary card" ]
            [ div [ class "score" ] [ text <| "Your score: " ++ toString model.pointsOverall ]
            , div [ class "nShape" ] [ text <| "Total shapes: " ++ toString model.nShape ]
            , div [ class "averageResponse" ] [ text <| "Average response: " ++ (toString <| round <| model.totalTime / (toFloat model.nShape)) ]
            , div [ class "personalBest" ] [ renderPersonalBest model ]
            ]
        ]

renderPersonalBest : Model -> Html
renderPersonalBest model =
    let score =
        case model.personalBest of
            Just s -> toString s
            Nothing -> "0"
    in text <| "Personal best: " ++ score

renderGame : Model -> Html
renderGame model =
    div [ class "game-container" ]
        [ div [ class "clock" ] [ model.timeLeft |> Time.inSeconds |> ceiling |> toString |> text ]
        , div [ class "game" ]
            [ S.svg [ A.width "200", A.height "200" ]
                    [ model
                        |> renderShapes
                        |> S.g [ A.transform "translate(50,50)" ]]
            ]
        ]

renderArrows : Signal.Address Action -> Model -> Html
renderArrows address model =
    div [ class "arrows-container" ]
        [ div [ onClick address (KeyPress Left)
              , class "left-arrow"] [ Html.p [] [ text "No match" ] ]
        , div [ onClick address (KeyPress Right)
              , class "right-arrow"] [ Html.p [] [ text "Match" ] ]
        ]

type RenderType = Dont | Render1 | Render2

renderWelcome : Model -> Html
renderWelcome model =
    let personalBest = case model.personalBest of
                        Just s -> "Your best score so far is " ++ (toString s) ++ ", try to beat that"
                        Nothing -> ""
    in
        div [ class "card card-fadeout welcome" ]
            [ Html.p [] [ text "To start game click or press left/right arrow" ]
            , Html.p [] [ text personalBest ]
            , Html.p [] [ text "Good luck!" ]
            ]

restartGame : Model -> Model
restartGame model =
    let startModel = fst init
    in
        model
            |> nextShape
            |>  (\model ->
                    { startModel
                    | stage = Playing
                    , seed = model.seed
                    , personalBest = model.personalBest
                    , prevShape = Nothing
                    , shape = model.shape
                    , timeLeft = totalTime })

view : Signal.Address Action -> Model -> Html
view address model =
    let m = model -- Debug.watch "model" model
        -- loadCssNode = Html.node "script" [] [ Html.text loadCss ]
        body = case model.stage of
            Welcome -> [ renderWelcome model, renderArrows address model ]
            Playing -> [ renderGame model, renderArrows address model ]
            GameOver -> [ renderSummary model , renderArrows address model ]
            GameOverGetReady -> [ renderSummary model
                                , renderArrows address model
                                , Html.p [] [ text "Press again to start another game" ]
                                ]
        container = div [ A.class "container" ] body
    in div [] [ container ]

loadCss : String
loadCss = "
    var head   = document.getElementsByTagName('head')[0];
    var link   = document.createElement('link');
    link.rel   = 'stylesheet';
    link.type  = 'text/css';
    link.href  = 'http://localhost:8000/style.css';
    link.media = 'all';
    head.appendChild(link);"


keyPressed : KeyPresses -> Maybe Arrow
keyPressed {wentDown} =
    if Set.member 37 wentDown then
        Just Left
    else if Set.member 39 wentDown then
        Just Right
    else if Set.member 38 wentDown then
        Just Up
    else
        Nothing

foldPresses : Set KeyCode -> KeyPresses -> KeyPresses
foldPresses presses acc =
    let {beingPressed,wentDown} = acc
        newWentDown = Set.diff presses beingPressed
    in { acc | wentDown = newWentDown, beingPressed = presses }

type alias KeyPresses =
    { beingPressed : Set KeyCode
    , wentDown : Set KeyCode
    }


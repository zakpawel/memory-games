import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Events as E
import Html.Attributes as A
import List
import StartApp
import Signal
import Task
import Maybe
import SpeedMatch
import MemoryMatrix


type alias Model =
  { speedMatchModel : SpeedMatch.Model
  , memoryMatrixModel : MemoryMatrix.Model
  , currentGame : Maybe Game
  , games : List Game
  }

type Game
  = SpeedMatchGame
  | MemoryMatrixGame

type Action
  = Select Game
  | LocationHashChange String
  | SpeedMatch SpeedMatch.Action
  | MemoryMatrix MemoryMatrix.Action


handleUpdate : 
  action -> 
  parentModel -> 
  (action -> model -> (model, Effects action)) -> 
  (parentModel -> model -> parentModel) ->
  (parentModel -> model) ->
  (action -> parentAction) -> 
  (parentModel, Effects parentAction)
handleUpdate action model update setter getter mapper =
  let (m, fx) = update action (getter model)
  in
    ( setter model m
    , Effects.map mapper fx
    )

update : Action -> Model -> (Model, Effects Action)
update action model = 
  let modelAct =
    case action of
      LocationHashChange addr ->
        ( { model | currentGame = hashToGame addr }, Effects.none )
      Select g ->
        ( { model | currentGame = Just g }
        , Effects.none
        )

      SpeedMatch act ->
        case SpeedMatch.isInitAction act of
          True ->
            handleUpdate act model SpeedMatch.update
              (\pm m -> { pm | speedMatchModel = m })
              (\pm -> pm.speedMatchModel)
              SpeedMatch
          _ ->
            case model.currentGame of
              Nothing -> (model, Effects.none)
              Just g ->
                handleUpdate act model SpeedMatch.update
                  (\pm m -> { pm | speedMatchModel = m })
                  (\pm -> pm.speedMatchModel)
                  SpeedMatch

      MemoryMatrix act ->
        case MemoryMatrix.isInitAction act of
          True ->
            handleUpdate act model MemoryMatrix.update
              (\pm m -> { pm | memoryMatrixModel = m })
              (\pm -> pm.memoryMatrixModel)
              MemoryMatrix
          _ ->
            case model.currentGame of
              Nothing -> (model, Effects.none)
              Just g ->
                handleUpdate act model MemoryMatrix.update
                  (\pm m -> { pm | memoryMatrixModel = m })
                  (\pm -> pm.memoryMatrixModel)
                  MemoryMatrix
  in modelAct


gameDesc : Game -> Model -> Html
gameDesc game model =
  case game of
    SpeedMatchGame ->
      div []
          [ div [] [ text "Speed Match" ]
          , SpeedMatch.renderPersonalBest model.speedMatchModel
          ]
    MemoryMatrixGame ->
      div []
          [ div [] [ text "Memory Matrix" ]
          , text <|
              "Personal best: " ++
              (model.memoryMatrixModel.personalBest
                |> Maybe.withDefault 0
                |> toString)
          ]

hashToGame : String -> Maybe Game
hashToGame hash =
  case hash of
    "#MemoryMatrixGame" -> Just MemoryMatrixGame
    "#SpeedMatchGame" -> Just SpeedMatchGame
    _ -> Nothing


gameHash : Game -> String
gameHash game =
  "#" ++ (toString game)
                    

init : (Model, Effects Action)
init =
  let
    (smModel, smFx) = SpeedMatch.init
    (mmModel, mmFx) = MemoryMatrix.init
  in
    ( { speedMatchModel = smModel
      , memoryMatrixModel = mmModel
      , games = 
          [ SpeedMatchGame
          , MemoryMatrixGame
          ]
      , currentGame = Nothing
      }
    , Effects.batch
        [ Effects.map SpeedMatch smFx
        , Effects.map MemoryMatrix mmFx
        ]
    )

view : Signal.Address Action -> Model -> Html
view address model = 
  case model.currentGame of
    Just g ->
      case g of
        SpeedMatchGame ->
          div [] 
              [ SpeedMatch.view
                  (Signal.forwardTo address SpeedMatch) 
                  model.speedMatchModel ]
        MemoryMatrixGame ->
          div [] 
              [ MemoryMatrix.view 
                  (Signal.forwardTo address MemoryMatrix) 
                  model.memoryMatrixModel ]
    Nothing -> renderChooseGame address model

renderChooseGame : Signal.Address Action -> Model -> Html
renderChooseGame address model =
  List.map
    (\g ->
      a [ E.onClick address (Select g)
        , A.class "tile"
        , A.href (gameHash g)
        ]
        [ (gameDesc g model) ])
    model.games

    |> div [ A.class "tiles-container" ]
    |> (flip (::)) []
    |> div [ A.class "home-screen" ]

app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs =
        [ Signal.map SpeedMatch SpeedMatch.input
        , Signal.map MemoryMatrix MemoryMatrix.input
        , Signal.map LocationHashChange hash
        ]
    }

main : Signal Html
main =
  app.html

port hash : Signal String

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

import Html exposing (Html, div, text, input)
import Html.Attributes exposing (class, type', placeholder)
import Html.Events exposing (onKeyPress)
import Keyboard exposing (enter)
import WebAPI.Window exposing (alert)
import Task exposing (Task)
import Signal exposing (Signal, Address)
import Effects exposing (Effects)
import StartApp exposing (App)

app : App Model
app =
    StartApp.start
        { init = init
        , update = update
        , view = view
        , inputs = []
        }

main : Signal Html
main = app.html

init : (Model, Effects Action)  
init =
  let initModel =
    { count = 0 }
  in ( initModel, Effects.none)


type alias Model = { count : Int }

type Action
    = NoOp
    | TodoAddFieldKeypressed Int

view : Address Action -> Model -> Html
view address model =
  div []
    [
      div
        [ class "panel" ]
        [ div [] [ text "entry1" ]
          , div [] [ text "entry2" ]
          , div [] [ text "entry3" ]

        ]
      , div []
        [ input
          [ type' "text"
          , placeholder "Enter task description here"
          , onKeyPress address TodoAddFieldKeypressed ]
          []
        ]
    ]

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    TodoAddFieldKeypressed key ->
      ( case key of
        enter ->
          ( model
          , alert "enter pressed" |>
            Task.map (always NoOp) |>
              Effects.task
          ) 
      )
    _ -> (model, Effects.none)

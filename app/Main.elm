import Html exposing (Html, div, text, input, del)
import Html.Attributes exposing (class, type', placeholder)
import Html.Events exposing (onKeyPress)
import List exposing (map)
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
      { todos =
        [ ("entry1", False)
        , ("entry2", False)
        , ("entry3", False)
        ]
      }
  in ( initModel, Effects.none)

type alias TodoItem = (String, Bool)

type alias Model =
  { todos: List ((String, Bool))
  }

type Action
    = NoOp
    | TodoAddFieldKeypressed Int

displayTodo : TodoItem -> Html
displayTodo (text', completed) =
  let row =
    div [] [ text text' ]
  in
  case completed of
    False ->
      row
    -- Wrap it in a <del> element
    True ->
      del [] [row]

view : Address Action -> Model -> Html
view address model =
  div []
    [
      div
        [ class "panel" ]
        (map displayTodo model.todos)
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

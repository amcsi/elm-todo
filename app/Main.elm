import Html exposing (Html, div, text, input, del)
import Html.Attributes exposing (class, type', placeholder, value)
import Html.Events exposing (on, onKeyPress, targetValue)
import List exposing (map)
import WebAPI.Window exposing (alert)
import Task exposing (Task)
import Signal exposing (Signal, Address)
import Effects exposing (Effects)
import StartApp exposing (App)

enterKey = 13

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
      , newEntryValue = ""
      }
  in ( initModel, Effects.none)

type alias TodoItem = (String, Bool)

type alias Model =
  { todos : List ((String, Bool))
  , newEntryValue : String
  }

type Action
    = NoOp
    | TodoAddFieldKeypressed Int
    | TodoAddFieldAfterKeypressed Int
    | UpdateField String

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
          , value model.newEntryValue
          , placeholder "Enter task description here"
          , onKeyPress address TodoAddFieldKeypressed
          , on "input" targetValue (Signal.message address << UpdateField) ]
          []
        ]
    ]

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    TodoAddFieldKeypressed key ->
      ( case key of
        13 ->
          { model
              | todos = (model.newEntryValue, False) :: model.todos
              , newEntryValue = ""
          }
        _ -> model
      , Effects.none
      )
    UpdateField value ->
      ( { model
          | newEntryValue = value
        }
        , Effects.none
      )
    _ -> (model, Effects.none)

import Basics exposing (toString)
import Html exposing (Html, button, div, text, input, del, ul, li)
import Html.Attributes exposing (class, type', placeholder, value)
import Html.Events exposing (on, onClick, onKeyPress, targetValue)
import List exposing (concat, indexedMap, isEmpty, length, map)
import Signal exposing (Signal, Address)
import Effects exposing (Effects)
import StartApp exposing (App)

enterKey : Int
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
    | ClearDone
    | Toggle Int
    | TodoAddFieldKeypressed Int
    | TodoAddFieldAfterKeypressed Int
    | UpdateField String

displayTodo : Address Action -> Int -> TodoItem -> Html
displayTodo address index (text', completed) =
  let
    row =
      li
        [ onClick address (Toggle index)
        , class "clickable"
        ]
        [ text text'
        ]
  in
    case completed of
      False ->
        row
      -- Wrap it in a <del> element
      True ->
        del [] [row]

toggle : Int -> Int -> TodoItem -> TodoItem
toggle targetIndex currentIndex (text', toggled) =
  if targetIndex == currentIndex then
    (text', not toggled)
  else
    (text', toggled)

doneTodos : List TodoItem -> List TodoItem
doneTodos = List.filter (\(_, done) -> done)

notDoneTodos : List TodoItem -> List TodoItem
notDoneTodos = List.filter (\(_, done) -> not done)

view : Address Action -> Model -> Html
view address model =
  div
    [ class "container col-md-4" ]
    [
      div
        [ class "panel" ]
        [ ul
            []
            (indexedMap (displayTodo address) model.todos)
        ]
      , div []
        [ input
          [ type' "text"
          , class "form-control"
          , value model.newEntryValue
          , placeholder "Enter task description here"
          , onKeyPress address TodoAddFieldKeypressed
          , on "input" targetValue (Signal.message address << UpdateField) ]
          []
        ]
      , div []
        <| concat [
          let buttonText = "Clear done " ++ (toString <| length <| doneTodos model.todos)
          in
            if (not <| isEmpty <| doneTodos model.todos) then
              [ button
                  [ type' "button"
                  , class "btn btn-danger"
                  , onClick address ClearDone
                  ]
                  [ text buttonText ]
              ]
            else []
        ]

    ]

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Toggle index ->
      ( { model |
            todos = indexedMap (toggle index) model.todos
        
        }
      , Effects.none
      )
    TodoAddFieldKeypressed key ->
      ( if key == enterKey
        then
          { model
              | todos = (model.newEntryValue, False) :: model.todos
              , newEntryValue = ""
          }
        else
          model
        , Effects.none
      )
    UpdateField value ->
      ( { model
          | newEntryValue = value
        }
        , Effects.none
      )
    ClearDone ->
      ( { model
            | todos = notDoneTodos model.todos
          }
        , Effects.none
      )
    _ -> (model, Effects.none)

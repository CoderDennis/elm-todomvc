module TodoMVC exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Todo =
    { title : String
    , completed : Bool
    , editing : Bool
    }


type FilterState
    = All
    | Active
    | Completed


type alias Model =
    { todos : List Todo
    , todo : Maybe Todo
    , filter : FilterState
    }


type Msg
    = Add Todo
    | Complete Todo
    | Delete Todo
    | Filter FilterState


view : Model -> Html Msg
view model =
    section [ class "todoapp" ]
        [ header [ class "header" ]
            [ h1 [] [ text "todos" ]
            , input
                [ class "new-todo"
                , placeholder "What needs to be done?"
                , autofocus True
                ]
                []
            ]
        , viewTodos model.todos
        , viewFooter model.todos
        ]


viewTodos : List Todo -> Html Msg
viewTodos todos =
    if List.isEmpty todos then
        text ""
    else
        section [ class "main" ]
            [ input
                [ class "toggle-all"
                , type_ "checkbox"
                ]
                []
            , label [ for "toggle-all" ]
                [ text "Mark all as complete"
                ]
            , ul [ class "todo-list" ]
                (List.map viewTodo todos)
            ]


viewTodo : Todo -> Html Msg
viewTodo todo =
    li (todoLiClass todo)
        [ div [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                ]
                []
            , label []
                [ text todo.title ]
            , button [ class "destroy" ] []
            ]
        , input
            [ class "edit"
            , value todo.title
            ]
            []
        ]


todoLiClass : Todo -> List (Attribute Msg)
todoLiClass todo =
    if todo.editing then
        [ class "editing" ]
    else if todo.completed then
        [ class "completed" ]
    else
        []


viewFooter : List Todo -> Html Msg
viewFooter todos =
    if List.isEmpty todos then
        text ""
    else
        footer [ class "footer" ]
            [ span [ class "todo-count" ]
                [ strong [] [ text (toString (countIncomplete todos)) ]
                , text "items left"
                ]
            , ul [ class "filters" ]
                [ li []
                    [ a
                        [ class "selected"
                        , href ""
                        ]
                        [ text "All" ]
                    , a [ href "" ]
                        [ text "Active" ]
                    , a [ href "" ]
                        [ text "Completed" ]
                    ]
                ]
            , button [ class "clear-completed" ] [ text "Clear completed" ]
            ]


countIncomplete : List Todo -> Int
countIncomplete todos =
    todos
        |> List.filter (\t -> not t.completed)
        |> List.length


update : Msg -> Model -> Model
update msg model =
    model


initialModel : Model
initialModel =
    { todos = []
    , todo = Nothing
    , filter = All
    }


testModel : Model
testModel =
    { todos =
        [ { title = "Taste JavaScript"
          , completed = True
          , editing = False
          }
        , { title = "Buy a unicorn"
          , completed = False
          , editing = False
          }
        ]
    , todo = Nothing
    , filter = All
    }


main =
    Html.beginnerProgram
        { model = testModel
        , view = view
        , update = update
        }

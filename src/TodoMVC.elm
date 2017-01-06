module TodoMVC exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
    exposing
        ( on
        , onInput
        , keyCode
        , onClick
        , onDoubleClick
        , onBlur
        )
import Json.Decode exposing (andThen, succeed, fail)
import Json.Encode as Json


{-
   TODO: set focus on edit box after double click. Evan has example in his
   TodoMVC implemenation, but I don't want to look at that yet.
   https://github.com/evancz/elm-todomvc
-}


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
    , newText : String
    , filter : FilterState
    }


type Msg
    = Add
    | ToggleAll
    | Input String
    | ToggleCompleted Todo
    | Delete Todo
    | Edit Todo Bool
    | TodoInput Todo String
    | ClearCompleted
    | Filter FilterState


newTodo : String -> Todo
newTodo text =
    { title = text
    , completed = False
    , editing = False
    }


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                succeed msg
            else
                fail "not ENTER"
    in
        on "keydown" (andThen isEnter keyCode)


view : Model -> Html Msg
view model =
    section [ class "todoapp" ]
        [ header [ class "header" ]
            [ h1 [] [ text "todos" ]
            , input
                [ class "new-todo"
                , placeholder "What needs to be done?"
                , autofocus True
                , onInput Input
                , onEnter Add
                , value model.newText
                ]
                []
            ]
        , viewTodos model
        , viewFooter model
        ]


viewTodos : Model -> Html Msg
viewTodos model =
    if List.isEmpty model.todos then
        text ""
    else
        let
            incomplete =
                countIncomplete model.todos

            count =
                List.length model.todos

            allChecked =
                incomplete == 0

            someChecked =
                incomplete < count && incomplete > 0
        in
            section [ class "main" ]
                [ input
                    [ class "toggle-all"
                    , type_ "checkbox"
                    , checked allChecked
                    , indeterminate someChecked
                    , onClick ToggleAll
                    ]
                    []
                , label [ for "toggle-all" ]
                    [ text "Mark all as complete"
                    ]
                , ul [ class "todo-list" ]
                    (model.todos
                        |> List.filter (showTodo model.filter)
                        |> List.map viewTodo
                    )
                ]


indeterminate : Bool -> Attribute msg
indeterminate bool =
    property "indeterminate" (Json.bool bool)


showTodo : FilterState -> Todo -> Bool
showTodo filter todo =
    case filter of
        All ->
            True

        Active ->
            not todo.completed

        Completed ->
            todo.completed


viewTodo : Todo -> Html Msg
viewTodo todo =
    li (todoLiClass todo)
        [ div [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onClick (ToggleCompleted todo)
                ]
                []
            , label [ onDoubleClick (Edit todo True) ]
                [ text todo.title ]
            , button
                [ class "destroy"
                , onClick (Delete todo)
                ]
                []
            ]
        , input
            [ class "edit"
            , value todo.title
            , onInput (TodoInput todo)
            , onBlur (Edit todo False)
            , onEnter (Edit todo False)
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


viewFooter : Model -> Html Msg
viewFooter model =
    if List.isEmpty model.todos then
        text ""
    else
        let
            count =
                countIncomplete model.todos
        in
            footer [ class "footer" ]
                [ span [ class "todo-count" ]
                    [ strong [] [ text (toString count) ]
                    , text (" " ++ (pluralize count "item") ++ " left")
                    ]
                , viewFilters model.filter
                , viewClearCompleted model.todos
                ]


viewFilters : FilterState -> Html Msg
viewFilters filter =
    ul [ class "filters" ]
        [ li []
            [ a
                [ class (filterClass filter All)
                , href "#"
                , onClick (Filter All)
                ]
                [ text "All" ]
            , a
                [ class (filterClass filter Active)
                , href "#"
                , onClick (Filter Active)
                ]
                [ text "Active" ]
            , a
                [ class (filterClass filter Completed)
                , href "#"
                , onClick (Filter Completed)
                ]
                [ text "Completed" ]
            ]
        ]


filterClass : FilterState -> FilterState -> String
filterClass a b =
    if a == b then
        "selected"
    else
        ""


viewClearCompleted : List Todo -> Html Msg
viewClearCompleted todos =
    let
        anyComplete =
            todos
                |> List.any (\t -> t.completed)
    in
        if anyComplete then
            button
                [ class "clear-completed"
                , onClick ClearCompleted
                ]
                [ text "Clear completed" ]
        else
            text ""


pluralize : Int -> String -> String
pluralize count noun =
    if count == 1 then
        noun
    else
        noun ++ "s"


countIncomplete : List Todo -> Int
countIncomplete todos =
    todos
        |> List.filter (\t -> not t.completed)
        |> List.length


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            { model
                | todos = (newTodo model.newText) :: model.todos
                , newText = ""
            }

        Input text ->
            { model | newText = text }

        ToggleCompleted todo ->
            { model | todos = (toggleTodo todo model.todos) }

        ToggleAll ->
            { model | todos = (toggleAll model.todos) }

        Delete todo ->
            { model
                | todos = model.todos |> List.filter (\t -> t /= todo)
            }

        ClearCompleted ->
            { model
                | todos = model.todos |> List.filter (\t -> not t.completed)
            }

        Edit todo editing ->
            { model
                | todos = (setEditing editing todo model.todos)
            }

        TodoInput todo text ->
            { model
                | todos = (setTitle text todo model.todos)
            }

        Filter state ->
            { model
                | filter = state
            }


setTitle : String -> Todo -> List Todo -> List Todo
setTitle text todo todos =
    let
        set text t =
            if t == todo then
                { t | title = text }
            else
                t
    in
        List.map (set text) todos


setEditing : Bool -> Todo -> List Todo -> List Todo
setEditing editing todo todos =
    let
        set e t =
            if t == todo then
                { t | editing = e }
            else
                t
    in
        List.map (set editing) todos


toggleTodo : Todo -> List Todo -> List Todo
toggleTodo todo todos =
    let
        toggle t =
            if t == todo then
                { t | completed = not todo.completed }
            else
                t
    in
        List.map toggle todos


toggleAll : List Todo -> List Todo
toggleAll todos =
    let
        allChecked =
            (countIncomplete todos) == 0

        toggle t =
            { t | completed = not allChecked }
    in
        List.map toggle todos


initialModel : Model
initialModel =
    { todos = []
    , newText = ""
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
    , newText = ""
    , filter = All
    }


main =
    Html.beginnerProgram
        { model = testModel
        , view = view
        , update = update
        }

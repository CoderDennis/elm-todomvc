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
import Task
import Dom


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
    | Edit Todo Int
    | CancelEdit Todo
    | TodoInput Todo String
    | ClearCompleted
    | Filter FilterState
    | NoOp


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
                        |> List.indexedMap viewTodo
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


viewTodo : Int -> Todo -> Html Msg
viewTodo index todo =
    li
        [ classList
            [ ( "editing", todo.editing )
            , ( "completed", todo.completed )
            ]
        ]
        [ div [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onClick (ToggleCompleted todo)
                ]
                []
            , label [ onDoubleClick (Edit todo index) ]
                [ text todo.title ]
            , button
                [ class "destroy"
                , onClick (Delete todo)
                ]
                []
            ]
        , input
            [ id (editId index)
            , class "edit"
            , value todo.title
            , onInput (TodoInput todo)
            , onBlur (CancelEdit todo)
            , onEnter (CancelEdit todo)
            ]
            []
        ]


editId : Int -> String
editId index =
    "todo-edit-" ++ (toString index)


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
                [ classList [ ( "selected", filter == All ) ]
                , href "#"
                , onClick (Filter All)
                ]
                [ text "All" ]
            , a
                [ classList [ ( "selected", filter == Active ) ]
                , href "#"
                , onClick (Filter Active)
                ]
                [ text "Active" ]
            , a
                [ classList [ ( "selected", filter == Completed ) ]
                , href "#"
                , onClick (Filter Completed)
                ]
                [ text "Completed" ]
            ]
        ]


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            ( { model
                | todos = (newTodo model.newText) :: model.todos
                , newText = ""
              }
            , Cmd.none
            )

        Input text ->
            ( { model | newText = text }, Cmd.none )

        ToggleCompleted todo ->
            ( { model | todos = (toggleTodo todo model.todos) }, Cmd.none )

        ToggleAll ->
            ( { model | todos = (toggleAll model.todos) }, Cmd.none )

        Delete todo ->
            ( { model
                | todos = model.todos |> List.filter (\t -> t /= todo)
              }
            , Cmd.none
            )

        ClearCompleted ->
            ( { model
                | todos = model.todos |> List.filter (\t -> not t.completed)
              }
            , Cmd.none
            )

        Edit todo index ->
            ( { model
                | todos = (setEditing True todo model.todos)
              }
            , Task.attempt (\_ -> NoOp) (Dom.focus (editId index))
            )

        CancelEdit todo ->
            ( { model
                | todos = (setEditing False todo model.todos)
              }
            , Cmd.none
            )

        TodoInput todo text ->
            ( { model
                | todos = (setTitle text todo model.todos)
              }
            , Cmd.none
            )

        Filter state ->
            ( { model
                | filter = state
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


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
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }

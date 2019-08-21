import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import List exposing (length)
import Task
import Time
import Url

-- MAIN

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- MODEL

type alias Model =
  { url : Url.Url
  , key : Nav.Key
  , timeZone : Time.Zone
  , exerciseNameEntry : String
  , exerciseDescriptionEntry : String
  , exercises : List Exercise
  , exerciseSummaries : List ExerciseSummaryItem
  , timeOfLastRefresh : Time.Posix
  }

type alias Exercise =
  { name : String
  , description : String
  }

type alias ExerciseSummaryItem =
  { exercise : Exercise
  , loadKg : Float
  , setsDailyTarget : Int
  , repsPerSet : Int
  , dayRecords : List ExerciseDayRecord
  }

type alias CalendarDate =
  { year : Int
  , month : Int
  , day : Int
  }

type alias ExerciseDayRecord =
  { exercise : Exercise
  , loadKg : Float
  , setsDailyTarget : Int
  , repsPerSet : Int
  , calendarDate : CalendarDate
  , setTimes : List LocalTimeRecord -- Applying toDate/toMonth/toDay to a member of setTimes should always return values that match what's in calendarDate
  }

type alias LocalTimeRecord =
  { time : Time.Posix
  , zone : Time.Zone
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model url key Time.utc "" "" [] [] (Time.millisToPosix 0)
  , Task.perform SetTimeZone Time.here
  )

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | SetTimeZone Time.Zone
  | ExerciseNameChange String
  | ExerciseDescriptionChange String
  | AddExercise
  | Refresh Time.Posix

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

    SetTimeZone timeZoneNew ->
      ( { model | timeZone = timeZoneNew }
      , Task.perform Refresh Time.now
      )

    ExerciseNameChange newName ->
      ( { model | exerciseNameEntry = newName }
      , Cmd.none
      )

    ExerciseDescriptionChange newDescription ->
      ( { model | exerciseDescriptionEntry = newDescription }
      , Cmd.none
      )

    AddExercise ->
      ( { model | exercises = Exercise model.exerciseNameEntry model.exerciseDescriptionEntry :: model.exercises }
      , Cmd.none
      )

    Refresh timeNow ->
      ( { model | timeOfLastRefresh = timeNow }
      , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 5000 Refresh

-- VIEW

view : Model -> Browser.Document Msg
view model =
  let
    hour   = String.fromInt (Time.toHour   model.timeZone model.timeOfLastRefresh)
    minute = String.fromInt (Time.toMinute model.timeZone model.timeOfLastRefresh)
    second = String.fromInt (Time.toSecond model.timeZone model.timeOfLastRefresh)
  in
    { title = "Rep Chaser (prototype)"
    , body =
        [ input [ placeholder "Exercise name", value model.exerciseNameEntry, onInput ExerciseNameChange ] []
        , input [ placeholder "Description", value model.exerciseDescriptionEntry, onInput ExerciseDescriptionChange ] []
        , button [ onClick AddExercise ] [ text "Add" ]
        , div [] [ text "Exercises:" ]
        , Keyed.node "ul" [] (List.map viewKeyedExercise model.exercises)
        , text "The current URL is: "
        , b [] [ text (Url.toString model.url) ]
        , ul []
            [ viewLink "/home"
            , viewLink "/profile"
            , viewLink "/reviews/the-century-of-the-self"
            , viewLink "/reviews/public-opinion"
            , viewLink "/reviews/shah-of-shahs"
            ]
        , text ("Last refresh time: " ++ hour ++ ":" ++ minute ++ ":" ++ second)
        ]
    }

viewKeyedExercise : Exercise -> (String, Html msg)
viewKeyedExercise exercise =
  ( exercise.name, lazy viewExercise exercise )

viewExercise : Exercise -> Html msg
viewExercise exercise =
  li [] [ text ( exercise.name ++ " : " ++ exercise.description ) ]

viewKeyedExerciseSummaryItem : ExerciseSummaryItem -> (String, Html msg)
viewKeyedExerciseSummaryItem exerciseSummaryItem =
  ( exerciseSummaryItem.exercise.name, lazy viewExerciseSummaryItem exerciseSummaryItem )

viewExerciseSummaryItem : ExerciseSummaryItem -> Html msg
viewExerciseSummaryItem exerciseSummaryItem =
  li [] [ text (String.fromInt (List.length exerciseSummaryItem.dayRecords)) ]

viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]
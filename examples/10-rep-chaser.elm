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
  , exerciseNameInput : String
  , exerciseDescriptionInput : String
  , exerciseLoadKgInput : String
  , exerciseRepsPerSetInput : String
  , exerciseSetsDailyTargetInput : String
  , exercise : Maybe Exercise
  , exercises : List Exercise
  , exerciseHistories : List ExerciseHistory
  , timeOfLastRefresh : Time.Posix
  }

type alias Exercise =
  { name : String
  , description : String
  , loadKg : Float
  , repsPerSet : Int
  , setsDailyTarget : Int
  }

type alias ExerciseHistory =
  { exercise : Exercise
  , dayRecords : List ExerciseDayRecord
  }

type alias CalendarDate =
  { year : Int
  , month : Int
  , day : Int
  }

type alias ExerciseDayRecord =
  { calendarDate : CalendarDate
  , setTimes : List LocalTimeRecord -- Applying toDate/toMonth/toDay to a member of setTimes should always return values that match what's in calendarDate
  }

type alias LocalTimeRecord =
  { time : Time.Posix
  , zone : Time.Zone
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model url key Time.utc "" "" "" "" "" Nothing [] [] (Time.millisToPosix 0)
  , Task.perform SetTimeZone Time.here
  )

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | SetTimeZone Time.Zone
  | ExerciseNameInputChange String
  | ExerciseDescriptionInputChange String
  | ExerciseLoadKgInputChange String
  | ExerciseRepsPerSetInputChange String
  | ExerciseSetsDailyTargetInputChange String
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

    ExerciseNameInputChange newName ->
      ( { model | exerciseNameInput = newName }
      , Cmd.none
      )

    ExerciseDescriptionInputChange newDescription ->
      ( { model | exerciseDescriptionInput = newDescription }
      , Cmd.none
      )

    ExerciseLoadKgInputChange newLoadKg ->
      ( { model | exerciseLoadKgInput = newLoadKg }
      , Cmd.none
      )

    ExerciseRepsPerSetInputChange newRepsPerSet ->
      ( { model | exerciseRepsPerSetInput = newRepsPerSet }
      , Cmd.none
      )

    ExerciseSetsDailyTargetInputChange newSetsDailyTarget ->
      ( { model | exerciseSetsDailyTargetInput = newSetsDailyTarget }
      , Cmd.none
      )

    AddExercise ->
      ( tryAddExercise model
      , Cmd.none
      )

    Refresh timeNow ->
      ( { model | timeOfLastRefresh = timeNow }
      , Cmd.none
      )

tryAddExercise model =
  case model.exercise of
    Nothing ->
      model
    Just modelExercise ->
      addExercise model modelExercise

addExercise model exercise =
  { model | exercises = exercise :: model.exercises }

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
        [ input [ placeholder "Exercise name", value model.exerciseNameInput, onInput ExerciseNameInputChange ] []
        , input [ placeholder "Description", value model.exerciseDescriptionInput, onInput ExerciseDescriptionInputChange ] []
        , input [ placeholder "Load (kg)", value model.exerciseLoadKgInput, onInput ExerciseLoadKgInputChange ] []
        , input [ placeholder "Reps per set", value model.exerciseRepsPerSetInput, onInput ExerciseRepsPerSetInputChange ] []
        , input [ placeholder "Sets daily target", value model.exerciseSetsDailyTargetInput, onInput ExerciseSetsDailyTargetInputChange ] []
        , button [ onClick AddExercise, style "color" (addExerciseButtonColour model) ] [ text "Add" ]
        , div [] [ text "Exercises:" ]
        , Keyed.node "ul" [] (List.map viewKeyedExercise model.exercises)
        , text ("Last refresh time: " ++ hour ++ ":" ++ minute ++ ":" ++ second)
        ]
    }

addExerciseButtonColour : Model -> String
addExerciseButtonColour model =
  case String.toInt model.exerciseLoadKgInput of
    Just _ -> "black"
    Nothing -> "red"

viewKeyedExercise : Exercise -> (String, Html msg)
viewKeyedExercise exercise =
  ( exercise.name, lazy viewExercise exercise )

viewExercise : Exercise -> Html msg
viewExercise exercise =
  li [] [ text ( exercise.name ++ " : " ++ exercise.description ) ]

viewKeyedExerciseHistory : ExerciseHistory -> (String, Html msg)
viewKeyedExerciseHistory exerciseHistory =
  ( exerciseHistory.exercise.name, lazy viewExerciseHistory exerciseHistory )

viewExerciseHistory : ExerciseHistory -> Html msg
viewExerciseHistory exerciseHistory =
  li [] [ text (String.fromInt (List.length exerciseHistory.dayRecords)) ]
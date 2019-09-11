import Browser
import Browser.Navigation as Nav
import Task
import Time
import Url

import RepChaserModel exposing (..)
import RepChaserView exposing (..)

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

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model url key Time.utc "" "" "" "" "" Nothing Nothing [] [] [] (Time.millisToPosix 0)
  , Task.perform SetTimeZone Time.here
  )

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url -> ( model, Nav.pushUrl model.key (Url.toString url) )
        Browser.External href -> ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

    SetTimeZone timeZoneNew ->
      ( { model | timeZone = timeZoneNew }
      , Task.perform Refresh Time.now
      )

    ExerciseNameInputChange newName ->
      ( tryUpdateExerciseFromInputFields { model | exerciseNameInput = newName }
      , Cmd.none
      )

    ExerciseDescriptionInputChange newDescription ->
      ( tryUpdateExerciseFromInputFields { model | exerciseDescriptionInput = newDescription }
      , Cmd.none
      )

    ExerciseLoadKgInputChange newLoadKg ->
      ( tryUpdateExercisePrescriptionFromInputFields { model | exerciseLoadKgInput = newLoadKg }
      , Cmd.none
      )

    ExerciseRepsPerSetInputChange newRepsPerSet ->
      ( tryUpdateExercisePrescriptionFromInputFields { model | exerciseRepsPerSetInput = newRepsPerSet }
      , Cmd.none
      )

    ExerciseSetsDailyTargetInputChange newSetsDailyTarget ->
      ( tryUpdateExercisePrescriptionFromInputFields { model | exerciseSetsDailyTargetInput = newSetsDailyTarget }
      , Cmd.none
      )

    AddExercise ->
      ( case model.exerciseInput of
          Nothing -> model
          Just modelExercise -> { model | exercises = modelExercise :: model.exercises }
      , Cmd.none
      )

    Refresh timeNow ->
      ( { model | timeOfLastRefresh = timeNow }
      , Cmd.none
      )

tryUpdateExerciseFromInputFields : Model -> Model
tryUpdateExerciseFromInputFields model =
  case tryGetExerciseFromInputFields model of
    Just exercise -> { model | exerciseInput = Just exercise }
    Nothing -> { model | exerciseInput = Nothing }

tryGetExerciseFromInputFields : Model -> Maybe Exercise
tryGetExerciseFromInputFields model =
  let
    exerciseNameInput = String.trim model.exerciseNameInput
  in
    if not (String.isEmpty exerciseNameInput) then
      Just
        { name = model.exerciseNameInput
        , description = String.trim model.exerciseDescriptionInput
        }
    else
      Nothing

tryUpdateExercisePrescriptionFromInputFields : Model -> Model
tryUpdateExercisePrescriptionFromInputFields model =
  case tryGetExercisePrescriptionFromInputFields model of
    Just exercisePrescriptionInput -> { model | exercisePrescriptionInput = Just exercisePrescriptionInput }
    Nothing -> { model | exercisePrescriptionInput = Nothing }

tryGetExercisePrescriptionFromInputFields : Model -> Maybe ExercisePrescription
tryGetExercisePrescriptionFromInputFields model =
  let
    loadKgMaybe = String.toFloat model.exerciseLoadKgInput
    repsPerSetMaybe = String.toInt model.exerciseRepsPerSetInput
    setsDailyTargetMaybe = String.toInt model.exerciseSetsDailyTargetInput

  in
    case (model.exerciseInput, (loadKgMaybe, repsPerSetMaybe, setsDailyTargetMaybe)) of
      (Just exercise, (Just loadKg, Just repsPerSet, Just setsDailyTarget)) -> Just
        { exercise = exercise
        , loadKg = loadKg
        , repsPerSet = repsPerSet
        , setsDailyTarget = setsDailyTarget
        }
      _ -> Nothing

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 5000 Refresh

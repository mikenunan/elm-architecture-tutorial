module RepChaserView exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Time
import Url

import RepChaserModel exposing (..)

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | SetTimeZone Time.Zone
  | ExerciseNameInputChange String
  | ExerciseDescriptionInputChange String
  | ExerciseLoadKgInputChange String
  | ExerciseRepsPerSetInputChange String
  | ExerciseSetsDailyTargetInputChange String
  | AddExercisePrescription
  | Refresh Time.Posix

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
        , button [ onClick AddExercisePrescription, style "color" (addExercisePrescriptionButtonColour model) ] [ text "Add" ]
        , div [] [ text "Exercises:" ]
        , Keyed.node "ul" [] (List.map viewKeyedExercise model.exercises)
        , div [] [ text "Exercise histories:" ]
        , Keyed.node "ul" [] (List.map viewKeyedExerciseHistory model.exerciseHistories)
        , div [] [ text "Exercise prescriptions:" ]
        , Keyed.node "ul" [] (List.map viewKeyedExercisePrescription model.exercisePrescriptions)
        , text ("Last refresh time: " ++ hour ++ ":" ++ minute ++ ":" ++ second)
        ]
    }

addExercisePrescriptionButtonColour : Model -> String
addExercisePrescriptionButtonColour model =
  case model.exercisePrescriptionInput of
    Nothing -> "red"
    Just _ -> "black"

viewKeyedExercise : Exercise -> (String, Html msg)
viewKeyedExercise exercise =
  ( exercise.name, lazy viewExercise exercise )

viewExercise : Exercise -> Html msg
viewExercise exercise =
  li [] [ text ( exercise.name ++ " : " ++ exercise.description ) ]

viewKeyedExerciseHistory : ExerciseHistory -> (String, Html msg)
viewKeyedExerciseHistory exerciseHistory =
  ( exerciseHistory.exercisePrescription.exercise.name, lazy viewExerciseHistory exerciseHistory )

viewExerciseHistory : ExerciseHistory -> Html msg
viewExerciseHistory exerciseHistory =
  li [] [ text (String.fromInt (List.length exerciseHistory.dayRecords)) ]

viewKeyedExercisePrescription : ExercisePrescription -> (String, Html msg)
viewKeyedExercisePrescription exercisePrescription =
  ( exercisePrescription.exercise.name, lazy viewExercisePrescription exercisePrescription )

viewExercisePrescription : ExercisePrescription -> Html msg
viewExercisePrescription exercisePrescription =
  li [] [ text ( exercisePrescription.exercise.name ++ " : " ++ (String.fromFloat exercisePrescription.loadKg) ) ]

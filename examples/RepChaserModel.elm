module RepChaserModel exposing (..)

import Browser.Navigation as Nav
import Time
import Url

type alias Model =
  { url : Url.Url
  , key : Nav.Key
  , timeZone : Time.Zone
  , exerciseNameInput : String
  , exerciseDescriptionInput : String
  , exerciseLoadKgInput : String
  , exerciseRepsPerSetInput : String
  , exerciseSetsDailyTargetInput : String
  , exerciseInput : Maybe Exercise
  , exercisePrescriptionInput : Maybe ExercisePrescription
  , exercises : List Exercise
  , exercisePrescriptions : List ExercisePrescription
  , exerciseHistories : List ExerciseHistory
  , timeOfLastRefresh : Time.Posix
  }

type alias Exercise =
  { name : String
  , description : String
  }

type alias ExercisePrescription =
  { exercise : Exercise
  , loadKg : Float
  , repsPerSet : Int
  , setsDailyTarget : Int
  }

type alias ExerciseHistory =
  { exercisePrescription : ExercisePrescription
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

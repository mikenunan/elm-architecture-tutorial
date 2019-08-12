import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
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
  , timeOfLastRefresh : Time.Posix
  }

type alias ExerciseSummaryItem =
  {
    exercise : String
  , description : String
  , loadKg : Float
  , setsDailyTarget : Int
  , repsPerSet : Int
  }

type alias ExerciseDayRecord =
  {
    exercise : String
  , description : String
  , loadKg : Float
  , setsDailyTarget : Int
  , repsPerSet : Int
  , sets : List ExerciseSetRecord
  }

type alias ExerciseSetRecord =
  {
    time : Time.Posix
  , zone : Time.Zone
  -- The two fields above capture the time at which the set was recorded locally. The following field holds the UTC time of midnight on the morning of the date
  -- on which the set was performed (the local date, which may differ from the date obtained by converting the time field above to UTC), and defines the set as
  -- having been performed on that calendar day
  , date : Time.Posix
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model url key Time.utc (Time.millisToPosix 0)
  , Task.perform SetTimeZone Time.here
  )

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | SetTimeZone Time.Zone
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
  { title = "URL Interceptor"
  , body =
      [ text "The current URL is: "
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

viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]
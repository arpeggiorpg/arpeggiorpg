module Model exposing (..)

import Dict
import Http
import Time
import Json.Decode as JD
import Json.Encode as JE
import Window

import Types as T

type alias GotCreatures = List T.CreatureID -> Msg

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

type Msg
    = Start
    | Batch (List Msg)
    | PollApp
    | ReceivedAppUpdate (Result Http.Error (T.App, JD.Value))
    | AppUpdate (Result Http.Error (T.App, JD.Value))
    | ShowError String
    | ClearError

    | Lazy (Model -> Msg)

    | NoMsg

defaultModel : ProgramFlags -> Model
defaultModel flags =
  { app = Nothing
  , raw_app = JE.null
  , error = ""
  , rpiURL = flags.rpi
  , mainReactComponent = flags.mainReactComponent
  , mainReactElement = flags.mainReactElement
  }

type alias Model =
  { app: Maybe T.App
  , raw_app: JD.Value
  , error: String
  , rpiURL : String
  , mainReactComponent: String
  , mainReactElement: String
  }

devFlags : ProgramFlags
devFlags = {rpi = "http://localhost:1337/", windowSize = (0, 0), mainReactComponent = "", mainReactElement =  ""}

type alias ProgramFlags =
  { rpi : String, windowSize: (Int, Int), mainReactComponent: String, mainReactElement: String }


module Main exposing (..)

import Platform
import Model as M
import Update as U

main : Platform.Program M.ProgramFlags M.Model M.Msg
main = Platform.programWithFlags
  { init = \flags -> (M.defaultModel flags, U.start)
  , update = U.update
  , subscriptions = M.subscriptions
  }

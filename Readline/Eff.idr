module Readline.Eff

import Effects
import Effect.StdIO
import Readline

export
readlineInit : Eff () [STDIO]
readlineInit = foreign FFI_C "readline_init" (IO ())

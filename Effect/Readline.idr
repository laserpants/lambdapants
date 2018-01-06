module Effects.Readline

import Effects
import Readline

%access public export

data Readline : Effect where
  Init : sig Readline ()
  Read : String -> sig Readline (Maybe String)
  AddHistory   : String -> sig Readline ()
  AddDictEntry : String -> sig Readline ()
  AddDictEntries : List String -> sig Readline ()

implementation Handler Readline IO where
    handle () Init                k = do readlineInit; k () ()
    handle () (Read p)            k = do x <- readline p; k x ()
    handle () (AddHistory l)      k = do addHistory l; k () ()
    handle () (AddDictEntry e)    k = do addDictEntry e; k () ()
    handle () (AddDictEntries es) k = do sequence_ (map addDictEntry es); k () ()

READLINE : EFFECT
READLINE = MkEff () Readline

export
readlineInit : Eff () [READLINE]
readlineInit = call (Init)

export
readline : String -> Eff (Maybe String) [READLINE]
readline prompt = call (Read prompt)

export
addHistory : String -> Eff () [READLINE]
addHistory line = call (AddHistory line)

export
addDictEntry : String -> Eff () [READLINE]
addDictEntry entry = call (AddDictEntry entry)

export
addDictEntries : List String -> Eff () [READLINE]
addDictEntries entries = call (AddDictEntries entries)

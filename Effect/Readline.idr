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

Handler Readline IO where
  handle () Init                k = do readlineInit; k () ()
  handle () (Read p)            k = do x <- readline p; k x ()
  handle () (AddHistory l)      k = do addHistory l; k () ()
  handle () (AddDictEntry e)    k = do addDictEntry e; k () ()
  handle () (AddDictEntries es) k = do sequence_ (map addDictEntry es); k () ()

READLINE : EFFECT
READLINE = MkEff () Readline

readlineInit : Eff () [READLINE]
readlineInit = call (Init)

readline : String -> Eff (Maybe String) [READLINE]
readline prompt = call (Read prompt)

addHistory : String -> Eff () [READLINE]
addHistory line = call (AddHistory line)

addDictEntry : String -> Eff () [READLINE]
addDictEntry entry = call (AddDictEntry entry)

addDictEntries : List String -> Eff () [READLINE]
addDictEntries entries = call (AddDictEntries entries)

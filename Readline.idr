module Readline

%include C "foreign.h"
%include C "readline/history.h"
%include C "readline/readline.h"
%lib     C "readline"
%link    C "foreign.o"

readline_c : String -> IO String
readline_c = foreign FFI_C "readline" (String -> IO String)

export 
readline : String -> IO (Maybe String)
readline prompt = do 
  text <- readline_c prompt
  null <- nullStr text
  pure (toMaybe (not null) text)

export 
addHistory : String -> IO ()
addHistory = foreign FFI_C "add_history" (String -> IO ())

export 
readlineInit : IO ()
readlineInit = foreign FFI_C "readline_init" (IO ())

export 
addDictEntry : String -> IO ()
addDictEntry = foreign FFI_C "add_dict_entry" (String -> IO ())

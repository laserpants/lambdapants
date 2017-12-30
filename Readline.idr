module Readline

%lib     C "readline"
%include C "readline/readline.h"
%include C "readline/history.h"

readline_unsafe : String -> IO String
readline_unsafe = foreign FFI_C "readline" (String -> IO String)

export readline : String -> IO (Maybe String)
readline prompt = do 
  text <- readline_unsafe prompt
  null <- nullStr text
  pure (toMaybe (not null) text)

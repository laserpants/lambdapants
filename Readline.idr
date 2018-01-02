module Readline

%include C "foreign.h"
%include C "readline/history.h"
%include C "readline/readline.h"
%lib     C "readline"
%link    C "foreign.o"

readline_unsafe : String -> IO String
readline_unsafe = foreign FFI_C "readline" (String -> IO String)

export readline : String -> IO (Maybe String)
readline prompt = do 
  text <- readline_unsafe prompt
  null <- nullStr text
  pure (toMaybe (not null) text)

export addHistory : String -> IO ()
addHistory = foreign FFI_C "add_history" (String -> IO ())

null : IO String
null = foreign FFI_C "null" (IO String)

yyy : List String -> String -> Int -> String
yyy dict text count = 
  case index' (cast count) res of
       Nothing => unsafePerformIO null 
       Just v  => v
where
  res : List String
  res = filter (\s => isPrefixOf s text) dict

export xxx : IO ()
xxx = foreign FFI_C "xxx" (CFnPtr (String -> Int -> String) -> IO ()) (MkCFnPtr (yyy []))

module Readline

%lib     C "readline"
%include C "readline/readline.h"
%include C "readline/history.h"

export readline : String -> IO String
readline = foreign FFI_C "readline" (String -> IO String) 

module Lambdapants.Colors

export
data AnsiColor 
  = Black  | IntenseBlack
  | Red    | IntenseRed
  | Green  | IntenseGreen  
  | Yellow | IntenseYellow 
  | Blue   | IntenseBlue   
  | Purple | IntensePurple 
  | Cyan   | IntenseCyan   
  | White  | IntenseWhite  

record AnsiStyle where	  
  constructor Style
  foreground : AnsiColor
  background : AnsiColor
  bold       : Bool
  underline  : Bool

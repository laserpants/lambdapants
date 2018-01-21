module Lambdapants.Colors

%default total

public export
data Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Purple
  | Cyan
  | White

public export
data AnsiColor = Regular Color | Intense Color

public export
data Style
  = Normal
  | Bold
  | Underline
  | Italic
  | Reverse
  | Strikethrough

color : Color -> String
color Black  = "0"
color Red    = "1"
color Green  = "2"
color Yellow = "3"
color Blue   = "4"
color Purple = "5"
color Cyan   = "6"
color White  = "7"

foreground : AnsiColor -> String
foreground (Regular code) =  "3" ++ color code
foreground (Intense code) =  "9" ++ color code

background : AnsiColor -> String
background (Regular code) =  "4" ++ color code
background (Intense code) = "10" ++ color code

font : Style -> String
font Normal        = "0"
font Bold          = "1"
font Italic        = "3"
font Underline     = "4"
font Reverse       = "7"
font Strikethrough = "9"

export
decorated : Style -> Maybe AnsiColor -> Maybe AnsiColor -> String -> String
decorated style fg bg input = "\ESC[" ++ body ++ "m" ++ input ++ "\ESC[0m" where
  codes : List String
  codes = catMaybes [ Just (font style), background <$> bg, foreground <$> fg ]
  body : String
  body = foldr (++) "" (intersperse ";" codes)

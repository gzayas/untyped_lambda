import Parser
import Parser.Basic
import Parser.Char
import Parser.Char.Basic
import Lamb.Parser.Basic

open Parser Parser.Char Parser.Char.Unicode
open Lamb.Parser.Basic

namespace Lamb.Parser.Combinators
private def comments : Parser Unit :=
  withErrorMessage "<comments>" do
    let _ ←  takeUntil (lookAhead (string "*/")) (whitespace <|> alpha)
    pure ()

def commentsP : Parser Unit :=
  withErrorMessage "expected end of comment" do
    string "/*" *> comments *> string "*/" *> pure ()

def ws : Parser Unit :=
  let ctrl : Parser Unit := GeneralCategory.control *> pure ();
  let ws' : Parser Unit := whitespace *> pure ();
  takeMany (ws' <|> ctrl <|> commentsP) *> pure ()

def lex {α} (p : Parser α) : Parser α :=
  p <* ws

def matchS (s : String) : Parser Unit :=
  lex (string s *> ws *> pure ())

def matchC (c : Char) : Parser Unit :=
  lex (char c *> pure ())

private def text : Parser String :=
  withErrorMessage "<text>" do
    let s ←  takeUntil (lookAhead (char '"')) (whitespace <|> alpha) -- "
    pure (s.fst.foldl (· ++ toString ·) "")

def literal : Parser String :=
  lex (withErrorMessage "expected string" do
    char '"' *> text <* char '"')

private def asString (s: Array Char) : String := s.foldl (· ++ toString ·) ""

def ident : Parser String :=
  withErrorMessage "<identifier>" do
  lex (do
    let fst ← (take 1 alpha)
    let rst ← (takeMany (ASCII.alphanum <|> char '_'))
    pure (asString fst ++ asString rst))

def parseFloat : Parser Float := lex (Parser.Char.ASCII.parseFloat)

def between {α} (o : Char) (p : Parser α) (c : Char) : Parser α :=
  matchC o *> p <* matchC c

end Lamb.Parser.Combinators

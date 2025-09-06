import Parser
import Parser.Basic
import Parser.Char
import Parser.Char.Basic
import Lamb.Parser.Basic

open Parser Parser.Char Parser.Char.Unicode
open Lamb.Parser.Basic

namespace Lamb.Parser.Combinators

def ws : Parser Unit := optional whitespace
def lex {α} (p : Parser α) : Parser α :=
  p <* ws

def matchS (s : String) : Parser Unit :=
  lex (string s *> whitespace *> pure ())

def matchC (c : Char) : Parser Unit :=
  lex (char c *> whitespace *> pure ())

private def text : Parser String :=
  withErrorMessage "<text>" do
    let s ←  takeUntil (char '"') (whitespace <|> alpha) -- "
    pure (s.fst.foldl (· ++ toString ·) "")

def literal : Parser String :=
  lex (withErrorMessage "expected string" do
    char '"' *> text <* char '"')

private def asString (s: Array Char) : String := s.foldl (· ++ toString ·) ""

def ident : Parser String :=
  lex (do
    let fst ← (take 1 alpha)
    let rst ← (takeMany (ASCII.alphanum <|> char '_'))
    pure (asString fst ++ asString rst))

def parseFloat : Parser Float := Parser.Char.ASCII.parseFloat

def between {α} (o : Char) (p : Parser α) (c : Char) : Parser α :=
  matchC o *> p <* matchC c

def sepBy (p : Parser α) (sep : Parser Unit) : Parser <| Array α := takeMany (sep *> p)

end Lamb.Parser.Combinators

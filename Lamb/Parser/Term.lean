import Parser
import Lamb.Syntax
import Lamb.Parser.Combinators
import Lamb.Parser.SourceInfo
import Lamb.Parser.Basic
open Lamb.Syntax

open Lamb.Parser.Basic
open Lamb.Parser.Combinators
open Lamb.Parser.SourceInfo
open Parser

namespace Lamb.Parser.Term

/- takes a [Parser α] and a function that expects both α and a Span, returning a [Parser β]  -/
def withSpan {α β} (p : Parser α) (f : α → Span → β) : Parser β := do
   let (v, start, stop) ← Parser.withCapture p
   pure (f v {s := (← getStream).str, start := start.byteIdx, stop := stop.byteIdx})

mutual
  partial def stringP : Parser Term := withSpan literal (fun l s => Term.string s l)

  partial def varP : Parser Term :=
     withSpan varP_ (fun (v, idx, lvl) s => Term.var s v idx lvl)
  where
     partial def varP_ : Parser (String × Nat × Nat) := do
       let x ← ident
       let ctx  ← get
       match Syntax.name2index ctx x  with
       | some idx => pure (x, idx, (List.length ctx))
       | none     => Parser.throwUnexpectedWithMessage none s!"Identifier {x} is unbound"

  partial def trueP : Parser Term := withSpan (matchS "true") (fun _ s => Term.true s)
  partial def falseP : Parser Term := withSpan (matchS "false") (fun _ s => Term.false s)

  partial def ifP : Parser Term :=
    withSpan ifP_ (fun (cond, thn, els) s => Term.if_ s cond thn els)
  where
    partial def ifP_ : Parser (Term × Term × Term) := do
      matchS "if"
      let cond ← term
      matchS "then"
      let thn ← term
      matchS "else"
      let els ← term
      pure (cond, thn, els)

  partial def letP : Parser Term := do
    withSpan letP_ (fun (x, t, t₁) s => Term.let_ s x t t₁)
  where
    partial def letP_ : Parser (String × Term × Term) := do
      matchS "let"
      let x ← ident
      matchC '='
      let t ← term
      matchS "in"
      let t₁ ← term
      pure (x, t, t₁)

  partial def absP : Parser Term := do
    withSpan absP_ (fun (x, t) s => Term.abs s x t)
  where
    partial def absP_ : Parser (String × Term) := do
        (matchC 'λ' <|> matchC '\\')
        let x ← ident
        matchS "."
        let t ← term
        pure (x, t)

  partial def zeroP : Parser Term := withSpan ( matchC '0') (fun _ s => Term.zero s)

  partial def succP : Parser Term :=
    withSpan (matchS "succ" *> term) (fun t s => Term.succ s t)

  partial def predP : Parser Term :=
    withSpan (matchS "pred" *> term) (fun t s => Term.pred s t)

  partial def isZeroP : Parser Term :=
    withSpan (matchS "iszero" *> term) (fun t s => Term.isZero s t)

  partial def floatP : Parser Term :=
    withSpan parseFloat (fun f s => Term.float s f)

  partial def timesFloatP : Parser Term :=
    withSpan (Prod.mk <$> term <*> term) (fun (t₁, t₂) s => Term.timesfloat s t₁ t₂)

  partial def namedField : Parser (Span × String × Term) :=
    withSpan (Prod.mk <$> (ident <* matchC '=') <*> term) (fun (x, t) s => (s, x, t))

  partial def unnamedField : Parser (Span × String × Term) :=
    withSpan term (fun t s => (s, "", t))

  partial def recordFields : Parser Term :=
    withSpan recordFields_ (fun l s => Term.record s l)
  where
     partial def recordFields_ : Parser (List (Span × String × Term)) := do
       let fields ← Combinators.sepBy (namedField <|> unnamedField) (matchC ',')
       let namedFields ← pure (List.map nameField (List.zipIdx fields.toList))
       pure namedFields
       where
        partial def nameField (f : ((Span × String × Term) × Nat)) : (Span × String × Term) :=
          match f.fst with
          | (s, "", t) => (s, f.snd.repr, t)
          | _  => f.fst

  partial def recordP : Parser Term := do
    between '{' recordFields  '}'

  partial def atom : Parser Term := do
    between '(' term ')' <|>
    stringP <|>
    trueP <|>
    falseP <|>
    recordP <|>
    ifP <|>
    letP <|>
    zeroP <|>
    floatP <|>
    timesFloatP <|>
    varP

  /- Apply terms to the left -/
  partial def postfixes (t : Term) : Parser Term :=
   (do
     let t' ←
       (do let arg ← atom; pure (Term.app (Syntax.spanOf t) t arg))
       <|>
       (do let _ ← matchC '.'; let f ← ident; pure (Term.proj (Syntax.spanOf t) t f))
     postfixes t')
   <|> pure t

  partial def term : Parser Term := do
    let t ← atom
    postfixes t
end


def termP : Parser Command :=
  withSpan (term) (fun t s => Command.eval s t)

def importP : Parser Command :=
  withSpan (matchS "import" *> ident) (fun module s => Command.import_ s module)

def bindIdentifier_ : Parser (String × Binding) := do
  let x ← (ident <* matchC '/')
  let ctx ← get
  set ((x, Binding.nameBind) :: ctx)
  pure (x, Binding.nameBind)
def bindIdentifier : Parser Command :=
  withSpan bindIdentifier_ (fun (x, b) s => Command.bind s x b)

def bindTerm_ : Parser (String × Binding) := do
    let x ← ident <* matchC '='
    let t ← term
    let ctx ← get
    set ((x, Binding.nameBind) :: ctx)
    pure (x, Binding.abbBind t)
def bindTerm : Parser Command :=
  withSpan bindTerm_ (fun (x, b) s => Command.bind s x b)

def bindP : Parser Command := bindIdentifier <|> bindTerm

def commandP  : Parser Command :=
  importP <|> termP <|> bindP

/- The top level of a file is a sequence of commands, each terminated
   by a semicolon. -/

def toplevel: Parser <| Array Command :=
  sepBy commandP (matchC ';')


def parse (input : String) : Except String <| Array Command :=
  match StateT.run ((ws *> toplevel <* Parser.endOfInput).run input.toSubstring) Inhabited.default with
  | (.ok _ stx, _) => .ok stx
  | (.error _ err, _) => .error ("error: " ++ toString err)

def parseTerm (input : String) : Except String Term  :=
  match StateT.run ((ws *> term <* Parser.endOfInput).run input.toSubstring) Inhabited.default with
  | (.ok _ stx, _) => .ok stx
  | (.error _ err, _) => .error ("error: " ++ toString err)


def parseFile (p : Parser α) (path : System.FilePath) : IO α := do
  let contents ← IO.FS.readFile path
  match StateT.run (p.run contents) Inhabited.default with
  | (.ok _ stx, _) => pure stx
  | (.error _ msg, _) => throw <| IO.userError s!"parse error: {msg} in {path}"

end Lamb.Parser.Term

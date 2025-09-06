namespace Lamb.Syntax

structure Span where
  s     : String
  start : Nat
  stop  : Nat
deriving Repr, Inhabited

inductive Term : Type
  | string (span : Span)  (s : String)
  | var (span : Span) (name : String) (idx level: Nat)
  | true (span : Span)
  | false (span : Span)
  | if_ (span : Span) (c t e: Term)
  | let_ (span : Span) (p : String) (t t₁: Term)
  | record (span : Span) (fields: List (Span × String × Term))
  | proj (span : Span) (t: Term)  (s : String)
  | abs (span : Span) (p : String) (t : Term)
  | app (span : Span) (t₁ t₂ : Term)
  | zero (span : Span)
  | succ (span : Span) (t : Term)
  | pred (span : Span) (t : Term)
  | isZero (span : Span) (t : Term)
  | float (span : Span) (f : Float)
  | timesfloat (span : Span) (t₁ t₂ : Term)
  deriving Repr, Inhabited

def spanOf (t : Term) : Span :=
  match t with
  | .string span _ => span
  | .var span _ _ _ => span
  | .true span => span
  | .false span => span
  | .if_ span _ _ _ => span
  | .let_ span _ _ _ => span
  | .record span _ => span
  | .proj span _ _ => span
  | .abs span _ _ => span
  | .app span _ _  => span
  | .zero span => span
  | .succ span _ => span
  | .pred span _ => span
  | .isZero span _ => span
  | .float span _ => span
  | .timesfloat span _ _  => span


inductive Binding
  | nameBind
  | abbBind (t : Term)
deriving Repr

abbrev Context := List (String × Binding)

def name2index (ctx : Context) (x : String) : (Option Nat) :=
 match ctx with
 | .nil              => none
 | .cons (y, _) rest => if y = x then some 0 else (do pure (1 + (← name2index rest x)))


inductive Command
  | import_ (span : Span) (module: String)
  | eval  (span : Span) (t : Term)
  | bind  (span : Span) (n : String) (b : Binding)
deriving Repr

end Lamb.Syntax

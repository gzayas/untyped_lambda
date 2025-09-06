namespace Lamb.Parser.SourceInfo

/-- Convert a UTF-8 byte offset into (line, column), both 0-based. -/
partial def utf8PosToLineCol (s : String) (pos : String.Pos) : Nat × Nat :=
  let rec loop (i : String.Pos) (line col : Nat) :=
    if _ : i < pos then
      let c := s.get i
      let i' := s.next i
      if c = '\n' then loop i' (line+1) 0
      else loop i' line (col+1)
    else
      (line, col)
  loop 0 0 0


def formatError (input : String) (err : String) : String :=
  match err.splitOn ":" with
  | posStr :: rest =>
    match posStr.toNat? with
    | some n =>
      let pos : String.Pos := ⟨n⟩
      let (line, col) := utf8PosToLineCol input pos
      s!"Error at line {line+1}, col {col+1}: {String.intercalate ":" rest}"
    | none => err
  | _ => err


end Lamb.Parser.SourceInfo

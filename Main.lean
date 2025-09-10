import Lamb.Parser
open Lamb.Parser.Term

def printHelp : IO UInt32 := do
  let stdout ← IO.getStdout
  stdout.putStrLn "Lamb - parses a file that contains lambda expressions."
  pure 0

def process (args : List String) : IO Unit := do
  match args with
  | [] => IO.println s!"Expected a file name, got nothing" *> printHelp *> pure ()
  | filename :: _ =>
    match (← parseFile (System.FilePath.mk filename)) with
    | .ok stx    => IO.println s!"Parsed {repr stx}"
    | .error msg => IO.println s!"Error: {msg}"

def main (args : List String): IO UInt32 :=
  match args with
  | [] => printHelp  *>
return -1
  | "--help":: _ => printHelp
  | _ => process args *> return 0

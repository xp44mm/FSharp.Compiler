namespace FSharp.Compiler

open Internal.Utilities.Text.Lexing

/// Used to save some aspects of the lexbuffer state
[<Struct>]
type LexbufState(startPos: Position,
                 endPos : Position,
                 pastEOF : bool) =
    member _.StartPos = startPos
    member _.EndPos = endPos
    member _.PastEOF = pastEOF

    override this.ToString() =
        $"({this.StartPos}--{this.EndPos})"


namespace FSharp.Compiler

open System
open System.Collections.Generic
open Internal.Utilities.Text.Lexing

open Internal.Utilities.Library
open FSharp.Compiler.AbstractIL.Diagnostics
open FSharp.Compiler.DiagnosticsLogger
open FSharp.Compiler.Features
open FSharp.Compiler.Lexhelp
open FSharp.Compiler.ParseHelpers
open FSharp.Compiler.Parser
open FSharp.Compiler.UnicodeLexing
open FSharp.Compiler
open FSharp.Compiler.PositionUtils

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


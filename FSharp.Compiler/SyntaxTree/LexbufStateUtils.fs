module FSharp.Compiler.LexbufStateUtils

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

// Make sure we don't report 'eof' when inserting a token, and set the positions to the
// last reported token position
let lexbufStateForInsertedDummyTokens (lastTokenStartPos, lastTokenEndPos) =
    LexbufState(lastTokenStartPos, lastTokenEndPos, false)

let getLexbufState(lexbuf: Lexbuf) =
    LexbufState(lexbuf.StartPos, lexbuf.EndPos, lexbuf.IsPastEndOfStream)

let setLexbufState (p: LexbufState) (lexbuf: Lexbuf) =
    lexbuf.StartPos <- p.StartPos
    lexbuf.EndPos <- p.EndPos
    lexbuf.IsPastEndOfStream <- p.PastEOF

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

/// Used to save the state related to a token
/// Treat as though this is read-only.
[<Class>]
type TokenTup =
    // This is mutable for performance reasons.
    val mutable Token : token
    val mutable LexbufState : LexbufState
    val mutable LastTokenPos: Position

    new (token, state, lastTokenPos) =
        { Token = token; LexbufState = state;LastTokenPos = lastTokenPos }

    /// Returns starting position of the token
    member x.StartPos = x.LexbufState.StartPos

    /// Returns end position of the token
    member x.EndPos = x.LexbufState.EndPos

    override this.ToString() =
        $"{this.Token} ({this.StartPos}--{this.EndPos})"

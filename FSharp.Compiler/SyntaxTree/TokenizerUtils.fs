module FSharp.Compiler.TokenizerUtils

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

//----------------------------------------------------------------------------
// Utilities for the tokenizer that are needed in other places
//--------------------------------------------------------------------------*)

[<return: Struct>]
let (|Equals|_|) (s: string) (span: ReadOnlySpan<char>) =
    if span.SequenceEqual(s.AsSpan()) then ValueSome Equals
    else ValueNone

[<return: Struct>]
let (|StartsWith|_|) (s: string) (span: ReadOnlySpan<char>) =
    if span.StartsWith(s.AsSpan()) then ValueSome StartsWith
    else ValueNone

// Strip a bunch of leading '>' of a token, at the end of a typar application
// Note: this is used in the 'service.fs' to do limited postprocessing
[<return: Struct>]
let (|TyparsCloseOp|_|) (txt: string) =
    if not (txt.StartsWith ">") then
        ValueNone
    else
        match txt.AsSpan().IndexOfAnyExcept '>' with
        | -1 -> ValueSome(struct (Array.init txt.Length (fun _ -> GREATER), ValueNone))
        | angles ->
            let afterAngles = txt.AsSpan angles

            let afterOp =
                match afterAngles with
                | Equals "." -> ValueSome DOT
                | Equals "]" -> ValueSome RBRACK
                | Equals "-" -> ValueSome MINUS
                | Equals ".." -> ValueSome DOT_DOT
                | Equals "?" -> ValueSome QMARK
                | Equals "??" -> ValueSome QMARK_QMARK
                | Equals ":=" -> ValueSome COLON_EQUALS
                | Equals "::" -> ValueSome COLON_COLON
                | Equals "*" -> ValueSome STAR
                | Equals "&" -> ValueSome AMP
                | Equals "->" -> ValueSome RARROW
                | Equals "<-" -> ValueSome LARROW
                | Equals "=" -> ValueSome EQUALS
                | Equals "<" -> ValueSome (LESS false)
                | Equals "$" -> ValueSome DOLLAR
                | Equals "%" -> ValueSome (PERCENT_OP "%")
                | Equals "%%" -> ValueSome (PERCENT_OP "%%")
                | StartsWith "="
                | StartsWith "!="
                | StartsWith "<"
                | StartsWith ">"
                | StartsWith "$" -> ValueSome (INFIX_COMPARE_OP (afterAngles.ToString()))
                | StartsWith "&" -> ValueSome (INFIX_AMP_OP (afterAngles.ToString()))
                | StartsWith "|" -> ValueSome (INFIX_BAR_OP (afterAngles.ToString()))
                | StartsWith "!"
                | StartsWith "?"
                | StartsWith "~"  -> ValueSome (PREFIX_OP (afterAngles.ToString()))
                | StartsWith "@"
                | StartsWith "^" -> ValueSome (INFIX_AT_HAT_OP (afterAngles.ToString()))
                | StartsWith "+"
                | StartsWith "-" -> ValueSome (PLUS_MINUS_OP (afterAngles.ToString()))
                | StartsWith "**" -> ValueSome (INFIX_STAR_STAR_OP (afterAngles.ToString()))
                | StartsWith "*"
                | StartsWith "/"
                | StartsWith "%" -> ValueSome (INFIX_STAR_DIV_MOD_OP (afterAngles.ToString()))
                | _ -> ValueNone
        
            ValueSome(struct (Array.init angles (fun _ -> GREATER), afterOp))


///// Match the close of '>' of a set of type parameters.
///// This is done for tokens such as '>>' by smashing the token
//[<Struct>]
//val (|TyparsCloseOp|_|): txt: string -> struct ((bool -> token)[] * token voption) voption

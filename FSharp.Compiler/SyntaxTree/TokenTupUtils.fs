module FSharp.Compiler.TokenTupUtils

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

let posOfTokenTup (tokenTup: TokenTup) =
    match tokenTup.Token with
    // EOF token is processed as if on column -1
    // This forces the closure of all contexts.
    | EOF _ -> tokenTup.LexbufState.StartPos.ColumnMinusOne, tokenTup.LexbufState.EndPos.ColumnMinusOne
    | _ -> tokenTup.LexbufState.StartPos, tokenTup.LexbufState.EndPos

let startPosOfTokenTup (tokenTup: TokenTup) =
    match tokenTup.Token with
    // EOF token is processed as if on column -1
    // This forces the closure of all contexts.
    | EOF _ -> tokenTup.LexbufState.StartPos.ColumnMinusOne
    | _ -> tokenTup.LexbufState.StartPos

let reportDiagnostic (reportF:exn -> 'a) (s: TokenTup) (msg:string) =
    reportF (IndentationProblem(msg, mkSynRange (startPosOfTokenTup s) s.LexbufState.EndPos))

let warn (s: TokenTup) (msg:string) =
    reportDiagnostic warning s msg

let error (s: TokenTup) (msg:string) =
    reportDiagnostic errorR s msg

let isAdjacent (leftTokenTup: TokenTup) (rightTokenTup: TokenTup) =
    let lparenStartPos = startPosOfTokenTup rightTokenTup
    let tokenEndPos = leftTokenTup.LexbufState.EndPos
    tokenEndPos = lparenStartPos

let isAdjacentLBrack (tokenTup: TokenTup) (lookaheadTokenTup: TokenTup) =
    match lookaheadTokenTup.Token with
    | LBRACK -> isAdjacent tokenTup lookaheadTokenTup
    | _ -> false

let nextTokenIsAdjacentLParen (tokenTup: TokenTup) (lookaheadTokenTup: TokenTup) =
    match lookaheadTokenTup.Token with
    | LPAREN -> isAdjacent tokenTup lookaheadTokenTup
    | _ -> false

let isSameLine (tokenTup: TokenTup) (nextTokenTup: TokenTup) =
    match tokenTup.Token with
    | EOF _ -> false
    | _ -> 
        let peekTokenPos = startPosOfTokenTup nextTokenTup
        let tokenStartPos = startPosOfTokenTup tokenTup
        peekTokenPos.OriginalLine = tokenStartPos.OriginalLine

let isControlFlowOrNotSameLine (tokenTup: TokenTup) (nextTokenTup: TokenTup) =
    match tokenTup.Token with
    | EOF _ -> false
    | _ ->
        not (isSameLine(tokenTup) nextTokenTup) || TokenUtils.isControlFlow(nextTokenTup.Token)

// Look for '=' or '.Id.id.id = ' after an identifier
let isLongIdentEquals popNextTokenTup delayToken (tokenTup: TokenTup) =
    match tokenTup.Token with
    | GLOBAL
    | IDENT _ ->
        let rec loop() =
            let tokenTup: TokenTup = popNextTokenTup()
            let res =
                match tokenTup.Token with
                | EOF _ -> false
                | DOT ->
                    let tokenTup = popNextTokenTup()
                    let res =
                        match tokenTup.Token with
                        | EOF _ -> false
                        | IDENT _ -> loop()
                        | _ -> false
                    delayToken tokenTup
                    res
                | EQUALS ->
                    true
                | _ -> false
            delayToken tokenTup
            res
        loop()
    | _ -> false


// Copyright (c) Microsoft Corporation. All Rights Reserved. See License.txt in the project root for license information.

/// LexFilter - process the token stream prior to parsing.
/// Implements the offside rule and a couple of other lexical transformations.
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
open FSharp.Compiler.TokenUtils
open FSharp.Compiler.TokenizerUtils


// LexFilterImpl does the majority of the work for offsides rules and other magic.
// LexFilter just wraps it with light post-processing that introduces a few more 'coming soon' symbols, to
// make it easier for the parser to 'look ahead' and safely shift tokens in a number of recovery scenarios.
type LexFilter (indentationSyntaxStatus: IndentationAwareSyntaxStatus, compilingFSharpCore, lexer, lexbuf: Lexbuf, debug) =
    let inner = LexFilterImpl(indentationSyntaxStatus, compilingFSharpCore, lexer, lexbuf, debug)

    // We don't interact with lexbuf state at all, any inserted tokens have same state/location as the real one read, so
    // we don't have to do any of the wrapped lexbuf magic that you see in LexFilterImpl.
    let delayedStack = Stack<token>()
    let delayToken tok = delayedStack.Push tok

    let popNextToken() =
        if delayedStack.Count > 0 then
            let tokenTup = delayedStack.Pop()
            tokenTup
        else
            inner.GetToken()

    let insertComingSoonTokens comingSoon isHere =
        if debug then dprintf "inserting 6 copies of %+A before %+A\n" comingSoon isHere
        delayToken isHere
        for i in 1..6 do
            delayToken comingSoon

    member _.LexBuffer = inner.LexBuffer

    member lexer.GetToken () =
        let token = popNextToken()
        match token with
        | RBRACE _ ->
            insertComingSoonTokens RBRACE_COMING_SOON RBRACE_IS_HERE
            lexer.GetToken()
        | RPAREN ->
            insertComingSoonTokens RPAREN_COMING_SOON RPAREN_IS_HERE
            lexer.GetToken()
        | OBLOCKEND _ ->
            insertComingSoonTokens OBLOCKEND_COMING_SOON OBLOCKEND_IS_HERE
            lexer.GetToken()
        | _ -> token

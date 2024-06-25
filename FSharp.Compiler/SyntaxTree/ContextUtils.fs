module FSharp.Compiler.ContextUtils

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

let rootCtxt (startPos:Position) = 
    CtxtSeqBlock(FirstInSeqBlock, startPos, NoAddBlockEnd)

let endTokenForACtxt getLastTokenEndRange (ctxt:Context) =
    match ctxt with
    | CtxtFun _
    | CtxtMatchClauses _
    | CtxtWithAsLet _ ->
        Some OEND

    | CtxtWithAsAugment _
    | CtxtDo _
    | CtxtLetDecl (true, _) ->
        Some (ODECLEND(getLastTokenEndRange ()))

    | CtxtSeqBlock(_, _, AddBlockEnd) ->
        Some (OBLOCKEND(getLastTokenEndRange ()))

    | CtxtSeqBlock(_, _, AddOneSidedBlockEnd) ->
        Some (ORIGHT_BLOCK_END(getLastTokenEndRange ()))

    | CtxtModuleHead(isNested = true) ->
        Some OBLOCKSEP

    | _ ->
        None

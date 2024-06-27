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

/// 上下文的结束token，如果有
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

let isBegin (ctxt:Context) =
    match ctxt with
    // open-parens of sorts
    | CtxtParen(TokenLExprParen, _) -> true
    // seq blocks
    | CtxtSeqBlock _ -> true
    // vanillas
    | CtxtVanilla _ -> true
    // preserve all other contexts
    | _ -> false

/// 
let isCorrectIndent (undentationLimit:PositionWithColumn) (newCtxt: Context)=
    let debug = true 

    //if ignoreIndent then true else

    match newCtxt with
    // Don't bother to check pushes of Vanilla blocks since we've
    // always already pushed a SeqBlock at this position.
    /// 香草总是跟着 SeqBlock?
    | CtxtVanilla _
    // String interpolation inner expressions are not limited (e.g. multiline strings)
    | CtxtParen((INTERP_STRING_BEGIN_PART _ | INTERP_STRING_PART _),_) -> true

    | _ ->
        //let p1 = OffsideStack.undentationLimit newCtxt offsideStack
        let c2 = newCtxt.StartCol
        let isCorrectIndent = c2 >= undentationLimit.Column

        if not isCorrectIndent then
            let msg =
                let offsidePos = PositionUtils.warningStringOfPosition undentationLimit.Position
                if debug then
                    sprintf "possible incorrect indentation: this token is offside of context at position %s, newCtxt = %A, newCtxtPos = %s, c1 = %d, c2 = %d"
                        offsidePos newCtxt (PositionUtils.stringOfPos newCtxt.StartPos) undentationLimit.Column c2
                else
                    FSComp.SR.lexfltTokenIsOffsideOfContextStartedEarlier (offsidePos)
            failwith msg

        true

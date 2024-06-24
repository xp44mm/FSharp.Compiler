﻿namespace FSharp.Compiler

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

type Context =
    // Position is position of keyword.
    // bool indicates 'LET' is an offside let that's part of a CtxtSeqBlock where the 'in' is optional
    | CtxtLetDecl of bool * Position
    | CtxtIf of Position
    | CtxtTry of Position
    | CtxtFun of Position
    | CtxtFunction of Position
    | CtxtWithAsLet of Position  // 'with' when used in an object expression
    | CtxtWithAsAugment of Position   // 'with' as used in a type augmentation
    | CtxtMatch of Position
    | CtxtFor of Position
    | CtxtWhile of Position
    | CtxtWhen of Position
    | CtxtVanilla of Position * bool // boolean indicates if vanilla started with 'x = ...' or 'x.y = ...'
    | CtxtThen of Position
    | CtxtElse of Position
    | CtxtDo of Position
    | CtxtInterfaceHead of Position
    | CtxtTypeDefns of Position * equalsEndPos: Position option // 'type <here> =', not removed when we find the "="

    | CtxtNamespaceHead of Position * token
    | CtxtModuleHead of Position * token * LexingModuleAttributes * isNested: bool
    | CtxtMemberHead of Position
    | CtxtMemberBody of Position
    // If bool is true then this is "whole file"
    //     module A.B
    // If bool is false, this is a "module declaration"
    //     module A = ...
    | CtxtModuleBody of Position * bool
    | CtxtNamespaceBody of Position
    | CtxtException of Position
    | CtxtParen of token * Position
    // Position is position of following token
    | CtxtSeqBlock of FirstInSequence * Position * AddBlockEnd
    // Indicates we're processing the second part of a match, after the 'with'
    // First bool indicates "was this 'with' followed immediately by a '|'"?
    | CtxtMatchClauses of bool * Position

    member c.StartPos =
        match c with
        | CtxtNamespaceHead (p, _) | CtxtModuleHead (p, _, _, _) | CtxtException p | CtxtModuleBody (p, _) | CtxtNamespaceBody p
        | CtxtLetDecl (_, p) | CtxtDo p | CtxtInterfaceHead p | CtxtTypeDefns(p, _) | CtxtParen (_, p) | CtxtMemberHead p | CtxtMemberBody p
        | CtxtWithAsLet p
        | CtxtWithAsAugment p
        | CtxtMatchClauses (_, p) | CtxtIf p | CtxtMatch p | CtxtFor p | CtxtWhile p | CtxtWhen p | CtxtFunction p | CtxtFun p | CtxtTry p | CtxtThen p | CtxtElse p | CtxtVanilla (p, _)
        | CtxtSeqBlock (_, p, _) -> p

    member c.StartCol = c.StartPos.Column

    override c.ToString() =
        match c with
        | CtxtNamespaceHead _ -> "nshead"
        | CtxtModuleHead _ -> "modhead"
        | CtxtException _ -> "exception"
        | CtxtModuleBody _ -> "modbody"
        | CtxtNamespaceBody _ -> "nsbody"
        | CtxtLetDecl(b, p) -> sprintf "let(%b, %s)" b (stringOfPos p)
        | CtxtWithAsLet p -> sprintf "withlet(%s)" (stringOfPos p)
        | CtxtWithAsAugment _ -> "withaug"
        | CtxtDo _ -> "do"
        | CtxtInterfaceHead _ -> "interface-decl"
        | CtxtTypeDefns _ -> "type"
        | CtxtParen(_, p) -> sprintf "paren(%s)" (stringOfPos p)
        | CtxtMemberHead _ -> "member-head"
        | CtxtMemberBody _ -> "body"
        | CtxtSeqBlock (b, p, _addBlockEnd) -> sprintf "seqblock(%s, %s)" (match b with FirstInSeqBlock -> "first" | NotFirstInSeqBlock -> "subsequent") (stringOfPos p)
        | CtxtMatchClauses _ -> "matching"

        | CtxtIf _ -> "if"
        | CtxtMatch _ -> "match"
        | CtxtFor _ -> "for"
        | CtxtWhile p -> sprintf "while(%s)" (stringOfPos p)
        | CtxtWhen _ -> "when"
        | CtxtTry _ -> "try"
        | CtxtFun _ -> "fun"
        | CtxtFunction _ -> "function"

        | CtxtThen _ -> "then"
        | CtxtElse p -> sprintf "else(%s)" (stringOfPos p)
        | CtxtVanilla (p, _) -> sprintf "vanilla(%s)" (stringOfPos p)

and AddBlockEnd = AddBlockEnd | NoAddBlockEnd | AddOneSidedBlockEnd
and FirstInSequence = FirstInSeqBlock | NotFirstInSeqBlock
and LexingModuleAttributes = LexingModuleAttributes | NotLexingModuleAttributes

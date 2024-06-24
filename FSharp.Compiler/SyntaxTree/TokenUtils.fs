module FSharp.Compiler.TokenUtils


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



let isInfix token =
    match token with
    | COMMA
    | BAR_BAR
    | AMP_AMP
    | AMP
    | OR
    | INFIX_BAR_OP _
    | INFIX_AMP_OP _
    | INFIX_COMPARE_OP _
    | DOLLAR
    // For the purposes of #light processing, <, > and = are not considered to be infix operators.
    // This is because treating them as infix conflicts with their role in other parts of the grammar,
    // e.g. to delimit "f<int>", or for "let f x = ...."
    //
    // This has the impact that a SeqBlock does not automatically start on the right of a "<", ">" or "=",
    // e.g.
    //     let f x = (x =
    //                   let a = 1 // no #light block started here, parentheses or 'in' needed
    //                   a + x)
    // LESS | GREATER | EQUALS

    | INFIX_AT_HAT_OP _
    | PLUS_MINUS_OP _
    | COLON_COLON
    | COLON_GREATER
    | COLON_QMARK_GREATER
    | COLON_EQUALS
    | MINUS
    | STAR
    | INFIX_STAR_DIV_MOD_OP _
    | INFIX_STAR_STAR_OP _
    | QMARK_QMARK -> true
    | _ -> false

let infixTokenLength token =
    match token with
    | COMMA -> 1
    | AMP -> 1
    | OR -> 1
    | DOLLAR -> 1
    | MINUS -> 1
    | STAR -> 1
    | BAR -> 1
    | LESS false -> 1
    | GREATER false -> 1
    | EQUALS -> 1
    | QMARK_QMARK -> 2
    | COLON_GREATER -> 2
    | COLON_COLON -> 2
    | COLON_EQUALS -> 2
    | BAR_BAR -> 2
    | AMP_AMP -> 2
    | INFIX_BAR_OP d
    | INFIX_AMP_OP d
    | INFIX_COMPARE_OP d
    | INFIX_AT_HAT_OP d
    | PLUS_MINUS_OP d
    | INFIX_STAR_DIV_MOD_OP d
    | INFIX_STAR_STAR_OP d -> d.Length
    | COLON_QMARK_GREATER -> 3
    | _ -> assert false; 1

/// Matches against a left-parenthesis-like token that is valid in expressions.
//
// LBRACK_LESS and GREATER_RBRACK are not here because adding them in these active patterns
// causes more offside warnings, while removing them doesn't add offside warnings in attributes.
[<return: Struct>]
let (|TokenLExprParen|_|) token =
    match token with
    | BEGIN | LPAREN | LBRACE _ | LBRACE_BAR | LBRACK | LBRACK_BAR | LQUOTE _ | LESS true
        -> ValueSome ()
    | _ -> ValueNone

/// Matches against a right-parenthesis-like token that is valid in expressions.
[<return: Struct>]
let (|TokenRExprParen|_|) token =
    match token with
    | END | RPAREN | RBRACE _ | BAR_RBRACE | RBRACK | BAR_RBRACK | RQUOTE _ | GREATER true
        -> ValueSome ()
    | _ -> ValueNone

/// Determine the tokens that may align with the 'if' of an 'if/then/elif/else' without closing
/// the construct
let rec isIfBlockContinuator token =
    match token with
    // The following tokens may align with the "if" without closing the "if", e.g.
    //    if ...
    //    then ...
    //    elif ...
    //    else ...
    | THEN | ELSE | ELIF -> true
    // Likewise
    //    if ... then (
    //    ) elif begin
    //    end else ...
    | END | RPAREN -> true
    // The following arise during reprocessing of the inserted tokens, e.g. when we hit a DONE
    | ORIGHT_BLOCK_END _ | OBLOCKEND _ | ODECLEND _ -> true
    | ODUMMY token -> isIfBlockContinuator token
    | _ -> false

/// Given LanguageFeature.RelaxWhitespace2,
/// Determine the token that may align with the 'match' of a 'match/with' without closing
/// the construct
let rec isMatchBlockContinuator token =
    match token with
    // These tokens may align with the "match" without closing the construct, e.g.
    //         match ...
    //         with ...
    | WITH -> true
    // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ORIGHT_BLOCK_END _ | OBLOCKEND _ | ODECLEND _ -> true
    | ODUMMY token -> isMatchBlockContinuator token
    | _ -> false

/// Determine the token that may align with the 'try' of a 'try/with' or 'try/finally' without closing
/// the construct
let rec isTryBlockContinuator token =
    match token with
    // These tokens may align with the "try" without closing the construct, e.g.
    //         try ...
    //         with ...
    | FINALLY | WITH -> true
    // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ORIGHT_BLOCK_END _ | OBLOCKEND _ | ODECLEND _ -> true
    | ODUMMY token -> isTryBlockContinuator token
    | _ -> false

let rec isThenBlockContinuator token =
    match token with
    // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ORIGHT_BLOCK_END _ | OBLOCKEND _ | ODECLEND _ -> true
    | ODUMMY token -> isThenBlockContinuator token
    | _ -> false

let rec isDoContinuator token =
    match token with
    // These tokens may align with the "for" without closing the construct, e.g.
    //                       for ...
    //                          do
    //                             ...
    //                          done *)
    | DONE -> true
    | ORIGHT_BLOCK_END _ | OBLOCKEND _ | ODECLEND _ -> true // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ODUMMY token -> isDoContinuator token
    | _ -> false

let rec isInterfaceContinuator token =
    match token with
    // These tokens may align with the token "interface" without closing the construct, e.g.
    //                       interface ... with
    //                         ...
    //                       end
    | END -> true
    | ORIGHT_BLOCK_END _ | OBLOCKEND _ | ODECLEND _ -> true // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ODUMMY token -> isInterfaceContinuator token
    | _ -> false

let rec isNamespaceContinuator token =
    match token with
    // These tokens end the construct, e.g.
    //     namespace A.B.C
    //     ...
    //     namespace <-- here
    //     ....
    | EOF _ | NAMESPACE -> false
    | ODUMMY token -> isNamespaceContinuator token
    | _ -> true // anything else is a namespace continuator

let rec isTypeContinuator token =
    match token with
    // The following tokens may align with the token "type" without closing the construct, e.g.
    //     type X =
    //     | A
    //     | B
    //     and Y = c <--- 'and' HERE
    //
    //     type X = {
    //        x: int
    //        y: int
    //     }                     <---          '}' HERE
    //     and Y = c
    //
    //     type Complex = struct
    //       val im : float
    //     end with                  <---          'end' HERE
    //       static member M() = 1
    //     end
    | RBRACE _ | WITH | BAR | AND | END -> true

    // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ORIGHT_BLOCK_END _ | OBLOCKEND _ | ODECLEND _ -> true
    | ODUMMY token -> isTypeContinuator token
    | _ -> false

let rec isForLoopContinuator token =
    match token with
    // This token may align with the "for" without closing the construct, e.g.
    //                      for ... do
    //                          ...
    //                       done
    | DONE -> true
    | ORIGHT_BLOCK_END _ | OBLOCKEND _ | ODECLEND _ -> true// The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ODUMMY token -> isForLoopContinuator token
    | _ -> false

let rec isWhileBlockContinuator token =
    match token with
    // This token may align with the "while" without closing the construct, e.g.
    //                       while ... do
    //                          ...
    //                       done
    | DONE -> true
    | ORIGHT_BLOCK_END _ | OBLOCKEND _ | ODECLEND _ -> true // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ODUMMY token -> isWhileBlockContinuator token
    | _ -> false

let rec isLetContinuator token =
    match token with
    // This token may align with the "let" without closing the construct, e.g.
    //                       let ...
    //                       and ...
    | AND -> true
    | ORIGHT_BLOCK_END _ | OBLOCKEND _ | ODECLEND _ -> true // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ODUMMY token -> isLetContinuator token
    | _ -> false

let rec isTypeSeqBlockElementContinuator token =
    match token with
    // A sequence of items separated by '|' counts as one sequence block element, e.g.
    // type x =
    //   | A                 <-- These together count as one element
    //   | B                 <-- These together count as one element
    //   member x.M1
    //   member x.M2
    | BAR -> true
    | OBLOCKBEGIN | ORIGHT_BLOCK_END _ | OBLOCKEND _ | ODECLEND _ -> true // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ODUMMY token -> isTypeSeqBlockElementContinuator token
    | _ -> false

// Work out when a token doesn't terminate a single item in a sequence definition
let rec isSeqBlockElementContinuator token =
    isInfix token ||
          // Infix tokens may align with the first column of a sequence block without closing a sequence element and starting a new one
          // e.g.
          //  let f x
          //      h x
          //      |> y                              <------- NOTE: Not a new element in the sequence

    match token with
    // The following tokens may align with the first column of a sequence block without closing a sequence element and starting a new one *)
    // e.g.
    // new MenuItem("&Open...",
    //              new EventHandler(fun _ _ ->
    //                  ...
    //              ), <------- NOTE RPAREN HERE
    //              Shortcut.CtrlO)
    | END | AND | WITH | THEN | RPAREN | RBRACE _ | BAR_RBRACE | RBRACK | BAR_RBRACK | RQUOTE _ -> true

    // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ORIGHT_BLOCK_END _ | OBLOCKEND _ | ODECLEND _ -> true
    | ODUMMY token -> isSeqBlockElementContinuator token
    | _ -> false

let rec isWithAugmentBlockContinuator token =
    match token with
    // This token may align with "with" of an augmentation block without closing the construct, e.g.
    //                       interface Foo
    //                          with
    //                             member ...
    //                          end
    | END -> true
    | ODUMMY token -> isWithAugmentBlockContinuator token
    | _ -> false

let isAtomicExprEndToken token =
    match token with
    | IDENT _
    | INT8 _ | INT16 _ | INT32 _ | INT64 _ | NATIVEINT _
    | UINT8 _ | UINT16 _ | UINT32 _ | UINT64 _ | UNATIVEINT _
    | DECIMAL _ | BIGNUM _ | STRING _ | BYTEARRAY _ | CHAR _
    | IEEE32 _ | IEEE64 _
    | RPAREN | RBRACK | RBRACE _ | BAR_RBRACE | BAR_RBRACK | END
    | NULL | FALSE | TRUE | UNDERSCORE -> true
    | _ -> false

//----------------------------------------------------------------------------
// give a 'begin' token, does an 'end' token match?
//--------------------------------------------------------------------------
let parenTokensBalance token1 token2 =
    match token1, token2 with
    | LPAREN, RPAREN
    | LBRACE _, RBRACE _
    | LBRACE_BAR, BAR_RBRACE
    | LBRACK, RBRACK
    | INTERFACE, END
    | CLASS, END
    | SIG, END
    | STRUCT, END
    | INTERP_STRING_BEGIN_PART _, INTERP_STRING_END _
    | INTERP_STRING_BEGIN_PART _, INTERP_STRING_PART _
    | INTERP_STRING_PART _, INTERP_STRING_PART _
    | INTERP_STRING_PART _, INTERP_STRING_END _
    | LBRACK_BAR, BAR_RBRACK
    | LESS true, GREATER true
    | BEGIN, END -> true
    | LQUOTE (q1,_), RQUOTE (q2,_) when q1 = q2 -> true
    | _ -> false

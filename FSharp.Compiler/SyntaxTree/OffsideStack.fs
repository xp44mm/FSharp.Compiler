module FSharp.Compiler.OffsideStack

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

// 'query { join x in ys ... }'
// 'query { ...
//          join x in ys ... }'
// 'query { for ... do
//          join x in ys ... }'
let detectJoinInCtxt (stack: Context list) =
    let rec check s =
            match s with
            | CtxtParen(LBRACE _, _) :: _ -> true
            | (CtxtSeqBlock _ | CtxtDo _ | CtxtFor _) :: rest -> check rest
            | _ -> false
    match stack with
    | CtxtVanilla _ :: rest -> check rest
    | _ -> false

///获得缩进的位置
let undentationLimit relaxWhitespace relaxWhitespace2 (newCtxt: Context) (offsideStack:Context list) =
    //(strict:bool) (ignoreIndent:bool) (tokenTup: TokenTup) 
    let rec loop (strict:bool) (stack:Context list) =
        match newCtxt, stack with
        | _, [] -> PositionWithColumn(newCtxt.StartPos, -1)

        // ignore Vanilla because a SeqBlock is always coming
        | _, CtxtVanilla _ :: rest -> loop strict rest

        | CtxtSeqBlock(FirstInSeqBlock, _, _), (CtxtDo _ as limitCtxt) :: CtxtSeqBlock _ :: (CtxtTypeDefns _ | CtxtModuleBody _) :: _ ->
            PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)

        | CtxtSeqBlock(FirstInSeqBlock, _, _), CtxtWithAsAugment _ :: (CtxtTypeDefns _ as limitCtxt) :: _ ->
            PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)

        | _, CtxtSeqBlock _ :: rest when not strict -> loop strict rest
        | _, CtxtParen _ :: rest when not strict -> loop strict rest

        // 'begin match' limited by minimum of two
        // '(match' limited by minimum of two
        | _, (CtxtMatch _ as ctxt1) :: CtxtSeqBlock _ :: (CtxtParen ((BEGIN | LPAREN), _) as ctxt2) :: _
                    -> if ctxt1.StartCol <= ctxt2.StartCol
                        then PositionWithColumn(ctxt1.StartPos, ctxt1.StartCol)
                        else PositionWithColumn(ctxt2.StartPos, ctxt2.StartCol)
        // Insert this rule to allow
        //     begin match 1 with
        //     | 1 -> ()
        //     | 2 ->
        //       f() // <- No offside warning here
        //     end
        // when relaxWhitespace2
        // Otherwise the rule of 'match ... with' limited by 'match' (given RelaxWhitespace2)
        // will consider the CtxtMatch as the limiting context instead of allowing undentation until the parenthesis
        // Test here: Tests/FSharp.Compiler.ComponentTests/Conformance/LexicalFiltering/Basic/OffsideExceptions.fs, RelaxWhitespace2_AllowedBefore11
        | _, (CtxtMatchClauses _ as ctxt1) :: (CtxtMatch _) :: CtxtSeqBlock _ :: (CtxtParen ((BEGIN | LPAREN), _) as ctxt2) :: _ when relaxWhitespace2
                    -> if ctxt1.StartCol <= ctxt2.StartCol
                        then PositionWithColumn(ctxt1.StartPos, ctxt1.StartCol)
                        else PositionWithColumn(ctxt2.StartPos, ctxt2.StartCol)

            // 'let ... = function' limited by 'let', precisely
            // This covers the common form
            //
            //     let f x = function
            //     | Case1 -> ...
            //     | Case2 -> ...
        | CtxtMatchClauses _, CtxtFunction _ :: CtxtSeqBlock _ :: (CtxtLetDecl _ as limitCtxt) :: _rest
                    -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)

        // Otherwise 'function ...' places no limit until we hit a CtxtLetDecl etc... (Recursive)
        | CtxtMatchClauses _, CtxtFunction _ :: rest
                    -> loop false rest

        // 'try ... with' limited by 'try'
        | _, (CtxtMatchClauses _ :: (CtxtTry _ as limitCtxt) :: _rest)
                    -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)

        // 'match ... with' limited by 'match' (given RelaxWhitespace2)
        | _, (CtxtMatchClauses _ :: (CtxtMatch _ as limitCtxt) :: _rest) when relaxWhitespace2
                    -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)

        // 'fun ->' places no limit until we hit a CtxtLetDecl etc... (Recursive)
        | _, CtxtFun _ :: rest
                    -> loop false rest

        // 'let ... = f ... begin'  limited by 'let' (given RelaxWhitespace2)
        // 'let ('  (pattern match) limited by 'let' (given RelaxWhitespace2)
        // 'let ['  (pattern match) limited by 'let' (given RelaxWhitespace2)
        // 'let {'  (pattern match) limited by 'let' (given RelaxWhitespace2)
        // 'let [|' (pattern match) limited by 'let' (given RelaxWhitespace2)
        // 'let x : {|'             limited by 'let' (given RelaxWhitespace2)
        // 'let x : Foo<'           limited by 'let' (given RelaxWhitespace2)
        // 'let (ActivePattern <@'  limited by 'let' (given RelaxWhitespace2)
        // 'let (ActivePattern <@@' limited by 'let' (given RelaxWhitespace2)
        // Same for 'match', 'if', 'then', 'else', 'for', 'while', 'member', 'when', and everything: No need to specify rules like the 'then' and 'else's below over and over again
        // Test here: Tests/FSharp.Compiler.ComponentTests/Conformance/LexicalFiltering/Basic/OffsideExceptions.fs, RelaxWhitespace2
        | _, CtxtParen (TokenLExprParen, _) :: rest
        // 'let x = { y =' limited by 'let'  (given RelaxWhitespace2) etc.
        // 'let x = {| y =' limited by 'let' (given RelaxWhitespace2) etc.
        // Test here: Tests/FSharp.Compiler.ComponentTests/Conformance/LexicalFiltering/Basic/OffsideExceptions.fs, RelaxWhitespace2
        | _, CtxtSeqBlock _ :: CtxtParen (TokenLExprParen, _) :: rest when relaxWhitespace2
                    -> loop false rest

        // 'f ...{' places no limit until we hit a CtxtLetDecl etc...
        // 'f ...[' places no limit until we hit a CtxtLetDecl etc...
        // 'f ...[|' places no limit until we hit a CtxtLetDecl etc...
        | _, CtxtParen ((LBRACE _ | LBRACK | LBRACK_BAR), _) :: CtxtSeqBlock _ :: rest
        | _, CtxtParen ((LBRACE _ | LBRACK | LBRACK_BAR), _) :: CtxtVanilla _ :: CtxtSeqBlock _ :: rest
        | _, CtxtSeqBlock _ :: CtxtParen((LBRACE _ | LBRACK | LBRACK_BAR), _) :: CtxtVanilla _ :: CtxtSeqBlock _ :: rest
                    -> loop false rest

        // MAJOR PERMITTED UNDENTATION This is allowing:
        //   if x then y else
        //   let x = 3 + 4
        //   x + x
        // This is a serious thing to allow, but is required since there is no "return" in this language.
        // Without it there is no way of escaping special cases in large bits of code without indenting the main case.
        | CtxtSeqBlock _, CtxtElse _ :: (CtxtIf _ as limitCtxt) :: _rest
                    -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)

        // Permitted inner-construct precise block alignment:
        //           interface ...
        //           with ...
        //           end
        //
        //           type ...
        //           with ...
        //           end
        | CtxtWithAsAugment _, (CtxtInterfaceHead _ | CtxtMemberHead _ | CtxtException _ | CtxtTypeDefns _ as limitCtxt :: _rest)
                    -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)

        // Permit undentation via parentheses (or begin/end) following a 'then', 'else' or 'do':
        //        if nr > 0 then (
        //              nr <- nr - 1
        //              acc <- d
        //              i <- i - 1
        //        ) else (
        //              i <- -1
        //        )

        // PERMITTED UNDENTATION: Inner construct (then, with, else, do) that dangle, places no limit until we hit the corresponding leading construct CtxtIf, CtxtFor, CtxtWhile, CtxtVanilla etc... *)
        //    e.g.   if ... then ...
        //              expr
        //           else
        //              expr
        //    rather than forcing
        //           if ...
        //           then expr
        //           else expr
        //   Also  ...... with
        //           ...           <-- this is before the "with"
        //         end

        | _, (CtxtWithAsAugment _ | CtxtThen _ | CtxtElse _ | CtxtDo _ ) :: rest
                    -> loop false rest


        // '... (function ->' places no limit until we hit a CtxtLetDecl etc....  (Recursive)
        //
        //   e.g.
        //        let fffffff() = function
        //          | [] -> 0
        //          | _ -> 1
        //
        //   Note this does not allow
        //        let fffffff() = function _ ->
        //           0
        //   which is not a permitted undentation. This undentation would make no sense if there are multiple clauses in the 'function', which is, after all, what 'function' is really for
        //        let fffffff() = function 1 ->
        //           0
        //          | 2 -> ...       <---- not allowed
        | _, CtxtFunction _ :: rest
                    -> loop false rest

        // 'module ... : sig'    limited by 'module'
        // 'module ... : struct' limited by 'module'
        // 'module ... : begin'  limited by 'module'
        // 'if ... then ('       limited by 'if'
        // 'if ... then {'       limited by 'if'
        // 'if ... then ['       limited by 'if'
        // 'if ... then [|'       limited by 'if'
        // 'if ... else ('       limited by 'if'
        // 'if ... else {'       limited by 'if'
        // 'if ... else ['       limited by 'if'
        // 'if ... else [|'       limited by 'if'
        | _, CtxtParen ((SIG | STRUCT | BEGIN), _) :: CtxtSeqBlock _ :: (CtxtModuleBody (_, false) as limitCtxt) :: _
        | _, CtxtParen ((BEGIN | LPAREN | LBRACK | LBRACE _ | LBRACE_BAR | LBRACK_BAR), _) :: CtxtSeqBlock _ :: CtxtThen _ :: (CtxtIf _ as limitCtxt) :: _
        | _, CtxtParen ((BEGIN | LPAREN | LBRACK | LBRACE _ | LBRACE_BAR | LBRACK_BAR | LBRACK_LESS), _) :: CtxtSeqBlock _ :: CtxtElse _ :: (CtxtIf _ as limitCtxt) :: _

        // 'f ... ('  in seqblock     limited by 'f'
        // 'f ... {'  in seqblock     limited by 'f'  NOTE: this is covered by the more generous case above
        // 'f ... ['  in seqblock     limited by 'f'
        // 'f ... [|' in seqblock      limited by 'f'
        // 'f ... Foo<' in seqblock      limited by 'f'
        | _, CtxtParen ((BEGIN | LPAREN | LESS true | LBRACK | LBRACK_BAR), _) :: CtxtVanilla _ :: (CtxtSeqBlock _ as limitCtxt) :: _

        // 'type C = class ... '       limited by 'type'
        // 'type C = interface ... '       limited by 'type'
        // 'type C = struct ... '       limited by 'type'
        | _, CtxtParen ((CLASS | STRUCT | INTERFACE), _) :: CtxtSeqBlock _ :: (CtxtTypeDefns _ as limitCtxt) ::  _
            -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)

        // 'type C(' limited by 'type'
        | _, CtxtSeqBlock _ :: CtxtParen(LPAREN, _) :: (CtxtTypeDefns _ as limitCtxt) :: _
        // 'static member C(' limited by 'static', likewise others
        | _, CtxtSeqBlock _ :: CtxtParen(LPAREN, _) :: (CtxtMemberHead _ as limitCtxt) :: _
        // 'static member P with get() = ' limited by 'static', likewise others
        | _, CtxtWithAsLet _ :: (CtxtMemberHead _ as limitCtxt) :: _
                when relaxWhitespace
                -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)

        // REVIEW: document these
        | _, CtxtSeqBlock _ :: CtxtParen((BEGIN | LPAREN | LBRACK | LBRACK_BAR), _) :: CtxtVanilla _ :: (CtxtSeqBlock _ as limitCtxt) :: _
        | CtxtSeqBlock _, CtxtParen ((BEGIN | LPAREN | LBRACE _ | LBRACE_BAR | LBRACK | LBRACK_BAR), _) :: CtxtSeqBlock _ :: (CtxtTypeDefns _ | CtxtLetDecl _ | CtxtMemberBody _ | CtxtWithAsLet _ as limitCtxt) :: _
                    -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)

        // Permitted inner-construct (e.g. "then" block and "else" block in overall
        // "if-then-else" block ) block alignment:
        //           if ...
        //           then expr
        //           elif expr
        //           else expr
        | (CtxtIf _ | CtxtElse _ | CtxtThen _), (CtxtIf _ as limitCtxt) :: _rest
                    -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)

        // Permitted inner-construct precise block alignment:
        //           while  ...
        //           do expr
        //           done
        | CtxtDo _, (CtxtFor _ | CtxtWhile _ as limitCtxt) :: _rest
                    -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)


        // These contexts all require indentation by at least one space
        | _, (CtxtInterfaceHead _ | CtxtNamespaceHead _ | CtxtModuleHead _ | CtxtException _ | CtxtModuleBody (_, false) | CtxtIf _ | CtxtWithAsLet _ | CtxtLetDecl _ | CtxtMemberHead _ | CtxtMemberBody _ as limitCtxt :: _)
                    -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)

        // These contexts can have their contents exactly aligning
        | _, (CtxtParen _ | CtxtFor _ | CtxtWhen _ | CtxtWhile _ | CtxtTypeDefns _ | CtxtMatch _ | CtxtModuleBody (_, true) | CtxtNamespaceBody _ | CtxtTry _ | CtxtMatchClauses _ | CtxtSeqBlock _ as limitCtxt :: _)
                    -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)
    loop true offsideStack


let popCtxt debug relaxWhitespace2 getOffsideStack setOffsideStack =
    let rec loop () =
        match getOffsideStack() with
        | [] -> ()
        | h :: rest ->
            if debug then dprintf "<-- popping Context(%A), stack = %A\n" h rest
            setOffsideStack rest
            // For CtxtMatchClauses, also pop the CtxtMatch, if present (we expect it always will be).
            if relaxWhitespace2 then
                match h, rest with
                | CtxtMatchClauses _ , CtxtMatch _ :: _ -> loop ()
                | _ -> ()
    loop ()

let tokenBalancesHeadContext (token:token) (stack: Context list) =
    match token, stack with
    | END, CtxtWithAsAugment _ :: _
    | (ELSE | ELIF), CtxtIf _ :: _
    | DONE, CtxtDo _ :: _
    // WITH balances except in the following contexts.... Phew - an overused keyword!
    | WITH, ( (CtxtMatch _ | CtxtException _ | CtxtMemberHead _ | CtxtInterfaceHead _ | CtxtTry _ | CtxtTypeDefns _ | CtxtMemberBody _) :: _
                            // This is the nasty record/object-expression case
                            | CtxtSeqBlock _ :: CtxtParen((LBRACE _ | LBRACE_BAR), _) :: _ )
    | FINALLY, CtxtTry _ :: _ ->
        true

    // for x in ienum ...
    // let x = ... in
    | IN, (CtxtFor _ | CtxtLetDecl _) :: _ ->
        true

    // 'query { join x in ys ... }'
    // 'query { ...
    //          join x in ys ... }'
    // 'query { for ... do
    //          join x in ys ... }'
    | IN, stack when detectJoinInCtxt stack ->
        true

    // NOTE: ;; does not terminate a 'namespace' body.
    | SEMICOLON_SEMICOLON, CtxtSeqBlock _ :: CtxtNamespaceBody _ :: _ ->
        true

    | SEMICOLON_SEMICOLON, CtxtSeqBlock _ :: CtxtModuleBody (_, true) :: _ ->
        true

    | t2, CtxtParen(t1, _) :: _ ->
        TokenUtils.parenTokensBalance t1 t2

    | _ ->
        false

// If you see a 'member' keyword while you are inside the body of another member, then it usually means there is a syntax error inside this method
// and the upcoming 'member' is the start of the next member in the class. For better parser recovery and diagnostics, it is best to pop out of the
// existing member context so the parser can recover.
//
// However there are two places where 'member' keywords can appear inside expressions inside the body of a member. The first is object expressions, and
// the second is static inline constraints. We must not pop the context stack in those cases, or else legal code will not parse.
//
// It is impossible to decide for sure if we're in one of those two cases, so we must err conservatively if we might be.
let thereIsACtxtMemberBodyOnTheStackAndWeShouldPopStackForUpcomingMember (ctxtStack:Context list) =
    // a 'member' starter keyword is coming; should we pop?
    if not(List.exists (function CtxtMemberBody _ -> true | _ -> false) ctxtStack) then
        false // no member currently on the stack, nothing to pop
    else
        // there is a member context
        if List.exists (function CtxtParen(LBRACE _, _) -> true | _ -> false) ctxtStack then
            false  // an LBRACE could mean an object expression, and object expressions can have 'member' tokens in them, so do not pop, to be safe
        elif List.count (function CtxtParen(LPAREN, _) -> true | _ -> false) ctxtStack >= 2 then
            false  // static member constraints always are embedded in at least two LPARENS, so do not pop, to be safe
        else
            true

// Balancing rule. Every 'in' terminates all surrounding blocks up to a CtxtLetDecl, and will be swallowed by
// terminating the corresponding CtxtLetDecl in the rule below.
// Balancing rule. Every 'done' terminates all surrounding blocks up to a CtxtDo, and will be swallowed by
// terminating the corresponding CtxtDo in the rule below.
let tokenForcesHeadContextClosure (token:token) (stack:Context list) =
    not (isNil stack) &&
    match token with
    | EOF _ -> true
    | SEMICOLON_SEMICOLON -> not (tokenBalancesHeadContext token stack)
    | TokenRExprParen
    | ELSE
    | ELIF
    | DONE
    | IN
    | WITH
    | FINALLY
    | INTERP_STRING_PART _
    | INTERP_STRING_END _ ->
        not (tokenBalancesHeadContext token stack) &&
        // Only close the context if some context is going to match at some point in the stack.
        // If none match, the token will go through, and error recovery will kick in in the parser and report the extra token,
        // and then parsing will continue. Closing all the contexts will not achieve much except aid in a catastrophic failure.
        stack 
        |> ListUtils.suffixExists (tokenBalancesHeadContext token)

    | _ -> false

// ... <<< code with unmatched ( or [ or { or [| >>> ... "type" ...
// We want a TYPE or MODULE keyword to close any currently-open "expression" contexts, as though there were close delimiters in the file, so:
let nextOuterMostInterestingContextIsNamespaceOrModule (offsideStack:Context list) =
    let rec loop offsideStack =
        match offsideStack with
        // next outermost is namespace or module
        | _ :: (CtxtNamespaceBody _ | CtxtModuleBody _) :: _ -> true
        // The context pair below is created a namespace/module scope when user explicitly uses 'begin'...'end',
        // and these can legally contain type definitions, so ignore this combo as uninteresting and recurse deeper
        | _ :: CtxtParen((BEGIN|STRUCT), _) :: CtxtSeqBlock _ :: _ -> loop(offsideStack.Tail.Tail)
        // at the top of the stack there is an implicit module
        | _ :: [] -> true
        // anything else is a non-namespace/module
        | _ -> false
    loop offsideStack

let peekAdjacentTypars 
    (peekNextTokenTup: unit -> TokenTup)
    (popNextTokenTup: unit -> TokenTup)
    (nextTokenIsAdjacentLParen: TokenTup -> bool)
    (delayToken: TokenTup -> unit)
    (pool:TokenTupPool)
    (indentation:bool)
    (tokenTup: TokenTup)
    =

    let lookaheadTokenTup = peekNextTokenTup()
    match lookaheadTokenTup.Token with
    | INFIX_COMPARE_OP "</"
    | INFIX_COMPARE_OP "<^"
    // NOTE: this is "<@"
    | LQUOTE ("<@ @>", false)
    | LESS _ ->
        let tokenEndPos = tokenTup.LexbufState.EndPos
        if TokenTupUtils.isAdjacent tokenTup lookaheadTokenTup then
            let mutable stack = []
            let rec scanAhead nParen =
                let lookaheadTokenTup = popNextTokenTup()
                let lookaheadToken = lookaheadTokenTup.Token
                stack <- (lookaheadTokenTup, true) :: stack
                let lookaheadTokenStartPos = TokenTupUtils.startPosOfTokenTup lookaheadTokenTup
                match lookaheadToken with
                | EOF _ | SEMICOLON_SEMICOLON -> false
                | _ when indentation && lookaheadTokenStartPos < tokenEndPos -> false
                | RPAREN | RBRACK ->
                    let nParen = nParen - 1
                    if nParen > 0 then
                        scanAhead nParen
                    else
                        false
                | GREATER _ | GREATER_RBRACK | GREATER_BAR_RBRACK ->
                    let nParen = nParen - 1
                    let hasAfterOp = (match lookaheadToken with GREATER _ -> false | _ -> true)
                    if nParen > 0 then
                        // Don't smash the token if there is an after op and we're in a nested paren
                        stack <- (lookaheadTokenTup, not hasAfterOp) :: stack.Tail
                        scanAhead nParen
                    else
                        // On successful parse of a set of type parameters, look for an adjacent (, e.g.
                        //    M<int>(args)
                        // and insert a HIGH_PRECEDENCE_PAREN_APP
                        if not hasAfterOp && nextTokenIsAdjacentLParen lookaheadTokenTup then
                            let dotTokenTup = peekNextTokenTup()
                            stack <- (pool.UseLocation(dotTokenTup, HIGH_PRECEDENCE_PAREN_APP), false) :: stack
                        true
                | INFIX_COMPARE_OP (TokenizerUtils.TyparsCloseOp(greaters, afterOp)) ->
                    let nParen = nParen - greaters.Length
                    if nParen > 0 then
                        // Don't smash the token if there is an after op and we're in a nested paren
                        stack <- (lookaheadTokenTup, not afterOp.IsSome) :: stack.Tail
                        scanAhead nParen
                    else
                        // On successful parse of a set of type parameters, look for an adjacent (, e.g.
                        //    M<C<int>>(args)
                        // and insert a HIGH_PRECEDENCE_PAREN_APP
                        if afterOp.IsNone && nextTokenIsAdjacentLParen lookaheadTokenTup then
                            let dotTokenTup = peekNextTokenTup()
                            stack <- (pool.UseLocation(dotTokenTup, HIGH_PRECEDENCE_PAREN_APP), false) :: stack
                        true
                | LPAREN
                | LESS _
                | LBRACK
                | LBRACK_LESS
                | INFIX_COMPARE_OP "</"
                | INFIX_COMPARE_OP "<^"
                // NOTE: this is "<@"
                | LQUOTE ("<@ @>", false) ->
                    scanAhead (nParen+1)

                // These tokens CAN occur in non-parenthesized positions in the grammar of types or type parameter definitions
                // Thus we explicitly DO consider these to be type applications:
                //      f<int>x
                //      f<global.int>x
                //      f<x * x>x
                //      f<x , x>x
                //      f<x ^ x>x
                //      f<x ^- x>x
                //      f<x / x>x
                //      f<x -> x>x
                //      f<{| C : int |}>x
                //      f<x # x>x
                //      f<x ' x>x
                | DEFAULT | COLON | COLON_GREATER | STRUCT | NULL | DELEGATE | AND | WHEN | AMP
                | DOT_DOT
                | NEW
                | LBRACE_BAR
                | SEMICOLON
                | BAR_RBRACE
                | INFIX_AT_HAT_OP "^"
                | INFIX_AT_HAT_OP "^-"
                | INFIX_STAR_DIV_MOD_OP "/"
                | MINUS
                | GLOBAL
                | CONST
                | KEYWORD_STRING _
                | NULL
                | INT8 _ | INT16 _ | INT32 _ | INT64 _ | NATIVEINT _
                | UINT8 _ | UINT16 _ | UINT32 _ | UINT64 _ | UNATIVEINT _
                | DECIMAL _ | BIGNUM _ | STRING _ | BYTEARRAY _ | CHAR _ | TRUE | FALSE
                | IEEE32 _ | IEEE64 _
                | DOT | UNDERSCORE | EQUALS
                | IDENT _ | COMMA | RARROW | HASH
                | STAR | QUOTE ->
                    scanAhead nParen


                // All other tokens ARE assumed to be part of the grammar of types or type parameter definitions
                | _ ->
                    if nParen > 1 then
                        scanAhead nParen
                    else
                        false

            let res = scanAhead 0
            // Put the tokens back on and smash them up if needed
            for tokenTup, smash in stack do
                if smash then
                    match tokenTup.Token with
                    | INFIX_COMPARE_OP "</" ->
                        delayToken (pool.UseShiftedLocation(tokenTup, INFIX_STAR_DIV_MOD_OP "/", 1, 0))
                        delayToken (pool.UseShiftedLocation(tokenTup, LESS res, 0, -1))
                        pool.Return tokenTup
                    | INFIX_COMPARE_OP "<^" ->
                        delayToken (pool.UseShiftedLocation(tokenTup, INFIX_AT_HAT_OP "^", 1, 0))
                        delayToken (pool.UseShiftedLocation(tokenTup, LESS res, 0, -1))
                        pool.Return tokenTup
                            
                    | INFIX_COMPARE_OP ">:" ->
                        delayToken (pool.UseShiftedLocation(tokenTup, COLON, 1, 0))
                        delayToken (pool.UseShiftedLocation(tokenTup, GREATER res, 0, -1))
                        pool.Return tokenTup
                    // NOTE: this is "<@"
                    | LQUOTE ("<@ @>", false) ->
                        delayToken (pool.UseShiftedLocation(tokenTup, INFIX_AT_HAT_OP "@", 1, 0))
                        delayToken (pool.UseShiftedLocation(tokenTup, LESS res, 0, -1))
                        pool.Return tokenTup
                    | GREATER_BAR_RBRACK ->
                        delayToken (pool.UseShiftedLocation(tokenTup, BAR_RBRACK, 1, 0))
                        delayToken (pool.UseShiftedLocation(tokenTup, GREATER res, 0, -2))
                        pool.Return tokenTup
                    | GREATER_RBRACK ->
                        delayToken (pool.UseShiftedLocation(tokenTup, RBRACK, 1, 0))
                        delayToken (pool.UseShiftedLocation(tokenTup, GREATER res, 0, -1))
                        pool.Return tokenTup
                    | GREATER _ ->
                        delayToken (pool.UseLocation(tokenTup, GREATER res))
                        pool.Return tokenTup
                    | INFIX_COMPARE_OP (TokenizerUtils.TyparsCloseOp(greaters, afterOp) as opstr) ->
                        match afterOp with
                        | ValueNone -> ()
                        | ValueSome tok -> delayToken (pool.UseShiftedLocation(tokenTup, tok, greaters.Length, 0))
                        for i = greaters.Length - 1 downto 0 do
                            delayToken (pool.UseShiftedLocation(tokenTup, greaters[i] res, i, -opstr.Length + i + 1))
                        pool.Return tokenTup
                    | _ -> delayToken tokenTup
                else
                    delayToken tokenTup
            res
        else
            false
    | _ -> false

// The TYPE and MODULE keywords cannot be used in expressions, but the parser has a hard time recovering on incomplete-expression-code followed by
// a TYPE or MODULE. So the lexfilter helps out by looking ahead for these tokens and (1) closing expression contexts and (2) inserting extra 'coming soon' tokens
// that the expression rules in the FsYacc parser can 'shift' to make progress parsing the incomplete expressions, without using the 'recover' action.
let insertComingSoonTokens
    compilingFSharpCore
    (pool:TokenTupPool)
    (offsideStack:Context list)
    tokenStartPos
    popCtxt
    delayTokenNoProcessing
    getLastTokenEndRange
    (tokenTup: TokenTup)
    (keywordName:string, comingSoon:token, isHere:token) 
    =
    let debug = true
    // compiling the source for FSharp.Core.dll uses unconventional syntax like
    //     (# "unbox.any !0" type ('T) x : 'T #)
    // where the type keyword is used inside an expression, so we must exempt FSharp.Core from some extra failed-parse-diagnostics-recovery-processing of the 'type' keyword
    let mutable effectsToDo: (unit -> unit) list = []
    if not compilingFSharpCore then
        while not offsideStack.IsEmpty && 
            (not(nextOuterMostInterestingContextIsNamespaceOrModule offsideStack)) &&
            (ContextUtils.isBegin offsideStack.Head) do
            match offsideStack.Head with
            | CtxtParen _ ->
                if debug then dprintf "%s at %a terminates CtxtParen()\n" keywordName PositionUtils.outputPos tokenStartPos
                popCtxt()
            | CtxtSeqBlock(_, _, AddBlockEnd) ->
                popCtxt()
                effectsToDo <- (fun() ->
                    if debug then dprintf "--> because %s is coming, inserting OBLOCKEND\n" keywordName
                    delayTokenNoProcessing (pool.UseLocation(tokenTup, OBLOCKEND(getLastTokenEndRange ())))) :: effectsToDo
            | CtxtSeqBlock(_, _, NoAddBlockEnd) ->
                if debug then dprintf "--> because %s is coming, popping CtxtSeqBlock\n" keywordName
                popCtxt()
            | CtxtSeqBlock(_, _, AddOneSidedBlockEnd) ->
                popCtxt()
                effectsToDo <- (fun() ->
                    if debug then dprintf "--> because %s is coming, inserting ORIGHT_BLOCK_END\n" keywordName
                    delayTokenNoProcessing (pool.UseLocation(tokenTup, ORIGHT_BLOCK_END(getLastTokenEndRange ())))) :: effectsToDo
            | CtxtVanilla _ ->
                if debug then dprintf "--> because %s is coming, popping CtxtVanilla\n" keywordName
                popCtxt()
            | _ -> failwith "impossible, the while loop guard just above prevents this"

    // See bugs 91609/92107/245850; we turn ...TYPE... into ...TYPE_COMING_SOON x6, TYPE_IS_HERE... to help the parser recover when it sees "type" in a parenthesized expression.
    // And we do the same thing for MODULE.
    // Why _six_ TYPE_COMING_SOON? It's rather arbitrary, this means we can recover from up to six unmatched parens before failing. The unit tests (with 91609 in the name) demonstrate this.
    // Don't "delayToken tokenTup", we are replacing it, so consume it.

    if debug then dprintf "inserting 6 copies of %+A before %+A\n" comingSoon isHere

    delayTokenNoProcessing (pool.UseLocation(tokenTup, isHere))
    for i in 1..6 do
        delayTokenNoProcessing (pool.UseLocation(tokenTup, comingSoon))

    // push any END tokens after pushing the TYPE_IS_HERE and TYPE_COMING_SOON stuff, so that they come before those in the token stream
    for e in List.rev effectsToDo do
        e() 

let insertHighPrecedenceApp 
    (pool:TokenTupPool)
    (peekNextTokenTup: unit -> TokenTup)
    nextTokenIsAdjacentLParen
    nextTokenIsAdjacentLBrack
    delayToken
    (tokenTup: TokenTup) 
    =
    let debug = true

    let dotTokenTup = peekNextTokenTup()
    if debug then dprintf "inserting HIGH_PRECEDENCE_PAREN_APP at dotTokenPos = %a\n" PositionUtils.outputPos (TokenTupUtils.startPosOfTokenTup dotTokenTup)
    let hpa =
        if nextTokenIsAdjacentLParen tokenTup then
            HIGH_PRECEDENCE_PAREN_APP
        elif nextTokenIsAdjacentLBrack tokenTup then
            HIGH_PRECEDENCE_BRACK_APP
        else
            failwith "unreachable"
    delayToken(pool.UseLocation(dotTokenTup, hpa))
    delayToken tokenTup
    true

let rulesForBothSoftWhiteAndHardWhite
    (pool:TokenTupPool)
    popNextTokenTup
    delayToken
    peekAdjacentTypars
    nextTokenIsAdjacentLBrack
    nextTokenIsAdjacentLParen
    nextTokenIsAdjacent
    insertHighPrecedenceApp
    prevWasAtomicEnd
    (tokenTup: TokenTup) 
    =
    let debug = true

    match tokenTup.Token with
    | HASH_IDENT ident ->
        let hashPos = LexbufState(tokenTup.StartPos, tokenTup.StartPos.ShiftColumnBy(1), false)
        let identPos = LexbufState(tokenTup.StartPos.ShiftColumnBy(1), tokenTup.EndPos, false)
        delayToken(TokenTup(IDENT(ident), identPos, tokenTup.LastTokenPos))
        delayToken(TokenTup(HASH, hashPos, tokenTup.LastTokenPos))
        true

    // Insert HIGH_PRECEDENCE_BRACK_APP if needed
    //    ident[3]
    | IDENT _ when nextTokenIsAdjacentLBrack tokenTup ->
        insertHighPrecedenceApp tokenTup

    // Insert HIGH_PRECEDENCE_PAREN_APP if needed
    //    ident(3)
    | IDENT _ when nextTokenIsAdjacentLParen tokenTup ->
        insertHighPrecedenceApp tokenTup

    // Insert HIGH_PRECEDENCE_TYAPP if needed
    | DELEGATE | IDENT _ | IEEE64 _ | IEEE32 _ | DECIMAL _ | INT8 _ | INT16 _ | INT32 _ | INT64 _ | NATIVEINT _ | UINT8 _ | UINT16 _ | UINT32 _ | UINT64 _ | UNATIVEINT _ | BIGNUM _ when peekAdjacentTypars false tokenTup ->
        let lessTokenTup = popNextTokenTup()
        delayToken (pool.UseLocation(lessTokenTup, match lessTokenTup.Token with LESS _ -> LESS true | _ -> failwith "unreachable"))

        if debug then dprintf "softwhite inserting HIGH_PRECEDENCE_TYAPP at dotTokenPos = %a\n" PositionUtils.outputPos (TokenTupUtils.startPosOfTokenTup lessTokenTup)

        delayToken (pool.UseLocation(lessTokenTup, HIGH_PRECEDENCE_TYAPP))
        delayToken tokenTup
        pool.Return lessTokenTup
        true

    // ..^1 will get parsed as DOT_DOT_HAT 1 while 1..^2 will get parsed as 1 DOT_DOT HAT 2
    // because of processing rule underneath this.
    | DOT_DOT_HAT ->
        let hatPos = LexbufState(tokenTup.EndPos.ShiftColumnBy(-1), tokenTup.EndPos, false)
        delayToken(let rented = pool.Rent() in rented.Token <- INFIX_AT_HAT_OP("^"); rented.LexbufState <- hatPos; rented.LastTokenPos <- tokenTup.LastTokenPos; rented)
        delayToken(pool.UseShiftedLocation(tokenTup, DOT_DOT, 0, -1))
        pool.Return tokenTup
        true

    // Split this token to allow "1..2" for range specification
    | INT32_DOT_DOT (i, v) ->
        let dotDotPos = LexbufState(tokenTup.EndPos.ShiftColumnBy(-2), tokenTup.EndPos, false)
        delayToken(let rented = pool.Rent() in rented.Token <- DOT_DOT; rented.LexbufState <- dotDotPos; rented.LastTokenPos <- tokenTup.LastTokenPos; rented)
        delayToken(pool.UseShiftedLocation(tokenTup, INT32(i, v), 0, -2))
        pool.Return tokenTup
        true
    // Split @>. and @@>. into two
    | RQUOTE_DOT (s, raw) ->
        let dotPos = LexbufState(tokenTup.EndPos.ShiftColumnBy(-1), tokenTup.EndPos, false)
        delayToken(let rented = pool.Rent() in rented.Token <- DOT; rented.LexbufState <- dotPos; rented.LastTokenPos <- tokenTup.LastTokenPos; rented)
        delayToken(pool.UseShiftedLocation(tokenTup, RQUOTE(s, raw), 0, -1))
        pool.Return tokenTup
        true

    | MINUS | PLUS_MINUS_OP _ | PERCENT_OP _ | AMP | AMP_AMP
        when ((match tokenTup.Token with
                | PLUS_MINUS_OP s -> (s = "+") || (s = "+.") || (s = "-.")
                | PERCENT_OP s -> (s = "%") || (s = "%%")
                | _ -> true) &&
                nextTokenIsAdjacent tokenTup &&
                not (prevWasAtomicEnd && (tokenTup.LastTokenPos = TokenTupUtils.startPosOfTokenTup tokenTup))) ->

        let plus =
            match tokenTup.Token with
            | PLUS_MINUS_OP s -> (s = "+")
            | _ -> false
        let plusOrMinus =
            match tokenTup.Token with
            | PLUS_MINUS_OP s -> (s = "+")
            | MINUS -> true
            | _ -> false
        let nextTokenTup = popNextTokenTup()

        /// Merge the location of the prefix token and the literal
        let delayMergedToken tok =
            let rented = pool.Rent()
            rented.Token <- tok
            rented.LexbufState <- LexbufState(tokenTup.LexbufState.StartPos, nextTokenTup.LexbufState.EndPos, nextTokenTup.LexbufState.PastEOF)
            rented.LastTokenPos <- tokenTup.LastTokenPos
            delayToken(rented)
            pool.Return nextTokenTup
            pool.Return tokenTup

        let noMerge() =
            let tokenName =
                match tokenTup.Token with
                | PLUS_MINUS_OP s
                | PERCENT_OP s -> s
                | AMP -> "&"
                | AMP_AMP -> "&&"
                | MINUS -> "-"
                | _ -> failwith "unreachable"
            let token = ADJACENT_PREFIX_OP tokenName
            delayToken nextTokenTup
            delayToken (pool.UseLocation(tokenTup, token))
            pool.Return tokenTup

        if plusOrMinus then
            match nextTokenTup.Token with
            | INT8(v, bad) -> delayMergedToken(INT8((if plus then v else -v), (plus && bad))) // note: '-' makes a 'bad' max int 'good'. '+' does not
            | INT16(v, bad) -> delayMergedToken(INT16((if plus then v else -v), (plus && bad))) // note: '-' makes a 'bad' max int 'good'. '+' does not
            | INT32(v, bad) -> delayMergedToken(INT32((if plus then v else -v), (plus && bad))) // note: '-' makes a 'bad' max int 'good'. '+' does not
            | INT32_DOT_DOT(v, bad) -> delayMergedToken(INT32_DOT_DOT((if plus then v else -v), (plus && bad))) // note: '-' makes a 'bad' max int 'good'. '+' does not
            | INT64(v, bad) -> delayMergedToken(INT64((if plus then v else -v), (plus && bad))) // note: '-' makes a 'bad' max int 'good'. '+' does not
            | NATIVEINT(v, bad) -> delayMergedToken(NATIVEINT((if plus then v else -v), (plus && bad))) // note: '-' makes a 'bad' max int 'good'. '+' does not
            | IEEE32 v -> delayMergedToken(IEEE32(if plus then v else -v))
            | IEEE64 v -> delayMergedToken(IEEE64(if plus then v else -v))
            | DECIMAL v -> delayMergedToken(DECIMAL(if plus then v else System.Decimal.op_UnaryNegation v))
            | BIGNUM(v, s) -> delayMergedToken(BIGNUM((if plus then v else "-" + v), s))
            | _ -> noMerge()
        else
            noMerge()
        true

    | _ ->
        false

let pushCtxtSeqBlockAt 
    (strict:bool) 
    (pool:TokenTupPool)
    tryPushCtxt
    pushCtxt
    delayToken
    (useFallback: bool) 
    (fallbackToken: TokenTup) 
    (tokenTup: TokenTup) 
    addBlockEnd 
    =
    let debug = true
    let newCtxt = CtxtSeqBlock(FirstInSeqBlock, TokenTupUtils.startPosOfTokenTup tokenTup, addBlockEnd)
    let pushed = tryPushCtxt strict false tokenTup (newCtxt)
    if not pushed && useFallback then
        // The upcoming token isn't sufficiently indented to start the new context.
        // The parser expects proper contexts structure, so we push a new recovery context at the fallback token position.
        let ctxt = CtxtSeqBlock(NotFirstInSeqBlock, TokenTupUtils.startPosOfTokenTup fallbackToken, addBlockEnd)
        pushCtxt fallbackToken (ctxt)

    if pushed || useFallback then
        let addBlockBegin =
            match addBlockEnd with
            | AddBlockEnd -> true
            | _ -> false

        if addBlockBegin then
            if debug then dprintf "--> insert OBLOCKBEGIN \n"
            let ctxtToken = if pushed then tokenTup else fallbackToken
            delayToken(pool.UseLocation(ctxtToken, OBLOCKBEGIN))

let tokenFetch 
    (pool:TokenTupPool)
    peekInitial
    hwTokenFetch
    tokensThatNeedNoProcessingCount
    decrTokensThatNeedNoProcessingCount
    returnToken
    peekNextToken
    popNextTokenTup
    tokenLexbufState
    tokenStartPos
    popCtxt
    getLastTokenEndRange
    insertToken
    reprocess
    delayToken
    tokenStartCol
    useBlockRule
    pushCtxt
    pushCtxtSeqBlock
    tryPushCtxt
    replaceCtxt
    pushCtxtSeqBlockAt
    isSemiSemi
    reprocessWithoutBlockRule
    relaxWhitespace2OffsideRule
    insertTokenFromPrevPosToCurrentPos
    insertComingSoonTokens
    delayTokenNoProcessing
    replaceCtxtIgnoreIndent
    relaxWhitespace2
    isControlFlowOrNotSameLine
    isSameLine
    tryPushCtxtSeqBlock
    peekNextTokenTup
    isLongIdentEquals
    strictIndentation
    (tokenTup: TokenTup) 
    (offsideStack:Context list) 
    =

    let debug = true
    let token = tokenTup.Token
    match token, offsideStack with
    // inserted faux tokens need no other processing
    | _ when tokensThatNeedNoProcessingCount > 0 ->
        //tokensThatNeedNoProcessingCount <- tokensThatNeedNoProcessingCount - 1
        decrTokensThatNeedNoProcessingCount()
        returnToken tokenLexbufState token

    | _ when tokenForcesHeadContextClosure token offsideStack ->
        let ctxt = offsideStack.Head
        if debug then dprintf "IN/ELSE/ELIF/DONE/RPAREN/RBRACE/END/INTERP at %a terminates context at position %a\n" PositionUtils.outputPos tokenStartPos PositionUtils.outputPos ctxt.StartPos
        popCtxt()
        match ContextUtils.endTokenForACtxt getLastTokenEndRange ctxt with
        | Some tok ->
            if debug then dprintf "--> inserting %+A\n" tok
            insertToken tok
        | _ ->
            reprocess()

    // reset on ';;' rule. A ';;' terminates ALL entries
    | SEMICOLON_SEMICOLON, [] ->
        if debug then dprintf ";; scheduling a reset\n"
        delayToken(pool.UseLocation(tokenTup, ORESET))
        returnToken tokenLexbufState SEMICOLON_SEMICOLON

    | ORESET, [] ->
        if debug then dprintf "performing a reset after a ;; has been swallowed\n"

        // NOTE: The parser thread of F# Interactive will often be blocked on this call, e.g. after an entry has been
        // processed and we're waiting for the first token of the next entry.
        peekInitial() 
        |> ignore

        pool.Return tokenTup
        hwTokenFetch true

    | IN, stack when detectJoinInCtxt stack ->
        returnToken tokenLexbufState JOIN_IN

    // Balancing rule. Encountering an 'in' balances with a 'let'. i.e. even a non-offside 'in' closes a 'let'
    // The 'IN' token is thrown away and becomes an ODECLEND
    | IN, CtxtLetDecl (blockLet, offsidePos) :: _ ->
        if debug then dprintf "IN at %a (becomes %s)\n" PositionUtils.outputPos tokenStartPos (if blockLet then "ODECLEND" else "IN")
        if tokenStartCol < offsidePos.Column then TokenTupUtils.warn tokenTup (FSComp.SR.lexfltIncorrentIndentationOfIn())
        popCtxt()
        // Make sure we queue a dummy token at this position to check if any other pop rules apply
        delayToken(pool.UseLocation(tokenTup, ODUMMY token))
        returnToken tokenLexbufState (if blockLet then ODECLEND(getLastTokenEndRange ()) else token)

    // Balancing rule. Encountering a 'done' balances with a 'do'. i.e. even a non-offside 'done' closes a 'do'
    // The 'DONE' token is thrown away and becomes an ODECLEND
    | DONE, CtxtDo offsidePos :: _ ->
        if debug then dprintf "DONE at %a terminates CtxtDo(offsidePos=%a)\n" PositionUtils.outputPos tokenStartPos PositionUtils.outputPos offsidePos
        popCtxt()
        // reprocess as the DONE may close a DO context
        delayToken(pool.UseLocation(tokenTup, ODECLEND(mkSynRange tokenTup.StartPos tokenTup.EndPos)))
        pool.Return tokenTup
        hwTokenFetch useBlockRule

    // Balancing rule. Encountering a ')' or '}' balances with a '(' or '{', even if not offside
    | ((TokenRExprParen | INTERP_STRING_END _ | INTERP_STRING_PART _) as t2), (CtxtParen (t1, _) :: _)
            when TokenUtils.parenTokensBalance t1 t2 ->
        if debug then dprintf "RPAREN/RBRACE/BAR_RBRACE/RBRACK/BAR_RBRACK/RQUOTE/END at %a terminates CtxtParen()\n" PositionUtils.outputPos tokenStartPos
        popCtxt()
        match t2 with
        // $".... { ... }  ... { ....} " pushes a block context at second {
        //              ~~~~~~~~
        //                 ^---------INTERP_STRING_PART
        | INTERP_STRING_PART _ ->
            pushCtxt tokenTup (CtxtParen (token, tokenTup.LexbufState.EndPos))
            pushCtxtSeqBlock tokenTup NoAddBlockEnd
        | INTERP_STRING_END _ -> ()
        | _ ->
            // Queue a dummy token at this position to check if any closing rules apply
            delayToken(pool.UseLocation(tokenTup, ODUMMY token))

        returnToken tokenLexbufState token

    // Balancing rule. Encountering a 'end' can balance with a 'with' but only when not offside
    | END, CtxtWithAsAugment offsidePos :: _
                when not (tokenStartCol + 1 <= offsidePos.Column) ->
        if debug then dprintf "END at %a terminates CtxtWithAsAugment()\n" PositionUtils.outputPos tokenStartPos
        popCtxt()
        delayToken(pool.UseLocation(tokenTup, ODUMMY token)) // make sure we queue a dummy token at this position to check if any closing rules apply
        returnToken tokenLexbufState OEND

    //  Transition rule. CtxtNamespaceHead ~~~> CtxtSeqBlock
    //  Applied when a token other then a long identifier is seen
    | _, CtxtNamespaceHead (namespaceTokenPos, prevToken) :: _ ->
        match prevToken, token with
        | (NAMESPACE | DOT | REC | GLOBAL), (REC | IDENT _ | GLOBAL) when namespaceTokenPos.Column < tokenStartPos.Column ->
            replaceCtxt tokenTup (CtxtNamespaceHead (namespaceTokenPos, token))
            returnToken tokenLexbufState token
        | IDENT _, DOT when namespaceTokenPos.Column < tokenStartPos.Column ->
            replaceCtxt tokenTup (CtxtNamespaceHead (namespaceTokenPos, token))
            returnToken tokenLexbufState token
        | _ ->
            if debug then dprintf "CtxtNamespaceHead: pushing CtxtSeqBlock\n"
            popCtxt()
            // Don't push a new context if next token is EOF, since that raises an offside warning
            match tokenTup.Token with
            | EOF _ ->
                returnToken tokenLexbufState token
            | _ ->
                delayToken tokenTup
                pushCtxt tokenTup (CtxtNamespaceBody namespaceTokenPos)
                pushCtxtSeqBlockAt false false tokenTup tokenTup AddBlockEnd
                hwTokenFetch false

    //  Transition rule. CtxtModuleHead ~~~> push CtxtModuleBody; push CtxtSeqBlock
    //  Applied when a ':' or '=' token is seen
    //  Otherwise it's a 'head' module declaration, so ignore it

    //  Here prevToken is either 'module', 'rec', 'global' (invalid), '.', or ident, because we skip attribute tokens and access modifier tokens
    | _, CtxtModuleHead (moduleTokenPos, prevToken, lexingModuleAttributes, isNested) :: rest ->
        match prevToken, token with
        | _, GREATER_RBRACK when lexingModuleAttributes = LexingModuleAttributes
                                    && moduleTokenPos.Column < tokenStartPos.Column ->
            replaceCtxt tokenTup (CtxtModuleHead (moduleTokenPos, prevToken, NotLexingModuleAttributes, isNested))
            returnToken tokenLexbufState token
        | _ when lexingModuleAttributes = LexingModuleAttributes
                    && moduleTokenPos.Column < tokenStartPos.Column ->
            returnToken tokenLexbufState token
        | MODULE, (PUBLIC | PRIVATE | INTERNAL) when moduleTokenPos.Column < tokenStartPos.Column ->
            returnToken tokenLexbufState token
        | MODULE, GLOBAL
        | (MODULE | REC | DOT), (REC | IDENT _)
        | IDENT _, DOT when moduleTokenPos.Column < tokenStartPos.Column ->
            replaceCtxt tokenTup (CtxtModuleHead (moduleTokenPos, token, NotLexingModuleAttributes, isNested))
            returnToken tokenLexbufState token
        | MODULE, LBRACK_LESS when moduleTokenPos.Column < tokenStartPos.Column  ->
            replaceCtxt tokenTup (CtxtModuleHead (moduleTokenPos, prevToken, LexingModuleAttributes, isNested))
            returnToken tokenLexbufState token
        | _, (EQUALS | COLON) ->
            if debug then dprintf "CtxtModuleHead: COLON/EQUALS, pushing CtxtModuleBody and CtxtSeqBlock\n"
            popCtxt()
            pushCtxt tokenTup (CtxtModuleBody (moduleTokenPos, false))
            pushCtxtSeqBlock tokenTup AddBlockEnd
            returnToken tokenLexbufState token
        | _ ->
            match rest with
            | [ CtxtSeqBlock _ ] ->
                if debug then dprintf "CtxtModuleHead: start of file, CtxtSeqBlock\n"
                popCtxt()
                // Don't push a new context if next token is EOF, since that raises an offside warning
                match tokenTup.Token with
                | EOF _ ->
                    returnToken tokenLexbufState token
                | _ ->
                    // We have reached other tokens without encountering '=' or ':', so this is a module declaration spanning the whole file
                    delayToken tokenTup
                    pushCtxt tokenTup (CtxtModuleBody (moduleTokenPos, true))
                    pushCtxtSeqBlock tokenTup AddBlockEnd
                    hwTokenFetch false
            | _ ->
                // Adding a new nested module, EQUALS hasn't been typed yet
                // and we've encountered declarations below
                if debug then dprintf "CtxtModuleHead: not start of file, popping CtxtModuleHead\n"
                popCtxt()
                insertTokenFromPrevPosToCurrentPos OBLOCKSEP

    //  Offside rule for SeqBlock.
    //      f x
    //      g x
    //    ...
    | _, CtxtSeqBlock(_, offsidePos, addBlockEnd) :: rest when

            // NOTE: ;; does not terminate a 'namespace' body.
            ((isSemiSemi && not (match rest with (CtxtNamespaceBody _ | CtxtModuleBody (_, true)) :: _ -> true | _ -> false)) ||
                let grace =
                    match token, rest with
                        // When in a type context allow a grace of 2 column positions for '|' tokens, permits
                        //  type x =
                        //      A of string    <-- note missing '|' here - bad style, and perhaps should be disallowed
                        //    | B of int
                    | BAR, CtxtTypeDefns _ :: _ -> 2

                        // This ensures we close a type context seq block when the '|' marks
                        // of a type definition are aligned with the 'type' token.
                        //
                        //  type x =
                        //  | A
                        //  | B
                        //
                        //  <TOKEN>    <-- close the type context sequence block here *)
                    | _, CtxtTypeDefns(posType, _) :: _ when offsidePos.Column = posType.Column && not (TokenUtils.isTypeSeqBlockElementContinuator token) -> -1

                        // This ensures we close a namespace body when we see the next namespace definition
                        //
                        //  namespace A.B.C
                        //  ...
                        //  
                        //  namespace <-- close the namespace body context here 
                    | _, CtxtNamespaceBody posNamespace :: _ when offsidePos.Column = posNamespace.Column && (match token with NAMESPACE -> true | _ -> false) -> -1

                    | _ ->
                        // Allow a grace of >2 column positions for infix tokens, permits
                        //  let x =
                        //        expr + expr
                        //      + expr + expr
                        // And
                        //    let x =
                        //          expr
                        //       |> f expr
                        //       |> f expr
                        // Note you need a semicolon in the following situation:
                        //
                        //  let x =
                        //        stmt
                        //       -expr     <-- not allowed, as prefix token is here considered infix
                        //
                        // i.e.
                        //
                        //  let x =
                        //        stmt
                        //        -expr
                        (if TokenUtils.isInfix token then TokenUtils.infixTokenLength token + 1 else 0)
                (tokenStartCol + grace < offsidePos.Column)) ->
        if debug then dprintf "offside token at column %d indicates end of CtxtSeqBlock started at %a!\n" tokenStartCol PositionUtils.outputPos offsidePos
        popCtxt()
        if debug then (match addBlockEnd with AddBlockEnd -> dprintf "end of CtxtSeqBlock, insert OBLOCKEND \n" | _ -> ())
        match addBlockEnd with
        | AddBlockEnd -> insertToken (OBLOCKEND(getLastTokenEndRange ()))
        | AddOneSidedBlockEnd -> insertToken (ORIGHT_BLOCK_END(getLastTokenEndRange ()))
        | NoAddBlockEnd -> reprocess()

    //  Offside rule for SeqBlock.
    //    fff
    //       eeeee
    //  <tok>
    | _, CtxtVanilla(offsidePos, _) :: _ when isSemiSemi || tokenStartCol <= offsidePos.Column ->
        if debug then dprintf "offside token at column %d indicates end of CtxtVanilla started at %a!\n" tokenStartCol PositionUtils.outputPos offsidePos
        popCtxt()
        reprocess()

    //  Offside rule for SeqBlock - special case
    //  [< ... >]
    //  decl

    | _, CtxtSeqBlock(NotFirstInSeqBlock, offsidePos, addBlockEnd) :: _ 
                when (match token with GREATER_RBRACK -> true | _ -> false) -> 
        // Attribute-end tokens mean CtxtSeqBlock rule is NOT applied to the next token
        replaceCtxt tokenTup (CtxtSeqBlock (FirstInSeqBlock, offsidePos, addBlockEnd))
        reprocessWithoutBlockRule()

    //  Offside rule for SeqBlock - avoiding inserting OBLOCKSEP on first item in block
    | _, CtxtSeqBlock (FirstInSeqBlock, offsidePos, addBlockEnd) :: _ when useBlockRule ->
        // This is the first token in a block, or a token immediately
        // following an infix operator (see above).
        // Return the token, but only after processing any additional rules
        // applicable for this token. Don't apply the CtxtSeqBlock rule for
        // this token, but do apply it on subsequent tokens.
        if debug then dprintf "repull for CtxtSeqBlockStart\n"
        replaceCtxt tokenTup (CtxtSeqBlock (NotFirstInSeqBlock, offsidePos, addBlockEnd))
        reprocessWithoutBlockRule()

    //  Offside rule for SeqBlock - inserting OBLOCKSEP on subsequent items in a block when they are precisely aligned
    //
    // let f1 () =
    //    expr
    //    ...
    // ~~> insert OBLOCKSEP
    //
    // let f1 () =
    //    let x = expr
    //    ...
    // ~~> insert OBLOCKSEP
    //
    // let f1 () =
    //    let x1 = expr
    //    let x2 = expr
    //    let x3 = expr
    //    ...
    // ~~> insert OBLOCKSEP
    | _, CtxtSeqBlock (NotFirstInSeqBlock, offsidePos, addBlockEnd) :: rest
            when useBlockRule
                && not (let isTypeCtxt = (match rest with | CtxtTypeDefns _ :: _ -> true | _ -> false)
                        // Don't insert 'OBLOCKSEP' between namespace declarations
                        let isNamespaceCtxt = (match rest with | CtxtNamespaceBody _ :: _ -> true | _ -> false)
                        if isNamespaceCtxt then (match token with NAMESPACE -> true | _ -> false)
                        elif isTypeCtxt then TokenUtils.isTypeSeqBlockElementContinuator token
                        else TokenUtils.isSeqBlockElementContinuator token)
                && (tokenStartCol = offsidePos.Column)
                && (tokenStartPos.OriginalLine <> offsidePos.OriginalLine) ->
            if debug then dprintf "offside at column %d matches start of block(%a)! delaying token, returning OBLOCKSEP\n" tokenStartCol PositionUtils.outputPos offsidePos
            replaceCtxt tokenTup (CtxtSeqBlock (FirstInSeqBlock, offsidePos, addBlockEnd))
            // No change to offside stack: another statement block starts...
            insertTokenFromPrevPosToCurrentPos OBLOCKSEP

    //  Offside rule for CtxtLetDecl
    // let .... =
    //    ...
    // <and>
    //
    // let .... =
    //    ...
    // <in>
    //
    //   let .... =
    //       ...
    //  <*>
    | _, CtxtLetDecl (true, offsidePos) :: _ when
                    isSemiSemi || (if relaxWhitespace2OffsideRule || TokenUtils.isLetContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "token at column %d is offside from LET(offsidePos=%a)! delaying token, returning ODECLEND\n" tokenStartCol PositionUtils.outputPos offsidePos
        popCtxt()
        insertToken (ODECLEND(getLastTokenEndRange ()))

    // do ignore (
    //     1
    // ), 2 // This is a 'unit * int', so for backwards compatibility, do not treat ')' as a continuator, don't apply relaxWhitespace2OffsideRule
    // Test here: Tests/FSharp.Compiler.ComponentTests/Conformance/LexicalFiltering/Basic/OffsideExceptions.fs, RelaxWhitespace2_AllowedBefore9
    | _, CtxtDo offsidePos :: _
            when isSemiSemi || (if TokenUtils.isDoContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "token at column %d is offside from DO(offsidePos=%a)! delaying token, returning ODECLEND\n" tokenStartCol PositionUtils.outputPos offsidePos
        popCtxt()
        insertToken (ODECLEND(getLastTokenEndRange ()))

    // class
    //    interface AAA
    //  ...
    // ...

    | _, CtxtInterfaceHead offsidePos :: _
            when isSemiSemi || (if relaxWhitespace2OffsideRule || TokenUtils.isInterfaceContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "token at column %d is offside from INTERFACE(offsidePos=%a)! pop and reprocess\n" tokenStartCol PositionUtils.outputPos offsidePos
        popCtxt()
        reprocess()

    | _, CtxtTypeDefns(offsidePos, _) :: _
            when isSemiSemi || (if relaxWhitespace2OffsideRule || TokenUtils.isTypeContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "token at column %d is offside from TYPE(offsidePos=%a)! pop and reprocess\n" tokenStartCol PositionUtils.outputPos offsidePos
        popCtxt()
        reprocess()

    // module A.B.M
    //    ...
    // module M = ...
    // end
    //  module M = ...
    // ...
    // NOTE: ;; does not terminate a whole file module body.
    | _, CtxtModuleBody (offsidePos, wholeFile) :: _ when (isSemiSemi && not wholeFile) || (if relaxWhitespace2OffsideRule then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "token at column %d is offside from MODULE with offsidePos %a! delaying token\n" tokenStartCol PositionUtils.outputPos offsidePos
        popCtxt()
        reprocess()

    // NOTE: ;; does not terminate a 'namespace' body.
    | _, CtxtNamespaceBody offsidePos :: _ when (* isSemiSemi || *) (if relaxWhitespace2OffsideRule || TokenUtils.isNamespaceContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "token at column %d is offside from NAMESPACE with offsidePos %a! delaying token\n" tokenStartCol PositionUtils.outputPos offsidePos
        popCtxt()
        reprocess()

    | _, CtxtException offsidePos :: _ when isSemiSemi || (if relaxWhitespace2OffsideRule then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "token at column %d is offside from EXCEPTION with offsidePos %a! delaying token\n" tokenStartCol PositionUtils.outputPos offsidePos
        popCtxt()
        reprocess()

    // Pop CtxtMemberBody when offside. Insert an ODECLEND to indicate the end of the member
    //     member _.d() = seq {
    //         1
    //     }; static member e() = [
    //         1 // This is not offside for backcompat, don't apply relaxWhitespace2OffsideRule
    //     ]
    // Test here: Tests/FSharp.Compiler.ComponentTests/Conformance/LexicalFiltering/Basic/OffsideExceptions.fs, RelaxWhitespace2_AllowedBefore9
    | _, CtxtMemberBody offsidePos :: _ when isSemiSemi || (if false then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "token at column %d is offside from MEMBER/OVERRIDE head with offsidePos %a!\n" tokenStartCol PositionUtils.outputPos offsidePos
        popCtxt()
        insertToken (ODECLEND(getLastTokenEndRange ()))

    // Pop CtxtMemberHead when offside
    | _, CtxtMemberHead offsidePos :: _ when isSemiSemi || (if relaxWhitespace2OffsideRule then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "token at column %d is offside from MEMBER/OVERRIDE head with offsidePos %a!\n" tokenStartCol PositionUtils.outputPos offsidePos
        popCtxt()
        reprocess()

    | _, CtxtIf offsidePos :: _
                when isSemiSemi || (if relaxWhitespace2OffsideRule || TokenUtils.isIfBlockContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "offside from CtxtIf\n"
        popCtxt()
        reprocess()

    | _, CtxtWithAsLet offsidePos :: _
                when isSemiSemi || (if relaxWhitespace2OffsideRule || TokenUtils.isLetContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "offside from CtxtWithAsLet\n"
        popCtxt()
        insertToken OEND

    | _, CtxtWithAsAugment offsidePos :: _
                when isSemiSemi || (if relaxWhitespace2OffsideRule || TokenUtils.isWithAugmentBlockContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "offside from CtxtWithAsAugment, isWithAugmentBlockContinuator = %b\n" (TokenUtils.isWithAugmentBlockContinuator token)
        popCtxt()
        insertToken (ODECLEND(getLastTokenEndRange ()))

    | _, CtxtMatch offsidePos :: _
                when isSemiSemi || (if relaxWhitespace2OffsideRule || relaxWhitespace2 && TokenUtils.isMatchBlockContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "offside from CtxtMatch\n"
        popCtxt()
        reprocess()

    | _, CtxtFor offsidePos :: _
                when isSemiSemi || (if relaxWhitespace2OffsideRule || TokenUtils.isForLoopContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "offside from CtxtFor\n"
        popCtxt()
        reprocess()

    | _, CtxtWhile offsidePos :: _
                when isSemiSemi || (if relaxWhitespace2OffsideRule || TokenUtils.isWhileBlockContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "offside from CtxtWhile\n"
        popCtxt()
        reprocess()

    | _, CtxtWhen offsidePos :: _
                when isSemiSemi || (if relaxWhitespace2OffsideRule then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "offside from CtxtWhen\n"
        popCtxt()
        reprocess()

    | _, CtxtFun offsidePos :: _
    // fun () -> async {
    //     1
    // }, 2 // This is a '(unit -> seq<int>) * int', so for backwards compatibility, do not treat '}' as a continuator, don't apply relaxWhitespace2OffsideRule
    // Test here: Tests/FSharp.Compiler.ComponentTests/Conformance/LexicalFiltering/Basic/OffsideExceptions.fs, RelaxWhitespace2_AllowedBefore9
                when isSemiSemi || (if (*relaxWhitespace2OffsideRule*)false then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "offside from CtxtFun\n"
        popCtxt()
        insertToken OEND
    // function () -> async {
    //     1
    // }, 2 // This is a '(unit -> seq<int>) * int', so for backwards compatibility, do not treat '}' as a continuator, don't apply relaxWhitespace2OffsideRule
    // Test here: Tests/FSharp.Compiler.ComponentTests/Conformance/LexicalFiltering/Basic/OffsideExceptions.fs, RelaxWhitespace2_AllowedBefore9
    | _, CtxtFunction offsidePos :: _
                when isSemiSemi || (if (*relaxWhitespace2OffsideRule*)false then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        popCtxt()
        reprocess()

    | _, CtxtTry offsidePos :: _
                when isSemiSemi || (if relaxWhitespace2OffsideRule || TokenUtils.isTryBlockContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "offside from CtxtTry\n"
        popCtxt()
        reprocess()

    //  then
    //     ...
    //  else
    //
    //  then
    //     ...
    | _, CtxtThen offsidePos :: _ when isSemiSemi || (if relaxWhitespace2OffsideRule || TokenUtils.isThenBlockContinuator token then tokenStartCol + 1 else tokenStartCol)<= offsidePos.Column ->
        if debug then dprintf "offside from CtxtThen, popping\n"
        popCtxt()
        reprocess()

    //  else ...
    // ....
    //
    | _, CtxtElse (offsidePos) :: _ when isSemiSemi || (if relaxWhitespace2OffsideRule then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->
        if debug then dprintf "offside from CtxtElse, popping\n"
        popCtxt()
        reprocess()

    // leadingBar=false permits match patterns without an initial '|'
    | _, CtxtMatchClauses (leadingBar, offsidePos) :: _
                when (isSemiSemi ||
                    (match token with
                        // BAR occurs in pattern matching 'with' blocks
                        | BAR ->
                            let cond1 = tokenStartCol + (if leadingBar then 0 else 2) < offsidePos.Column
                            let cond2 = tokenStartCol + (if leadingBar then 1 else 2) < offsidePos.Column
                            if (cond1 <> cond2) then
                                errorR(IndentationProblem(FSComp.SR.lexfltSeparatorTokensOfPatternMatchMisaligned(), mkSynRange (TokenTupUtils.startPosOfTokenTup tokenTup) tokenTup.LexbufState.EndPos))
                            cond1
                        | END -> tokenStartCol + (if leadingBar then -1 else 1) < offsidePos.Column
                        | _ -> tokenStartCol + (if leadingBar then -1 else 1) < offsidePos.Column)) ->
        if debug then dprintf "offside from WITH, tokenStartCol = %d, offsidePos = %a, delaying token, returning OEND\n" tokenStartCol PositionUtils.outputPos offsidePos
        popCtxt()
        insertToken OEND


    //  namespace ... ~~~> CtxtNamespaceHead
    | NAMESPACE, _ :: _ ->
        if debug then dprintf "NAMESPACE: entering CtxtNamespaceHead, awaiting end of long identifier to push CtxtSeqBlock\n"
        pushCtxt tokenTup (CtxtNamespaceHead (tokenStartPos, token))
        returnToken tokenLexbufState token

    //  module ... ~~~> CtxtModuleHead
    | MODULE, _ :: _ ->
        insertComingSoonTokens("MODULE", MODULE_COMING_SOON, MODULE_IS_HERE)
        if debug then dprintf "MODULE: entering CtxtModuleHead, awaiting EQUALS to go to CtxtSeqBlock (%a)\n" PositionUtils.outputPos tokenStartPos
        let isNested = match offsideStack with | [ CtxtSeqBlock _ ] -> false | _ -> true
        pushCtxt tokenTup (CtxtModuleHead (tokenStartPos, token, NotLexingModuleAttributes, isNested))
        pool.Return tokenTup
        hwTokenFetch useBlockRule

    // exception ... ~~~> CtxtException
    | EXCEPTION, _ :: _ ->
        if debug then dprintf "EXCEPTION: entering CtxtException(%a)\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtException tokenStartPos)
        returnToken tokenLexbufState token

    //  let ... ~~~> CtxtLetDecl
    //     -- this rule only applies to
    //              - 'static let'
    | LET isUse, ctxt :: _ when (match ctxt with CtxtMemberHead _ -> true | _ -> false) ->
        if debug then dprintf "LET: entering CtxtLetDecl(), awaiting EQUALS to go to CtxtSeqBlock (%a)\n" PositionUtils.outputPos tokenStartPos
        let startPos = match ctxt with CtxtMemberHead startPos -> startPos | _ -> tokenStartPos
        popCtxt() // get rid of the CtxtMemberHead
        pushCtxt tokenTup (CtxtLetDecl(true, startPos))
        returnToken tokenLexbufState (OLET isUse)

    // let ... ~~~> CtxtLetDecl
    //     -- this rule only applies to
    //              - 'let' 'right-on' a SeqBlock line
    //              - 'let' in a CtxtMatchClauses, which is a parse error, but we need to treat as OLET to get various O...END tokens to enable parser to recover
    | LET isUse, ctxt :: _ ->
        let blockLet = match ctxt with | CtxtSeqBlock _ -> true
                                        | CtxtMatchClauses _ -> true
                                        | _ -> false
        if debug then dprintf "LET: entering CtxtLetDecl(blockLet=%b), awaiting EQUALS to go to CtxtSeqBlock (%a)\n" blockLet PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtLetDecl(blockLet, tokenStartPos))
        returnToken tokenLexbufState (if blockLet then OLET isUse else token)

    //  let!  ... ~~~> CtxtLetDecl
    | BINDER b, ctxt :: _ ->
        let blockLet = match ctxt with CtxtSeqBlock _ -> true | _ -> false
        if debug then dprintf "LET: entering CtxtLetDecl(blockLet=%b), awaiting EQUALS to go to CtxtSeqBlock (%a)\n" blockLet PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtLetDecl(blockLet, tokenStartPos))
        returnToken tokenLexbufState (if blockLet then OBINDER b else token)

    //  and!  ... ~~~> CtxtLetDecl
    | AND_BANG isUse, ctxt :: _ ->
        let blockLet = match ctxt with CtxtSeqBlock _ -> true | _ -> false
        if debug then dprintf "AND!: entering CtxtLetDecl(blockLet=%b), awaiting EQUALS to go to CtxtSeqBlock (%a)\n" blockLet PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtLetDecl(blockLet,tokenStartPos))
        returnToken tokenLexbufState (if blockLet then OAND_BANG isUse else token)

    | (VAL | STATIC | ABSTRACT | MEMBER | OVERRIDE | DEFAULT), ctxtStack when thereIsACtxtMemberBodyOnTheStackAndWeShouldPopStackForUpcomingMember ctxtStack ->
        if debug then dprintf "STATIC/MEMBER/OVERRIDE/DEFAULT: already inside CtxtMemberBody, popping all that context before starting next member...\n"
        // save this token, we'll consume it again later...
        delayTokenNoProcessing tokenTup
        // ... after we've popped all contexts and inserted END tokens
        while (match offsideStack.Head with CtxtMemberBody _ -> false | _ -> true) do
            match ContextUtils.endTokenForACtxt getLastTokenEndRange  offsideStack.Head with
            // some contexts require us to insert various END tokens
            | Some tok ->
                popCtxt()
                if debug then dprintf "--> inserting %+A\n" tok
                delayTokenNoProcessing (pool.UseLocation(tokenTup, tok))
            // for the rest, we silently pop them
            | _ -> popCtxt()
        popCtxt() // pop CtxtMemberBody
        if debug then dprintf "...STATIC/MEMBER/OVERRIDE/DEFAULT: finished popping all that context\n"
        hwTokenFetch useBlockRule

    //  static member ... ~~~> CtxtMemberHead
    //  static ... ~~~> CtxtMemberHead
    //  member ... ~~~> CtxtMemberHead
    //  override ... ~~~> CtxtMemberHead
    //  default ... ~~~> CtxtMemberHead
    //  val ... ~~~> CtxtMemberHead
    | (VAL | STATIC | ABSTRACT | MEMBER | OVERRIDE | DEFAULT), ctxt :: _ when (match ctxt with CtxtMemberHead _ -> false | _ -> true) ->
        if debug then dprintf "STATIC/MEMBER/OVERRIDE/DEFAULT: entering CtxtMemberHead, awaiting EQUALS to go to CtxtSeqBlock (%a)\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtMemberHead tokenStartPos)
        returnToken tokenLexbufState token

    //  public new... ~~~> CtxtMemberHead
    | (PUBLIC | PRIVATE | INTERNAL), _ctxt :: _ when (match peekNextToken() with NEW -> true | _ -> false) ->
        if debug then dprintf "PUBLIC/PRIVATE/INTERNAL NEW: entering CtxtMemberHead, awaiting EQUALS to go to CtxtSeqBlock (%a)\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtMemberHead tokenStartPos)
        returnToken tokenLexbufState token

    //  new( ~~~> CtxtMemberHead, if not already there because of 'public'
    | NEW, ctxt :: _ when (match peekNextToken() with LPAREN -> true | _ -> false) && (match ctxt with CtxtMemberHead _ -> false | _ -> true) ->
        if debug then dprintf "NEW: entering CtxtMemberHead, awaiting EQUALS to go to CtxtSeqBlock (%a)\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtMemberHead tokenStartPos)
        returnToken tokenLexbufState token

    //  'let ... = ' ~~~> CtxtSeqBlock
    | EQUALS, CtxtLetDecl _ :: _ ->
        if debug then dprintf "CtxtLetDecl: EQUALS, pushing CtxtSeqBlock\n"
        pushCtxtSeqBlock tokenTup AddBlockEnd
        returnToken tokenLexbufState token

    | EQUALS, CtxtTypeDefns(p, _) :: _ ->
        if debug then dprintf "CtxType: EQUALS, pushing CtxtSeqBlock\n"
        replaceCtxtIgnoreIndent tokenTup (CtxtTypeDefns(p, Some tokenTup.EndPos))
        pushCtxtSeqBlock tokenTup AddBlockEnd
        returnToken tokenLexbufState token

    | (LAZY | ASSERT), _ ->
        if isControlFlowOrNotSameLine() then
            if debug then dprintf "LAZY/ASSERT, pushing CtxtSeqBlock\n"
            pushCtxtSeqBlock tokenTup AddBlockEnd
            returnToken tokenLexbufState (match token with LAZY -> OLAZY | _ -> OASSERT)
        else
            returnToken tokenLexbufState token


    //  'with id = ' ~~~> CtxtSeqBlock
    //  'with M.id = ' ~~~> CtxtSeqBlock
    //  'with id1 = 1
    //        id2 = ...  ~~~> CtxtSeqBlock
    //  'with id1 = 1
    //        M.id2 = ...  ~~~> CtxtSeqBlock
    //  '{ id = ... ' ~~~> CtxtSeqBlock
    //  '{ M.id = ... ' ~~~> CtxtSeqBlock
    //  '{ id1 = 1
    //     id2 = ... ' ~~~> CtxtSeqBlock
    //  '{ id1 = 1
    //     M.id2 = ... ' ~~~> CtxtSeqBlock
    | EQUALS, CtxtWithAsLet _ :: _  // This detects 'with = '.
    | EQUALS, CtxtVanilla (_, true) :: CtxtSeqBlock _ :: (CtxtWithAsLet _ | CtxtParen((LBRACE _ | LBRACE_BAR), _)) :: _ ->
        if debug then dprintf "CtxtLetDecl/CtxtWithAsLet: EQUALS, pushing CtxtSeqBlock\n"
        // We don't insert begin/end block tokens for single-line bindings since we can't properly distinguish single-line *)
        // record update expressions such as "{ t with gbuckets=Array.copy t.gbuckets; gcount=t.gcount }" *)
        // These have a syntactically odd status because of the use of ";" to terminate expressions, so each *)
        // "=" binding is not properly balanced by "in" or "and" tokens in the single line syntax (unlike other bindings) *)
        if isControlFlowOrNotSameLine() then
            pushCtxtSeqBlock tokenTup AddBlockEnd
        else
            pushCtxtSeqBlock tokenTup NoAddBlockEnd
        returnToken tokenLexbufState token

    //  'new(... =' ~~~> CtxtMemberBody, CtxtSeqBlock
    //  'member ... =' ~~~> CtxtMemberBody, CtxtSeqBlock
    //  'static member ... =' ~~~> CtxtMemberBody, CtxtSeqBlock
    //  'default ... =' ~~~> CtxtMemberBody, CtxtSeqBlock
    //  'override ... =' ~~~> CtxtMemberBody, CtxtSeqBlock
    | EQUALS, CtxtMemberHead offsidePos :: _ ->
        if debug then dprintf "CtxtMemberHead: EQUALS, pushing CtxtSeqBlock\n"
        replaceCtxt tokenTup (CtxtMemberBody offsidePos)
        pushCtxtSeqBlock tokenTup AddBlockEnd
        returnToken tokenLexbufState token

    // '(' tokens are balanced with ')' tokens and also introduce a CtxtSeqBlock
    // $".... { ... }  ... { ....} " pushes a block context at first {
    // ~~~~~~~~
    //    ^---------INTERP_STRING_BEGIN_PART
    | (TokenLExprParen | SIG | INTERP_STRING_BEGIN_PART _), _ ->
        if debug then dprintf "LPAREN etc., pushes CtxtParen, pushing CtxtSeqBlock, tokenStartPos = %a\n" PositionUtils.outputPos tokenStartPos
        let pos = match token with
                    | INTERP_STRING_BEGIN_PART _ -> tokenTup.LexbufState.EndPos
                    | _ -> tokenStartPos
        pushCtxt tokenTup (CtxtParen (token, pos))
        pushCtxtSeqBlock tokenTup NoAddBlockEnd
        returnToken tokenLexbufState token

    // '(' tokens are balanced with ')' tokens and also introduce a CtxtSeqBlock
    | STRUCT, ctxts
            when (match ctxts with
                    | CtxtSeqBlock _ :: (CtxtModuleBody _ | CtxtTypeDefns _) :: _ ->
                        // type ... = struct ... end
                        // module ... = struct ... end
                    true

                    | _ -> false) (* type X<'a when 'a : struct> *) ->
        if debug then dprintf "LPAREN etc., pushes CtxtParen, pushing CtxtSeqBlock, tokenStartPos = %a\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtParen (token, tokenStartPos))
        pushCtxtSeqBlock tokenTup NoAddBlockEnd
        returnToken tokenLexbufState token

    | RARROW, ctxts
            // Only treat '->' as a starting a sequence block in certain circumstances
            when (match ctxts with
                    // comprehension/match
                    | (CtxtWhile _ | CtxtFor _ | CtxtWhen _ | CtxtMatchClauses _ | CtxtFun _) :: _ -> true
                    // comprehension
                    | CtxtSeqBlock _ :: CtxtParen ((LBRACK | LBRACE _ | LBRACE_BAR | LBRACK_BAR), _) :: _ -> true
                    // comprehension
                    | CtxtSeqBlock _ :: (CtxtDo _ | CtxtWhile _ | CtxtFor _ | CtxtWhen _ | CtxtMatchClauses _ | CtxtTry _ | CtxtThen _ | CtxtElse _) :: _ -> true
                    | _ -> false) ->
        if debug then dprintf "RARROW, pushing CtxtSeqBlock, tokenStartPos = %a\n" PositionUtils.outputPos tokenStartPos
        pushCtxtSeqBlock tokenTup AddOneSidedBlockEnd
        returnToken tokenLexbufState token

    | LARROW, _ when isControlFlowOrNotSameLine() ->
        if debug then dprintf "LARROW, pushing CtxtSeqBlock, tokenStartPos = %a\n" PositionUtils.outputPos tokenStartPos
        pushCtxtSeqBlock tokenTup AddBlockEnd
        returnToken tokenLexbufState token

    //  do  ~~> CtxtDo;CtxtSeqBlock  (unconditionally)
    | (DO | DO_BANG), _ ->
        if debug then dprintf "DO: pushing CtxtSeqBlock, tokenStartPos = %a\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtDo tokenStartPos)
        tryPushCtxtSeqBlock tokenTup AddBlockEnd
        returnToken tokenLexbufState (match token with DO -> ODO | DO_BANG -> ODO_BANG | _ -> failwith "unreachable")

    // The r.h.s. of an infix token begins a new block.
    | _, ctxts when (TokenUtils.isInfix token &&
                        not (isSameLine()) &&
                        // This doesn't apply to the use of any infix tokens in a pattern match or 'when' block
                        // For example
                        //
                        //       match x with
                        //       | _ when true &&
                        //                false ->   // the 'false' token shouldn't start a new block
                        //                let x = 1
                        //                x
                        (match ctxts with CtxtMatchClauses _ :: _ -> false | _ -> true)) ->

        if debug then dprintf "(Infix etc.), pushing CtxtSeqBlock, tokenStartPos = %a\n" PositionUtils.outputPos tokenStartPos
        pushCtxtSeqBlock tokenTup NoAddBlockEnd
        returnToken tokenLexbufState token

    | WITH, (CtxtTry _ | CtxtMatch _) :: _ ->
        let lookaheadTokenTup = peekNextTokenTup()
        let lookaheadTokenStartPos = TokenTupUtils.startPosOfTokenTup lookaheadTokenTup
        let leadingBar = match (peekNextToken()) with BAR -> true | _ -> false

        if debug then dprintf "WITH, pushing CtxtMatchClauses, lookaheadTokenStartPos = %a, tokenStartPos = %a\n" PositionUtils.outputPos lookaheadTokenStartPos PositionUtils.outputPos tokenStartPos
        tryPushCtxt strictIndentation false lookaheadTokenTup (CtxtMatchClauses(leadingBar, lookaheadTokenStartPos)) |> ignore

        returnToken tokenLexbufState OWITH

    | FINALLY, CtxtTry _ :: _ ->
        if debug then dprintf "FINALLY, pushing pushCtxtSeqBlock, tokenStartPos = %a\n" PositionUtils.outputPos tokenStartPos
        pushCtxtSeqBlock tokenTup AddBlockEnd
        returnToken tokenLexbufState token

    | WITH, (CtxtException _ | CtxtTypeDefns _ | CtxtMemberHead _ | CtxtInterfaceHead _ | CtxtMemberBody _ as limCtxt) :: _
    | WITH, (CtxtSeqBlock _ as limCtxt :: CtxtParen((LBRACE _ | LBRACE_BAR), _) :: _) ->
        let lookaheadTokenTup = peekNextTokenTup()
        let lookaheadTokenStartPos = TokenTupUtils.startPosOfTokenTup lookaheadTokenTup
        match lookaheadTokenTup.Token with
        | RBRACE _
        | IDENT _
        // The next clause detects the access annotations after the 'with' in:
        //    member  x.PublicGetSetProperty
        //                 with public get i = "Ralf"
        //                 and  private set i v = ()
        //
        //    as well as:
        //    member  x.PublicGetSetProperty
        //                 with inline get() = "Ralf"
        //                 and  [<Foo>] set v = ()
        //
        | PUBLIC | PRIVATE | INTERNAL | INLINE ->

            let offsidePos =
                if lookaheadTokenStartPos.Column > tokenTup.LexbufState.EndPos.Column then
                    // This detects:
                    //    { new Foo
                    //      with M() = 1
                    //      and  N() = 2 }
                    // and treats the inner bindings as if they were member bindings.
                    // (HOWEVER: note the above language construct is now deprecated/removed)
                    //
                    // It also happens to detect
                    //    { foo with m = 1
                    //               n = 2 }
                    // So we're careful to set the offside column to be the minimum required *)
                    tokenStartPos
                else
                    // This detects:
                    //    { foo with
                    //        m = 1
                    //        n = 2 }
                    // So we're careful to set the offside column to be the minimum required *)
                    limCtxt.StartPos
            if debug then dprintf "WITH, pushing CtxtWithAsLet, tokenStartPos = %a, lookaheadTokenStartPos = %a\n" PositionUtils.outputPos tokenStartPos PositionUtils.outputPos lookaheadTokenStartPos
            pushCtxt tokenTup (CtxtWithAsLet offsidePos)

            // Detect 'with' bindings of the form
            //
            //    with x = ...
            //
            // Which can only be part of
            //
            //   { r with x = ... }
            //
            // and in this case push a CtxtSeqBlock to cover the sequence
            let isFollowedByLongIdentEquals =
                let tokenTup = popNextTokenTup()
                let res = isLongIdentEquals tokenTup
                delayToken tokenTup
                res

            if isFollowedByLongIdentEquals then
                pushCtxtSeqBlock tokenTup NoAddBlockEnd

            returnToken tokenLexbufState OWITH
        | _ ->
            if debug then dprintf "WITH, pushing CtxtWithAsAugment and CtxtSeqBlock, tokenStartPos = %a, limCtxt = %A\n" PositionUtils.outputPos tokenStartPos limCtxt
            //
            //  For attributes on properties:
            //      member  x.PublicGetSetProperty
            //         with [<Foo>]  get() = "Ralf"
            if (match lookaheadTokenTup.Token with LBRACK_LESS -> true | _ -> false) && (lookaheadTokenStartPos.OriginalLine = tokenTup.StartPos.OriginalLine) then
                let offsidePos = tokenStartPos
                pushCtxt tokenTup (CtxtWithAsLet offsidePos)
                returnToken tokenLexbufState OWITH

            // Recovery for `interface ... with` member without further indented member implementations
            elif lookaheadTokenStartPos.Column <= limCtxt.StartCol && (match limCtxt with CtxtInterfaceHead _ -> true | _ -> false) then
                returnToken tokenLexbufState token

            else
                // In these situations
                //    interface I with
                //        ...
                //    end
                //    exception ... with
                //        ...
                //    end
                //    type ... with
                //        ...
                //    end
                //    member x.P
                //       with get() = ...
                //       and  set() = ...
                //    member x.P with
                //        get() = ...
                // The limit is "interface"/"exception"/"type"
                let offsidePos = limCtxt.StartPos

                pushCtxt tokenTup (CtxtWithAsAugment offsidePos)
                pushCtxtSeqBlock tokenTup AddBlockEnd
                returnToken tokenLexbufState token

    | WITH, stack ->
        if debug then dprintf "WITH\n"
        if debug then dprintf "WITH --> NO MATCH, pushing CtxtWithAsAugment (type augmentation), stack = %A" stack
        pushCtxt tokenTup (CtxtWithAsAugment tokenStartPos)
        tryPushCtxtSeqBlock tokenTup AddBlockEnd
        returnToken tokenLexbufState token

    | FUNCTION, _ ->
        let lookaheadTokenTup = peekNextTokenTup()
        let lookaheadTokenStartPos = TokenTupUtils.startPosOfTokenTup lookaheadTokenTup
        let leadingBar = match (peekNextToken()) with BAR -> true | _ -> false
        pushCtxt tokenTup (CtxtFunction tokenStartPos)
        pushCtxt lookaheadTokenTup (CtxtMatchClauses(leadingBar, lookaheadTokenStartPos))
        returnToken tokenLexbufState OFUNCTION

    | THEN, _ ->
        if debug then dprintf "THEN, replacing THEN with OTHEN, pushing CtxtSeqBlock;CtxtThen(%a)\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtThen tokenStartPos)
        pushCtxtSeqBlock tokenTup AddBlockEnd
        returnToken tokenLexbufState OTHEN

    | ELSE, _ ->
        let lookaheadTokenTup = peekNextTokenTup()
        let lookaheadTokenStartPos, lookaheadTokenEndPos = TokenTupUtils.posOfTokenTup lookaheadTokenTup
        match peekNextToken() with
        | IF when isSameLine() ->
            // We convert ELSE IF to ELIF since it then opens the block at the right point,
            // In particular the case
            //    if e1 then e2
            //    else if e3 then e4
            //    else if e5 then e6
            popNextTokenTup() |> pool.Return
            if debug then dprintf "ELSE IF: replacing ELSE IF with ELIF, pushing CtxtIf, CtxtVanilla(%a)\n" PositionUtils.outputPos tokenStartPos
            pushCtxt tokenTup (CtxtIf tokenStartPos)
            // Combine the original range of both tokens as the range for the ELIF keyword.
            let correctedTokenLexbufState = LexbufState(tokenStartPos, lookaheadTokenEndPos, false)
            returnToken correctedTokenLexbufState ELIF

        | _ ->
            if debug then dprintf "ELSE: replacing ELSE with OELSE, pushing CtxtSeqBlock, CtxtElse(%a)\n" PositionUtils.outputPos lookaheadTokenStartPos
            pushCtxt tokenTup (CtxtElse tokenStartPos)
            pushCtxtSeqBlock tokenTup AddBlockEnd
            returnToken tokenLexbufState OELSE

    | (ELIF | IF), _ ->
        if debug then dprintf "IF, pushing CtxtIf(%a)\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtIf tokenStartPos)
        returnToken tokenLexbufState token

    | (MATCH | MATCH_BANG), _ ->
        if debug then dprintf "MATCH, pushing CtxtMatch(%a)\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtMatch tokenStartPos)
        returnToken tokenLexbufState token

    | FOR, _ ->
        if debug then dprintf "FOR, pushing CtxtFor(%a)\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtFor tokenStartPos)
        returnToken tokenLexbufState token

    | (WHILE | WHILE_BANG), _ ->
        if debug then dprintf "WHILE, pushing CtxtWhile(%a)\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtWhile tokenStartPos)
        returnToken tokenLexbufState token

    | WHEN, CtxtSeqBlock _ :: _ ->
        if debug then dprintf "WHEN, pushing CtxtWhen(%a)\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtWhen tokenStartPos)
        returnToken tokenLexbufState token

    | FUN, _ ->
        if debug then dprintf "FUN, pushing CtxtFun(%a)\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtFun tokenStartPos)
        returnToken tokenLexbufState OFUN

    // type I = interface .... end
    | INTERFACE, CtxtSeqBlock _ :: CtxtTypeDefns(typePos, Some equalsEndPos) :: _ when
            (tokenTup.LastTokenPos = equalsEndPos &&

                // Allow deindenting interface representation when started after `=`:
                //
                // type I = interface
                //   abstract P: int
                // end

                let allowDeindent = tokenTup.EndPos.Line = equalsEndPos.Line

                let lookaheadTokenTup = peekNextTokenTup ()
                let lookaheadTokenStartPos = TokenTupUtils.startPosOfTokenTup lookaheadTokenTup
                match lookaheadTokenTup.Token with
                | END ->
                    lookaheadTokenStartPos.Column >= typePos.Column

                | DEFAULT | OVERRIDE | INTERFACE | NEW | TYPE | STATIC | MEMBER | ABSTRACT | INHERIT | LBRACK_LESS ->
                    let limitPos = if allowDeindent then typePos else tokenTup.StartPos
                    lookaheadTokenStartPos.Column >= limitPos.Column + 1

                | _ ->
                    false) ->

        if debug then dprintf "INTERFACE, pushing CtxtParen, tokenStartPos = %a\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtParen (token, tokenStartPos))
        pushCtxtSeqBlock tokenTup AddBlockEnd
        returnToken tokenLexbufState token

    // type C with interface .... with
    // type C = interface .... with
    | INTERFACE, _ ->
        if debug then dprintf "INTERFACE, pushing CtxtInterfaceHead, tokenStartPos = %a, lookaheadTokenStartPos \n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtInterfaceHead tokenStartPos)
        returnToken tokenLexbufState OINTERFACE_MEMBER

    | CLASS, _ ->
        if debug then dprintf "CLASS, pushing CtxtParen(%a)\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtParen (token, tokenStartPos))
        pushCtxtSeqBlock tokenTup AddBlockEnd
        returnToken tokenLexbufState token

    | TYPE, _ ->
        insertComingSoonTokens("TYPE", TYPE_COMING_SOON, TYPE_IS_HERE)
        if debug then dprintf "TYPE, pushing CtxtTypeDefns(%a)\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtTypeDefns(tokenStartPos, None))
        pool.Return tokenTup
        hwTokenFetch useBlockRule

    | TRY, _ ->
        if debug then dprintf "Try, pushing CtxtTry(%a)\n" PositionUtils.outputPos tokenStartPos
        pushCtxt tokenTup (CtxtTry tokenStartPos)
        // The ideal spec would be to push a begin/end block pair here, but we can only do that
        // if we are able to balance the WITH with the TRY. We can't do that because of the numerous ways
        // WITH is used in the grammar (see what happens when we hit a WITH below.
        // This hits in the single line case: "try make ef1 t with _ -> make ef2 t".

        pushCtxtSeqBlock tokenTup AddOneSidedBlockEnd
        returnToken tokenLexbufState token

    | OBLOCKBEGIN, _ ->
        returnToken tokenLexbufState token

    | ODUMMY _, _ ->
        if debug then dprintf "skipping dummy token as no offside rules apply\n"
        pool.Return tokenTup
        hwTokenFetch useBlockRule

    // Ordinary tokens start a vanilla block
    | _, CtxtSeqBlock _ :: _ ->
        pushCtxt tokenTup (CtxtVanilla(tokenStartPos, isLongIdentEquals tokenTup))
        if debug then dprintf "pushing CtxtVanilla at tokenStartPos = %a\n" PositionUtils.outputPos tokenStartPos
        returnToken tokenLexbufState token

    | _ ->
        returnToken tokenLexbufState token

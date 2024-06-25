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


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


//----------------------------------------------------------------------------
// build a LexFilter
//--------------------------------------------------------------------------*)
type LexFilterImpl (
    indentationSyntaxStatus: IndentationAwareSyntaxStatus,
    compilingFSharpCore: bool,
    lexer: Lexbuf -> token,
    lexbuf: Lexbuf,
    debug: bool
) =

    //----------------------------------------------------------------------------
    // Part I. Building a new lex stream from an old
    //
    // A lexbuf is a stateful object that can be enticed to emit tokens by calling
    // 'lexer' functions designed to work with the lexbuf. Here we fake a new stream
    // coming out of an existing lexbuf. Ideally lexbufs would be abstract interfaces
    // and we could just build a new abstract interface that wraps an existing one.
    // However that is not how F# lexbufs currently work.
    //
    // Part of the fakery we perform involves buffering a lookahead token which
    // we eventually pass on to the client. However, this client also looks at
    // other aspects of the 'state' of lexbuf directly, e.g. F# lexbufs have a triple
    //    (start-pos, end-pos, eof-reached)
    //
    // You may ask why the F# parser reads this lexbuf state directly. Well, the
    // pars.fsy code itself it doesn't, but the parser engines (prim-parsing.fs)
    // certainly do for F#. e.g. when these parsers read a token
    // from the lexstream they also read the position information and keep this
    // a related stack.
    //
    // Anyway, this explains the functions getLexbufState(), setLexbufState() etc.
    //--------------------------------------------------------------------------

    //----------------------------------------------------------------------------
    // TokenTup pool
    //--------------------------------------------------------------------------

    let pool = TokenTupPool()

    //----------------------------------------------------------------------------
    // Part II. The state of the new lex stream object.
    //--------------------------------------------------------------------------

    // Ok, we're going to the wrapped lexbuf. Set the lexstate back so that the lexbuf
    // appears consistent and correct for the wrapped lexer function.
    let mutable savedLexbufState = Unchecked.defaultof<LexbufState>
    let mutable haveLexbufState = false

    let runWrappedLexerInConsistentLexbufState() =
        let state = if haveLexbufState then savedLexbufState else LexbufStateUtils.getLexbufState(lexbuf)
        LexbufStateUtils.setLexbufState state lexbuf
        let lastTokenEnd = state.EndPos
        let token = lexer lexbuf

        LexbufLocalXmlDocStore.AddGrabPoint(lexbuf)

        // Now we've got the token, remember the lexbuf state, associating it with the token
        // and remembering it as the last observed lexbuf state for the wrapped lexer function.
        let tokenLexbufState = LexbufStateUtils.getLexbufState(lexbuf)
        savedLexbufState <- tokenLexbufState
        haveLexbufState <- true

        let tokenTup = pool.Rent()
        tokenTup.Token <- token
        tokenTup.LexbufState <- tokenLexbufState
        tokenTup.LastTokenPos <- lastTokenEnd
        tokenTup

    //----------------------------------------------------------------------------
    // Fetch a raw token, either from the old lexer or from our delayedStack
    //--------------------------------------------------------------------------

    let delayedStack = Stack<TokenTup>()
    let mutable tokensThatNeedNoProcessingCount = 0

    let delayToken (tokenTup:TokenTup) = delayedStack.Push tokenTup
    let delayTokenNoProcessing (tokenTup:TokenTup) = 
        delayToken tokenTup
        tokensThatNeedNoProcessingCount <- tokensThatNeedNoProcessingCount + 1

    let popNextTokenTup() =
        if delayedStack.Count > 0 then
            let tokenTup = delayedStack.Pop()
            if debug then dprintf "popNextTokenTup: delayed token, tokenStartPos = %a\n" PositionUtils.outputPos (TokenTupUtils.startPosOfTokenTup tokenTup)
            tokenTup
        else
            if debug then dprintf "popNextTokenTup: no delayed tokens, running lexer...\n"
            runWrappedLexerInConsistentLexbufState()


    //----------------------------------------------------------------------------
    // Part III. Initial configuration of state.
    //
    // We read a token. In F# Interactive the parser thread will be correctly blocking
    // here.
    //--------------------------------------------------------------------------

    let mutable initialized = false
    let mutable offsideStack: Context list = []
    let mutable prevWasAtomicEnd = false

    let getOffsideStack () = offsideStack
    let setOffsideStack vl = offsideStack<-vl

    let peekInitial() =
        // Forget the lexbuf state we might have saved from previous input
        haveLexbufState <- false
        let initialLookaheadTokenTup = popNextTokenTup()
        if debug then dprintf "first token: initialLookaheadTokenLexbufState = %a\n" PositionUtils.outputPos (TokenTupUtils.startPosOfTokenTup initialLookaheadTokenTup)

        delayToken initialLookaheadTokenTup
        initialized <- true
        offsideStack <- 
            let rootCtxt = 
                initialLookaheadTokenTup
                |> TokenTupUtils.startPosOfTokenTup
                |> ContextUtils.rootCtxt 
            rootCtxt :: offsideStack
        initialLookaheadTokenTup

    //----------------------------------------------------------------------------
    // Part IV. Helper functions for pushing contexts and giving good warnings
    // if a context is undented.
    //
    // Undentation rules
    //--------------------------------------------------------------------------

    let relaxWhitespace2 = lexbuf.SupportsFeature LanguageFeature.RelaxWhitespace2

    let strictIndentation =
        lexbuf.StrictIndentation 
        |> Option.defaultWith (fun _ -> lexbuf.SupportsFeature LanguageFeature.StrictIndentation)

    //let indexerNotationWithoutDot = lexbuf.SupportsFeature LanguageFeature.IndexerNotationWithoutDot
    let relaxWhitespace = lexbuf.SupportsFeature LanguageFeature.RelaxWhitespace

    let undentationLimit = OffsideStack.undentationLimit relaxWhitespace relaxWhitespace2

    let tryPushCtxt (strict:bool) (ignoreIndent:bool) (tokenTup: TokenTup) (newCtxt: Context) =
        let isCorrectIndent =
            if ignoreIndent then true else
            let p1 = undentationLimit newCtxt offsideStack
            ContextUtils.isCorrectIndent p1 newCtxt

        if strict && not isCorrectIndent then false else

        let newOffsideStack = newCtxt :: offsideStack
        if debug then dprintf "--> pushing, stack = %A\n" newOffsideStack
        offsideStack <- newOffsideStack
        true

    let pushCtxt (tokenTup: TokenTup) (newCtxt: Context) =
        tryPushCtxt false false tokenTup newCtxt 
        |> ignore

    let popCtxt() = OffsideStack.popCtxt debug relaxWhitespace2 getOffsideStack setOffsideStack

    let replaceCtxt (p: TokenTup) (ctxt: Context) = 
        popCtxt()
        pushCtxt p ctxt

    let replaceCtxtIgnoreIndent (p: TokenTup) (ctxt: Context) =
        popCtxt()
        tryPushCtxt false true p ctxt 
        |> ignore

    //----------------------------------------------------------------------------
    // Peek ahead at a token, either from the old lexer or from our delayedStack
    //--------------------------------------------------------------------------

    let peekNextTokenTup() =
        let tokenTup = popNextTokenTup()
        delayToken tokenTup
        tokenTup

    let peekNextToken() =
        peekNextTokenTup().Token

     //----------------------------------------------------------------------------
     // Adjacency precedence rule
     //--------------------------------------------------------------------------

    let nextTokenIsAdjacent (firstTokenTup: TokenTup) =
        let lookaheadTokenTup = peekNextTokenTup()
        TokenTupUtils.isAdjacent firstTokenTup lookaheadTokenTup

    let nextTokenIsAdjacentLBrack (tokenTup: TokenTup) =
        let lookaheadTokenTup = peekNextTokenTup()
        TokenTupUtils.isAdjacentLBrack tokenTup lookaheadTokenTup

    let nextTokenIsAdjacentLParen (tokenTup: TokenTup) =
        let lookaheadTokenTup = peekNextTokenTup()
        TokenTupUtils.nextTokenIsAdjacentLParen tokenTup lookaheadTokenTup

    let peekAdjacentTypars (indentation:bool) (tokenTup: TokenTup) =
        OffsideStack.peekAdjacentTypars 
            (peekNextTokenTup: unit -> TokenTup)
            (popNextTokenTup: unit -> TokenTup)
            (nextTokenIsAdjacentLParen: TokenTup -> bool)
            (delayToken: TokenTup -> unit)
            (pool:TokenTupPool)
            (indentation:bool)
            (tokenTup: TokenTup) 

    //----------------------------------------------------------------------------
    // End actions
    //--------------------------------------------------------------------------

    let returnToken (tokenLexbufState: LexbufState) (tok:token) =
        LexbufStateUtils.setLexbufState tokenLexbufState lexbuf
        prevWasAtomicEnd <- TokenUtils.isAtomicExprEndToken tok
        tok

    //----------------------------------------------------------------------------
    // Parse and transform the stream of tokens coming from popNextTokenTup, pushing
    // contexts where needed, popping them where things are offside, balancing
    // parentheses and other constructs.
    //--------------------------------------------------------------------------

    let insertHighPrecedenceApp (tokenTup: TokenTup) =
        OffsideStack.insertHighPrecedenceApp 
            (pool:TokenTupPool)
            (peekNextTokenTup: unit -> TokenTup)
            nextTokenIsAdjacentLParen
            nextTokenIsAdjacentLBrack
            delayToken
            (tokenTup: TokenTup)

    let rulesForBothSoftWhiteAndHardWhite(tokenTup: TokenTup) =
        OffsideStack.rulesForBothSoftWhiteAndHardWhite
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

    let pushCtxtSeqBlockAt strict (useFallback: bool) (fallbackToken: TokenTup) (tokenTup: TokenTup) addBlockEnd =
        OffsideStack.pushCtxtSeqBlockAt 
            (strict:bool) 
            (pool:TokenTupPool)
            tryPushCtxt
            pushCtxt
            delayToken
            (useFallback: bool) 
            (fallbackToken: TokenTup) 
            (tokenTup: TokenTup) 
            addBlockEnd 

    let pushCtxtSeqBlock fallbackToken addBlockEnd =
        pushCtxtSeqBlockAt strictIndentation true fallbackToken (peekNextTokenTup ()) addBlockEnd

    let tryPushCtxtSeqBlock fallbackToken addBlockEnd =
        pushCtxtSeqBlockAt strictIndentation false fallbackToken (peekNextTokenTup ()) addBlockEnd

    let rec hwTokenFetch (useBlockRule:bool) =
        let tokenTup = popNextTokenTup()
        let tokenReplaced = rulesForBothSoftWhiteAndHardWhite tokenTup

        if tokenReplaced then hwTokenFetch useBlockRule else

        let tokenStartPos = TokenTupUtils.startPosOfTokenTup tokenTup
        let token = tokenTup.Token
        let tokenLexbufState = tokenTup.LexbufState
        let tokenStartCol = tokenStartPos.Column

        //最后一个token和peektoken是否在同一行
        let isSameLine() = 
            TokenTupUtils.isSameLine tokenTup (peekNextTokenTup())

        //最后一个token和peektoken是否
        let isControlFlowOrNotSameLine() = 
            TokenTupUtils.isControlFlowOrNotSameLine tokenTup (peekNextTokenTup()) 

        // Look for '=' or '.Id.id.id = ' after an identifier
        let isLongIdentEquals = 
            TokenTupUtils.isLongIdentEquals popNextTokenTup delayToken

        let reprocess() =
            delayToken tokenTup
            hwTokenFetch useBlockRule

        let reprocessWithoutBlockRule() =
            delayToken tokenTup
            hwTokenFetch false

        // 零长度范围
        let getLastTokenEndRange () =
            let lastTokenPos = tokenTup.LastTokenPos
            mkSynRange lastTokenPos lastTokenPos

        let insertTokenFromPrevPosToCurrentPos (tok:token) =
            delayToken tokenTup
            if debug then dprintf "inserting %+A\n" tok
            // span of inserted token lasts from the col + 1 of the prev token
            // to the beginning of current token
            let lastTokenPos =
                let pos = tokenTup.LastTokenPos
                pos.ShiftColumnBy 1
            returnToken (LexbufStateUtils.lexbufStateForInsertedDummyTokens (lastTokenPos, tokenTup.LexbufState.StartPos)) tok

        let insertToken (tok:token) =
            delayToken tokenTup
            if debug then dprintf "inserting %+A\n" tok
            returnToken (LexbufStateUtils.lexbufStateForInsertedDummyTokens (TokenTupUtils.startPosOfTokenTup tokenTup, tokenTup.LexbufState.EndPos)) tok

        let isSemiSemi = 
            TokenUtils.isSemiSemi token

        let relaxWhitespace2OffsideRule = 
            TokenUtils.relaxWhitespace2OffsideRule relaxWhitespace2 token

        let insertComingSoonTokens(keywordName:string, comingSoon:token, isHere:token) =
            OffsideStack.insertComingSoonTokens
                compilingFSharpCore
                (pool:TokenTupPool)
                (offsideStack:Context list)
                tokenStartPos
                popCtxt
                delayTokenNoProcessing
                getLastTokenEndRange
                (tokenTup: TokenTup)
                (keywordName, comingSoon, isHere)

        let returnToken (tokenLexbufState:LexbufState) (token:token) =
            pool.Return tokenTup
            returnToken tokenLexbufState token

        let decrTokensThatNeedNoProcessingCount () =
            tokensThatNeedNoProcessingCount <- tokensThatNeedNoProcessingCount - 1

        OffsideStack.tokenFetch 
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

    let rec swTokenFetch() =
        let tokenTup = popNextTokenTup()
        let tokenReplaced = rulesForBothSoftWhiteAndHardWhite tokenTup

        if tokenReplaced then 
            swTokenFetch()
        else
            let lexbufState = tokenTup.LexbufState
            let tok = tokenTup.Token
            pool.Return tokenTup
            returnToken lexbufState tok

    //----------------------------------------------------------------------------
    // Part VI. Publish the new lexer function.
    //--------------------------------------------------------------------------

    member _.LexBuffer = lexbuf

    member _.GetToken() =
        if not initialized then
            let _firstTokenTup = peekInitial()
            ()

        if indentationSyntaxStatus.Status then 
            hwTokenFetch true
        else 
            swTokenFetch()



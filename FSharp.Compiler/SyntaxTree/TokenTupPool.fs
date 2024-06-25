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

type TokenTupPool() =

    /// Arbitrary.
    /// When parsing the compiler's source files, the pool didn't come close to reaching this limit.
    /// Therefore, this seems like a reasonable limit to handle 99% of cases.
    [<Literal>]
    let maxSize = 100

    let mutable currentPoolSize = 0
    let stack:Stack<TokenTup> = Stack(10)

    /// stack.Pop()
    member pool.Rent() =
        if stack.Count = 0 then
            if currentPoolSize < maxSize then
                stack.Push(TokenTup(Unchecked.defaultof<_>, Unchecked.defaultof<_>, Unchecked.defaultof<_>))
                currentPoolSize <- currentPoolSize + 1
                pool.Rent()
            else
                assert false
                TokenTup(Unchecked.defaultof<_>, Unchecked.defaultof<_>, Unchecked.defaultof<_>)
        else
            stack.Pop()

    /// stack.Push x
    member _.Return(x: TokenTup) =
        x.Token <- Unchecked.defaultof<_>
        x.LexbufState <- Unchecked.defaultof<_>
        x.LastTokenPos <- Unchecked.defaultof<_>
        if stack.Count >= maxSize then
            assert false
        else
            stack.Push x

    /// Returns a token 'tok' with the same position as this token
    member pool.UseLocation(x: TokenTup, tok:token) =
        let tokState = x.LexbufState
        let tokTup = pool.Rent()
        tokTup.Token <- tok
        tokTup.LexbufState <- LexbufState(tokState.StartPos, tokState.EndPos, false)
        tokTup.LastTokenPos <- x.LastTokenPos
        tokTup

    /// Returns a token 'tok' with the same position as this token, except that
    /// it is shifted by specified number of characters from the left and from the right
    /// Note: positive value means shift to the right in both cases
    member pool.UseShiftedLocation(x: TokenTup, tok:token, shiftLeft:int, shiftRight:int) =
        let tokState = x.LexbufState
        let tokTup = pool.Rent()
        tokTup.Token <- tok
        tokTup.LexbufState <- LexbufState(
            tokState.StartPos.ShiftColumnBy shiftLeft, 
            tokState.EndPos.ShiftColumnBy shiftRight, false)
        tokTup.LastTokenPos <- x.LastTokenPos
        tokTup


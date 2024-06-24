// Copyright (c) Microsoft Corporation. All Rights Reserved. See License.txt in the project root for license information.

/// LexFilter - process the token stream prior to parsing.
/// Implements the offside rule and a couple of other lexical transformations.
namespace FSharp.Compiler

open Internal.Utilities.Text.Lexing
open FSharp.Compiler.Lexhelp
open FSharp.Compiler.Parser


/// A stateful filter over the token stream that adjusts it for indentation-aware syntax rules
/// Process the token stream prior to parsing. Implements the offside rule and other lexical transformations.
type LexFilter =

    /// Create a lex filter
    new:
        indentationSyntaxStatus: IndentationAwareSyntaxStatus *
        compilingFSharpCore: bool *
        lexer: (LexBuffer<char> -> token) *
        lexbuf: LexBuffer<char> *
        debug: bool ->
            LexFilter

    /// The LexBuffer associated with the filter
    member LexBuffer: LexBuffer<char>

    /// Get the next token
    member GetToken: unit -> token

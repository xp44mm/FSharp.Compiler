// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler

module  SR =
    val GetString: string -> string

module  DiagnosticMessage =
    type ResourceString<'T> =
        new: string * Printf.StringFormat<'T> -> ResourceString<'T>
        member Format: 'T

    val DeclareResourceString: string * Printf.StringFormat<'T> -> ResourceString<'T>

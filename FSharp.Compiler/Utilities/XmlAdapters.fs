// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module  Internal.Utilities.XmlAdapters

//Replacement for: System.Security.SecurityElement.Escape(line) All platforms
let s_escapeChars = [| '<'; '>'; '\"'; '\''; '&' |]

let getEscapeSequence c =
    match c with
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '\"' -> "&quot;"
    | '\'' -> "&apos;"
    | '&' -> "&amp;"
    | ch -> ch.ToString()

let escape str = String.collect getEscapeSequence str

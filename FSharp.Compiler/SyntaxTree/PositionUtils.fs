module FSharp.Compiler.PositionUtils

open Internal.Utilities.Text.Lexing

let stringOfPos (pos: Position) = sprintf "(%d:%d)" pos.OriginalLine pos.Column

let outputPos os (pos: Position) = Printf.fprintf os "(%d:%d)" pos.OriginalLine pos.Column

/// Used for warning strings, which should display columns as 1-based and display
/// the lines after taking '# line' directives into account (i.e. do not use
/// p.OriginalLine)
let warningStringOfPosition (pos: Position) = ParseHelpers.warningStringOfCoords pos.Line pos.Column


// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

// This file is compiled twice in the codebase
//    - as the internal implementation of printf '%A' formatting
//    - as the internal implementation of structured formatting in FSharp.Compiler.Service/Private.dll
//           defines: COMPILER
//           NOTE: this implementation is used by fsi.exe.
//
// The one implementation file is used to keep the implementations of
// structured formatting the same for fsi.exe and '%A' printing. However fsi.exe may have
// a richer feature set.
//
// Note no layout objects are ever transferred between the above implementations.

#if COMPILER
namespace FSharp.Compiler.Text
#else
namespace Microsoft.FSharp.Text.StructuredPrintfImpl
#endif

open System
open System.IO
open Microsoft.FSharp.Core
open Microsoft.FSharp.Collections

#if COMPILER

/// Data representing joints in structured layouts of terms.  The representation
/// of this data type is only for the consumption of formatting engines.
[<StructuralEquality; NoComparison>]
type  Joint =
    | Unbreakable
    | Breakable of indentation: int
    | Broken of indentation: int

/// Represents the tag of some tagged text
[<StructuralEquality; NoComparison; RequireQualifiedAccess>]
type TextTag =
    | ActivePatternCase
    | ActivePatternResult
    | Alias
    | Class
    | Union
    | UnionCase
    | Delegate
    | Enum
    | Event
    | Field
    | Interface
    | Keyword
    | LineBreak
    | Local
    | Record
    | RecordField
    | Method
    | Member
    | ModuleBinding
    | Function
    | Module
    | Namespace
    | NumericLiteral
    | Operator
    | Parameter
    | Property
    | Space
    | StringLiteral
    | Struct
    | TypeParameter
    | Text
    | Punctuation
    | UnknownType
    | UnknownEntity

/// Represents text with a tag
type  TaggedText =
    /// Creates text with a tag
    new: tag: TextTag * text: string -> TaggedText

    /// Gets the tag
    member Tag: TextTag

    /// Gets the text
    member Text: string

type  TaggedTextWriter =
    abstract Write: t: TaggedText -> unit
    abstract WriteLine: unit -> unit

/// Data representing structured layouts of terms.
[<NoEquality; NoComparison>]
type  Layout =
    | ObjLeaf of juxtLeft: bool * object: obj * juxtRight: bool
    | Leaf of juxtLeft: bool * text: TaggedText * juxtRight: bool
    | Node of leftLayout: Layout * rightLayout: Layout * joint: Joint
    | Attr of text: string * attributes: (string * string) list * layout: Layout

    static member JuxtapositionMiddle: left: Layout * right: Layout -> bool

#else
/// Data representing structured layouts of terms.
type  Layout

type  TextTag

[<Class>]
type  TaggedText =
    member Tag: TextTag
    member Text: string
#endif

#if COMPILER
module  TaggedText =
#else
module  TaggedText =
#endif
    val tagText: string -> TaggedText
    val tagClass: string -> TaggedText
    val  tagField: string -> TaggedText
    val  tagKeyword: string -> TaggedText
    val  tagLocal: string -> TaggedText
    val  tagProperty: string -> TaggedText
    val  tagMethod: string -> TaggedText
    val  tagUnionCase: string -> TaggedText

    val comma: TaggedText

#if COMPILER
    val tagNamespace: string -> TaggedText
    val tagParameter: string -> TaggedText
    val tagSpace: string -> TaggedText

    // common tagged literals
    val dot: TaggedText
    val colon: TaggedText
    val minus: TaggedText
    val lineBreak: TaggedText
    val space: TaggedText

    val  mkTag: TextTag -> string -> TaggedText
    val  keywordFunctions: Set<string>
    val  tagAlias: string -> TaggedText
    val  tagDelegate: string -> TaggedText
    val  tagEnum: string -> TaggedText
    val  tagEvent: string -> TaggedText
    val  tagInterface: string -> TaggedText
    val  tagLineBreak: string -> TaggedText
    val  tagModuleBinding: string -> TaggedText
    val  tagFunction: string -> TaggedText
    val  tagRecord: string -> TaggedText
    val  tagRecordField: string -> TaggedText
    val  tagModule: string -> TaggedText
    val  tagNumericLiteral: string -> TaggedText
    val  tagOperator: string -> TaggedText
    val  tagStringLiteral: string -> TaggedText
    val  tagStruct: string -> TaggedText
    val  tagTypeParameter: string -> TaggedText
    val  tagPunctuation: string -> TaggedText
    val  tagActivePatternCase: string -> TaggedText
    val  tagActivePatternResult: string -> TaggedText
    val  tagUnion: string -> TaggedText
    val  tagMember: string -> TaggedText
    val  tagUnknownEntity: string -> TaggedText
    val  tagUnknownType: string -> TaggedText

    val  leftAngle: TaggedText
    val  rightAngle: TaggedText
    val  keywordTrue: TaggedText
    val  keywordFalse: TaggedText
    val  semicolon: TaggedText
    val  leftParen: TaggedText
    val  rightParen: TaggedText
    val  leftBracket: TaggedText
    val  rightBracket: TaggedText
    val  leftBrace: TaggedText
    val  rightBrace: TaggedText
    val  leftBraceBar: TaggedText
    val  rightBraceBar: TaggedText
    val  equals: TaggedText
    val  arrow: TaggedText
    val  questionMark: TaggedText
    val  structUnit: TaggedText
    val  keywordStatic: TaggedText
    val  keywordMember: TaggedText
    val  keywordVal: TaggedText
    val  keywordEvent: TaggedText
    val  keywordWith: TaggedText
    val  keywordSet: TaggedText
    val  keywordGet: TaggedText
    val  bar: TaggedText
    val  keywordStruct: TaggedText
    val  keywordClass: TaggedText
    val  keywordInterface: TaggedText
    val  keywordInherit: TaggedText
    val  keywordBegin: TaggedText
    val  keywordEnd: TaggedText
    val  keywordNested: TaggedText
    val  keywordType: TaggedText
    val  keywordDelegate: TaggedText
    val  keywordOf: TaggedText
    val  keywordInternal: TaggedText
    val  keywordPrivate: TaggedText
    val  keywordAbstract: TaggedText
    val  keywordOverride: TaggedText
    val  keywordEnum: TaggedText
    val  leftBracketBar: TaggedText
    val  rightBracketBar: TaggedText
    val  keywordTypeof: TaggedText
    val  keywordTypedefof: TaggedText
    val  leftBracketAngle: TaggedText
    val  rightBracketAngle: TaggedText
    val  star: TaggedText
    val  keywordNew: TaggedText
    val  keywordInline: TaggedText
    val  keywordModule: TaggedText
    val  keywordNamespace: TaggedText
    val  punctuationUnit: TaggedText

type  IEnvironment =
    /// Return to the layout-generation
    /// environment to layout any otherwise uninterpreted object
    abstract GetLayout: obj -> Layout

    /// The maximum number of elements for which to generate layout for
    /// list-like structures, or columns in table-like
    /// structures.  -1 if no maximum.
    abstract MaxColumns: int

    /// The maximum number of rows for which to generate layout for table-like
    /// structures.  -1 if no maximum.
    abstract MaxRows: int
#endif

/// A layout is a sequence of strings which have been joined together.
/// The strings are classified as words, separators and left and right parenthesis.
/// This classification determines where spaces are inserted.
/// A joint is either unbreakable, breakable or broken.
/// If a joint is broken the RHS layout occurs on the next line with optional indentation.
/// A layout can be squashed to for given width which forces breaks as required.
module  Layout =
    /// The empty layout
    val emptyL: Layout

    /// Is it the empty layout?
    val isEmptyL: layout: Layout -> bool

#if COMPILER
    /// Check if the last character in the layout is the given character
    val endsWithL: text: string -> layout: Layout -> bool
#endif

    /// An uninterpreted leaf, to be interpreted into a string
    /// by the layout engine. This allows leaf layouts for numbers, strings and
    /// other atoms to be customized according to culture.
    val objL: value: obj -> Layout

    /// An string leaf
    val wordL: text: TaggedText -> Layout

    /// An string which requires no spaces either side.
    val sepL: text: TaggedText -> Layout

    /// An string which is right parenthesis (no space on the left).
    val rightL: text: TaggedText -> Layout

    /// An string which is left  parenthesis (no space on the right).
    val leftL: text: TaggedText -> Layout

    /// Join, unbreakable.
    val (^^): layout1: Layout -> layout2: Layout -> Layout

    /// Join, possible break with indent=0
    val (++): layout1: Layout -> layout2: Layout -> Layout

    /// Join, possible break with indent=1
    val (--): layout1: Layout -> layout2: Layout -> Layout

    /// Join, possible break with indent=2
    val (---): layout1: Layout -> layout2: Layout -> Layout

    /// optional break, indent=3
    val  (----): Layout -> Layout -> Layout

    /// optional break, indent=4
    val  (-----): Layout -> Layout -> Layout

    /// Join broken with ident=0
    val (@@): layout1: Layout -> layout2: Layout -> Layout

    /// Join broken with ident=1
    val (@@-): layout1: Layout -> layout2: Layout -> Layout

    /// Join broken with ident=2
    val (@@--): layout1: Layout -> layout2: Layout -> Layout

    /// Join broken with ident=3
    val (@@---): layout1: Layout -> layout2: Layout -> Layout

    /// Join broken with ident=4
    val (@@----): layout1: Layout -> layout2: Layout -> Layout

    /// Join layouts into a comma separated list.
    val commaListL: layouts: Layout list -> Layout

    /// Join layouts into a space separated list.
    val spaceListL: layouts: Layout list -> Layout

    /// Join layouts into a semi-colon separated list.
    val semiListL: layouts: Layout list -> Layout

    /// Join layouts into a list separated using the given Layout.
    val sepListL: layout1: Layout -> layouts: Layout list -> Layout

    /// Wrap round brackets around Layout.
    val bracketL: layout: Layout -> Layout

    /// Wrap square brackets around layout.
    val squareBracketL: layout: Layout -> Layout

    /// Wrap braces around layout.
    val braceL: layout: Layout -> Layout

    /// Form tuple of layouts.
    val tupleL: layouts: Layout list -> Layout

    /// Layout two vertically.
    val aboveL: layout1: Layout -> layout2: Layout -> Layout

    /// Layout list vertically.
    val aboveListL: layouts: Layout list -> Layout

    /// Layout like an F# option.
    val optionL: selector: ('T -> Layout) -> value: 'T option -> Layout

    /// Layout like an F# list.
    val listL: selector: ('T -> Layout) -> value: 'T list -> Layout

    /// See tagL
    val tagAttrL: text: string -> maps: (string * string) list -> layout: Layout -> Layout

    /// For limiting layout of list-like sequences (lists,arrays,etc).
    /// unfold a list of items using (project and z) making layout list via itemL.
    /// If reach maxLength (before exhausting) then truncate.
    val unfoldL:
        selector: ('T -> Layout) ->
        folder: ('State -> ('T * 'State) option) ->
        state: 'State ->
        count: int ->
            Layout list

/// A record of options to control structural formatting.
/// For F# Interactive properties matching those of this value can be accessed via the 'fsi'
/// value.
///
/// Floating Point format given in the same format accepted by System.Double.ToString,
/// e.g. f6 or g15.
///
/// If ShowProperties is set the printing process will evaluate properties of the values being
/// displayed.  This may cause additional computation.
///
/// The ShowIEnumerable is set the printing process will force the evaluation of IEnumerable objects
/// to a small, finite depth, as determined by the printing parameters.
/// This may lead to additional computation being performed during printing.
[<NoEquality; NoComparison>]
type  FormatOptions =
    { FloatingPointFormat: string
      AttributeProcessor: string -> (string * string) list -> bool -> unit
#if COMPILER  // FSharp.Core.dll: PrintIntercepts aren't used there
      PrintIntercepts: (IEnvironment -> obj -> Layout option) list
      StringLimit: int
#endif
      FormatProvider: IFormatProvider
      BindingFlags: System.Reflection.BindingFlags
      PrintWidth: int
      PrintDepth: int
      PrintLength: int
      PrintSize: int
      ShowProperties: bool
      ShowIEnumerable: bool }

    static member Default: FormatOptions

module  Display =

#if COMPILER

    val asTaggedTextWriter: writer: TextWriter -> TaggedTextWriter

    val any_to_layout: options: FormatOptions -> value: 'T * typValue: Type -> Layout

    val squashTo: width: int -> layout: Layout -> Layout

    val squash_layout: options: FormatOptions -> layout: Layout -> Layout

    val output_layout_tagged: options: FormatOptions -> writer: TaggedTextWriter -> layout: Layout -> unit
#else

    // Most functions aren't needed in FSharp.Core.dll, but we add one inernal entry for printf
    val anyToStringForPrintf:
        options: FormatOptions -> bindingFlags: System.Reflection.BindingFlags -> value: 'T * Type -> string
#endif

    /// Convert any value to a layout using the given formatting options.  The
    /// layout can then be processed using formatting display engines such as
    /// those in the Layout module.  any_to_string and output_any are
    /// built using any_to_layout with default format options.
    val layout_to_string: options: FormatOptions -> layout: Layout -> string

#if COMPILER
    val fsi_any_to_layout: options: FormatOptions -> value: 'T * typValue: Type -> Layout
#endif

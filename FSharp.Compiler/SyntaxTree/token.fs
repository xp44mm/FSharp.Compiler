namespace FSharp.Compiler.Parser

open FSharp.Compiler
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

type token =
| BYTEARRAY of byte[] * SynByteStringKind * ParseHelpers.LexerContinuation
| STRING of string * SynStringKind * ParseHelpers.LexerContinuation
| INTERP_STRING_BEGIN_END of string * SynStringKind * ParseHelpers.LexerContinuation
| INTERP_STRING_BEGIN_PART of string * SynStringKind * ParseHelpers.LexerContinuation
| INTERP_STRING_PART of string * ParseHelpers.LexerContinuation
| INTERP_STRING_END of string * ParseHelpers.LexerContinuation
| LBRACE of ParseHelpers.LexerContinuation
| RBRACE of ParseHelpers.LexerContinuation
| KEYWORD_STRING of string * string
| IDENT of string
| HASH_IDENT of string
| INFIX_STAR_STAR_OP    of string
| INFIX_COMPARE_OP      of string
| INFIX_AT_HAT_OP       of string
| INFIX_BAR_OP          of string
| PREFIX_OP             of string
| INFIX_STAR_DIV_MOD_OP of string
| INFIX_AMP_OP          of string
| PLUS_MINUS_OP         of string
| ADJACENT_PREFIX_OP    of string
| FUNKY_OPERATOR_NAME   of string

| INT8 of sbyte * bool 
| INT16 of int16 * bool 
| INT32 of int32 * bool 
| INT32_DOT_DOT of int32 * bool 
| INT64 of int64 * bool 
| NATIVEINT of int64 * bool 
| UINT8 of byte

| UINT16     of uint16
| UINT32     of uint32
| UINT64     of uint64
| UNATIVEINT of uint64
| IEEE32     of single
| IEEE64     of double
| CHAR of char
| DECIMAL of decimal
| BIGNUM of string * string
| LET of isUse:bool
| YIELD of bool
| YIELD_BANG of bool
| AND_BANG of bool

| LESS of bool 
| GREATER of bool

| PERCENT_OP of string
| BINDER of string

| LQUOTE of string * bool 
| RQUOTE of string * bool 
| RQUOTE_DOT of string * bool

| BAR_BAR | UPCAST | DOWNCAST | NULL | RESERVED | MODULE | NAMESPACE | DELEGATE | CONSTRAINT | BASE
| AND | AS | ASSERT | OASSERT | ASR | BEGIN | DO | DONE | DOWNTO | ELSE | ELIF | END | DOT_DOT | DOT_DOT_HAT
| EXCEPTION | FALSE | FOR | FUN | FUNCTION | IF | IN | JOIN_IN | FINALLY | DO_BANG
| LAZY | OLAZY | MATCH | MATCH_BANG | MUTABLE | NEW | OF
| OPEN | OR | REC | THEN | TO | TRUE | TRY | TYPE | VAL | INLINE | INTERFACE | INSTANCE | CONST
| WHEN | WHILE | WHILE_BANG | WITH | HASH | AMP | AMP_AMP | QUOTE | LPAREN | RPAREN | RPAREN_COMING_SOON | RPAREN_IS_HERE | STAR | COMMA | RARROW | GREATER_BAR_RBRACK | LPAREN_STAR_RPAREN
| QMARK | QMARK_QMARK | DOT | COLON | COLON_COLON | COLON_GREATER | COLON_QMARK_GREATER | COLON_QMARK | COLON_EQUALS | SEMICOLON
| SEMICOLON_SEMICOLON | LARROW | EQUALS | LBRACK | LBRACK_BAR | LBRACE_BAR | LBRACK_LESS
| BAR_RBRACK | BAR_RBRACE | UNDERSCORE
| BAR | RBRACK | RBRACE_COMING_SOON | RBRACE_IS_HERE | MINUS | DOLLAR
| GREATER_RBRACK | STRUCT | SIG
| STATIC | MEMBER | CLASS | ABSTRACT | OVERRIDE | DEFAULT | CONSTRUCTOR | INHERIT
| EXTERN | VOID | PUBLIC | PRIVATE | INTERNAL | GLOBAL
| TYPE_COMING_SOON | TYPE_IS_HERE | MODULE_COMING_SOON | MODULE_IS_HERE


| HIGH_PRECEDENCE_BRACK_APP
| HIGH_PRECEDENCE_PAREN_APP
| HIGH_PRECEDENCE_TYAPP

| OLET of bool
| OBINDER of string
| OAND_BANG of bool 
| ODO             
| ODO_BANG      
| OTHEN           
| OELSE            
| OWITH           
| OFUNCTION        
| OFUN             


| ORESET          

| OBLOCKBEGIN      
| OBLOCKSEP       
| OEND                                 
| ODECLEND          of range           
| ORIGHT_BLOCK_END  of range           
| OBLOCKEND         of range           
| OBLOCKEND_COMING_SOON 
| OBLOCKEND_IS_HERE

| OINTERFACE_MEMBER
| FIXED
| ODUMMY of token // ODUMMY is a context closer token, after its context is closed

| LEX_FAILURE of string
| COMMENT      of ParseHelpers.LexerContinuation
| WHITESPACE   of ParseHelpers.LexerContinuation
| HASH_LINE    of ParseHelpers.LexerContinuation
| HASH_LIGHT   of ParseHelpers.LexerContinuation
| INACTIVECODE of ParseHelpers.LexerContinuation
| LINE_COMMENT of ParseHelpers.LexerContinuation
| STRING_TEXT of ParseHelpers.LexerContinuation
| EOF of ParseHelpers.LexerContinuation
| HASH_IF of range * string * ParseHelpers.LexerContinuation
| HASH_ELSE of range * string * ParseHelpers.LexerContinuation
| HASH_ENDIF of range * string * ParseHelpers.LexerContinuation

| TokenLExprParen // from Context Impl
| TokenRExprParen

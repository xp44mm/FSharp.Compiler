# Lexical Filtering

## Lightweight Syntax

F# supports lightweight syntax, in which whitespace makes indentation significant. 

The lightweight syntax option is a conservative extension of the explicit language syntax, in the sense that it simply lets you leave out certain tokens such as `in` and `;;` because the parser takes indentation into account. Indentation can make a surprising difference in the readability of code. Compiling your code with the indentation-aware syntax option is useful even if you continue to use explicit tokens, because the compiler reports many indentation problems with your code and ensures a regular, clear formatting style. 

In the processing of lightweight syntax, comments are considered pure whitespace. This means that the compiler ignores the indentation position of comments. Comments act as if they were replaced by whitespace characters. Tab characters cannot be used in F# files. 

### Basic Lightweight Syntax Rules by Example

The basic rules that the F# compiler applies when it processes lightweight syntax are shown below, illustrated by example. 

`;;` delimiter

When the lightweight syntax option is enabled, top level  expressions do not require the `;;` delimiter because every construct that starts in the first column is implicitly a new declaration. The `;;` delimiter is still required to terminate interactive entries to `fsi.exe`, but not when using F# Interactive from Visual Studio.


Lightweight Syntax

```F#
printf "Hello"   
printf "World"  
```

Normal Syntax

```F#
printf "Hello";;
printf "World";;
```

`in` keyword

When the lightweight syntax option is enabled, the `in` keyword is optional. The token after the "`=`" in a '`let`' definition begins a new block, and the pre-parser inserts an implicit separating in token between each definition that begins at the same column as  that token.  

Lightweight Syntax

```F#
let SimpleSample() =
   let x = 10 + 12 - 3 
   let y = x * 2 + 1 
   let r1,r2 = x/3, x%3 
   (x,y,r1,r2)
```

Normal Syntax

```F#
let SimpleSample() =
   let x = 10 + 12 - 3 in
   let y = x * 2 + 1 in 
   let r1,r2 = x/3, x%3 in
   (x,y,r1,r2)
```

`done` keyword

When the lightweight syntax option is enabled, the `done` keyword is optional. Indentation establishes the scope of structured constructs such as `match`, `for`, `while` and `if`/`then`/`else`.  

Lightweight Syntax

```F#
let FunctionSample() =
   let tick x = printfn "tick %d" x 
   let tock x = printfn "tock %d" x 
   let choose f g h x = 
     if f x then g x else h x 
   for i = 0 to 10 do
     choose (fun n -> n%2 = 0) tick tock i 
   printfn "done!"
```

Normal Syntax

```F#
let FunctionSample() =
   let tick x = printfn "tick %d" x in 
   let tock x = printfn "tock %d" x in 
   let choose f g h x = 
     if f x then g x else h x in 
   for i = 0 to 10 do
     choose (fun n -> n%2 = 0) tick tock i 
   done;
   printfn "done!"
```

`if`/`then`/`else` Scope

When the lightweight syntax option is enabled, the scope of `if`/`then`/`else` is implicit from indentation. Without the lightweight syntax option, `begin`/`end` or parentheses are often required to delimit such constructs.

Lightweight Syntax

```F#
let ArraySample() =
   let numLetters = 26 
   let results = Array.create numLetters 0 
   let data = "The quick brown fox" 
   for i = 0 to data.Length - 1 do 
     let c = data.Chars(i) 
     let c = Char.ToUpper(c) 
     if c >= 'A' && c <= 'Z' then 
       let i = Char.code c - Char.code 'A' 
       results.[i] <- results.[i] + 1
   printfn "done!"
```

Normal Syntax

```F#
let ArraySample() =
   let numLetters = 26 in 
   let results = Array.create numLetters 0 in 
   let data = "The quick brown fox" in 
   for i = 0 to data.Length - 1 do 
     let c = data.Chars(i) in 
     let c = Char.ToUpper(c) in 
     if c >= 'A' && c <= 'Z' then begin
       let i = Char.code c - Char.code 'A' in 
       results.[i] <- results.[i] + 1
     end
   done;
   printfn "done!"
```

### Inserted Tokens

Lexical filtering inserts the following hidden tokens :

```F#
token $in    // Note: also called ODECLEND 
token $done  // Note: also called ODECLEND 
token $begin // Note: also called OBLOCKBEGIN 
token $end   // Note: also called OEND, OBLOCKEND and ORIGHT_BLOCK_END 
token $sep   // Note: also called OBLOCKSEP 
token $app   // Note: also called HIGH_PRECEDENCE_APP 
token $tyapp // Note: also called HIGH_PRECEDENCE_TYAPP 
```

Note: The following tokens are also used in the Microsoft F# implementation. They are translations of the corresponding input tokens and help provide better error messages for lightweight syntax code:

```F#
tokens $let $use $let! $use! $do $do! $then $else $with $function $fun
```

### Grammar Rules Including Inserted Tokens

Additional grammar rules take into account the token transformations performed by lexical filtering:

```F#
expr +:= 
  | let function-defn $in expr 
  | let value-defn $in expr 
  | let rec function-or-value-defns $in expr 
  | while expr do expr $done  
  | if expr then $begin expr $end 
  | for pat in expr do expr $done 
  | for expr to expr do expr $done 
  | try expr $end with expr $done 
  | try expr $end finally expr $done 
  | expr $app expr       // equivalent to "expr(expr)"
  | expr $sep expr       // equivalent to "expr; expr"
  | expr $tyapp < types >   // equivalent to "expr<types>"
  | $begin expr $end  // equivalent to "expr"

elif-branch +:=
  | elif expr then $begin expr $end

else-branch +:=
  | else $begin expr $end

class-or-struct-type-body +:= 
  | $begin class-or-struct-type-body $end
               // equivalent to class-or-struct-type-body
               
module-elems +:=
 | $begin module-elem ... module-elem $end

module-abbrev +:= 
 | module ident = $begin long-ident $end

module-defn +:=
 | module ident = $begin module-defn-body $end

module-signature-elements +:= 
 | $begin module-signature-element ... module-signature-element $end

module-signature +:=
 | module ident = $begin module-signature-body $end
```

### Offside Lines 

Lightweight syntax is sometimes called the “**offside rule**”. In F# code, **offside lines** occur at column positions. For example, an `=` token associated with `let` introduces an offside line at the column of the first non-whitespace token after the `=` token.

Other structured constructs also introduce offside lines at the following places: 

- The column of the first token after `then` in an `if`/`then`/`else` construct.

- The column of the first token after `try`, `else`, `->`, `with` (in a `match`/`with` or `try`/`with`), or `with` (in a type extension). 

- The column of the first token of a `(`, `{` or `begin` token. 

- The start of a `let`, `if` or `module` token. 

 

Here are some examples of how the offside rule applies to F# code. In the first example, `let` and `type` declarations are not properly aligned, which causes F# to generate a warning.

```F#
// "let" and "type" declarations in 
// modules must be precisely aligned.
let x = 1
 let y = 2 <-- unmatched 'let'
let z = 3  <-- warning FS0058: possible 
               incorrect indentation: this token is offside of  
               context at position (2:1)
```

In the second example, the `|` markers in the match patterns do not align properly:

```F#
// The "|" markers in patterns must align.
// The first "|" should always be inserted. 

let f () = 
  match 1+1 with 
  | 2 -> printf "ok"
 | _ -> failwith "no!"  <-- syntax error
```



### The Pre-Parse Stack 

F# implements the lightweight syntax option by pre-parsing the token stream that results from a lexical analysis of the input text according to the lexical rules in §15.1.3. Pre-parsing for lightweight syntax uses a stack of contexts. 

- When a column position becomes an offside line, a context is pushed. 

- The closing bracketing tokens `)`, `}`, and `end` terminate offside contexts up to and including the context that the corresponding opening token introduced. 

### Full List of Offside Contexts 

This section describes the full list of offside contexts that is kept on the pre-parse stack. 

The `SeqBlock` context is the primary context of the analysis. It indicates a sequence of items that must be column-aligned. Where necessary for parsing, the compiler automatically inserts a delimiter that replaces the regular `in` and `;` tokens between the syntax elements. The `SeqBlock` context is pushed at the following times: 

- Immediately after the start of a file, excluding lexical directives such as `#if`.

- Immediately after an `=` token is encountered in a `Let` or `Member` context.

- Immediately after a `Paren`, `Then`, `Else`, `WithAugment`, `Try`, `Finally`, `Do` context is pushed.

- Immediately after an infix token is encountered.

- Immediately after a `->` token is encountered in a `MatchClauses` context.

- Immediately after an `interface`, `class`, or `struct` token is encountered in a type declaration.

- Immediately after an `=` token is encountered in a record expression when the subsequent token either 

	(a) occurs on the next line or 

	(b) is one of *try*, *match*, *if*, *let*, *for*, *while* or *use*.

- Immediately after a `<-` token is encountered when the subsequent token either 

	(a) does not occur on the same line or 

	(b) is one of *try*, *match*, *if*, *let*, *for*, *while* or *use*.

 

Here “immediately after” refers to the fact that the column position associated with the `SeqBlock` is the first token following the significant token.

In the last two rules, a new line is significant. For example, the following do not start a `SeqBlock` on the right-hand side of the “`<-`“ token, so it does not parse correctly:

```F#
let mutable x = 1
// The subsequent token occurs on the same line. 
X <- printfn "hello"
   2 + 2  
```

 

To start a `SeqBlock` on the right, either parentheses or a new line should be used:

```F#
// The subsequent token does not occur on the same line, so a SeqBlock is pushed.
X <- 
   printfn "hello"
   2 + 2
```

 

The following contexts are associated with nested constructs that are introduced by the specified keywords:

| **Context**  | **Pushed when the token stream contains…**                 |
| ------------ | ------------------------------------------------------------ |
| Let          | The `let` keyword                                            |
| If           | The `if` or `elif` keyword                                   |
| Try          | The `try` keyword                                            |
| Lazy         | The `lazy` keyword                                           |
| Fun          | The `fun` keyword                                            |
| Function     | The `function` keyword                                       |
| WithLet      |                                                              |
| WithAugment  |                                                              |
| Match        | the `match` keyword                                          |
| For          | the `for` keyword                                            |
| While        | The `while` keyword                                          |
| Then         | The `then` keyword                                           |
| Else         | The `else` keyword                                           |
| Do           | The `do` keyword                                             |
| Type         | The `type` keyword                                           |
| Namespace    | The `namespace` keyword                                      |
| Module       | The `module` keyword                                         |
| Member       |                                                              |
| Paren(token) | `(`, `begin`, `struct`, `sig`, `{`, `[`, `[|`, or `quote-op-left` |
| MatchClauses | The `with` keyword in a `Try` or `Match` context immediately after a `function` keyword. |
| Vanilla      | An otherwise unprocessed keyword in a `SeqBlock` context.   |



WithLet

The `with` keyword as part of a record  expression or an object expression whose members use the syntax

```F#
{ new Foo with M() = 1 and N() = 2 }
```


WithAugment

The `with` keyword as part of an  extension, interface, or object expression whose members use the syntax 

```F#
{ new Foo member x.M() = 1 member x. N() =  2 }
```


Member

§ The `member`, `abstract`, `default`, or `override` keyword, if the  Member context is not already active, because multiple tokens may be present. 

—or— 

§ `(` is the next  token after the `new` keyword. This distinguishes the  member declaration `new(x) = ...` from the expression `new x()`

### Balancing Rules

When the compiler processes certain tokens, it pops contexts off the offside stack until the stack reaches a particular condition. When it pops a context, the compiler may insert extra tokens to indicate the end of the construct. This procedure is called balancing the stack. 

The following table lists the contexts that the compiler pops and the balancing condition for each:

End

Enclosing context is one of the following:

- WithAugment 
- Paren(interface)
- Paren(class)
- Paren(sig)
- Paren(struct)
- Paren(begin)

| **Token**      | **Contexts Popped and Balancing   Conditions:** |
| -------------- | ----------------------------------------------- |
| End            |                                                 |
| ;;             | Pop all  contexts from stack                    |
| else           | If                                              |
| elif           | If                                              |
| done           | Do                                              |
| in             | For or Let                                      |
| with           | Match, Member,  Interface, Try, Type            |
| finally        | Try                                             |
| )              | Paren(()                                        |
| }              | Paren({)                                        |
| ]              | Paren([)                                        |
| \|]            | Paren([\|)                                      |
| quote-op-right | Paren(quote-op-left)                            |

### Offside Tokens, Token Insertions, and Closing Contexts

The offside limit for the current offside stack is the rightmost offside line for the offside contexts on the context stack. The following figure shows the offside limits: 

```F#
let FunctionSample() =
    let tick x = printfn "tick %d" x 
    let tock x = printfn "tock %d" x 
    let choose f g h x = 
        if f x then g x else h x 
    for i = 0 to 10 do
        choose (fun n -> n%2 = 0) tick tock i 
    printfn "done!"
        ^ // offside limit for inner let and for contexts
    ^ // offside limit for outer let context
```

When a token occurs on or before the offside limit for the current offside stack, and a permitted undentation does not apply, enclosing contexts are closed until the token is no longer offside. This may result in the insertion of extra delimiting tokens.

Contexts are closed as follows:

- When a `Fun` context is closed, the `$end` token is inserted.

- When a `SeqBlock`, `MatchClauses`, `Let`, or `Do` context is closed, the `$end` token is inserted, with the exception of the first `SeqBlock` pushed for the start of the file.

- When a `While` or `For` context is closed, and the offside token that forced the close is not `done`, the `$done` token is inserted.

- When a `Member` context is closed, the `$end` token is inserted.

- When a `WithAugment` context is closed, the `$end` token is inserted.

 

If a token is offside and a context cannot be closed, then an “undentation” warning or error is issued to indicate that the construct is badly formatted. 

Tokens are also inserted in the following situations: 

- When a `SeqBlock` context is pushed, the `$begin` token is inserted, with the exception of the first `SeqBlock` pushed for the start of the file.

- When a token other than `and` appears directly on the offside line of `Let` context, and the next surrounding context is a `SeqBlock`, the `$in` token is inserted.

- When a token occurs directly on the offside line of a `SeqBlock` on the second or subsequent lines of the block, the `$sep` token is inserted. This token plays the same role as `;` in the grammar rules.

For example, consider this source text:

```F#
let x = 1
x
```

The raw token stream contains `let`, `x`, `=`, `1`, `x` and the end-of-file marker `eof`. An initial `SeqBlock` is pushed immediately after the start of the file, at the first token in the file, with an offside line on column `0`. The `let` token pushes a Let context. The `=` token in a `Let` context pushes a `SeqBlock` context and inserts a `$begin` token. The `1` pushes a `Vanilla` context. The final token, `x`, is offside from the `Vanilla` context, which pops that context. It is also offside from the `SeqBlock` context, which pops the context and inserts `$end`. It is also offside from the `Let` context, which inserts another `$end` token. It is directly aligned with the `SeqBlock` context, so a `$seq` token is inserted.

### Exceptions to the Offside Rules

The compiler makes some exceptions to the offside rules when it analyzes a token to determine whether it is offside from the current context. The following table summarizes the exceptions and shows examples of each.

SeqBlock

An infix token may be offside by the size of the token plus one.

```F#
let x = 
    expr + expr 
  + expr + expr 

 let x = 
    expr 
 |> f expr 
 |> f expr
```



SeqBlock

An infix token may align precisely with the offside line of the `SeqBlock`.

```F#
let someFunction(someCollection) = 
    someCollection
    |> List.map (fun x -> x + 1)
```



SeqBlock

The infix `|>` token that begins the last line is not considered as a new element in the sequence block on the right-hand side of the definition. The same also applies to `end`, `and`, `with`, `then`, and right-parenthesis operators.

In the example, the first `)` token does not indicate a new element in a sequence of items, even though it aligns precisely with the sequence block that starts at the beginning of the argument list.

```F#
new MenuItem("&Open...", 
  new EventHandler(fun _ _ -> 
                           ...
             ))
```

Let

The `and` token may align precisely with the `let` keyword.

```F#
let rec x = 1
and y = 2
x + y
```

Type

The `}`, `end`, `and`, and `|` tokens may align precisely with the `type` keyword.

```F#
type X = 
| A
| B
with 
    member x.Seven = 21 / 3
end
and Y = {
    x : int
}
and Z() = class
    member x.Eight = 4 + 4
end
```

For

The `done` token may align precisely with the `for` keyword.

```F#
for i = 1 to 3 do 
  expr
done
```

SeqBlock; Match 

On the right-hand side of an arrow for a match expression, a token may align precisely with the `match` keyword. This exception allows the last expression to align with the `match`, so that a long series of matches does not increase indentation.

```F#
match x with 
| Some(_) -> 1
| None -> 
match y with 
| Some(_) -> 2
| None -> 
3
```

Interface

The `end` token may align precisely with the `interface` keyword.

```F#
interface IDisposable with 
   member x.Dispose() = printfn disposing!"
end
```

If

The `then`, `elif`, and `else` tokens may align precisely with the `if` keyword.

```F#
if big 
then callSomeFunction()
elif small
then callSomeOtherFunction()
else doSomeCleanup()
```

Try

The `finally` and `with` tokens may align precisely with the `try` keyword.

```F#
// Example 1:
try 
    callSomeFunction()
finally
    doSomeCleanup()
// Example 2:
try 
    callSomeFunction()
with Failure(s) ->
    doSomeCleanup()

```

Do

The `done` token may align precisely with the `do` keyword.

```F#
for i = 1 to 3 
   do 
     expr
   done
```

### Permitted Undentations 

As a general rule, incremental indentation requires that nested expressions occur at increasing column positions in indentation-aware code. Warnings or syntax errors are issued when this is not the case. However, undentation is permitted for the following constructs:

- Bodies of function expressions

- Branches of if/then/else expressions

- Bodies of modules and module types

#### Undentation of Bodies of Function Expressions

The bodies of functions may be undented from the `fun` or `function` symbol. As a result, the compiler ignores the symbol when determining whether the body of the function satisfies the incremental indentation rule. For example, the printf expression in the following example is undented from the `fun` symbol that delimits the function definition:

```F#
let HashSample(tab: Collections.HashTable<_,_>) =
  tab.Iterate (fun c v -> 
    printfn "Entry (%O,%O)" c v)
```

However, the block must not undent past other offside lines. The following is not permitted because the second line breaks the offside line established by the `=` in the first line:

```F#
let x = (function (s, n) ->
  (fun z ->
    s+n+z))
```

Constructs enclosed in brackets may be undented.

#### Undentation of Branches of If/Then/Else Expressions 

The body of a `( ... )` or `begin ... end` block in an `if/then/else` expression may be undented when the body of the block follows the `then` or `else` keyword but may not undent further than the `if` keyword. In this example, the parenthesized block follows then, so the body can be undented to the offside line established by `if`:

```F#
let IfSample(day: System.DayOfWeek) =
  if day = System.DayOfWeek.Monday then (
    printf "I don't like Mondays"
  )
```



#### Undentation of Bodies of Modules and Module Types 

The bodies of modules and module types that are delimited by `begin` and `end` may be undented. For example, in the following code the two let statements that comprise the module body are undented from the `=`. 

```F#
module MyNestedModule = begin
  let one = 1
  let two = 2
 end
```

Similarly, the bodies of classes, interfaces, and structs delimited by `{ ... }`, `class ... end`, `struct ... end`, or `interface ... end` may be undented to the offside line established by the `type` keyword. For example:

```F#
type MyNestedModule = interface
  abstract P : int
end
```



#### Undentation of Record and Sequence expressions  ({ … } expressions)

The bodies of expressions that are delimited by `{` and `}` may be undented. For example, in the following code the `yield` token is undented from the `(`. 

```F#
System.Console.WriteLine(seq {
  yield 1 
})
```

In this version of F# this does not apply to `[ … ]`, `[| … |]` or `(…)` expressions, though this limitation may be lifted in a later release.

## High Precedence Application 

The entry `f x` in the precedence table in §4.4.2 refers to a function application in which the function and argument are separated by spaces. The entry "`f(x)`" indicates that in expressions and patterns, identifiers that are followed immediately by a left parenthesis without intervening whitespace form a “high precedence” application. Such expressions are parsed with higher precedence than prefix and dot-notation operators. Conceptually this means that

```F#
B(e)
```

is analyzed lexically as 

```F#
B $app (e)
```

where `$app` is an internal symbol inserted by lexical analysis. We do not show this symbol in the remainder of this specification and simply show the original source text. 

This means that the following two statements

```F#
B(e).C
B (e).C
```

are parsed as 

```F#
(B(e)).C
B ((e).C)
```

respectively. 

Furthermore, arbitrary chains of method applications, property lookups, indexer lookups (`.[]`), field lookups, and function applications can be used in sequence if the arguments of method applications are parenthesized and immediately follow the method name, with no intervening spaces. For example: 

```F#
e.Meth1(arg1,arg2).Prop1.[3].Prop2.Meth2()
```

Although the grammar and these precedence rules technically allow the use of high-precedence application expressions as direct arguments, an additional check prevents such use. Instead, such expressions must be surrounded by parentheses. For example, 

```F#
f e.Meth1(arg1,arg2) e.Meth2(arg1,arg2)
```

must be written

```F#
f (e.Meth1(arg1,arg2)) (e.Meth2(arg1,arg2))
```

However, indexer, field, and property dot-notation lookups may be used as arguments without adding parentheses. For example:

```F#
f e.Prop1 e.Prop2.[3]
```



## Lexical Analysis of Type Applications



The entry `f<types> x` in the precedence table (§4.4.2) refers to any identifier that is followed immediately by a `<` symbol and a sequence of all of the following: 

- `_`, `,`, `*`, `'`, `[`, `]`, whitespace, or identifier tokens.

- A parentheses `(` or `<` token followed by any tokens until a matching parentheses `)` or `>` is encountered.

- A final `>` token.

 

During this analysis, any token that is composed only of the `>` character (such as `>`, `>>`, or `>>>`) is treated as a series of individual `>` tokens. Likewise, any token composed only of `>` characters followed by a period (such as `>.`, `>>.`, or `>>>.`) is treated as a series of individual > tokens followed by a period.

If such a sequence of tokens follows an identifier, lexical analysis marks the construct as a high precedence type application and subsequent grammar rules ensure that the enclosed text is parsed as a type. Conceptually this means that

```F#
B<int>.C<int>(e).C
```

is returned as the following stream of tokens:

```F#
B $app <int> .C $app <int>(e).C 
```

where `$app` is an internal symbol inserted by lexical analysis. We do not show this symbol elsewhere in this specification and simply show the original source text.

The lexical analysis of type applications does not apply to the character sequence “`<>`”. A character sequence such as “`< >`” with intervening whitespace should be used to indicate an empty list of generic arguments.

```F#
type Foo() =
  member this.Value = 1

let b = new Foo< >() // valid
let c = new Foo<>() // invalid
```



Part I. Building a new lex stream from an old

A lexbuf is a stateful object that can be enticed to emit tokens by calling '`lexer`' functions designed to work with the lexbuf. Here we fake a new stream coming out of an existing lexbuf. Ideally lexbufs would be abstract interfaces and we could just build a new abstract interface that wraps an existing one. However that is not how F# lexbufs currently work.

Part of the fakery we perform involves buffering a lookahead token which we eventually pass on to the client. However, this client also looks at other aspects of the 'state' of lexbuf directly, e.g. F# lexbufs have a triple (start-pos, end-pos, eof-reached)

You may ask why the F# parser reads this lexbuf state directly. Well, the pars.fsy code itself it doesn't, but the parser engines (prim-parsing.fs) certainly do for F#. e.g. when these parsers read a token from the lexstream they also read the position information and keep this a related stack.

Anyway, this explains the functions `getLexbufState()`, `setLexbufState()` etc.

Offside rule for CtxtLetDecl (in types or modules) / CtxtMemberHead / CtxtTypeDefns... (given RelaxWhitespace2)
This should not be applied to contexts with optional closing tokens! (CtxtFun, CtxtFunction, CtxtDo, CtxtMemberBody, CtxtSeqBlock etc)

```fsharp
let (         member Foo (       for x in (       while (
 ...           ...                ...              ...
) = ...       ) = ...            ) do ...         ) do ...
let [         member Foo [       for x in [       while f [
 ...           ...                ...              ...
] = ...       ] = ...            ] do ...         ] do ...
let {         member Foo {       for x in {       while f {
 ...           ...                ...              ...
} = ...       } = ...            } do ...         } do ...
let [|        member Foo [|      for x in [|      while f [|
 ...           ...                ...              ...
|] = ...      |] = ...           |] do ...        |] do ...
let x : {|    member Foo : {|    for x in f {|    while f {|
 ...           ...                ...              ...
|} = ...      |} = ...           |} do ...        |} do ...
let x : Foo<  member x : Foo<    for x in foo<    for x in foo<
 ...           ...                ...              ...
> = ...       > = ...            > = ...          > = ...
type Foo(
 ...
) = ...
```



If you see a '`member`' keyword while you are inside the body of another member, then it usually means there is a syntax error inside this method
and the upcoming '`member`' is the start of the next member in the class. For better parser recovery and diagnostics, it is best to pop out of the
existing member context so the parser can recover.

However there are two places where '`member`' keywords can appear inside expressions inside the body of a member. The first is object expressions, and
the second is static inline constraints. We must not pop the context stack in those cases, or else legal code will not parse.

It is impossible to decide for sure if we're in one of those two cases, so we must err conservatively if we might be.



Balancing rule. 

Every '`in`' terminates all surrounding blocks up to a `CtxtLetDecl`, and will be swallowed by terminating the corresponding `CtxtLetDecl` in the rule below.

Balancing rule. 

Every '`done`' terminates all surrounding blocks up to a `CtxtDo`, and will be swallowed by terminating the corresponding `CtxtDo` in the rule below.

For the purposes of #light processing, `<`, `>` and `=` are not considered to be infix operators. This is because treating them as infix conflicts with their role in other parts of the grammar, e.g. to delimit "`f<int>`", or for "`let f x = ....`"

This has the impact that a `SeqBlock` does not automatically start on the right of a "`<`", "`>`" or "`=`",
e.g.

```F#
    let f x = (x =
                   let a = 1 // no #light block started here, parentheses or 'in' needed
                   a + x)
// LESS | GREATER | EQUALS
```


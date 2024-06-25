module FSharp.Compiler.ListUtils

let rec suffixExists (p:'a list -> bool) (l:'a list) = 
    match l with
    | [] -> false 
    | _ :: t -> p t || suffixExists p t


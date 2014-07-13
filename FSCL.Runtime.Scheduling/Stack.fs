namespace FSCL.Runtime.Scheduling

module VarStack =
    type VarStack =
        | EmptyStack
        | StackNode of (Quotations.Var * Quotations.Expr * bool) * VarStack
         
    let head = function
        | EmptyStack -> failwith "Empty stack"
        | StackNode(hd, tl) -> hd
 
    let tail = function
        | EmptyStack -> failwith "Emtpy stack"
        | StackNode(hd, tl) -> tl
        
    let pop = function
        | EmptyStack -> failwith "Emtpy stack"
        | StackNode(hd, tl) -> tl
 
    let cons hd tl = StackNode(hd, tl)
 
    let empty = EmptyStack
 
    let rec update index value s =
        match index, s with
        | index, EmptyStack -> failwith "Index out of range"
        | 0, StackNode(hd, tl) -> StackNode(value, tl)
        | n, StackNode(hd, tl) -> StackNode(hd, update (index - 1) value tl)
 
    let rec push x y =
        match x with
        | EmptyStack -> StackNode(y, EmptyStack)
        | StackNode(hd, tl) -> StackNode(y, StackNode(hd, tl))
 
    let rec contains x = function
        | EmptyStack -> false
        | StackNode((v, value, _), tl) -> v = x || contains x tl
 
    let rec find x = function
        | EmptyStack -> None
        | StackNode((v, value, _), tl) -> 
            if v = x then
                Some(value)
            else
                find x tl
                
    let rec set x newValue = function
        | EmptyStack -> EmptyStack
        | StackNode((v, value, b), tl) -> 
            if v = x then
                StackNode((v, newValue, true), tl)
            else
                StackNode((v, value, b), set x newValue tl)

    let rec findAndTail x restrictNonMutated = function
        | EmptyStack -> None, EmptyStack
        | StackNode((v, value, b), tl) -> 
            if v = x then
                if not (restrictNonMutated && b) then
                    Some(value), tl
                else
                    failwith "Cannot tail the stack cause traceback is passing through a mutated variable"
            else
                if not (restrictNonMutated && b) then
                    findAndTail x restrictNonMutated tl 
                else
                    failwith "Cannot tail the stack cause traceback is passing through a mutated variable"
                    

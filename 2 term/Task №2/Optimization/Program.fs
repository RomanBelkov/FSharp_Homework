type Expr =
    | Const of int
    | Var of string
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr

let rec optim expr =
    let simplification = function
        | Add (Const x, Const y) -> Const (x + y)
        | Add (x, Const 0) | Add (Const 0, x) ->  x

        | Sub (Const x, Const y) -> Const (x - y)
        | Sub (x, Const 0) | Sub (Const 0, x) ->  x
        | Sub (Var x, Var y) ->
            if (x = y) then
                Const 0
            else
                Sub (Var x, Var y)

        | Mul (Const x, Const y) -> Const (x + y)
        | Mul (x, Const 0) | Mul (Const 0, x) -> Const 0
        | Mul (x, Const 1) | Mul (Const 1, x) ->  x

        | Div (Const x, Const y) -> Const (x / y)
        | Div (x, Const 1) ->  x
        | Div (Var x, Var y) ->
            if x = y then
                Const 1
            else
                Div (Var x, Var y)
        | Div (x, Const 0) -> failwith "ERROR! Division by zero is strictly prohibited"
        | expression -> expression 

    match expr with
    | Add(a, b) -> simplification(Add(optim a, optim b))
    | Sub(a, b) -> simplification(Sub(optim a, optim b))
    | Mul(a, b) -> simplification(Mul(optim a, optim b))
    | Div(a, b) -> simplification(Div(optim a, optim b))
    | Const a -> expr
    | Var a -> expr

let first = Sub(Const 1, Const 1)
let second = Sub(Const 2, Const 2)
let e = Sub(first, second)
printfn "%A" (optim e)
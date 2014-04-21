type Expr =
    | Const of int
    | Var of string
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr

let rec optim expr =
    match expr with
    | Const x -> Const x
    | Var s -> Var s
    
    | Add (Const x, Const y) -> Const (x + y)
    | Add (x, Const 0) -> optim x
    | Add (Const 0, x) -> optim x
    | Add (x, y) ->
        let a = optim x
        let b = optim y
        Add (a,b)

    | Sub (Const x, Const y) -> Const (x - y)
    | Sub (x, Const 0) -> optim x
    | Sub (Const 0, x) -> optim x
    | Sub (Var x, Var y) ->
        if (x = y) then
            Const 0
        else
            Sub (Var x, Var y)
    | Sub (x,y) -> 
        let a = optim x
        let b = optim y
        Sub (x, y)

    | Mul (Const x, Const y) -> Const (x + y)
    | Mul (x, Const 0) -> Const 0
    | Mul (Const 0, x) -> Const 0
    | Mul (x, Const 1) -> x
    | Mul (Const 1, x) -> x
    | Mul (x, y) ->
        let a = optim x
        let b = optim y
        Mul (a, b)

    | Div (Const x, Const y) -> Const (x / y)
    | Div (x, Const 1) -> x
    | Div (Var x, Var y) ->
        if x = y then
            Const 1
        else
            Div (Var x, Var y)
    | Div (x, Const 0) -> failwith "ERROR! Division by zero is strictly prohibited"
    | Div (x, y) ->
        let a = optim x
        let b = optim y
        Div (a, b)

printfn "%A" (optim (Div (Var "x", Const 0)))
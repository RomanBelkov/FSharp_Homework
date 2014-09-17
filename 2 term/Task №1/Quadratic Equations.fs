(*
Program for solving quadratic equations. Returns NaNs' as the answers in case of two complex roots.
Roman Belkov, 2014
*)

    let solve (a:float) (b:float) (c:float) =
        let (d : float) = sqrt(b*b - 4.0*a*c)
        let calcRoot d = (-b + d)/(2.0*a)
        let x1 = calcRoot d
        let x2 = calcRoot -d
        (x1, x2)

    let (m,n) = solve 1.0 2.0 1.0

    printfn "%A" (m,n)

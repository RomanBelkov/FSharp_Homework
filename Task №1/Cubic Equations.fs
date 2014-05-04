let eq = (1.0, 3.0, -4.0, -12.0)

let first (a, _, _, _) = a
let second (_, b, _, _) = b
let third (_, _, c, _) = c
let fourth (_, _, _, d) = d

let solver (coeff : float * float * float * float) =
    let a = first coeff

    match a with
    | 0.0 -> failwith "First coefficient should not equal zero"
    | _ -> 
        let b = (second coeff)/a
        let c = (third coeff)/a
        let d = (fourth coeff)/a

        let mutable q = (b*b - 3.0*c)/9.0
        let mutable r = (b*(2.0*b*b - 9.0*c) + 27.0*d)/54.0
        let r2 = r*r
        let q3 = q*q*q
        let b' = b/3.0

        if r2 < q3 then
            let t = acos (r/sqrt(q3))
            q <- -2.0*sqrt(q)

            let x = q*cos (t/3.0) - b'
            let x' = q*cos ((t + 2.0*System.Math.PI)/3.0) - b'
            let x'' = q*cos ((t - 2.0*System.Math.PI)/3.0) - b'
            (x, x', x'')
        else 
            if r < 0.0 then r <- -r
            let alpha = -((r + sqrt(r2 - q3)) ** (1.0/3.0))
            let mutable beta = 0.0
            if alpha <> 0.0 then
                beta <- q/alpha
            q <- alpha + beta
            r <- alpha - beta

            let x = q - b'
            let x' = (-0.5)*q - b'
            let x'' = (sqrt(3.0)*0.5*abs(r))
            (x, x', x'')

let test = solver eq
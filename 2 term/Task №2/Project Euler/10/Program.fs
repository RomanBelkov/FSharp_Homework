    open System.Numerics

    let isPrime (n : BigInteger) =
        match n with
        | _ when n > 3I && (n % 2I = 0I || n % 3I = 0I) -> false
        | _ ->
            let upperLimit = BigInteger(System.Math.Sqrt(float n)) + 1I
            let rec loop (d : BigInteger) (i : BigInteger) = 
                if d > upperLimit then 
                    true
                else
                    if n % d = 0I then 
                        false
                    else
                        loop (d + i) i    
            loop 5I 2I
    
    let result = [2I..1999999I] |> List.filter (fun x -> isPrime x) |> List.sum
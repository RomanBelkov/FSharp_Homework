    open System.Numerics

    let factorial (number : BigInteger) =
        let rec loop (number : BigInteger) (acc : BigInteger) =
            match number with 
            | _ when number < 1I -> acc
            | _ -> loop (number - 1I) (acc * number)
        loop number 1I

    let summary (number : BigInteger) =
        let rec loop (acc : BigInteger) (number : BigInteger)  =
            match number with
            | _ when number = 0I -> acc
            | _ -> loop (acc + (number % 10I)) (number / 10I)
        loop 0I number

    let gogo = summary (factorial 100I)

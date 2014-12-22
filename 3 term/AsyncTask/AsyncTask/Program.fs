open System
open System.IO
open System.Text
open System.Diagnostics
open System.Threading.Tasks
open FParsec
//open FSharpx
//open FSharpx.Prelude
//open FSharpx.Operators

type CoolType = 
    | Degree of int32 
    | Cos 
    | Sin

let rnd = new Random()
let sw = new Stopwatch()

let split (s : string) = s.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)

let awaitTask = Async.Ignore << Async.AwaitIAsyncResult

let magic = 2

let randSign = function
    | x when x % 2 = 0 -> " + " 
    | _ -> " - "

let monoPars inp x =
    match inp with
    | (coeff, Degree d) -> let intDeg = float (d + 1) in coeff * (x ** intDeg) / intDeg
    | (coeff, Cos)      -> coeff * sin(x)
    | (coeff, Sin)      -> -1. * coeff * cos(x)

//Parser-related business
let test p str =
    match run p str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith "errorFail"

let sign c =
    match c with
    | '+' -> 1.
    | '-' -> -1.
    | _ -> failwith "sign failed"

let cases = (skipString "x^" >>. pint32 |>> (fun x -> Degree (x))) 
            <|> (skipString "x" >>. preturn (Degree (1))) 
            <|> (skipString "cos(x)" >>. preturn Cos)
            <|> (skipString "sin(x)" >>. preturn Sin)
            <|> (skipString "" >>. preturn (Degree (0)))
let mono = (pfloat .>>. cases) .>> spaces

let signParse = (pchar '+' <|> pchar '-') .>> spaces

let signedMono = pipe2 signParse mono (fun s (a, b) -> (a * sign s, b))

let coolParser = pipe2 mono (many signedMono) (fun x l -> x :: l)

//Checking the folders
let inFolder = @"C:\Work\IN\"
let outFolder = @"C:\Work\OUT\"

let dirVerify = 
    let foo = Directory.Exists inFolder
    let bar = Directory.Exists outFolder

    if foo = false then
        let dir = Directory.CreateDirectory inFolder
        Console.WriteLine("The directory was created successfully at {0}.", Directory.GetCreationTime(inFolder))

    if bar = false then
        let dir = Directory.CreateDirectory outFolder
        Console.WriteLine("The directory was created successfully at {0}.", Directory.GetCreationTime(inFolder))

sw.Start()

let generate = 
    seq {
        for index in 1..magic -> async {
            let inName = "input" + index.ToString("000000") + ".txt"
            
            use genStream = 
                new FileStream(inFolder + inName, FileMode.Create, 
                               FileAccess.Write, FileShare.None, 
                               bufferSize = 4096(*, useAsync = true*))
            use writer = new StreamWriter(genStream)

            let str = 
                Array.init (rnd.Next(2000, 5000)) (fun _ -> rnd.Next(1, 100))
                |> Array.map (fun e -> e.ToString() + "x^" + rnd.Next(1, 100).ToString())
                |> Array.reduce (fun x y -> x + randSign (rnd.Next(0, 1000)) + y)

            do! writer.WriteLineAsync(str) |> awaitTask
            do! writer.WriteLineAsync(rnd.Next(-10, 10).ToString() + " " + rnd.Next(0, 10).ToString())|> awaitTask
        }
    } |> Async.Parallel |> Async.RunSynchronously |> ignore

sw.Stop()

printfn "Generation of files: %A" sw.Elapsed.TotalSeconds
sw.Reset()

sw.Start()

let worker = 
    seq {
        for index in 1..magic -> async {
            let inName = "input" + index.ToString("000000") + ".txt"
            let outName = "output" + index.ToString("000000") + ".txt"
            
            use inSrcStream = 
                new FileStream(inFolder + inName, FileMode.Open, 
                               FileAccess.Read, FileShare.None, 
                               bufferSize = 4096(*, useAsync = true*))
            use outSrcStream = 
                new FileStream(outFolder + outName, FileMode.Create, 
                               FileAccess.Write, FileShare.None, 
                               bufferSize = 4096(*, useAsync = true*))
            use reader = new StreamReader(inSrcStream)
            use writer = new StreamWriter(outSrcStream)
//            use r1 = new AsyncStreamReader(inSrcStream)           
//            let! parsedInput = Async.map (test coolParser) (r1.ReadLine()) // cool (Async.map из Fsharpx)
        
            //let! str = reader.ReadLineAsync() |> Async.AwaitTask
            let str = reader.ReadLine()
            let parsedInput = test coolParser str
                   
//            let! (a, b) = 
//                    (split >> array.map float >> (fun arr -> if arr.length <> 2 then failwith "nope lol" else (arr.[0], arr.[1])))
//                    |> (flip async.map) (reader.readlineasync() |> async.awaittask) 

            let (a, b) =   
                reader.ReadLine().Split([|' '|], StringSplitOptions.RemoveEmptyEntries)             
                |> Array.map float                                                                  
                |> (fun arr -> if arr.Length <> 2 then failwith "nope lol" else (arr.[0], arr.[1]))
                
            let res = 
                let integ x = 
                    List.fold (fun acc inp -> acc + (monoPars inp x)) 0. parsedInput
                (integ b - integ a).ToString()

            do! writer.WriteAsync(res)|> awaitTask
        } 
    } |> Async.Parallel |> Async.RunSynchronously |> ignore

sw.Stop()
printfn "Worker took: %A" sw.Elapsed.TotalSeconds
//sw.Reset()
//sw.Start()
//
//for index in 1..magic do
//    let inName = "input" + index.ToString("000000") + ".txt"
//    let outName = "output" + index.ToString("000000") + ".txt"
//            
//    use inSrcStream = 
//        new FileStream(inFolder + inName, FileMode.Open, 
//                        FileAccess.Read, FileShare.None, 
//                        bufferSize = 4096)
//    use outSrcStream = 
//        new FileStream(outFolder + outName, FileMode.Create, 
//                        FileAccess.Write, FileShare.None, 
//                        bufferSize = 4096)
//    use reader = new StreamReader(inSrcStream)
//    use writer = new StreamWriter(outSrcStream)
//
//    let coeffArray = 
//        reader.ReadLine().Split([|' '|], StringSplitOptions.RemoveEmptyEntries) 
//        |> Array.map float  
//            
//    let (a, b) = 
//        reader.ReadLine().Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
//        |> Array.map float
//        |> (fun arr -> if arr.Length <> 2 then failwith "nope lol" else (arr.[0], arr.[1]))
//                
//    let res = 
//        let integ x = 
//            Array.foldBack (fun e (s, i, pow) -> s + e * pow / i, i + 1., pow * x) coeffArray (0., 1., x) |> fst3
//        (integ b - integ a).ToString()
//
//    writer.Write(res)
//    
//sw.Stop()
//printfn "Slow worker took: %A" sw.Elapsed.TotalSeconds
//Console.ReadKey() |> ignore
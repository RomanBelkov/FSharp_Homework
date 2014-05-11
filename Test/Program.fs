open NUnit.Framework
open FsUnit

type LineElement =
    | Word of string
    | Space of int

let splitLines (s : string) =
    s.Split [| ' ' |]
let processData inputString = inputString |> splitLines |> Array.toList

let linesCreator list length =
    let rec loop currLine currLineLength (temp : string list) lines =
        match temp with
        | hd :: tail -> 
            let lengthLeft = length - currLineLength - hd.Length - 1
            match lengthLeft with
            | a when a >= 0 -> 
                if currLineLength = 0 then
                    loop (Word hd :: currLine) hd.Length tail lines
                else
                    loop (Word hd :: Space 1 :: currLine) (currLineLength + hd.Length + 1) tail lines
            | _ -> loop [] 0 temp ((Space (length - currLineLength) :: currLine) :: lines)
        | [] -> ((Space (length - currLineLength) :: currLine) :: lines)
    loop [] 0 list []
    |> List.rev
    |> List.map List.rev


let alignRight lines = 
    List.map (fun line -> Seq.last line :: line |> List.rev |> List.tail |> List.rev) lines

let alignWide lines =
    List.map (fun line -> 
        let spaces = List.map (function Space s -> s | _ -> 0) line |> List.sum
        let words = List.choose (function Word w -> Some w | _ -> None) line
        let n = words.Length
        
        if n = 1 then 
            line
        else
            let base' = spaces / (n - 1)
            let spacesLeft = spaces % (n - 1)
            Word (List.head words) :: (List.tail words
            |> List.mapi (fun i w -> (i, w) )
            |> List.collect(fun (i, w) -> [Space (base' + if i < spacesLeft then 1 else 0); Word w]))) lines 

let alignMid lines =
    List.map (fun (line : LineElement list) ->
        let el = Seq.last line
        let spaces = List.map (function Space s -> s | _ -> 0) [el] |> List.sum
        let firstS = spaces / 2
        let lastS = spaces - firstS
        let line'' = Space (firstS) :: line |> List.rev |> List.tail |> List.rev

        Space (lastS) :: (line'' |> List.rev) |> List.rev) lines


[<TestFixture>]
module test =
    [<Test>]
    let ``Creator`` () = 
        let inputString = "Invader flower inside the dark room"
        let lineLength = 10

        let res = linesCreator (processData inputString) lineLength 
        res |> should equal [[Word "Invader"; Space 3]; 
                             [Word "flower"; Space 4];
                             [Word "inside"; Space 1; Word "the"; Space 0];
                             [Word "dark"; Space 1; Word "room"; Space 1]]
    [<Test>]
    let ``AlignRight`` () =
        let inputString = "Invader flower inside the dark room"
        let lineLength = 10

        let res = linesCreator (processData inputString) lineLength |> alignRight
        res |> should equal [[Space 3; Word "Invader"]; 
                             [Space 4; Word "flower"];
                             [Space 0; Word "inside"; Space 1; Word "the"];
                             [Space 1; Word "dark"; Space 1; Word "room"]]
    [<Test>]
    let ``AlignWide`` () =
        let inputString = "Invader flower inside the dark room"
        let lineLength = 10

        let res = linesCreator (processData inputString) lineLength |> alignWide
        res |> should equal [[Word "Invader"; Space 3]; 
                             [Word "flower"; Space 4];
                             [Word "inside"; Space 1; Word "the"]; 
                             [Word "dark"; Space 2; Word "room"]]
    [<Test>]
    let ``AlignMid`` () =
        let inputString = "Invader flower inside the dark room"
        let lineLength = 10

        let res = linesCreator (processData inputString) lineLength |> alignMid
        res |> should equal [[Space 1; Word "Invader"; Space 2]; 
                            [Space 2; Word "flower"; Space 2];
                            [Space 0; Word "inside"; Space 1; Word "the"; Space 0];
                            [Space 0; Word "dark"; Space 1; Word "room"; Space 1]]
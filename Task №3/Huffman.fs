open NUnit.Framework
open FsUnit

module Huffman = 

    type CodeTree =
        | Branch of CodeTree * CodeTree * list<char> * int
        | Leaf of char * int

    let chars = function
            | Branch(_,_,chars,_) -> chars
            | Leaf(char,_) -> [ char ]

    let weight = function
            | Branch(_,_,_, weight)
            | Leaf(_, weight) -> weight

    let createCodeTree text =
        let rec combineTrees trees = 
            let makeCodeTree l r =
                Branch(l, r, chars l @ chars r, weight l + weight r)
            match trees with
                | fst :: snd :: rest -> combineTrees (makeCodeTree fst snd :: rest |> List.sortBy weight)
                | _ -> trees
        let orderedLeafList =
            text 
                |> Seq.countBy id
                |> Seq.sortBy snd
                |> Seq.map (fun f -> Leaf(fst f, snd f))
                |> List.ofSeq
        orderedLeafList
            |> combineTrees
            |> List.head

    let decode tree bits =
        let rec doDecode _tree bits chars =
            match _tree with
                | Branch(l, r, _, _)  ->
                    match bits |> List.head with
                        | 0 -> doDecode l bits.Tail chars
                        | 1 -> doDecode r bits.Tail chars
                        | _ -> failwith "Invalid bit in bits"
                | Leaf(c, _) when bits.IsEmpty -> c :: chars
                | Leaf(c, _) -> doDecode tree bits (c :: chars)
        doDecode tree bits [] |> List.rev

    let encode tree text = 
        let hasCharInBranch tree c =
          match tree with
            | Branch(_, _, cs, _) -> cs |> List.exists (fun _c -> _c = c)
            | Leaf(char, _) -> char = c

        let rec doEncode _tree (chars : list<char>) bits =
          if chars.IsEmpty then bits
          else
            match _tree with 
              | Branch(left, right, _, _) ->
                if hasCharInBranch left chars.Head then
                  doEncode left chars (0 :: bits)
                else
                  doEncode right chars (1 :: bits)
              | Leaf(c, _) -> doEncode tree chars.Tail bits

        doEncode tree text [] |> List.rev

[<TestFixture>]
module test =
    [<Test>]
    let ``test`` () =
        let src = "Test"
        let temp = Huffman.createCodeTree src
        let out = Huffman.encode temp (src |> List.ofSeq)
        Huffman.decode temp out |> should equal (src |> List.ofSeq)

    [<Test>]
    let ``Welcome to the jungle`` () =
        let src = "Welcome to the jungle"
        let temp = Huffman.createCodeTree src
        let out = Huffman.encode temp (src |> List.ofSeq)
        Huffman.decode temp out |> should equal (src |> List.ofSeq)

    [<Test>]
    let ``fail`` () =
        let src = "fail"
        let temp = Huffman.createCodeTree src
        let out = Huffman.encode temp (src |> List.ofSeq)
        (fun () -> Huffman.decode temp (out @ [2]) |> ignore)
            |> should throw typeof<System.Exception>
 (*
 Various list-oriented functions
 Belkov Roman, 2014
 *)
    
    //Counts numbers of elements present in the list given
    let rec length list =
        let rec loop list acc =
            match list with
                | [] -> acc
                | hd :: tl -> loop tl (acc + 1)
        loop list 0

    let testValue1 = length [2;3;4]
    printfn "%A" testValue1
    

    //Counts summary of list's elements
    let sum list =
       let rec loop list acc =
           match list with
               | hd :: tl -> loop tl (acc + hd)
               | [] -> acc
       loop list 0

    let testValue2 = sum [1; 2; 3; 4]
    printfn "%A" testValue2


    //Pretty obvious, isn't it?
    let rec pushToBack list number = 
        match list with
            | [] -> number :: []
            | hd :: tl -> hd :: pushToBack tl number

    let testValue3 = pushToBack [1;2;3;4] 5
    let testValue4 = pushToBack [] 5
    printfn "%A" testValue3
    printfn "%A" testValue4


    //Reverses the list
    let rec reverse list =
        let rec loop acc = function
                | [] -> acc
                | hd :: tl -> loop (hd :: acc) tl
        loop [] list

    let testList = reverse [1..10]
    printfn "%A" testList


    //Filters the list
    let filter func list =
        let rec loop acc = function
        | [] -> acc
        | hd :: tl ->
            match func hd with
            | true -> loop (hd :: acc) tl
            | false -> loop (acc) tl
        List.rev (loop [] list) //reverse to make the list look OK

    let filteredList = filter (fun x -> x % 3 = 1) [1..20]
    printfn "%A" filteredList


    //Generates a new list based on a list given
    let generator func list =
        let rec loop acc = function
        | [] -> acc
        | hd :: tl -> loop (func hd :: acc) tl
        List.rev (loop [] list)

    let newList = generator (fun x -> sqrt(x)) [1.0..10.0]
    printfn "%A" newList

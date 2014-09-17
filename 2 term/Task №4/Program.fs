let rec fib n =
    if n <= 2I then
        1
    else
        fib (n-1I) + fib (n - 2I)

let res = fib 30I   //mini-test
        

let fibFast (n : bigint) =
    let mem = new System.Collections.Generic.Dictionary<bigint, bigint> ()
    let rec fibCached n = 
        if mem.ContainsKey n then mem.[n]
        elif n <= 2I then 1I
        else
            let res = fibCached (n-1I) + fibCached (n-2I)
            mem.Add (n, res)
            res
    fibCached n

let res' = fibFast 100I    //mini-test


let src1 =  "no cm mwwwawwwewwmetetejeeeeee elwlcmllectloollwllcocwowdoocccejcwceocowommooo com mtaoommmemfedmeme eeceelao  em w m   om tt  ttwotueotootmcomt otjtoeoool o  c jo  ltcec cb yhccceewccoooo o  lomjooodo dwddodd daw eeedeeeejoedeeet ll   e     jectjojjjjjlmad aamatdoaaaa tpalmtmemmmmomemw  tenx".ToCharArray() |> Array.toList
let sub1 = "welcome to code jam".ToCharArray() |> Array.toList

let rec naiveFun = function
    | _, [] -> 1I
    | [], _ -> 0I
    | s :: ss as src,  (sb :: sbs as sub) -> 
        if s = sb then
            naiveFun (ss, sbs) + naiveFun (ss, sub)
        else
            naiveFun (ss, sub)

let res'' = naiveFun (src1, sub1)    // mini-test


let src1' = "oe ooc ljwwwwwwwwwwwimeweeweeeeeee eeeeecltllllllwllllecccclccccceccoccooooooooommomoommmommmmee mmemmeeee meeeeeeee     t      w t oa ttt ttttt oto toooooooootod  o   t  e     oc c  cclcc m ccccomeooooococodooododddddddddddeedleeeedeedeee oaee  e        j  jj jjjjejjjjjjojjaajaaaaaaalaaaeammajmmmmmmmm mmmlocoxek".ToCharArray() |> Array.toList
let sub1' = "welcome to code jam".ToCharArray() |> Array.toList

let fastFun src sub = 
    let memo = new System.Collections.Generic.Dictionary<(list<char> * list<char>), bigint> ()
    let rec supFun srcLst subLst = 
        if memo.ContainsKey (srcLst, subLst) then 
            memo.[(srcLst, subLst)]
        else
            let temp = 
                match (srcLst, subLst) with
                | _, [] -> 1I
                | [], _ -> 0I
                | s :: ss, sb :: sbs -> 
                    if s = sb then
                        supFun ss sbs + supFun ss subLst
                    else
                        supFun ss subLst
            memo.Add ((srcLst, subLst), temp)
            temp
    supFun src sub

let success = fastFun src1' sub1'

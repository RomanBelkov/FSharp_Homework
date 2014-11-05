module Checker

open System.Text.RegularExpressions

let isMail src =
    let pre = "[a-zA-Z+_][\w\-]*(\.[\-\w]+)*"
    let post = "([\w\-]+[\.])+((\w{2,3}|info|museum|travel|name|mobi|jobs|coop|asia|aero|marketing|sales|support|abuse|security|postmaster|hostmaster|usenet|webmaster))"
    let pattern = "^" + pre + "@" + post + "$" 
    Regex.Match(src, pattern).Success

//[<EntryPoint>]
//let main argv = 
//    let b = isMail "yo@domain.somedomain"
//    printfn "%A" b
//    0 
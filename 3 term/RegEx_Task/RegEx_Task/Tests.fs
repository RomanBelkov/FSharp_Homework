module CheckTests

open NUnit.Framework
open FsUnit
open Checker

[<TestFixture>]
module test = 
    [<Test>]
    let ``OK1`` () = 
        let input = "a@b.cc"
        let res = isMail input
        res |> should equal true

    [<Test>]
    let ``OK2`` () = 
        let input = "victor.polozov@mail.ru"
        let res = isMail input
        res |> should equal true

    [<Test>]
    let ``OK3`` () = 
        let input = "vector.vek@gmail.com"
        let res = isMail input
        res |> should equal true

    [<Test>]
    let ``OK4`` () = 
        let input = "my@domain.info"
        let res = isMail input
        res |> should equal true

    [<Test>]
    let ``OK5`` () = 
        let input = "_.1@mail.com"
        let res = isMail input
        res |> should equal true

    [<Test>]
    let ``OK6`` () = 
        let input = "paints_department@hermitage.museum"
        let res = isMail input
        res |> should equal true

    [<Test>]
    let ``NO1`` () = 
        let input = "a@b.c"
        let res = isMail input
        res |> should equal false

    [<Test>]
    let ``NO2`` () = 
        let input = "a..b@mail.ru"
        let res = isMail input
        res |> should equal false

    [<Test>]
    let ``NO3`` () = 
        let input = ".a@mail.ru"
        let res = isMail input
        res |> should equal false

    [<Test>]
    let ``NO4`` () = 
        let input = "yo@domain.somedomain"
        let res = isMail input
        res |> should equal false

    [<Test>]
    let ``NO5`` () = 
        let input = "1@mail.ru"
        let res = isMail input
        res |> should equal false

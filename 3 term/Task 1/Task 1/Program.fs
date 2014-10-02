type Creature(name) =
    let mutable alive = false

    member this.LifeStatus with get () = alive and set (value) = alive <- value
    member this.Name = name

    member this.Born() = alive <- true 
    abstract member Die : unit -> unit
    default this.Die() = alive <- false

type Human(name, surname) = 
    inherit Creature(name)

    member this.Surname = surname

    member this.Study() = printfn "nopenopenope"
    abstract member Greet : unit -> unit
    default this.Greet() = printfn "Hello there!"

type Democrat(name, surname, stBalance) =
    inherit Human(name, surname)
    let mutable money = stBalance

    override this.Die() = 
        printfn "Damn!"
        this.LifeStatus <- false

    member this.BankAccount = money
    member this.LoseMoney() value = money <- money - value 

type Redneck(name, surname) =
    inherit Human(name, surname)
    let mutable ammoCount = 20 
    let mutable money = 0
    
    override this.Greet() = printfn "Howdy, pardner"
    member this.Wallet = money
    member this.ShootAir() = 
        ammoCount <- ammoCount - 1
        printfn "Redneck is happy. He got only %A ammo left" ammoCount
    member this.FindMoney() value = money <- money + value
    member this.KillDemocrat (d : Democrat) = 
        d.Die()
        ammoCount <- ammoCount - 1
        this.FindMoney() d.BankAccount
        d.LoseMoney() d.BankAccount

let check (h : Human) = 
    match h with
    | :? Democrat -> printfn "Democrat"
    | :? Redneck -> printfn "Redneck"
    | _ -> ()

let printLifeStatus (x : Creature) = 
    match x.LifeStatus with
    | true -> printfn "The %A is alive" x.Name
    | false -> printfn "The %A is dead " x.Name

let Poke = new Creature("PsyDuck")
Poke.Born()
printLifeStatus Poke
Poke.Die()
printLifeStatus Poke

let RB = new Human("Roman", "Belkov")
RB.Born()
RB.Greet()
RB.Study()
printLifeStatus RB

let Bobby = new Redneck("Bob", "Woodleg")
Bobby.ShootAir()
Bobby.Greet()
Bobby.ShootAir()
Bobby.Born()
Bobby.ShootAir()

let Bill = new Democrat("Bill", "Clinton", 15000)
Bill.Born()
Bill |> printLifeStatus
Bobby.KillDemocrat(Bill)
Bill |> printLifeStatus
printfn "Bobby has %A dollars in his pocket" Bobby.Wallet
Bobby.ShootAir()

check Bill
check Bobby
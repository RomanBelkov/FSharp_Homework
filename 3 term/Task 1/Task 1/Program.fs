type Creature(name) =
    let mutable alive = false

    member this.LifeStatus = alive
    member this.Name = name

    member this.Born = alive <- true
    member this.Die = alive <- false

type Human(name, surname) = 
    inherit Creature(name)
    member this.Surname = surname

    member this.Study = printfn "nopenopenope"
    abstract member Greet : unit -> unit
    default this.Greet() = printfn "Hello there!"

type Redneck(name, surname) =
    inherit Human(name, surname)
    let mutable ammoCount = 20 
    
    override this.Greet() = printfn "Howdy, pardner"
    member this.Shoot = 
        ammoCount <- ammoCount - 1
        printfn "Redneck is happy. He got only %A ammo left" ammoCount


let printLifeStatus (x : Creature) = 
    match x.LifeStatus with
    | true -> printfn "The %A is alive" x.Name
    | false -> printfn "The %A is dead " x.Name


let Poke = new Creature("PsyDuck")
Poke.Born
printLifeStatus Poke
Poke.Die
printLifeStatus Poke

let RB = new Human("Roman", "Belkov")
RB.Born
RB.Greet()
RB.Study
printLifeStatus RB

let Bobby = new Redneck("Bob", "Woodleg")
Bobby.Shoot
Bobby.Greet()
Bobby.Shoot
Bobby.Born
Bobby.Shoot

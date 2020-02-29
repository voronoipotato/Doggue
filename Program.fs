// Learn more about F# at http://fsharp.org

open System
open Elmish
module Terminal = 
  type Msg = | ChangeValue of string
  type Element = Dialog of string * ( string -> unit ) | Rogue of char * (char -> unit)
module Program =
  let withTerminal program: Program<_,_,_,_> =
    let setState model dispatch = 
      let el = (Program.view program) model dispatch
      match el with
      | Terminal.Dialog (d, f) -> 
        Console.WriteLine(d)
        Console.ReadLine() |> f
      | Terminal.Rogue (c, f) -> Console.ReadKey(true).KeyChar  |> f
      
      ()
    program |> Program.withSetState setState


type Model = { Value : string }

//type Msg = | ChangeValue of string
let init () = { Value = "no way" }, Cmd.none

let update (msg:Terminal.Msg) (model: Model) =
  match msg with
  | Terminal.ChangeValue newValue ->
    { Value = newValue}, Cmd.none

let view model dispatch =
  //(Console.ReadKey(true)).KeyChar |> string |> Terminal.ChangeValue |> dispatch
  let s = (sprintf "Wow! %s" model.Value)
  Terminal.Dialog ( s , Terminal.ChangeValue >> dispatch )// (fun i -> dispatch (Terminal.ChangeValue i)))


[<EntryPoint>]
let main argv =
  Program.mkProgram init update view
  |> Program.withTerminal
  |> Program.run
  
  0
// Learn more about F# at http://fsharp.org

open System
open Elmish
module Terminal = 
  type Msg = | ChangeValue of string
  type Event = OnString of (string -> unit) | OnChar of (char -> unit)
  
  type Element = Dialog of string * Event | Rogue of string * (char -> unit)
module Program =
  let withTerminal program: Program<_,_,_,_> =
    let setState model dispatch = 
      let el = (Program.view program) model dispatch
      match el with
      | Terminal.Dialog (t, Terminal.OnString f) -> 
        Console.WriteLine(t)
        Console.ReadLine() |> f
      | Terminal.Dialog (t, Terminal.OnChar f) -> 
        Console.WriteLine(t)
        Console.ReadKey(true).KeyChar |> f
      | Terminal.Rogue (t, f) -> Console.ReadKey(true).KeyChar  |> f
      
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
  Terminal.Dialog ( s , Terminal.OnString (Terminal.ChangeValue >> dispatch) )// (fun i -> dispatch (Terminal.ChangeValue i)))


[<EntryPoint>]
let main argv =
  Program.mkProgram init update view
  |> Program.withTerminal
  |> Program.run
  
  0
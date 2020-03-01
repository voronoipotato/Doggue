// Learn more about F# at http://fsharp.org

open System
open Elmish
module Terminal = 
  type Msg = | TextInput of string | GameInput of char
  type OnString = (string -> unit)
  type OnChar = (char -> unit)
  type Width = int
  type Length = int
  type Position = {x: int; y: int}
  type Dimensions = Width * Length

  type Item = char * Position
  type Header = string
  type Element =  | Screen of Header * Element
                  | Dialog of Header * OnString
                  | CharInput of OnChar




module Program =
  let withTerminal program: Program<_,_,_,_> =
    let  setState model dispatch = 
      let el = (Program.view program) model dispatch
      let rec setState' el = 
        match el with
        | Terminal.Dialog (t, f) -> 
          Console.WriteLine(t)
          Console.ReadLine() |> f
        | Terminal.CharInput (f) -> 
          Console.ReadKey(true).KeyChar  |> f
        | Terminal.Screen (s, el) -> 
          Console.WriteLine(s)
          setState' el
      setState' el
      ()
    program |> Program.withSetState setState

module Game = 
  type Width = int 
  type Length = int
  type Position = {x: int; y: int}
  type Dimensions = Width * Length
  type Entity =   | Rogue of char
                  | Item of char
                  | Room of Dimensions * Entity
  type Model = { Name : string; Player : Position; Inventory: char list }

  let update (msg:Terminal.Msg) (model: Model) =
    match msg with
    | Terminal.TextInput newValue ->
      {model with Name = newValue}, Cmd.none
    | Terminal.GameInput c ->
      match c with 
      | 'w' -> {model with Player = {model.Player with y = model.Player.y + 1} } , Cmd.none
      | 'a' -> {model with Player = {model.Player with x = model.Player.x - 1} }, Cmd.none
      | 's' -> {model with Player = {model.Player with y = model.Player.y - 1} }, Cmd.none
      | 'd' -> {model with Player = {model.Player with x = model.Player.x + 1} }, Cmd.none
      | 'e' -> {model with Inventory = 'a' :: model.Inventory }, Cmd.none
      | _ -> model , Cmd.none

//Begin game
type Msg = | TextInput of string | Move of char | Interact
type Model = Game.Model
let init () = { Model.Name = "bork bork"; 
                Model.Player = {x = 0; y = 0}; 
                Model.Inventory = [] }, Cmd.none
let update = Game.update

// let update msg model = 
//   match msg with
//   | TextInput s -> (Game.update (Terminal.TextInput s) model)
//   | Move c -> (Game.update (Terminal.GameInput c) model)
//   | Interact -> (Game.update (Terminal.GameInput 'e') model)

let view model dispatch =

  //(Console.ReadKey(true)).KeyChar |> string |> Terminal.ChangeValue |> dispatch
  let doggo =  Game.Rogue 'd' 
  let gameInput = Terminal.CharInput (Terminal.GameInput >> dispatch)
  let bedroom = Game.Room
  let s = (sprintf "Player Position %A" model)
  Terminal.Screen (s, gameInput)


[<EntryPoint>]
let main argv =
  Program.mkProgram init update view
  |> Program.withTerminal
  |> Program.run
  
  0
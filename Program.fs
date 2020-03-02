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
          Console.ReadKey(true).KeyChar |> f
        | Terminal.Screen (s, el) ->
          Console.Clear()
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
  type Image = char
  type Name = string
  type Description = string
  type Attr = 
    | Attr
  type Entity =   
    | Start of Position
    | Item of Image * Description * Position
    | Room of Name * Dimensions * Entity list
    | Container of Image * Image list
    | Empty
  type Orientation = |North|East|West|South
  type Model = {  name : string; 
                  player : Position * Orientation; 
                  inventory: Image list; 
                  level: Entity }
  type KeyEvent = 
    | Up 
    | Down 
    | Left 
    | Right 
    | Interact
  module KeyEvent = 
    let fromOrientation = function
      |North -> Up
      |East  -> Right
      |West  -> Left
      |South -> Down
  let update (msg:Terminal.Msg) (model: Model) =
    let { name= name; 
          player = player, orientation; 
          inventory = inventory;
          level = level} = model
    let {x=x; y=y} = player
    let updateInteraction keyEvent =
      let move = function
        | Up ->   Some {player with y = y + 1}
        | Down -> Some {player with y = y - 1}
        | Left -> Some {player with x = x - 1}
        | Right ->Some {player with x = x + 1}
        | _ -> None

      let reorient  = function
          | Up ->    Some North
          | Down ->  Some South
          | Left ->  Some West
          | Right -> Some East
          | _ -> None

      let p,o =
        let p = move keyEvent |> Option.defaultValue player
        let o = reorient keyEvent |> Option.defaultValue orientation
        p, o
      let p',o' =
        match level with 
        | Room (_, (w,l), _) -> 
          { x = min w p.x |> max 0; 
            y = min l p.y |> max 0 }, o
        | _ -> p, o

      let i = 
        let check = KeyEvent.fromOrientation o
        
        match keyEvent with
        | Interact -> 'a' :: inventory
        | _ -> inventory
      {model with player = p',o'; inventory = i}, Cmd.none

    match msg with
    | Terminal.TextInput newValue ->
      {model with name = newValue}, Cmd.none
    | Terminal.GameInput c ->
      match c with 
      | 'w' -> updateInteraction Up 
      | 'a' -> updateInteraction Left
      | 's' -> updateInteraction Down
      | 'd' -> updateInteraction Right
      | 'e' -> updateInteraction Interact
      | _ -> model , Cmd.none

//Begin game
//type Msg = | TextInput of string | Move of char | Interact
type Model = Game.Model

let bedroom = Game.Room ("Bedroom", (4,4), [Game.Item ('s', "a sock", {x=0; y=0})])
let init () = { Game.name = ""; 
                Game.player = {x = 0; y = 0}, Game.North; 
                Game.inventory = []; 
                Game.level = bedroom}, Cmd.none
////not currently in use
// let update' msg model = 
//   match msg with
//   | TextInput s -> (Terminal.TextInput s), model
//   | Move c ->   (Terminal.GameInput c), model
//   | Interact -> (Terminal.GameInput 'e'), model
// let update msg model = update' msg model ||> Game.update

let view (model) dispatch =
  let origin: Game.Position = {x = 0; y = 0}
  let { Game.name = name; Game.level = room; } = model
  //pomeranian service dog
  let doggo = "d"
  //(Console.ReadKey(true)).KeyChar |> string |> Terminal.ChangeValue |> dispatch
  let nameEntry = Terminal.Dialog ("Enter your name:", Terminal.TextInput >> dispatch)
  let map = 
    let gameInput = Terminal.CharInput (Terminal.GameInput >> dispatch)
    let playerPosition = (sprintf "Player Position %A" model)
    Terminal.Screen (playerPosition, gameInput)
  let prettyName = (sprintf "Your name is %A" name) 
  if model.name = "" then
    nameEntry
  else
    map

[<EntryPoint>]
let main argv =
  Program.mkProgram init Game.update view
  |> Program.withTerminal
  |> Program.run
  
  0
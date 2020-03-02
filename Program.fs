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
                  | WriteAtPos of string * Position * Element

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
        | Terminal.WriteAtPos (s, p, el) ->
          Console.SetCursorPosition(p.x,p.y)
          Console.Write(s)
          setState' el
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
  type Entity =   
    | Item of Image * Description * Position
    | Container of Image * Image list * Position
  type Carryable = Bauble of Image * Description
  type Orientation = |North|East|West|South
  type Model = {  name : string; 
                  player : Position * Orientation; 
                  inventory: Carryable list; 
                  level: Name * Dimensions;
                  entities: Entity list}
  type KeyEvent = 
    | Up 
    | Down 
    | Left 
    | Right 
    | Interact
  let toTerminalPosition p : Terminal.Position= 
    {x=p.x; y=p.y}  
  module Entity =
    let getImage = function
      | Item(i,_,_) ->  i
      | Container (i ,l, p)-> i
  module Model = 
    let removeItem (m: Model) (p: Position)= 
      let isItem = function | Item (i,d,p') -> p' <> p  | _ -> true
      
      let filteredEntities = m.entities |> List.filter isItem 
      {m with entities = filteredEntities}


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
          level = level
          entities = entities} = model
    let {x=x; y=y} = player
    let updateInteraction keyEvent =
      let move = function
        | Up ->   Some {player with y = y - 1}
        | Down -> Some {player with y = y + 1}
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
        let p' = move keyEvent |> Option.defaultValue player
        let o' = reorient keyEvent |> Option.defaultValue orientation
        match level with 
        | _ , (w,l) ->
          { x = min w p'.x |> max 0; 
            y = min l p'.y |> max 0 }, o'

      let updateInventory m = 
        let tryGetItem e = 
          match e with
          | Item _ -> true
          | _ -> false
        let checkpos = KeyEvent.fromOrientation >> move
        let filterItem e = 
          let checkPosition = checkpos o
          match checkPosition, e with
          | Some x, Item (i,d,p) -> (x = p)
          | _  -> false

        let item = 
          entities
          |> List.filter tryGetItem 
          |> List.tryFind filterItem 
        let pickUp = function
            | Item (i,d,p)-> 
              let m' = {m with inventory = (Bauble (i,d)) :: inventory}
              Model.removeItem m' p
            | _ -> m
        match keyEvent with
        | Interact ->
          item 
          |> Option.map pickUp
          |> Option.defaultValue m
        | _ -> m
      let model = updateInventory model
      {model with player = p,o}, Cmd.none

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

type Model = Game.Model

let bedroom = ("Bedroom", (5,5))
let init () = { Game.name = ""; 
                Game.player = {x = 0; y = 0}, Game.North; 
                Game.inventory = []; 
                Game.level = bedroom;
                Game.entities = [Game.Item ('s', "a sock", {x=0; y=3}); Game.Container ('c', [], {x=1;y=1})]}, Cmd.none

let view (model) dispatch =
  let origin: Game.Position = {x = 0; y = 0}
  let { Game.name = name; Game.player = pos, ori; Game.level = room; Game.entities = entities} = model
  //pomeranian service dog
  let doggo = "d"
  let nameEntry = Terminal.Dialog ("Enter your name:", Terminal.TextInput >> dispatch)
  let map = 
    let gameInput =   Terminal.CharInput (Terminal.GameInput >> dispatch)
    let character = Terminal.WriteAtPos("d", Game.toTerminalPosition pos, gameInput)
    let entitiesToRender = entities |> List.map (function | Game.Item (i,d,p) -> i,p | Game.Container (i,l,p) -> i,p)
    let items = entitiesToRender |> List.fold (fun acc (i,p)  -> Terminal.WriteAtPos(string i, Game.toTerminalPosition p,acc)) character
    Terminal.Screen ("", items )
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
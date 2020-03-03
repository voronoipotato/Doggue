// Learn more about F# at http://fsharp.org

open System
open Elmish

let inline K x _ = x

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
  type Element =  
    | Screen of Header * Element
    | Dialog of Header * OnString
    | CharInput of OnChar
    | WriteAtPos of string * Position * Element

module Program =
  let withTerminal program: Program<_,_,_,_> =
    Console.CursorVisible <- false
    let  setState model dispatch = 
      let el = (Program.view program) model dispatch
      let rec setState' el = 
        match el with
        | Terminal.Dialog (t, f) ->
            Console.Write(t)
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

// type Width = int 
// type Length = int
type Position = {x: int; y: int}

type Dimensions = {width: int; length: int} //Width * Length
type Image = char
type Name = string
type Description = string
type Carryable = Bauble of Image * Description
module Carryable = 
  let getImage = function
  | Bauble (i,_) -> i

type Entity =   
  | Item of Image * Description * Position
  | Container of Image * Carryable list * Position
module Entity = 
  let getImage = function
    | Item(i,_,_) ->  i
    | Container (i,_,_)-> i
  let getPosition = function
    |Item (_,_,p) -> p
    | Container (_,_,p) -> p
type Orientation = |North|East|West|South

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

type Model = {  name : string; 
                player : Position * Orientation; 
                inventory: Carryable option; 
                level: Name * Dimensions;
                entities: Entity list}
module Model = 
  let removeItem (m: Model) (p: Position)= 
    let isItem = function 
      | Item (_,_,p') -> p' <> p  
      | _ -> true
    
    let filteredEntities = m.entities |> List.filter isItem 
    {m with entities = filteredEntities}


let toTerminalPosition p : Terminal.Position= 
  {x=p.x; y=p.y}  

let update (msg:Terminal.Msg) (model: Model) =
  let { name= name; 
        player = playerPosition, orientation; 
        inventory = inventory;
        level = level
        entities = entities } = model


  let updateInteraction keyEvent player =
    let {x=x; y=y} = player

    let move = function
      | Up   -> Some {player with y = y - 1}
      | Down -> Some {player with y = y + 1}
      | Left -> Some {player with x = x - 1}
      | Right ->Some {player with x = x + 1}
      | _ -> None

    let reorient  = function
        | Up    -> Some North
        | Down  -> Some South
        | Left  -> Some West
        | Right -> Some East
        | _ -> None

    //TODO: this should be a function
    let position,orientation =
      let _ , {width=w;length=l} = level
      let fitToRoom p = { 
          x = min w p.x |> max 0 
          y = min l p.y |> max 0 }
      let avoidObstacles newPosition =
        entities 
        |> List.map Entity.getPosition
        |> List.contains newPosition
        |> (fun hitObject -> if hitObject then player else newPosition )
      let newPosition = 
        move keyEvent 
        |> Option.defaultValue player
        |> fitToRoom
        |> avoidObstacles
      let newOrientation = reorient keyEvent |> Option.defaultValue orientation
      if (newPosition <> player) 
        then newPosition, newOrientation 
        else player, orientation
      
    //TODO write behavior for adding and removing items from containers
    //TODO write behavior for dropping items
    let updateInventory model = 
      let isItem  = 
        function
        | Item _ -> true
        | _ -> false
      let positionToCheck = KeyEvent.fromOrientation >> move
      let itemInPosition e = 
        let checkPosition = positionToCheck orientation
        match checkPosition, e with
        | Some x, Item (i,d,p) -> (x = p)
        | _  -> false

      let item = 
        entities
        |> List.filter isItem 
        |> List.tryFind itemInPosition 
      let pickUp = function
          | Item (i,d,p)-> 
            let bauble = Bauble (i,d)
            match inventory with
            | Some _ -> model
            | None  -> Model.removeItem {model with inventory = Some bauble} p
          | _ -> model
      match keyEvent with
      | Interact ->
        item 
        |> Option.map pickUp
        |> Option.defaultValue model
      | _ -> model
    let updatedModel = updateInventory model
    {updatedModel with player = position,orientation}, Cmd.none

  match msg with
  | Terminal.TextInput newValue ->
    {model with name = newValue}, Cmd.none
  | Terminal.GameInput c ->
    match c with 
    | 'w' -> updateInteraction Up playerPosition
    | 'a' -> updateInteraction Left playerPosition
    | 's' -> updateInteraction Down playerPosition
    | 'd' -> updateInteraction Right playerPosition
    | 'e' -> updateInteraction Interact playerPosition
    | _ -> model , Cmd.none

//Begin game


let init () = 
  let bedroom = ("Bedroom",{width=5;length=5})
  let initialPosition = {x = 0; y = 0}, North
  let sock = Item ('s', "a sock", {x=0; y=3})
  let sock' = Item ('s', "another sock", {x=2; y=3})
  let cabinet = Container ('c', [], {x=1;y=1})
  { name = ""; 
    player = initialPosition; 
    inventory = None; 
    level = bedroom;
    entities = [sock; sock'; cabinet]}, Cmd.none

let view (model) dispatch =
  let { name = name;
        player = pos, ori;
        inventory = currentInventory;
        level = (roomName, {width=roomWidth; length=roomLength} );
        entities = entities } = model
  //pomeranian service dog
  let doggo = "d"
  let nameEntry = Terminal.Dialog ("Enter your name: ", Terminal.TextInput >> dispatch)
  let map = 
    let floor el =  
      let floorMat = 
        let buildRow _ = 
          [1..roomLength] 
          |> Seq.map (K ".")
          |> Seq.reduce (+)
        let buildGrid l = 
          l
          |> Seq.map buildRow 
          |> Seq.reduce (sprintf "%s\n%s")
        buildGrid [1..roomWidth] 
      Terminal.Screen (floorMat, el)

    let gameInput = Terminal.CharInput (Terminal.GameInput >> dispatch)
    let character el = Terminal.WriteAtPos(doggo, toTerminalPosition pos, el)
    let entities el = 
      let getImageAndPosition = 
        function 
        | Item (i,_,p) -> i,p
        | Container (i,_,p) -> i,p
      let entitiesToRender = 
        entities 
        |> List.map getImageAndPosition
      let createElement acc (i,p) = 
        Terminal.WriteAtPos(string i, toTerminalPosition p,acc)
      
      entitiesToRender |> List.fold (createElement) el
    let inventory el = 
      let displayPosition = toTerminalPosition {x= roomLength + 2; y = 0}
      let inv =
        let getImageAndDescription = function | Bauble (i,d) -> (i,d)
        let displayImageAndDescription (i,d) = sprintf "%c: %s" i d
        match currentInventory with
        | None -> "[]"
        | Some inv -> 
            inv  
            |> (getImageAndDescription >> displayImageAndDescription)
            |> sprintf "[%s]"
      Terminal.WriteAtPos(inv, displayPosition, el )
    character gameInput 
    |> entities
    |> inventory 
    |> floor

  if model.name = "" then
    nameEntry
  else
    map

[<EntryPoint>]
let main argv =
  Program.mkProgram init update view
  |> Program.withTerminal
  |> Program.run
  
  0
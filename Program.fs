// Learn more about F# at http://fsharp.org

open System
open Elmish

let inline K x _ = x

module Terminal = 
  type OnString = (string -> unit)
  type OnChar = (char -> unit)
  type Position = {x: int; y: int}

  type Item = char * Position
  type Header = string
  type Element =  
    | Screen of Header * Element
    | Dialog of Header * OnString
    | CharInput of OnChar
    | PositionWrite of string * Position * Element

module Program =
  let withTerminal program: Program<_,_,_,_> =
    Console.CursorVisible <- false
    let setState model dispatch = 
      let el = (Program.view program) model dispatch
      let rec setState' el = 
        match el with
        | Terminal.Dialog (t, f) ->
            Console.Write(t)
            Console.ReadLine() |> f
        | Terminal.CharInput (f) -> 
            Console.ReadKey(true).KeyChar |> f
        | Terminal.PositionWrite (s, p, el) ->
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

type Msg = | Dialog of string | GameInput of char

type Position = {x: int; y: int}

type Dimensions = {width: int; length: int}
type Image = char
type Name = string
type Description = string
type Carryable = Bauble of Image * Description

type Entity =
  | Item of Image * Description * Position
  | Container of Image * Description * Carryable list * Position

module Carryable =
  let getImage = function
  | Bauble (i,_) -> i
  let fromItem i =
    match i with
    | Item (i,d,p) -> Ok (Bauble (i,d))
    | Container _ -> Error "you can't pick up a cabinet"
module Entity = 
  let getImage = function
    | Item(i,_,_) ->  i
    | Container (i,_,_,_) -> i
  let getPosition = function
    | Item (_,_,p) -> p
    | Container (_,_,_,p) -> p
  let getDescription = function
    | Item (_,d,_) -> d
    | Container (_,d,_,_) -> d
  let getImageAndPosition =
    function
    | Item (i,_,p) -> i,p
    | Container (i,_,_,p) -> i,p
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
  let pickUpItem (m: Model) (p: Position) c = 
    let isItem = function 
      | Item (_,_,p') -> p' <> p  
      | _ -> true
    let filteredEntities = m.entities |> List.filter isItem 
    {m with inventory=Some c;entities = filteredEntities}

  let placeItem m p (c: Carryable) =
    let (playerPosition,o) = m.player
    if playerPosition <> p then
      match c with 
      | Bauble (i,d) ->  {m with inventory = None; entities = Item(i,d,p) :: m.entities}
    else m


let toTerminalPosition p : Terminal.Position= 
  {x=p.x; y=p.y}  

let update (msg:Msg) (model: Model) =
  let { name= name; 
        player = playerPosition, orientation; 
        inventory = inventory;
        level = level
        entities = entities } = model

  let updateInteraction keyEvent player =
    let {x=x; y=y} = player
    let levelName, {width=w;length=l} = level

    let move = function
      | Up   ->  {player with y = y - 1}
      | Down ->  {player with y = y + 1}
      | Left ->  {player with x = x - 1}
      | Right -> {player with x = x + 1}
      | _ -> player

    let reorient  = function
        | Up    -> Some North
        | Down  -> Some South
        | Left  -> Some West
        | Right -> Some East
        | _ -> None
    let fitToRoom p = { 
      x = min (w-1) p.x |> max 0 
      y = min (l-1) p.y |> max 0 }

    //TODO: this should be a function
    let updatePosition keyEvent position =
      let avoidObstacles newPosition =
        entities 
        |> List.map Entity.getPosition
        |> List.contains newPosition
        |> (fun hitObject -> if hitObject then player else newPosition )

      let newPosition = 
        keyEvent
        |> move  
        |> fitToRoom
        |> avoidObstacles
      let newOrientation = reorient keyEvent |> Option.defaultValue orientation
      if (newPosition <> player) 
        then newPosition, newOrientation
        else player, orientation
    //TODO write behavior for adding and removing items from containers
    //TODO items should drop in front of doggo
    let updateInventory model = 
      let isItem  = 
        function
        | Item _ -> true
        | _ -> false
      let positionToCheck = orientation |> (KeyEvent.fromOrientation >> move >> fitToRoom)
      let itemInPosition e = 
        match positionToCheck, e with
        | x, Item (i,d,p) -> (x = p)
        | _  -> false

      let currentEntity = entities |> List.tryFind itemInPosition
      let putDown =
        match inventory, positionToCheck with
            | Some (carryable), p -> 
              Model.placeItem model p carryable
              //{model with inventory = None; entities = (Item (image,description,p)) :: entities}
            | None, _ -> model
      let pickUp = 
        function
          | Some (Item (image,description,position)) -> 
            let bauble = Bauble (image,description)
            match inventory with
            | Some _ -> model
            | None  -> Model.pickUpItem model position bauble
          | Some (Container (i,d,l,p)) ->
            //add code to fill container
            {model with inventory = None}
          | None -> 
            putDown
      match keyEvent with
      | Interact ->
        currentEntity 
        |> pickUp
      | _ -> model
    let updatedModel = updateInventory model
    {updatedModel with player = (updatePosition keyEvent player)}, Cmd.none

  match msg with
  | Dialog newValue ->
    {model with name = newValue}, Cmd.none
  | GameInput c ->
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
  let cabinet = Container ('c',"this ottoman reeks of old magazines",  [], {x=1;y=1})
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
  let ahead = 
    match ori with
      | North   ->  {pos with y = pos.y - 1}
      | South ->  {pos with y = pos.y + 1}
      | West ->  {pos with x = pos.x - 1}
      | East -> {pos with x = pos.x + 1}
  let map =
    let floor el = 
      let floorMat =
        let buildRow _ = [1 .. roomLength] |> Seq.map (K ".") |> Seq.reduce (+)
        let buildGrid l = l |> Seq.map buildRow |> Seq.reduce (sprintf "%s\n%s")
        buildGrid [1..roomWidth]
      Terminal.Screen (floorMat, el)

    let gameInput = Terminal.CharInput (GameInput >> dispatch)
    let character el = Terminal.PositionWrite(doggo, toTerminalPosition pos, el)

    let showEntities el =
      let entitiesToRender = entities |> List.map Entity.getImageAndPosition
      let createElement acc (i,p) =
        Terminal.PositionWrite(string i, toTerminalPosition p,acc)
      entitiesToRender |> List.fold (createElement) el
    let showInventory el =
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
      Terminal.PositionWrite(inv, displayPosition, el )
    let information el = 
      let filterEntities (e: Entity) =
        let p = ahead
        match e with
        | Item(i,d,p') -> p = p'
        | Container (i,d,l,p') -> p=p'
      let desc = 
        entities 
        |> List.filter filterEntities
        |> List.map (Entity.getDescription)
        |> List.tryHead
      let message = desc |> Option.defaultValue ""
      Terminal.PositionWrite(sprintf "Description: %s" message, toTerminalPosition {x= roomLength + 2; y = 1},el )
    character gameInput |> showEntities |> showInventory |> information |> floor

  let nameEntry = Terminal.Dialog ("Enter your name: ", Dialog >> dispatch)

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
// Learn more about F# at http://fsharp.org

open System
open Elmish

module Program =
  type AppState = {
    render : unit -> string
    setState : AppState -> unit
  }

  let mutable appState = None
  let withConsole program: Program<_,_,_,_> =
    let setState model dispatch = 
      let render _ = 
        let (s: string) = (Program.view program) model dispatch
        Console.WriteLine(s)
        s

      match appState with
      | Some state -> state.setState {state with render = render}
      | _ -> appState <- Some { render = render
                                setState = ignore }
      render () |> ignore
    program |> Program.withSetState setState


type Model = { Value : string }

type Msg = | ChangeValue of string
let init () = { Value = "" }, Cmd.none

let update (msg:Msg) (model: Model) =
  match msg with
  | ChangeValue newValue ->
    { Value = newValue}, Cmd.none

let view model dispatch =
  //ChangeValue "test" |> dispatch
  sprintf "Wow! %s" model.Value

[<EntryPoint>]
let main argv =
  Program.mkProgram init update view
  //|> Program.withConsoleTrace
  |> Program.withConsole
  |> Program.run

  
  0
    // let model = {Player= {Name = ""}; Equipment= [] }
    

    // let inline k x _ = x 
    // let f input model = model
    // repl 0<s> model f
    // 0 // return an integer exit code
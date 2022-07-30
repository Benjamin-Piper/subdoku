module App

open Elmish
open Feliz



(* TUTORIAL: Messages are developer-defined events that update State. *)
type Msg =
  | Increment
  | Decrement


(* TUTORIAL: The State (aka Model) tracks data while the app runs. *)
type State = { count: int }
let init () = { count = 0 }, Cmd.none


(* 
  TUTORIAL: Update takes the current Message and State and updates the State.
  Optionally one can trigger a Command after the State has been updated.
*)
let update (msg: Msg) (state: State) : State * Cmd<Msg> =
  match msg with
  | Increment -> { state with count = state.count + 1 }, Cmd.none
  | Decrement -> { state with count = state.count - 1 }, Cmd.none


(*
  TUTORIAL: Render (ak View) takes the state nd builds the interface.
  User input may trigger/dispatch Messages.
*)
let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    Html.h1 (state.count |> string)
    Html.div [
      prop.children [
        Html.button [
          prop.className "p-2 border-2 border-black m-1"
          prop.text "Increment" 
          prop.onClick (fun _ -> dispatch Increment)
        ]
        Html.button [
          prop.className "p-2 border-2 border-black m-1"
          prop.text "Decrement" 
          prop.onClick (fun _ -> dispatch Decrement)
        ]
      ]
    ]
  ]
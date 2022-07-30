module App

open Elmish
open Feliz
open System



(* FUNCTIONS & HELPER TYPES *)



/// Returns a random sequence of unique Digits where one of them is None.
let setupLine () =
  let rng = new Random()
  let lineLength = 9
  (* 
    TUTORIAL: An option<int> can either be (Some int) or None.
    It's comparable to int?, Maybe Int, Optional<Integer> etc.
  *)
  let output = ResizeArray<option<int>>()
  (* 
    TUTORIAL: Everything in F# is immutable by default.
    So we use mutable to explicitly indicate otherwise.
  *)
  let mutable orderedDigits = [| 1 .. lineLength |]
  (*
    TUTORIAL: F# functions are curried.
    Functions only have one argument.
    Functions return another function if there are more inputs.
    We create function getDigitByIndex using function Array.get.
  *)
  let getDigitByIndex = Array.get orderedDigits
  let digitToBeReplaced = getDigitByIndex (rng.Next lineLength)

  for currentLength in lineLength .. -1 .. 1 do
    let randomIndex = rng.Next currentLength
    let randomDigit = getDigitByIndex randomIndex
    orderedDigits <- Array.removeAt randomIndex orderedDigits
    if randomDigit = digitToBeReplaced
    then output.Add None
    else output.Add (Some randomDigit)

  output.ToArray()


/// Checks all digits in the submitted line are unique.
let checkLine (line: array<option<int>>) =
  (*
    TUTORIAL: The pipe operator |> works like Unix pipe |.
    It pipes the output from the left to the function on the right.
  *)
  let set =
    line
    (* TUTORIAL: Array.choose removes None, so the function just returns the input. *)
    |> Array.choose (fun x -> x)
    |> Set.ofArray

  (Array.length line) = (Set.count set)


(* TUTORIAL: Discriminated unions are not enums. The former don't require a default type/value. *)
type Submission =
  | Correct
  | Incorrect
  | Unsubmitted



(* ELMISH ARCHITECTURE *)



(* TUTORIAL: Messages are developer-defined events that update State. *)
type Msg =
  (* 
    TUTORIAL: Cases Submit, Display have their own sets of fields.
    There can be no Submit without an option<int> array, no Display without a Submission type.
  *)
  | Submit of array<option<int>>
  | Display of Submission
  | Refresh


(* TUTORIAL: The State (aka Model) tracks data while the app runs. *)
type State =
  { 
    blankIndex: int
    line: array<option<int>>
    status: Submission
  }
// The initial value doesn't matter because we trigger Refresh
let init () =
  {
    blankIndex = 0
    line = [||]
    status = Unsubmitted
  },
  Cmd.ofMsg Refresh


(* 
  TUTORIAL: Update takes the current Message and State and updates the State.
  Optionally one can trigger a Command after the State has been updated.
*)
let update (msg: Msg) (state: State) : State * Cmd<Msg> =
  (* TUTORIAL: Pattern matching with match is like using a switch statement but way cooler. *)
  match msg with
  | Submit newLine ->
    (*
      TUTORIAL: Nearly everything in F# is an expression.
      The following evaluates to a particular Submission and is bound to submission.
    *)
    let submission =
      if checkLine newLine
      then Correct
      else Incorrect
    { state with line = newLine }, Cmd.ofMsg (Display submission)
  
  | Display submission ->
    let delayMsg msg =
      async {
        do! Async.Sleep 500
        return msg
      }
    match submission with
    | Correct -> { state with status = Correct }, Cmd.OfAsync.result (delayMsg Refresh)
    (* TUTORIAL: Backwards pipe <| is used here instead of () *)
    | Incorrect -> { state with status = Incorrect }, Cmd.OfAsync.result <| delayMsg (Display Unsubmitted)
    | Unsubmitted ->
      let newLine = Array.updateAt state.blankIndex None state.line
      { state with line = newLine; status = Unsubmitted }, Cmd.none
    
  | Refresh ->
    let newLine = setupLine()
    {
      state with
        blankIndex = Array.findIndex (fun (x: option<int>) -> x.IsNone) newLine
        line = newLine
        status = Unsubmitted
    },
    Cmd.none


(*
  TUTORIAL: Render (aka View) takes the state and builds the interface.
  User input may trigger/dispatch Messages.
*)
let render (state: State) (dispatch: Msg -> unit) =
  Html.main [
    prop.className "max-w-7xl mx-auto outline-0"
    prop.children [

      Html.ul [
        prop.classes [
          "text-8xl text-center place-content-evenly"
          "md:mt-32 md:flex"
        ]
        prop.children [
          for index, item in Array.indexed state.line ->
            Html.li [
              prop.classes [
                (if index = state.blankIndex then "font-bold" else "")
                // Decided to set colours at ul to reduce style inheritance.
                match state.status with
                | Correct -> "text-green-600"
                | Incorrect -> "text-red-600"
                | Unsubmitted -> ""
              ]
              prop.text (if item.IsSome then string item.Value else "-")
            ]
        ]
      ]

      Html.div [
        prop.className "text-2xl w-64 mt-16 mx-auto grid grid-cols-3"
        prop.children [
          for digit in 1 .. Array.length state.line ->
            Html.button [
              let isMarked = state.status <> Unsubmitted
              prop.classes [
                "py-4 border-2 border-black m-1"
                (if isMarked then "opacity-30" else "")
              ]
              prop.disabled isMarked
              prop.text digit
              prop.onClick (fun _ ->
                let newLine = Array.updateAt state.blankIndex (Some digit) state.line
                dispatch (Submit newLine)
              )
            ]
        ]
      ]
    ]
  ]
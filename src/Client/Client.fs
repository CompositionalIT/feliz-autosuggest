module Client

open Elmish
open Elmish.React
open Thoth.Fetch

open Shared

type Model =
    { Hello: string }

type Msg =
    | GotHello of string

let init() =
    let model : Model =
        { Hello = "" }
    let getHello() = Fetch.get<unit, string> Route.hello
    let cmd = Cmd.OfPromise.perform getHello () GotHello
    model, cmd

let update msg model =
    match msg with
    | GotHello hello ->
        { model with Hello = hello }, Cmd.none

open Fable.React
open Fable.React.Props

let view model dispatch =
    div [ Style [ TextAlign TextAlignOptions.Center; Padding 40 ] ] [
        div [] [
            img [ Src "favicon.png" ]
            h1 [] [ str "autosuggest" ]
            h2 [] [ str model.Hello ]
        ]
    ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run

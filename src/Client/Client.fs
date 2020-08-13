module Client

open Elmish
open Elmish.React
open Fable.Core
open Fable.Core.JsInterop
open Feliz

type IAutoSuggestProp = interface end

[<Erase>]
type AutoSuggest() =
    static member inline create props = Interop.reactApi.createElement (importDefault "react-autosuggest", createObj !!props)

    static member inline suggestions<'T>(items:seq<'T>) = Interop.mkAttr "suggestions" (Seq.toArray items)
    static member inline getSuggestionValue<'T> (getter:'T -> string) = Interop.mkAttr "getSuggestionValue" getter
    static member inline onSuggestionsFetchRequested<'T>(updater:'T -> unit) = Interop.mkAttr "onSuggestionsFetchRequested" (fun s -> updater s?value)
    static member inline onSuggestionsClearRequested (clear:unit -> unit) = Interop.mkAttr "onSuggestionsClearRequested" clear
    static member inline renderSuggestion<'T>(renderer:'T -> ReactElement) = Interop.mkAttr "renderSuggestion" renderer

    static member inline inputProps (props:IAutoSuggestProp seq) = Interop.mkAttr "inputProps" (createObj !!props)
    static member inline value (value:string) = prop.value value |> unbox<IAutoSuggestProp>
    static member inline onChange (handler: string -> unit) = Interop.mkAttr "onChange" (System.Func<_,_,unit> (fun _ event -> handler event?newValue)) |> unbox<IAutoSuggestProp>

type Suggestion = {| Original : string; Text : string |}

type Model =
    { Text: string
      AllTenants : Suggestion array
      Suggestions : Suggestion array }

type Msg =
    | TextChanged of string
    | TenantsLoaded of string array
    | ClearSuggestions
    | UpdateSuggestions of string

let findSuggestions (allSuggestions:Suggestion array) (text:string) =
    allSuggestions
    |> Seq.filter(fun s -> s.Text.Contains (text.ToLower()))
    |> Seq.truncate 10
    |> Seq.toArray

let init() =
    let model = { Text = ""; Suggestions = Array.empty; AllTenants = Array.empty }
    let getTenants () = Thoth.Fetch.Fetch.get<_, string array> "api/tenant"
    model, Cmd.OfPromise.perform getTenants () TenantsLoaded

let update msg (model:Model) =
    let model =
        match msg with
        | TextChanged text -> { model with Text = text }
        | ClearSuggestions -> { model with Suggestions = Array.empty }
        | UpdateSuggestions text -> { model with Suggestions = findSuggestions model.AllTenants text }
        | TenantsLoaded t -> { model with AllTenants = t |> Array.map(fun s -> {| Original = s; Text = s.ToLower() |}) }
    { model with Text = if isNull model.Text then "" else model.Text }, Cmd.none

open Fable.React
open Fable.React.Props

let view model dispatch =
    div [] [
        img [ Src "favicon.png" ]
        h1 [] [ str "autosuggest" ]
        AutoSuggest.create [
            AutoSuggest.suggestions model.Suggestions

            AutoSuggest.getSuggestionValue(fun (s:Suggestion) -> s.Text)
            AutoSuggest.renderSuggestion(fun (s:Suggestion) -> Html.span s.Original)

            AutoSuggest.onSuggestionsFetchRequested (UpdateSuggestions >> dispatch)
            AutoSuggest.onSuggestionsClearRequested (fun () -> dispatch ClearSuggestions)

            AutoSuggest.inputProps [
                AutoSuggest.value model.Text
                AutoSuggest.onChange (TextChanged >> dispatch)
            ]
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

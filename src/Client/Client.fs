module Client

open Elmish
open Elmish.React
open Fable.Core
open Fable.Core.JsInterop
open Feliz

/// Represents a prop for the Auto Suggester
type IAutoSuggestProp = interface end

/// Creates an Auto Suggester tied to a specific type of value used to bind to suggestions with.
[<Erase>]
type AutoSuggest<'T> =
    static member inline create props = Interop.reactApi.createElement (importDefault "react-autosuggest", createObj !!props)
    static member inline suggestions(items:seq<'T>) = Interop.mkAttr "suggestions" (Seq.toArray items)
    static member inline getSuggestionValue(getter:'T -> string) = Interop.mkAttr "getSuggestionValue" getter
    static member inline onSuggestionsFetchRequested(updater:string -> unit) = Interop.mkAttr "onSuggestionsFetchRequested" (fun s -> updater s?value)
    static member inline onSuggestionsClearRequested (clear:unit -> unit) = Interop.mkAttr "onSuggestionsClearRequested" clear
    static member inline renderSuggestion(renderer:'T -> ReactElement) = Interop.mkAttr "renderSuggestion" renderer
    static member inline inputProps (props:IAutoSuggestProp seq) = Interop.mkAttr "inputProps" (createObj !!props)
    static member inline value (value:string) = prop.value value |> unbox<IAutoSuggestProp>
    static member inline onChange (handler: string -> unit) = Interop.mkAttr "onChange" (System.Func<_,_,unit> (fun _ event -> handler event?newValue)) |> unbox<IAutoSuggestProp>

type Tenant = {| Original : string; Text : string |}

type Model =
    { Text: string
      AllTenants : Tenant array
      Suggestions : Tenant array }

let findSuggestions (allSuggestions:Tenant array) (text:string) =
    allSuggestions
    |> Seq.filter(fun s -> s.Text.Contains (text.ToLower()))
    |> Seq.truncate 10
    |> Seq.toArray

let init() =
    let model = { Text = ""; Suggestions = Array.empty; AllTenants = Array.empty }
    let getTenants () = Thoth.Fetch.Fetch.get<_, string array> "api/tenant"
    let handleLoad tenants model =
        { model with
            AllTenants =
                tenants
                |> Array.map(fun tenant ->
                    {| Original = tenant
                       Text = tenant.ToLower() |}
                )
        }
    model, Cmd.OfPromise.perform getTenants () handleLoad

let update update (model:Model) =
    { update model with
        Text = if isNull model.Text then "" else model.Text }, Cmd.none

open Fable.React
open Fable.React.Props

type TenantSuggester = AutoSuggest<Tenant>
let view model dispatch =
    div [] [
        img [ Src "favicon.png" ]
        h1 [] [ str "autosuggest" ]
        TenantSuggester.create [
            TenantSuggester.suggestions model.Suggestions

            TenantSuggester.getSuggestionValue(fun s -> s.Original)
            TenantSuggester.renderSuggestion(fun s -> Html.span s.Original)

            TenantSuggester.onSuggestionsFetchRequested (fun text -> dispatch (fun model -> { model with Suggestions = findSuggestions model.AllTenants text }))
            TenantSuggester.onSuggestionsClearRequested (fun () -> dispatch (fun model -> { model with Suggestions = Array.empty }))

            TenantSuggester.inputProps [
                TenantSuggester.value model.Text
                TenantSuggester.onChange (fun text -> dispatch (fun model -> { model with Text = text }))
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

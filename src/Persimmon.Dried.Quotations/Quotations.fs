module Persimmon.Dried.Quotations

open System.Reflection
open Persimmon
open Persimmon.Dried
open Microsoft.FSharp.Quotations
open Patterns

let private (|PropertyName|) (info: PropertyInfo) = info.Name
let private (|Property|) (info: PropertyInfo) = (info.GetValue(null, null), info.Name)

type QuotationPropertiesBuilder private (builder: PropertiesBuilder) =
  new() = QuotationPropertiesBuilder(PropertiesBuilder())
  new(name: string) = QuotationPropertiesBuilder(PropertiesBuilder(name))
  member __.Yield(()) = builder.Yield(())
  [<CustomOperation("verbosity")>]
  member __.Verbosity(s, v) = builder.Verbosity(s, v)
  [<CustomOperation("minSuccessfulTests")>]
  member __.MinSuccessfulTests(s, v) = builder.MinSuccessfulTests(s, v)
  [<CustomOperation("minSize")>]
  member __.MinSize(s, v) = builder.MinSize(s, v)
  [<CustomOperation("maxSize")>]
  member __.MaxSize(s, v) = builder.MaxSize(s, v)
  [<CustomOperation("prngState")>]
  member __.PrngState(s, v) = builder.PrngState(s, v)
  [<CustomOperation("workers")>]
  member __.Workers(s, v) = builder.Workers(s, v)
  [<CustomOperation("callback")>]
  member __.Callback(s, v) = builder.Callback(s, v)
  [<CustomOperation("maxDiscardRatio")>]
  member __.MaxDiscardRatio(s, v) = builder.MaxDiscardRatio(s, v)
  [<CustomOperation("apply")>]
  member __.Apply<'T, 'U when 'U :> Prop>(s: PropertiesState<'T>, [<ReflectedDefinition(true)>] expr: Expr<'U>) =
    let p =
      match expr with
      | ValueWithName(p, _, name) 
      | WithValue(p, _, PropertyGet(Some(PropertyGet (None, PropertyName name, [])), _, []))
      | WithValue(p, _, ValueWithName(_, _, name))
      | WithValue(p, _, Call(_, _, [PropertyGet(_, PropertyName name, _)]))
      | WithValue(p, _, PropertyGet(_, PropertyName name, _))
      | PropertyGet(_, Property(p, name), _) ->
        p :?> Prop |@ name
      | WithValue(p, _, _)
      | Value(p, _) -> p :?> Prop
      | _ -> failwithf "expected value, but was %A" expr
    { s with Properties = seq { yield! s.Properties; yield p } }
  [<CustomOperation("applyReturn")>]
  member __.ApplyReturn(s, [<ReflectedDefinition(true)>] expr: Expr<Prop<'T>>) =
    let p =
      match expr with
      | ValueWithName(p, _, name)
      | WithValue(p, _, PropertyGet(Some(PropertyGet (None, PropertyName name, [])), _, []))
      | WithValue(p, _, ValueWithName(_, _, name))
      | WithValue(p, _, Call(_, _, [PropertyGet(_, PropertyName name, _)]))
      | WithValue(p, _, PropertyGet(_, PropertyName name, _))
      | PropertyGet(_, Property(p, name), _) ->
        let p = p :?> Prop<'T>
        new Prop<'T>(p.Sample, p |@ name)
      | WithValue(p, _, _)
      | Value(p, _) -> p :?> Prop<'T>
      | _ -> failwithf "expected value, but was %A" expr
    {
      RunnerParams = s.RunnerParams
      PrettyParams = s.PrettyParams
      Properties = seq { yield! s.Properties; yield p :> Prop }
      Sample = p.Sample
    }
  member __.Delay(f: unit -> _) = f
  member __.Run(f) = builder.Run(f)

let property (name: string) = QuotationPropertiesBuilder(name)

module UseTestNameByReflection =
  let property = QuotationPropertiesBuilder()

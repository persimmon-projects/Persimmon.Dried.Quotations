namespace Persimmon.Dried.Quotations.Tests

open System
open Persimmon
open Persimmon.Dried

module QuotationsTest =

  open Quotations
  open UseTestNameByReflection

  let ``number is zero`` = Prop.forAll Arb.int ((=) 0)

  type QuotationPropertiesBuilder with
    [<CustomOperation("test")>]
    member __.Test(s: PropertiesState<_>, f) =
      let meta = { Name = None; Parameters = [] }
      TestCase(meta, fun () ->
        let p = Prop.all s.Properties
        let r = Runner.check s.RunnerParams p
        let pr = p.Apply(Gen.Parameters.Default)
        match assertPred <| f r pr with
        | Passed () -> Done(meta, NonEmptyList.singleton (Passed ()), TimeSpan.Zero)
        | NotPassed cause -> Done(meta, NonEmptyList.singleton (NotPassed cause), TimeSpan.Zero)
      )
    member __.Run(f: unit -> TestCase<_>) =
      try f ()
      with e -> TestCase.makeError None [] e

  let ``record variable name`` = property {
    apply ``number is zero``
    test (fun r pr -> not <| Runner.Result.isPassed r && pr.Labels |> Seq.exists ((=) "number is zero"))
  }

  let ``record variable name and return value`` = property {
    applyReturn ``number is zero``
    test (fun r pr -> not <| Runner.Result.isPassed r && pr.Labels |> Seq.exists ((=) "number is zero"))
  }

  let ``value does not have label`` = property {
    apply (Prop.forAll Arb.int ((=) 0))
    test (fun r pr -> not <| Runner.Result.isPassed r && Set.isEmpty pr.Labels)
  }

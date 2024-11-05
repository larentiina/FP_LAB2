module PropertyTests

open System
open Xunit
open Dict
open FsCheck
open FsCheck.Xunit
let genEntry (genKey: Gen<'Key>) (genValue: Gen<'Value>) : Gen<Entry<'Key, 'Value>> =
    gen {
        let! key = genKey
        let! value = genValue
        return { Key = key; Value = value }
    }

let hash (key: 'Key) (size: int) : int =
    match box key with
    | null -> 0
    | _ -> abs (key.GetHashCode() % size)


let genHashMap (genKey: Gen<'Key>) (genValue: Gen<'Value>) (size: int) : Gen<HashMap<'Key, 'Value>> =
    gen {
        let hashMap = emptyMap
        let! entries = Gen.listOf (genEntry genKey genValue)
        return entries |> List.fold (fun acc entry -> put entry.Key entry.Value acc) hashMap
    }

let arbHashMap<'Key, 'Value when 'Key : comparison> () : Arbitrary<HashMap<'Key, 'Value>> =
    Arb.fromGen (genHashMap Arb.generate Arb.generate 5)

type HashMapGenerators =
    static member HashMapArbitrary<'Key, 'Value when 'Key : comparison>() : Arbitrary<HashMap<'Key, 'Value>> =
        arbHashMap()

Arb.register<HashMapGenerators>() |> ignore

[<Property(Arbitrary = [| typeof<HashMapGenerators> |])>]
let prop_emptyMerge (map: HashMap<int, string>) =
    let emptyMap = emptyMap
    let mergedMap = merge map emptyMap
    equal map mergedMap 


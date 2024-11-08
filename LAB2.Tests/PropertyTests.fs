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

let genHashMap (genKey: Gen<'Key>) (genValue: Gen<'Value>) : Gen<HashMap<'Key, 'Value>> =
    gen {
        let hashMap = emptyMap
        let! entries = Gen.listOf(genEntry genKey genValue)
        return entries |> List.fold (fun acc entry -> put entry.Key entry.Value acc) hashMap
    }

let arbHashMap<'Key, 'Value when 'Key : comparison> () : Arbitrary<HashMap<'Key, 'Value>> =
    Arb.fromGen (genHashMap Arb.generate Arb.generate) 

type HashMapGenerators =
    static member HashMapArbitrary<'Key, 'Value when 'Key : comparison>() : Arbitrary<HashMap<'Key, 'Value>> =
        arbHashMap()

Arb.register<HashMapGenerators>() |> ignore


let reinsertAll (hashMap: HashMap<'Key, 'Value>) : HashMap<'Key, 'Value> =
    let empty = emptyMap
    Array.fold (fun acc chain ->
        List.fold (fun acc' entry ->
            put entry.Key entry.Value acc') acc chain) empty hashMap

[<Property(Arbitrary = [| typeof<HashMapGenerators> |])>]
let prop_emptyMerge (map: HashMap<int, int>) =
    let map = reinsertAll map
    let mergedMap = merge map emptyMap
    equal map mergedMap 


[<Property(Arbitrary = [| typeof<HashMapGenerators> |])>]
let prop_association (map1: HashMap<int, int>) (map2:HashMap<int, int> ) (map3:HashMap<int, int> ) =
    let mergedMap1 = merge(merge map1 map2) map3
    let mergedMap2 = merge (merge map1 map3) map2
    equal mergedMap1 mergedMap2 

[<Property>]
let prop_equalMaps (key1: int) (value1: int) (key2: int) (value2: int) =
    key1 <> key2 ==> lazy (
        let map1 = emptyMap |> put key1 value1 |> put key2 value2
        let map2 = emptyMap |> put key2 value2 |> put key1 value1
        equal map1 map2
    )

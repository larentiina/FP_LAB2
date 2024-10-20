module PropertyTests

open System
open Xunit
open FsCheck
open Dict

[<Fact>]
let ``Test merge HashMap and emptymap with random key`` () =
    let key = System.Random().Next(0, 100) 
    let value = "testValue" 

    let originalMap = put key value emptyMap
    let mergedMap = merge originalMap emptyMap

    printfn "Testing merge of HashMap with emptyMap. Key: %d, Value: %s" key value
    Assert.Equal<HashMap>(originalMap, mergedMap)


[<Fact>]
let ``Test merge order of HashMaps`` () =
    let testData = [
        (1, "value1", 2, "value2")
        (3, "value3", 4, "value4")
        (5, "value5", 6, "value6")
    ]

    for (key1, value1, key2, value2) in testData do
    
        let map1 = put key1 value1 emptyMap
        let map2 = put key2 value2 emptyMap

        let mergedMap1 = merge map1 map2
        let mergedMap2= merge map2 map1
        printfn "Testing merge order. Key1: %d, Value1: %s; Key2: %d, Value2: %s" key1 value1 key2 value2

        Assert.Equal<HashMap>(mergedMap1, mergedMap2)

[<Fact>]
let ``Test removal from HashMap`` () =
    let testData = [
        (1, "value1")
        (2, "value2")
        (3, "value3")
    ]

    let map =
        testData
        |> List.fold (fun acc (key, value) -> put key value acc) emptyMap

    for (key, value) in testData do
        let updatedMap = remove key map

        let retrievedValue = get key updatedMap
        printfn "Testing removal. Key: %d" key
        Assert.Null(retrievedValue)

        for (k, v) in testData do
            if k <> key then
                let remainingValue = get k updatedMap
                Assert.Equal(Some v, remainingValue)
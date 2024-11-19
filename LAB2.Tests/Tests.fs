module Tests

open System
open Xunit
open FsCheck
open Dict

[<Fact>]
let ``Put Test`` () =
    let map = emptyMap
    let map1 = put 1 "value1" map
    let map2 = emptyMap |> put "second" "value1" 
   

    let result1 = get 1 map1
    let result2 = get "second" map2
    Assert.Equal(result1, result2)

[<Fact>]
let ``Remove Test`` () =
    let map = emptyMap |> put 1 "value1" |> put 2 "value2"
    let updatedMap = remove 1 map
    let result = get 1 updatedMap
    Assert.Equal(result, None)



[<Fact>]
let ``Filter Test`` () =
    let map =
        emptyMap<int, string>
        |> put 1 "value1"
        |> put 2 "value2"

    let filteredMap =
        filter (fun entry -> entry.Value ="value2") map

    let expectedMap = emptyMap |> put 2 "value2"

    let result = equal filteredMap expectedMap
    Assert.Equal(true, result)



[<Fact>]
let ``Map Function Test`` () =
    let map1 = emptyMap |> put 1 "value1" |> put 2 "value2"

    let mapper value = value + "_mapped"
    let mappedMap = map mapper map1

    let expectedMap: HashMap<int, string> = emptyMap |> put 1 "value1_mapped" |> put 2 "value2_mapped"

    Assert.Equal(true, equal mappedMap expectedMap)

[<Fact>]
let ``Fold Test <int,int>`` () =
    let addValues acc entry = acc + entry.Value

    let map: HashMap<int, int> =
        [| [ { Key = 1; Value = 1 } ]
           [ { Key = 2; Value = 2 } ]
           [ { Key = 3; Value = 3 } ] |]


    let result = fold addValues 0 map
    printfn "Total length of all values: %d" result
    Assert.Equal(6, result)

[<Fact>]
let ``Fold Test <string,int>`` () =
    let addValues acc entry = acc + entry.Value

    let map: HashMap<string, int> =
        [| [ { Key = "1"; Value = 1 } ]
           [ { Key = "2"; Value = 2 } ]
           [ { Key = "3"; Value = 3 } ] |]


    let result = fold addValues 0 map
    printfn "Total length of all values: %d" result
    Assert.Equal(6, result)

[<Fact>]
let ``FoldBack Test for Concatenation`` () =
    let map1: HashMap<int, String> =
        [| [ { Key = 1; Value = "value1" } ]
           [ { Key = 2; Value = "value2" } ]
           [ { Key = 3; Value = "value3" } ] |]

    let concatenateValues acc value = acc + value

    let result = foldBack concatenateValues map1 ""

    printfn "Concatenated result: %s" result

    Assert.Equal("value1value2value3", result)

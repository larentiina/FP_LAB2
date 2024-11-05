module Tests

open System
open Xunit
open FsCheck
open Dict

// [<Fact>]
// let ``Put Test`` () =
//     let map = emptyMap
//     let map1 = put 1 "value1" map
//     let map2 = emptyMap |> put "second" "value1" 
   

//     let result1 = get 1 map1
//     let result2 = get "second" map2
//     printfn "Checking if 'value1' exists in the bucket: %A" result1
//     Assert.Equal(result1, result2)

[<Fact>]
let ``Remove Test`` () =
    let map = emptyMap |> put 1 "value1" |> put 2 "value2"
    let updatedMap = remove 1 map
    let result = get 1 updatedMap
    Assert.Equal(result, None)

// [<Fact>]
// let ``Resize Test`` () =

//     let map: HashMap<int, int> =
//         [| [ { Key = 1; Value = 1 } ]
//            [ { Key = 2; Value = 2 } ]
//            [ { Key = 3; Value = 3 } ] |]

//     let updatedMap = put  4 4 map

//     let result = size updatedMap
//     Assert.Equal(6, result)

// [<Fact>]
// let ``Merge Test`` () =
//     let map1 = emptyMap |> put 1 "value1"
//     let map2 = emptyMap |> put 1 "value2" |> put 2 "value2"
//     let mergedMap = merge map1 map2
//     let mergedBacket = mergedMap.[hash 1]

//     let result1 =
//         mergedBacket
//         |> List.exists (fun entry -> entry.Value = "value1")

//     let result2 =
//         mergedBacket
//         |> List.exists (fun entry -> entry.Value = "value2")

//     printfn "Merged bucket contains the following entries:"

//     mergedBacket
//     |> List.iter (fun entry -> printfn "Key: %d, Value: %s" entry.Key entry.Value)

//     Assert.Equal(true, result1 && result2)

[<Fact>]
let ``Filter Test`` () =
    let map =
        emptyMap<int, string>
        |> put 1 "value1"
        |> put 2 "value2"

    let filteredMap =
        filter (fun (entry: Entry<int, string>) -> entry.Value.Contains("2")) map

    let expectedMap = emptyMap |> put 2 "value2"

    let result = equal filteredMap expectedMap
    Assert.Equal(true, result)



// [<Fact>]
// let ``Map Function Test`` () =
//     let map1 = emptyMap |> put 1 "value1" |> put 2 "value2"

//     let mapper value = value + "_mapped"
//     let mappedMap = map mapper map1

//     let value1 = mappedMap.[hash 1]
//     let value2 = mappedMap.[hash 2]

//     printfn "Mapped values: Key 1 = %s, Key 2 = %s" (List.head value1).Value (List.head value2).Value

//     Assert.Equal("value1_mapped", (List.head value1).Value)
//     Assert.Equal("value2_mapped", (List.head value2).Value)

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

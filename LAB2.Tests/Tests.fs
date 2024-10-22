module Tests

open System
open Xunit
open FsCheck
open Dict

[<Fact>]
let ``Put Test`` () =
    let map = emptyMap
    let map1 = put 1 "value1" map
    let map2 = put 2 "value2" map1
    let backet1 = map2.[hash 1 ] 
    let result1 = backet1 |> List.exists (fun entry -> entry.Value = "value1")
    printfn "Checking if 'value1' exists in the bucket: %A" result1
    Assert.Equal(true, result1)
    let backet2 = map2.[hash 2] 
    let result2 = backet2 |> List.exists (fun entry -> entry.Value = "value2")
    printfn "Checking if 'value2' exists in the bucket: %A" result2
    Assert.Equal(true, result1)

[<Fact>]
let ``Remove Test`` () =
    let map = emptyMap |> put 1 "value1" |> put 2 "value2"
    let updatedMap = remove 1 map
    let bucket = updatedMap.[hash 1 ]
    printfn "Bucket with hash %d after removing key 1 has %d elements." (hash 1 ) (List.length bucket)

    Assert.Equal(0, List.length bucket)

[<Fact>]
let ``Merge Test`` () =
    let map1 = emptyMap |> put 1 "value1"
    let map2 = emptyMap |> put 1 "value2" |> put 2 "value2"
    let mergedMap = merge map1 map2
    let mergedBacket = mergedMap.[hash 1]
    let result1 = mergedBacket |> List.exists (fun entry -> entry.Value = "value1")
    let result2 = mergedBacket |> List.exists (fun entry -> entry.Value = "value2")
    printfn "Merged bucket contains the following entries:"
    mergedBacket |> List.iter (fun entry -> printfn "Key: %d, Value: %s" entry.Key entry.Value)
    Assert.Equal(true, result1 && result2) 
[<Fact>]
let ``Filter Test`` () =
    let map = emptyMap<int, string> |> put 1 "value1" |> put 2 "value2"
    let filteredMap = filter (fun (entry: Entry<int, string>) -> entry.Value.Contains("2")) map

    let expectedCount = 1
    let actualCount = filteredMap |> Array.sumBy List.length
    printfn "Expected count: %d, Actual count: %d" expectedCount actualCount
    Assert.Equal(expectedCount, actualCount)

    let containsKey2 = filteredMap |> Array.exists (fun list -> List.exists (fun entry -> entry.Key = 2) list)
    printfn "Does the filtered map contain entry with key 2? %A" containsKey2
    Assert.True(containsKey2)

[<Fact>]
let ``Map Function Test`` () =
    let map1 = emptyMap |> put 1 "value1" |> put 2 "value2"
    
    let mapper value = value + "_mapped"
    let mappedMap = map mapper map1

    let value1 = mappedMap.[hash 1]
    let value2 = mappedMap.[hash 2]

    printfn "Mapped values: Key 1 = %s, Key 2 = %s" (List.head value1).Value (List.head value2).Value

    Assert.Equal("value1_mapped", (List.head value1).Value)
    Assert.Equal("value2_mapped", (List.head value2).Value)

[<Fact>]
let ``Fold Test`` () =
    let addValues acc entry =
        acc + entry.Value

    let map: HashMap<int, int> = [|
        [{ Key = 1; Value = 1 }];  
        [{ Key = 2; Value = 2 }];  
        [{ Key = 3; Value = 3 }]    
    |]

   
    let result = fold addValues 0 map
    printfn "Total length of all values: %d" result
    Assert.Equal(6,result)

[<Fact>]
let ``FoldBack Test for Concatenation`` () =
    let map1: HashMap<int, String> = [|
        [{ Key = 1; Value = "value1" }];  
        [{ Key = 2; Value = "value2" }];  
        [{ Key = 3; Value = "value3" }]    
    |]
    
    let concatenateValues acc value = acc + value

    let result = foldBack concatenateValues map1 ""
    
    printfn "Concatenated result: %s" result
    
    Assert.Equal("value1value2value3", result)



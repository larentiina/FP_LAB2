module Dict

type Entry<'Key, 'Value> = { Key: 'Key; Value: 'Value }

type Chain<'Key, 'Value> = Entry<'Key, 'Value> list
type HashMap<'Key, 'Value> = Chain<'Key, 'Value> array
let initalSize  = 10

let hash (key: 'Key) (size: int) : int =
    match box key with
    | null -> 0
    | _ -> abs (key.GetHashCode()) % size

let resize (map: HashMap<'Key, 'Value>) : HashMap<'Key, 'Value> =
    let newSize = int (float (Array.length map) * 1.5) 
    let newMap = Array.init newSize (fun _ -> []) 

    map |> Array.iter (fun chain ->
        chain |> List.iter (fun entry ->
            let index = hash entry.Key newSize
            newMap.[index] <- newMap.[index] @ [entry]
        )
    )

    newMap
 

// Функция для вставки нового элемента в хеш-таблицу
let put (key: 'Key) (value: 'Value) (map: HashMap<'Key, 'Value>) =
    let index = hash key (Array.length map)

    let chain = map.[index]

    let putChain key value chain =
        match List.tryFind (fun entry -> entry.Key = key) chain with
        | Some _ ->
            List.map
                (fun entry ->
                    if entry.Key = key then
                        { entry with Value = value }
                    else
                        entry)
                chain
        | None -> { Key = key; Value = value } :: chain

    let newChain = putChain key value chain


    let newMap = 
        if float (Array.length map) * 0.7 < float (Array.sumBy List.length map) then
            resize map 
        else
            map

    Array.mapi (fun i c -> if i = index then newChain else c) newMap


let get (key: 'Key) (map: HashMap<'Key, 'Value>) : Option<'Value> =
    let index = hash key (Array.length map)
    let chain = map.[index]

    chain
    |> List.tryFind (fun entry -> entry.Key = key)
    |> Option.map (fun entry -> entry.Value)


// Удаление из HashMap
let remove key (map: HashMap<'Key, 'Value>) =

    let index = hash key (Array.length map)
    let chain = map.[index]

    let modifiedChain = List.filter (fun entry -> entry.Key <> key) chain

    Array.mapi (fun i c -> if i = index then modifiedChain else c) map


// Фильтрация элементов в HashMap по предикату
let filter (predicate: Entry<'Key, 'Value> -> bool) (map: HashMap<'Key, 'Value>) : HashMap<'Key, 'Value> =
    let addToBucket bucket entry =
        if predicate entry then
            entry :: bucket
        else
            bucket

    let filterBucket bucket = List.fold addToBucket [] bucket

    let filteredMap = Array.map filterBucket map

    filteredMap


// Функция свёртки
let rec fold f acc (hashmap: HashMap<'Key, 'Value>) =
    let rec foldChain acc chain =
        match chain with
        | [] -> acc
        | entry :: rest -> foldChain (f acc entry) rest

    match hashmap with
    | [||] -> acc
    | _ ->
        let newAcc = Array.fold (fun acc chain -> foldChain acc chain) acc hashmap
        newAcc

// Функция обратной свёртки
let foldBack folder (map: HashMap<'Key, 'Value>) initial =
    let rec foldChain chain acc =
        match List.rev chain with
        | [] -> acc
        | entry :: rest -> foldChain rest (folder entry.Value acc)

    let rec foldMap index acc =
        if index < 0 then
            acc
        else
            let chain = map.[index]
            let newAcc = foldChain chain acc
            foldMap (index - 1) newAcc

    foldMap (Array.length map - 1) initial

//Отображение map
let map mapper (map: HashMap<'Key, 'Value>) =
    let rec mapChain chain =
        match chain with
        | [] -> []
        | entry :: rest ->
            { Key = entry.Key
              Value = mapper entry.Value }
            :: mapChain rest

    let rec mapHashMap index =
        if index >= Array.length map then
            [||]
        else
            let updatedChain = mapChain map.[index]
            let restMap = mapHashMap (index + 1)
            Array.append [| updatedChain |] restMap

    mapHashMap 0



let emptyMap<'Key, 'Value> : HashMap<'Key, 'Value> = Array.init initalSize (fun _ -> [])

let merge (map1: HashMap<'Key, 'Value>) (map2: HashMap<'Key, 'Value>) : HashMap<'Key, 'Value> =
    
    let mergedMap= emptyMap
    let size = Array.length mergedMap

    let addAll (map: HashMap<'Key, 'Value>) (targetMap: HashMap<'Key, 'Value>) =
        map
        |> Array.iteri (fun i chain ->
            chain |> List.iter (fun entry ->
                let index = hash entry.Key size
                targetMap.[index] <- entry :: targetMap.[index]))

    addAll map1 mergedMap
    addAll map2 mergedMap

    if float (Array.sumBy List.length mergedMap) > float size * 0.7 then
        resize mergedMap
    else
        mergedMap

let equal (map1: HashMap<'Key, 'Value>) (map2: HashMap<'Key, 'Value>) : bool =
    let minSize = min (Array.length map1) (Array.length map2)

    let extraBucketsEmpty (map: HashMap<'Key, 'Value>) startIndex =
        Array.forall (fun bucket -> bucket = []) map.[startIndex..]

    let allEntriesPresent chain otherChain =
        List.forall (fun entry -> 
            List.exists (fun e -> e.Key = entry.Key && e.Value = entry.Value) otherChain) chain

    let areCommonBucketsEqual =
        Array.forall2 
            (fun (b1: Chain<'Key, 'Value>) (b2: Chain<'Key, 'Value>) ->
                allEntriesPresent b1 b2 && allEntriesPresent b2 b1)
            map1.[..minSize - 1] 
            map2.[..minSize - 1]

    areCommonBucketsEqual &&
    (extraBucketsEmpty map1 minSize || extraBucketsEmpty map2 minSize)

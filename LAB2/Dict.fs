module Dict

type Entry<'Key, 'Value> = { Key: 'Key; Value: 'Value }

type Chain<'Key, 'Value> = Entry<'Key, 'Value> list
type HashMap<'Key, 'Value> = Chain<'Key, 'Value> array
let size (map: HashMap<_, _>) = 10

let hash (key: 'Key) : int =
    match box key with
    | null -> 0
    | _ -> key.GetHashCode() % 10

// Функция для вставки нового элемента в хеш-таблицу
let put (key: 'Key) (value: 'Value) (map: HashMap<'Key, 'Value>) =
    let index = hash key

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
    Array.mapi (fun i c -> if i = index then newChain else c) map


let get (key: 'Key) (map: HashMap<'Key, 'Value>) : Option<'Value> =
    let index = hash key
    let chain = map.[index]

    chain
    |> List.tryFind (fun entry -> entry.Key = key)
    |> Option.map (fun entry -> entry.Value)


// Удаление из HashMap
let remove key (map: HashMap<'Key, 'Value>) =

    let index = hash key
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


// Функция для объединения двух HashMap
let merge (map1: HashMap<'Key, 'Value>) (map2: HashMap<'Key, 'Value>) : HashMap<'Key, 'Value> =
    let mergeEntry (acc: Entry<_, _> list) entry = entry :: acc

    Array.init (size map1) (fun index ->
        let chain1 = map1.[index]
        let chain2 = map2.[index]
        List.fold mergeEntry chain1 chain2)

let emptyMap<'Key, 'Value> : HashMap<'Key, 'Value> = Array.init 10 (fun _ -> [])

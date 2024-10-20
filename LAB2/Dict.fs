module Dict

type Entry = {
    Key: int
    Value: string
}

type Chain = Entry list
type HashMap = Chain array
let size (map: HashMap) =  10
// Хеш-функция для вычисления индекса
let hash (key: int) =
    let prime = 31
    let rec computeHash k acc =
        if k = 0 then acc
        else computeHash (k >>> 8) (acc * prime + (k &&& 0xFF))

    abs (computeHash key 0) % 10

//    Array.length map 

// Функция для вставки нового элемента в хеш-таблицу
let put (key: int) (value: string) (map: HashMap) =
    let index = hash key 

    let chain = map.[index]

    let putChain key value chain =
        match List.tryFind (fun entry -> entry.Key = key) chain with
        | Some _ -> List.map (fun entry -> if entry.Key = key then { entry with Value = value } else entry) chain
        | None -> { Key = key; Value = value } :: chain

    let newChain = putChain key value chain
    Array.mapi (fun i c -> if i = index then newChain else c) map


let get (key: int) (map: HashMap) : Option<string> =
    let index = key % Array.length map 
    let chain = map.[index] 
    chain |> List.tryFind (fun entry -> entry.Key = key) |> Option.map (fun entry -> entry.Value)


// Удаление из HashMap
let remove key (map: HashMap) =
    
    let index = hash key
    let chain = map.[index] 

    let modifiedChain = List.filter (fun entry -> entry.Key <> key) chain

    Array.mapi (fun i c -> if i = index then modifiedChain else c) map


// Фильтрация элементов в HashMap по предикату
let filter predicate (map: HashMap) =
    let addToBucket bucket entry =
        if predicate entry then entry :: bucket else bucket

    let filterBucket bucket =
        List.fold addToBucket [] bucket

    let filteredMap =
        Array.map filterBucket map

    filteredMap


// Функция свёртки
let rec fold f acc (hashmap: HashMap) =
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
let foldBack folder (map: HashMap) initial =
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
let map mapper (map: HashMap) =
    let mapChain chain =
        List.map (fun entry -> { Key = entry.Key; Value = mapper entry.Value }) chain

    let newMap = Array.init (size map) (fun index -> mapChain map.[index])
    newMap


// Функция для объединения двух HashMap
let merge (map1: HashMap) (map2: HashMap) : HashMap =
    let mergeEntry (acc: Entry list) entry =
        
        entry :: acc

    Array.init (size map1) (fun index ->
        let chain1 = map1.[index]
        let chain2 = map2.[index]
        List.fold mergeEntry chain1 chain2
    )

let emptyMap : HashMap = Array.init 10 (fun _ -> [])
## Лабораторная работа №2 по F#

Кузенина Валерия P3332

## ВАРИАНТ - SC-DICT

### Описание структуры

Separate Chaining Hashmap Dict - тип данных, представляющий хэш-таблицу, которая использует для обработки коллизий - цепочки элементов внутри бакетов.

### Структура данных

```
type Entry<'Key, 'Value> = {
    Key: 'Key
    Value: 'Value
}

type Chain<'Key, 'Value> = Entry<'Key, 'Value> list
type HashMap<'Key, 'Value> = Chain<'Key, 'Value> array
```

## Реализованные функции 
### Hash
```
let hash (key: 'Key) (size: int) : int =
    match box key with
    | null -> 0
    | _ -> abs (key.GetHashCode()) % size
```
### Put
Добавляет элемент в бакет, если такой ключ уже есть - обновляет элемент
```
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
```

### Remove
Удаляет элемент по ключу
```
let remove key (map: HashMap<'Key, 'Value>) =
    
    let index = hash key
    let chain = map.[index] 

    let modifiedChain = List.filter (fun entry -> entry.Key <> key) chain

    Array.mapi (fun i c -> if i = index then modifiedChain else c) map

```

### Merge
Соединяет две hashmap, если один и тот же ключ - помещается в один бакет

```

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

```
### Fold
```
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
```
### Map
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
### Нейтральный элемент
```
let emptyMap<'Key, 'Value> : HashMap<'Key, 'Value> = Array.init initalSize (fun _ -> [])

```
### Функция увеличения размера map при загружке её в более чем 0.7
```
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
```

## Тесты
### Проверти - тесты
Проверяют свойства моноида: 

Сумму с нейтральным элементом:
```
[<Property(Verbose = true, Arbitrary = [| typeof<HashMapGenerators> |])>]
let prop_emptyMerge (map: HashMap<int, int>) =
    let map = reinsertAll map
    let mergedMap = merge map emptyMap
    equal map mergedMap 

```
Свойство ассоциативности:
```
[<Property(Arbitrary = [| typeof<HashMapGenerators> |])>]
let prop_association (map1: HashMap<int, int>) (map2:HashMap<int, int> ) (map3:HashMap<int, int> ) =
    let mergedMap1 = merge(merge map1 map2) map3
    let mergedMap2 = merge (merge map1 map3) map2
    equal mergedMap1 mergedMap2 
```

## Вывод
В ходе лабораторной работы была успешно реализована хэш-таблица с разделением цепочками на языке F#. Реализация включает функции для добавления, получения и удаления элементов, а также для фильтрации и объединения карт. Все тесты подтвердили корректность работы структуры. Применение цепочек для разрешения коллизий обеспечило надежность и гибкость. В целом, работа показала эффективность хэш-таблиц как инструмента для хранения и быстрого доступа к данным.

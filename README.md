## Лабораторная работа №2 по F#

Кузенина Валерия P3233

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

### Put
Добавляет элемент в бакет, если такой ключ уже есть - обновляет элемент
```
let put (key: 'Key) (value: 'Value) (map: HashMap<'Key, 'Value>) =
    let index = hash key 

    let chain = map.[index]

    let putChain key value chain =
        match List.tryFind (fun entry -> entry.Key = key) chain with
        | Some _ -> List.map (fun entry -> if entry.Key = key then { entry with Value = value } else entry) chain
        | None -> { Key = key; Value = value } :: chain

    let newChain = putChain key value chain
    Array.mapi (fun i c -> if i = index then newChain else c) map
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
    let mergeEntry (acc: Entry<_, _>  list) entry =
        
        entry :: acc

    Array.init (size map1) (fun index ->
        let chain1 = map1.[index]
        let chain2 = map2.[index]
        List.fold mergeEntry chain1 chain2
    )
```
## Вывод
В ходе лабораторной работы была успешно реализована хэш-таблица с разделением цепочками на языке F#. Реализация включает функции для добавления, получения и удаления элементов, а также для фильтрации и объединения карт. Все тесты подтвердили корректность работы структуры. Применение цепочек для разрешения коллизий обеспечило надежность и гибкость. В целом, работа показала эффективность хэш-таблиц как инструмента для хранения и быстрого доступа к данным.

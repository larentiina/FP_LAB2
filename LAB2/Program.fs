//sc-dict
open System
open Dict
// Пример функции для fold
let addValues acc entry =
    acc + entry.Value.Length


[<EntryPoint>]
let main argv =
    // Создаем тестовый HashMap
    let map: HashMap = [|
        [{ Key = 1; Value = "value1" }];  // первая цепочка
        [{ Key = 2; Value = "value2" }];  // вторая цепочка
        [{ Key = 3; Value = "value3" }]    // третья цепочка
    |]

    // Используем fold, чтобы посчитать общую длину всех значений
    let result = fold addValues 0 map

    // Выводим результат
    printfn "Total length of all values: %d" result
    0 // Возвращает код завершения

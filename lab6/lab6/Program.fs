// Нумералы Черча

// В этой лабе нельзя будет пользоваться рекурсивными функциями и ветвлениями
// (условные оператор, сопоставление с образцом, циклы и т. п.)

// Первое исключение: y-комбинатор. Он рекурсивный и уже написан за вас
let rec y' f n = f (y' f) n

// 1. В качестве разгорева реализуйте факториал на int'ах (0.5 баллов)
// Здесь можно использовать ветвления
let fact =
    y' (fun func n ->
        match n with
        |0 -> 1
        |n -> n * func (n - 1))
     
printfn "%A" (fact 0 = 1)
printfn "%A" (fact 1 = 1)
printfn "%A" (fact 2 = 2)
printfn "%A" (fact 3 = 6)
printfn "%A" (fact 4 = 24)
printfn "%A" (fact 5 = 120)
printfn "%A" (fact 6 = 720)


// 2. Нумералы Черча (0.5 баллов). Здесь можно использовать ветвления и рекурсию
// (это же просто служебная функция для удобной конструкции нумералов)
let rec numeral' n f x =
    match n with
    | 0 -> x
    | n -> numeral' (n - 1) f (f x)

let zero' f x = numeral' 0 f x
let one' f x = numeral' 1 f x
let two' f x = numeral' 2 f x
let three' f x = numeral' 3 f x
let four' f x = numeral' 4 f x
let five' f x = numeral' 5 f x

printfn "%A" (five' (( + ) 1) 0 = 5)


// 3. Арифметические операции на нумералах Черча (0.5 баллов)
let sum' n m f x =
    m f (n f x)
let mult' n m f x =
    m (n f) x

printfn "%A" (sum' five' five' (( + ) 1) 0 = 10)
printfn "%A" (mult' five' five' (( + ) 1) 0 = 25)


// 4. Логические константы (0.5 баллов)
let true' x y = x
let false' x y = y

printfn "%A" (true' one' zero' (( + ) 1) 0 = 1)
printfn "%A" (false' one' zero' (( + ) 1) 0 = 0)


// 5. Комбинатор проверки нумерала Черча на 0 (0.5 баллов)
let isZero' n =
     n (fun func -> false') true'

printfn "%A" (isZero' zero' one' zero' (( + ) 1) 0 = 1)
printfn "%A" (isZero' five' one' zero' (( + ) 1) 0 = 0)


// 6. Логические операции (0.5 баллов)
let and' x y =
    fun a b -> x (y a b) b
let or' x y =
    fun a b -> x a (y a b)
let not' x =
    fun a b -> x b a

printfn "%A" ((and' true' true') 1 0 = true' 1 0)
printfn "%A" ((and' true' false') 1 0 = false' 1 0)
printfn "%A" ((and' false' true') 1 0 = false' 1 0)
printfn "%A" ((and' false' false') 1 0 = false' 1 0)
printfn "%A" ((or' true' true') 1 0 = true' 1 0)
printfn "%A" ((or' true' false') 1 0 = true' 1 0)
printfn "%A" ((or' false' true') 1 0 = true' 1 0)
printfn "%A" ((or' false' false') 1 0 = false' 1 0)
printfn "%A" ((not' true') 1 0 = false' 1 0)
printfn "%A" ((not' false') 1 0 = true' 1 0)


// 7. Комбинатор пары (0.5 баллов)
let pair' x y b =
    b x y
let fst' p =
    p true'
let snd' p =
    p false'

printfn "%A" (fst' (pair' 1 2))
printfn "%A" (snd' (pair' 1 2))

// 8. Комбинатор инкремента и декремента (0.5 баллов)
let inc' n f x =
    n f (f x)
let dec' n f x =
    snd' (n (fun p -> pair' (f (fst' p)) (fst' p)) (pair' x x))

printfn "%A" (inc' five' (( + ) 1) 0 = 6)
printfn "%A" (dec' five' (( + ) 1) 0 = 4)

// 9. Реализация факториала на нумералах Черча (4 балла)
let fact' n = failwith "Not implemented"

printfn "%A" (fact' five' (( + ) 1) 0 = 120)

// Disclaimer: решение этой задачи в заданных ограничениях (нет рекурсии и
// ветвлений) семинаристу неизвестно. Возможно, решения не существует

// Поэтому задача исследовательская, а критерии такие:
// - если задача решена целиком и полностью, то 4 балла
// - если приведено убедительное объяснение, почему решение невозможно,
//   то тоже 4 балла
// - если задача решена с нарушением ограничений, то 2 балла
// - если задача решена на другом языке программирования (любом), но соблюдая
//   ограничения, то тоже 2 балла
// Частичные баллы, разумеется, тоже возможны, но без формальных критериев

// P.S. Очевидно, все написанное выше (кроме самих нумералов) использовать
// необязательно. Эти примитивы нужны были для написания факториала на нумералах
// черча в лекциях по математической логике

// P.P.S. Кстати, возможно, то решение получится. Особенно если что-то поменять
// в определении реализованных примитивов

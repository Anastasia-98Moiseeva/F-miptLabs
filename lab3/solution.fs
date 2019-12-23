// Монада Reader - это вычисление, допускающее чтения из разделяемого
// окружения. Монадический тип - это просто функция, которая принимает
// на вход окружение и возвращает некоторое значение.
type Reader<'Env, 'T> = Reader of ('Env -> 'T)

// Часть 1. Реализация монады (3 балла).

// runReader : 'Env -> Reader<'Env, 'T> -> 'T
// Для удобства вам понадобится функция runReader, которая принимает
// окружение и читателя. Эта функция должна запускать ридера.
let runReader env reader =
  match reader with
  |Reader func -> func env


// Реализуйте builder type для монады.
type ReaderBuilder () =
    // return : 'T -> Reader<'Env, 'T>
    // Метод return должен игнорировать окружение.
    member this.Return value =
      Reader (fun env -> value)
    
    // bind : Reader<'Env, 'T> * ('T -> Reader<'Env, 'U>) -> Reader<'Env, 'U>
    // Метод bind возвращает монадический тип - то есть функцию, которая
    // получает на вход окружение. Это окружение должно быть передано
    // обоим вычислениями. Здесь будет полезно использовать runReader.
    member this.Bind (reader, func) =
      Reader (fun env -> runReader env (func (runReader env reader)))   

// ---

// Создаем обертку для монадических вычислений.
let reader = ReaderBuilder ()

// Проверяем return.
let reader1 value = reader { return string(value) }

// Проверяем runReader.
printfn "%A" (runReader "42" (reader1 42.))  // "42"

// Обратите внимание на "42" - это окружение, в котором
// проводится вычисление.

// Проверяем let!.
let reader2 value = reader {
    let! v = reader1 value
    return v + "!"
}
printfn "%A" (runReader "42" (reader2 3.14))  // "3.14!"

////////////////////////////////////////////////////////////////////////////////

// Пока что функционал для работы с монадами весьма скуден. Кажется, сложнее
// примеров выше пока что нельзя ничего придумать.

// Часть 2. Дополнительного функционала (2 балла).

// map : ('T -> 'U) -> Reader<'Env, 'T> -> Reader<'Env, 'U>
// Дана функция и вычисление. Надо вернуть вычисление, которое применяет
// функцию к данному вычислению.
let map func reader =
  Reader (fun env -> func (runReader env reader))
  

// apply : Reader<'Env, ('T -> 'U)> -> Reader<'Env, 'T'> -> Reader<'Env, 'U>
// Дано два ридера. Один вычисляет функцию, а второй - значение. Надо вернуть
// вычисление, применяющее результат вычисленную функцию к вычисленному
// значению.
let apply mapReader reader =
  Reader (fun env -> (runReader env mapReader) (runReader env reader))

// --- 

// Проверяем map.
let mapReader value = map (fun (str : string) -> str.Length) (reader1 value)
let four : Reader<string, int> = mapReader 3.14
printfn "%A" (runReader "42" four)  // 4

// Проверяем apply.
let funcReader value = reader { return (fun v -> v * value) }
let multByThree : Reader<string, (int -> int)> = funcReader 3
printfn "%A" (runReader "42" (apply multByThree four))  // 12

////////////////////////////////////////////////////////////////////////////////

// Если внимательно присмотреться, значение "42", то есть непосредственно
// окружение, в вычислении не использовалось. В таком виде монада достаточно
// бесполезна.

// Часть 3. Работа с окружением (3 балла).

// ask : Reader<'Env, 'Env>
// Реализуйте читатель ask, вычисляющий текущее окружение.
let ask =
  Reader (fun env -> env)

// asks : ('Env -> 'T) -> Reader<'Env, 'T>Reader (fun env -> env)
// Реализуйте функцию asks, которая оборачивает переданную функцию в вычисление.
let asks func =
  Reader (fun env -> func env)

// local : ('Env -> 'Env) -> Reader<'Env, 'T> -> Reader<'Env, 'T>
// Реадизуйте функцию local, которая выполняет вычисление в локально
// модифицированном окружении.
let local (func : 'Env -> 'Env) reader = 

// ---

// Проверяем ask.
let reader3 : Reader<int, int> = reader {
    let! env = ask
    return env
}
printfn "%A" (runReader 42 reader3)  // 42

// Проверяем asks.
let reader4 = reader {
    let! env = asks (fun env -> string(env * 2))
    return env
}
printfn "%A" (runReader 42 reader4)  // "84"

// Проверяем local.
let reader5 = reader {
    let! env = ask
    let! newEnv = local (fun env -> env * 2) ask
    return (env, newEnv)
}
printfn "%A" (runReader 42 reader5)  // (42, 84)


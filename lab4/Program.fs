// Во всех заданиях запрещено использовать цикл for, переменные (mutable) и
// ссылочные значения (указатели, ref).

// Задание 1 (1 балл). Без использования функций из модуля Seq получить
// последовательность 1, 2, 3, 4, 5, 6, ... (натуральные числа).
let naturals = 
    
    let rec nat x = seq{
        yield x+1
        yield! nat (x+1)
    }
    nat 0
    
    
(*printf "%A" (Seq.toList( Seq.take 10 naturals))*)
    
         
// Задание 2 (1 балл). Без использования функций из модуля Seq получить
// последовательность 0, 1, 0, 2, 0, 3, ... (нечетные элементы - нули, четные -
// последовательность натуральных чисел).
let oddNaturals =

    let rec oddNat x = seq{
        yield 0
        yield x + 1
        yield! oddNat (x+1)
    }
    oddNat 0

(*printf "%A" (Seq.toList( Seq.take 10 oddNaturals))*)


// Задание 3 (1 балл). Без использования модуля Seq реализовать функцию
// Seq.initInfinite.
let seqInitInfinite initializer =
    
    let rec seqInit x = seq {
        yield initializer x + 1
        yield! seqInit (x + 1)
    }
    seqInit 0
    
    
let s = seq {
    yield! seqInitInfinite (fun x -> x + 3)
}


(*printf "%A" (Seq.toList( Seq.take 10 s))*)


// Задание 4 (1 балл). Без использования модуля Seq реализовать функцию
// Seq.unfold.
let seqUnfold generator state =
    
    let rec seqUn x = seq {
        match generator x with
        | None -> ()
        | Some (x, newState) ->
            yield x
            yield! seqUn newState
    }
    seqUn state
 
let s1 = seqUnfold (fun state -> if (state > 3) then None else Some(state, state + 1)) 0
for x in s1 do printf "%d " x


// Задание 5 (1 балл). Протестируйте вашу функцию seqUnfold, получив с ее
// помощью последовательность 0, 1, 2, 2, 3, 5, ... (числа Фибоначчи).
let fibonaccies = failwith "Not implemented"

// Задание 6 (1 балл). Реализовать функцию Seq.chunkBySize без использования
// функции Seq.chunkBySize.
let rec seqChunkBySize size sequence = failwith "Not implemented"

// Задание 7 (1 балл). Реализовать функцию Seq.pairwise без использования
// функции Seq.pairwise.
let rec seqPairwise sequence = failwith "Not implemented"

// Задание 8 (1 балл). Выберите произвольную функцию из модуля Seq и реализуйте
// ее без использования модуля Seq.


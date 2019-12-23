module Solution

open System

type 'a MySet = | MySet of 'a list

// При решении запрещено пользоваться функциями из модуля List, оператором ( @ ) и циклом for
// Каждый пункт без звездочки стоит 1 балл, каждый пункт со звездочкой - 2 балла,
// причем второй балл добавляется при использовании хвостовой рекурсии
// (в том числе во всех используемых рекурсивных функциях)
// При значительном несоблюдении код-стайла итоговый балл за всю работу понижается на 1

let set0 = MySet []
let set1 = MySet [ 0; 2; 3; 4; 7; 8; 10; 21 ]
let set2 = MySet [ 0; 1; 2; 4; 5; 10; 11; 12; 14 ]

// Проверить, является ли множество пустым
let isEmpty set =
  match set with 
  | MySet [] -> true
  | _ -> false

// Проверить, лежит ли элемент во множестве
let rec isMember element set = 
  match set with
  | MySet [] -> false
  | MySet (x::xs) -> x = element || isMember element (MySet xs)

// Проверить, что первое множество является подмножеством второго множества
let rec isSubset set1 set2 =
  match set1 with
  | MySet [] -> true
  | MySet (x::xs) -> (isMember x set2) && isSubset (MySet xs) set2

// Проверить, равны ли два множества
let isEqual set1 set2 = isSubset set1 set2 && isSubset set2 set1

// * Получить мощность множества
let rec cardinality set =
  match set with
  | MySet [] -> 0
  | MySet (x::xs) -> cardinality (MySet xs) + 1

// * Найти объединение двух множеств
let rec unite set1 set2 =
  match set2 with
  | MySet [] -> set1
  | MySet (x::xs) ->
    if isMember x set1 then unite set1 (MySet xs)
    else unite (MySet (x::(match set1 with
                           | MySet (list) -> list))) (MySet xs) 
  
// * Найти пересечение двух множеств
let rec intersect set1 set2 =
  match set2 with
  | MySet [] -> MySet []
  | MySet (x::xs) -> 
    if isMember x set1 then MySet (x::(match (intersect (MySet xs) set1) with
                                   | MySet (list) -> list))
    else intersect (MySet xs) set1


// * Найти декартово произведение двух множеств
let product set1 set2 = raise (NotImplementedException ())

  
// * Сконструировать множество из списка
let mySet list =
  let rec makeSet list set =
    match list with
    | [] -> set
    | [] -> set
    | x::xs ->
      if isMember x (MySet xs) then makeSet xs set
      else makeSet xs (unite set (MySet [x]))
  makeSet list (MySet [])


// * Найти множество из всех подмножеств данного множества
let powerSet set = raise (NotImplementedException ())
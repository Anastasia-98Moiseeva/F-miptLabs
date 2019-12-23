// При выполнении задания запрещено использовать условный оператор.

type 'T Bst =
    | Nil
    | Node of 'T * 'T Bst * 'T Bst

// has : 'T -> 'T Bst -> bool (1 балл)
// Вернуть true или false, в зависимоти от того, есть ключ key в дереве bst
// или нет
let rec has key bst =
    match bst with
    | Node (k, left, right) when key = k -> true
    | Node (k, left, right) when k < key -> has key right
    | Node (k, left, right) when k > key -> has key left
    | Nill -> false
   

// insert : 'T -> 'T Bst -> 'T Bst (1 балл)
// Вернуть дерево bst после вставки в него узла со ключом key
let rec insert key bst =
    match bst with
    | Node (k, left, right) when k >= key -> Node(k, insert key left, right)
    | Node (k, left, right) when k < key -> Node(k, left, insert key right)
    | Nill -> Node(key, Nill, Nill)
      

// min : 'T Bst -> 'T Bst
// Вернуть наиболее глубокий узел дерева bst с минимальным ключом
// (либо Nil, если такого нет)    
let rec min bst =
    match bst with
    | Node (key, left, right) when left = Nil -> bst
    | Node (key, left, right) when left <> Nil -> min left
    | Nil -> Nil
    

// next : 'T -> 'T Bst -> 'T Bst
// Вернуть некоторый узел дерева bst с наименьшим ключом,
// большим ключа key (либо Nil, если такого нет)
let rec next key bst = 
    match bst with
    | Node (k, left, right) when k <= key -> next key right
    | Node (k, left, right) when k > key ->
        let nex = next key left
        match nex with
        | Nil -> bst
        | _ -> nex
    | Nil -> Nil
        
 

// remove : 'T -> 'T Bst -> 'T Bst
// Вернуть bst после удаления ключа key (если таких несколько, то удалить надо
// только один)
let rec remove key bst = Nil

////////////////////////////////////////////////////////////////////////////////

let rec isBst = function
    | Nil -> true
    | Node (root, leftChild, rightChild) ->
        match leftChild, rightChild with
        | Nil, Nil -> true
        | Nil, Node (key, _, _) -> root < key
        | Node (key, _, _), Nil -> key <= root
        | Node (key1, _, _), Node (key2, _, _) ->
            key1 <= root && root < key2 && isBst leftChild && isBst rightChild

let  rec toList = function
    | Nil -> []
    | Node (root, leftChild, rightChild) ->
        (toList leftChild) @ (root :: (toList rightChild))

let isEqual bst1 bst2 =
    if not (isBst bst1) || not (isBst bst2) then false else
    match bst1, bst2 with
    | Nil, Nil -> true
    | Nil, _ | _, Nil -> false
    | _ -> (toList bst1) = (toList bst2)

let check func bst expected =
    if func bst <> expected then failwith "Fail"

let checkBst func bst expected =
    let bst2 = func bst
    if not (isBst bst2) || not (isEqual bst2 expected) then failwith "Fail"

////////////////////////////////////////////////////////////////////////////////

let bst01 = Nil
let bst02 = Node (42, Nil, Nil)
let bst03 = Node (42, Node (24, Nil, Nil), Nil)
let bst04 = Node (42, Nil, Node (43, Nil, Nil))
let bst05 = Node (13, Node (13, Node (13, Nil, Nil), Nil), Node (14, Nil, Nil))

check (has 48) bst01 false
check (has 42) bst02 true
check (has 24) bst02 false
check (has 42) bst03 true
check (has 24) bst03 true
check (has 43) bst03 false
check (has 42) bst04 true
check (has 43) bst04 true
check (has 24) bst04 false
check (has 13) bst05 true
check (has 14) bst05 true
check (has 24) bst05 false

checkBst (insert 1) bst01 (Node (1, Nil, Nil))
checkBst (insert 1) bst02 (Node (42, Node (1, Nil, Nil), Nil))
checkBst (insert 42) bst03 (Node (42, (Node (42, Node (24, Nil, Nil), Nil)), Nil))
checkBst (insert 43) bst04 (Node (43, Node (43, Node (42, Nil, Nil), Nil), Nil))
checkBst (insert 14) bst05 (Node (13, Node (13, Node (13, Nil, Nil), Nil), Node (14, Node (14, Nil, Nil), Nil)))

checkBst min bst01 Nil
checkBst min bst02 (Node (42, Nil, Nil))
checkBst min bst03 (Node (24, Nil, Nil))
checkBst min bst04 (Node (42, Nil, Node (43, Nil, Nil)))
checkBst min bst05 (Node (13, Nil, Nil))

checkBst (next 48) bst01 Nil
checkBst (next 42) bst02 Nil
checkBst (next 24) bst02 (Node (42, Nil, Nil))
checkBst (next 42) bst03 Nil
checkBst (next 24) bst03 (Node (42, Node (24, Nil, Nil), Nil))
checkBst (next 13) bst03 (Node (24, Nil, Nil))
checkBst (next 42) bst04 (Node (43, Nil, Nil))
checkBst (next 43) bst04 Nil
checkBst (next 10) bst04 (Node (42, Nil, Node (43, Nil, Nil)))
checkBst (next 13) bst05 (Node (14, Nil, Nil))
checkBst (next 14) bst05 Nil
(*
checkBst (remove 1) bst01 bst01
checkBst (remove 42) bst02 Nil
checkBst (remove 24) bst02 bst02
checkBst (remove 42) bst03 (Node (24, Nil, Nil))
checkBst (remove 24) bst03 (Node (42, Nil, Nil))
checkBst (remove 12) bst03 bst03
checkBst (remove 42) bst04 (Node (43, Nil, Nil))
checkBst (remove 43) bst04 (Node (42, Nil, Nil))
checkBst (remove 24) bst04 bst04
checkBst (remove 13) bst05 (Node (13, Node (13, Nil, Nil), Node (14, Nil, Nil)))
checkBst (remove 14) bst05 (Node (13, Node (13, Node (13, Nil, Nil), Nil), Nil))
checkBst (remove 10) bst05 bst05
*)
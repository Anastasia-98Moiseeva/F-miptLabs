open System

// Задача - написать парсер строки в Json-объект
// http://www.json.org
// Нельзя использовать библиотеки для работы с Json

type Json =
    | Null
    | Boolean of bool
    | Number of int
    | String of string
    | Array of (Json list)
    | Object of ((string * Json) list)

// Ограничения:
// 1. Все подаваемые строки корректны и не содержат пробельных символов 
// 2. Литералы типов Null, Boolean в нижнем регистре (null, false, true)
// 3. Литералы типа Number - неорицательные десятичные числа без ведущих нулей, они укладываются в int
// 4. Литералы типа String не содержат спецсимволов и написаны в двойных кавычках
// 5. Все ключи в Object различны

// Также см. тесты
// Если что-то пошло не так, вызывайте failwith -https://docs.microsoft.com/ru-ru/dotnet/fsharp/language-reference/exception-handling/the-failwith-function

// Шаг 1 (1 балл). Преобразование из строки в список символов
 
let explode (str : string) =
    [for s in str -> s]  


// Шаг 2 (3 балла). Написать токенизатор - функцию, которая принимает список символов и возвращает список токенов

type Token =
    | OpenBrace'
    | CloseBrace'
    | Colon'
    | OpenBracket'
    | CloseBracket'
    | Comma'
    | Null'
    | Boolean' of bool
    | Number' of int
    | String' of string
    

let rec tokenize (chars : char list) : Token list =
    
    let rec isSubstring (str1 : char list) (str2 : char list) =
        match str1 with
        | [] -> (true, str2)
        | x::xs ->
            match str2 with
            | [] -> (false, [])
            | y::ys ->
                if x <> y then (false, [])
                else isSubstring xs ys

  
    // Возможно, вам понадобятся вспомогательные функции, которые принимают список символов
    // и возврашают пару из токена и остатка списка
    
    let takeNull (chars : char list) : (Token * char list) =
        let n = ['n'; 'u'; 'l'; 'l']
        let (x, y) = isSubstring n chars
        if x = true then (Null', y)
        else (Null', [' '])
        
        
    let takeBoolean (chars : char list) : (Token * char list) =
        let t = ['t'; 'r'; 'u'; 'e']
        let f = ['f'; 'a'; 'l'; 's'; 'e']
        let (x1, y1) = isSubstring t chars
        let (x2, y2) = isSubstring f chars
        if x1 = true then (Boolean' true, y1)
        elif x2 = true then (Boolean' false, y2)
        else (Boolean' false, [' '])
    
    let takeSymbol (chars : char list) =
            match chars with
            | [] -> (' ', [])
            | x::xs -> (char(x), xs)
        
    let takeNumber (chars : char list) : (Token * char list) =
        
        let isNumber (n : char) =    
            if n = '0' || n = '1' || n = '2' || n = '3' || n = '4' || n = '5' ||
                n = '6' || n = '7' || n = '8' || n = '9' then true
            else false
        
        let rec readNumber (chars : char list) (num : char list) : (char list * char list)=
            let (x, y) = takeSymbol chars
            let flag = isNumber x
            if flag = true then readNumber y (num@[x])
            else (num, chars)
        
        let rec makeNumber (chars : char list) (num : int)=
            match chars with
            | [] -> num
            | x::xs -> makeNumber xs (10 * num + int x - int '0')
                
        let (x, y) = readNumber chars []
        if x = [] then (Number' 0, [' '])
        else (Number' (makeNumber x 0), y) 
    
    let takeString (chars : char list) : (Token * char list) =
        
        let rec readString (chars : char list) (str : char list) =
            match chars with
            | [] -> ([' '], [' '])
            | x::xs ->
                if x = '"' then (str, xs)
                else readString xs (str@[x])
                
        let sum (s : String) (c : Char) =
            let temp = s + c.ToString()
            temp
        
        let rec makeString (chars : char list) (str : String) =
            match chars with
            | [] -> str
            | x::xs -> makeString xs (sum str x)
        
        let (x, y) = isSubstring ['"'] chars
        let (a, b) = readString y []
        let str = makeString a ""
        if x = false then (String' "0", [' '])
        else (String' str, b)
    
    let takeWorkingSymbol (chars : char list) =
        let (x, y) = takeSymbol chars
        if x = '{' then (OpenBrace', y)
        elif x = '}' then (CloseBrace', y)
        elif x = ':' then (Colon', y)
        elif x = '[' then (OpenBracket', y)
        elif x = ']' then (CloseBracket', y)
        elif x = ',' then (Comma', y)
        else (Comma', [' ']) 
    
    let rec makeToken (chars : char list) (token : Token list) : Token list=
        
        let (n1, n2) = takeNull chars
        let (b1, b2) = takeBoolean chars
        let (num1, num2) = takeNumber chars
        let (s1, s2) = takeString chars
        let (symbol1, symbol2) = takeWorkingSymbol chars
        if n2 <> [' '] then makeToken n2 (token@[n1])
        elif b2 <> [' '] then makeToken b2 (token@[b1])
        elif num2 <> [' '] then makeToken num2 (token@[num1])
        elif s2 <> [' '] then makeToken s2 (token@[s1])
        elif symbol2 <> [' '] then makeToken symbol2 (token@[symbol1])
        else token
              
    let token = makeToken chars []
    token

// Шаг 3 (3 балла). Распарсить токены в Json-объект

let parse (tokens : Token list) : Json = (*raise (NotImplementedException ())*)
    
    let rec parseTokens (tokens : Token list) =
        match tokens with
        | (Null')::xs -> Null 
        | (Boolean' value)::xs -> Boolean value
        | (Number' value)::xs -> Number value
        | (String' value)::xs -> String value
        
        
        
    let json = parseTokens tokens
    json
    
// Шаг 4 (9 баллов). Прохождение тестов

let jsonize (str : string) : Json = str |> explode |> tokenize |> parse

let rec stringify = function
  | Object list ->
    "{" + (String.concat "," (List.map (fun (key, value) -> "\"" + key + "\":" + stringify value) list)) + "}"
  | Array array  -> "[" + (String.concat "," (List.map stringify array)) + "]"
  | String value -> "\"" + value + "\""
  | Number value -> string value
  | Boolean true -> "true"
  | Boolean false -> "false"
  | Null -> "null"

// Группа тестов на +1 балл
let jsonNull = "null"
let jsonBoolean2 = "false"
let jsonBoolean3 = "true"
let jsonString1 = "\"\""
let jsonString2 = "\"string\""
let jsonNumber1 = "0"
let jsonNumber2 = "1024"
// Группа тестов на +2 балла
let jsonArray1 = "[]"
let jsonArray2 = "[null,true,\"string\",1024]"
// Группа тестов на +2 балла
let jsonObject1 = "{}"
let jsonObject2 = "{\"null\":null,\"bool\":true,\"number\":1024,\"string\":\"string\"}"
// Группа тестов на +2 балла
let jsonArray3 = "[[],{}]"
let jsonArray4 = "[" + jsonArray2 + "," + jsonObject2 + "]"
// Групап тестов на +2 балла
let jsonObject3 = "{\"jsonArray1\":[],\"jsonObject1\":{}}"
let jsonObject4 = "{\"jsonArray2\":" + jsonArray2 + ",\"jsonObject2\":" + jsonObject2 + "}"

// Здесь можно закомментировать строчку, и тогда тест не добавится в список
let jsons = [
    jsonNull
    jsonBoolean2
    jsonBoolean3
    jsonString1
    jsonString2
    jsonNumber1
    jsonNumber2
    jsonArray1
    jsonArray2
    jsonObject1
    jsonObject2
    jsonArray3
    jsonArray4
    jsonObject3
    jsonObject4
]

let convetedJsons = (List.map (fun str -> stringify (jsonize str)) jsons)
let isPassed = List.map2 ( = ) jsons convetedJsons

printfn "%A" (List.zip jsons convetedJsons)
printfn "%A" isPassed

if List.contains false isPassed then
    failwith "Tests are not passed"

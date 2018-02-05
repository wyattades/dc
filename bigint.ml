(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let mapi      = List.mapi
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        let checklength index v =
                            if index > 0 && index mod 70 = 69
                            then "\\\n" ^ (string_of_int v)
                            else string_of_int v in
                        (mapi checklength reversed))

    let rec cmp' list1 list2 = match (list1, list2) with
        | [], []                    -> 0
        | list1, []                 -> 1
        | [], list2                 -> -1
        | car1::cdr1, car2::cdr2    ->
            if car1 > car2 
            then 1
            else if car2 > car1
            then -1
            else cmp' cdr1 cdr2

    let cmp list1 list2 = 
        let diff = List.length list1 - List.length list2 in
        if diff > 0 then 1
        else if diff < 0 then -1
        else cmp' (reverse list1) (reverse list2)

    let rec trim' list' = match list' with
        | []        -> []
        | [0]       -> []
        | car::cdr  ->
            let cdr' = trim' cdr
            in match car, cdr' with
                | 0, [] -> []
                | car, cdr' -> car::cdr'

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
            let sum = car1 + car2 + carry
            in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> trim' (sub' list1 [carry] 0)
        | [], list2, carry   -> trim' (sub' [carry] list2 0)
        | car1::cdr1, car2::cdr2, carry ->
            if (car1 - carry) < car2
            then let dif = (10 + car1) - (car2 + carry)
                in  dif mod radix :: trim' (sub' cdr1 cdr2 1)
            else let dif = car1 - car2 - carry
                in  dif mod radix :: trim' (sub' cdr1 cdr2 0)

    let double value = add' value value 0   
    
    let myprint key value = (Printf.printf "%s: %s\n" key (string_of_bigint (Bigint (Pos, value))))

    let rec get_mul_values max vals bins =
        let next = double (car bins) in
        if cmp next max > 0 then (vals, bins)
        else get_mul_values max ((double (car vals))::vals) (next::bins)
        
    let rec mul' vals bins max res =
        if bins = [] then res
         else (if (cmp max (car bins)) >= 0
            then mul' (cdr vals) (cdr bins) (sub' max (car bins) 0) (add' res (car vals) 0)
            else mul' (cdr vals) (cdr bins) max res)

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else match (cmp value1 value2) with
            | 1  -> Bigint(neg1, trim' (sub' value1 value2 0))
            | -1 -> Bigint(neg2, trim' (sub' value2 value1 0))
            | 0 -> Bigint(neg2, trim' (sub' value2 value1 0))
            | _  -> zero

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then (if (cmp value1 value2) > 0
            then Bigint (neg1, trim' (sub' value1 value2 0))
            else Bigint ((if neg1 = Pos then Neg else Pos), trim' (sub' value2 value1 0)))
        else if neg1 = Neg 
        then Bigint (Neg, add' value2 value1 0)
        else if neg2 = Neg
        then Bigint (Pos, add' value1 value2 0)
        else zero
    
    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let compared = cmp value1 value2 in
        let smaller = (if compared > 0 then value2 else value1) in
        let bigger = (if compared > 0 then value1 else value2) in
        let vals, bins = get_mul_values smaller [bigger] [[1]] in

        Bigint ((if neg1 = neg2 then Pos else Neg), (mul' vals bins smaller []))

    let div = add

    let rem = add

    let pow = add
    
end


(* wades Wyatt Ades, lnwhite Lee White *)

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
        | value -> let reversed = reverse value in strcat ""
                    ((if sign = Pos then "" else "-") ::
                    (mapi (fun index v ->
                        if index > 0 && (index mod 69) = 68
                        then (string_of_int v) ^ "\\\n"
                        else string_of_int v) reversed))

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

    let rec get_bin_values max vals bins =
        let next = double (car bins) in
        if cmp next max > 0 then (vals, bins)
        else get_bin_values max ((double (car vals))::vals) (next::bins)
        
    let rec mul' vals bins max res =
        if bins = [] then res
         else (if (cmp max (car bins)) >= 0
            then mul' 
                (cdr vals)
                (cdr bins)
                (sub' max (car bins) 0)
                (add' res (car vals) 0)
            else mul' (cdr vals) (cdr bins) max res)

    let rec div' vals bins quot remain =
        if bins = [] then (quot, remain)
        else (if (cmp remain (car vals)) >= 0
            then div' 
                (cdr vals)
                (cdr bins)
                (add' quot (car bins) 0)
                (sub' remain (car vals) 0)
            else div' (cdr vals) (cdr bins) quot remain
        )

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
            else Bigint (
                (if neg1 = Pos then Neg else Pos), 
                trim' (sub' value2 value1 0)))
        else if neg1 = Neg 
        then Bigint (Neg, add' value2 value1 0)
        else if neg2 = Neg
        then Bigint (Pos, add' value1 value2 0)
        else zero
    
    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let compared = cmp value1 value2 in
        let smaller = (if compared > 0 then value2 else value1) in
        let bigger = (if compared > 0 then value1 else value2) in
        let vals, bins = get_bin_values smaller [bigger] [[1]] in

        Bigint (
            (if neg1 = neg2 then Pos else Neg),
            (mul' vals bins smaller []))

    let div (Bigint (neg1, numer)) (Bigint (neg2, denom)) =
        match (cmp numer denom) with
            | -1 -> zero
            | 0  -> Bigint (Pos, [1])
            | _  -> let vals, bins = 
                        get_bin_values numer [denom] [[1]] in
                    let quot, remain = div' vals bins [] numer in
                    Bigint((if neg1 = neg2 then Pos else Neg), quot)

    let rem (Bigint (neg1, numer)) (Bigint (neg2, denom)) =
        match (cmp numer denom) with
            | -1 -> Bigint (neg1, numer)
            | 0  -> zero
            | _  -> let vals, bins = 
                        get_bin_values numer [denom] [[1]] in
                    let quot, remain = div' vals bins [] numer in
                    Bigint(neg1, remain)

    let rec pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let base = Bigint(neg1, value1) in
        if neg2 = Neg then zero
        else if value2 = [] then Bigint (Pos, [1])
        else if value2 = [1] then base
        else (
            let vals, bins = get_bin_values value2 [[2]] [[1]] in
            let quot, remain = div' vals bins [] value2 in
            (if remain = [1] 
                then mul
                        (pow (mul base base) (Bigint (neg1, quot))) 
                        base
                else pow (mul base base) (Bigint (neg1, quot)))
        )
    
end

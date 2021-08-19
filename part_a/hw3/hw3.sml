(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
                 | Variable of string
                 | UnitP
                 | ConstP of int
                 | TupleP of pattern list
                 | ConstructorP of string * pattern

datatype valu = Const of int
              | Unit
              | Tuple of valu list
              | Constructor of string * valu

fun g f1 f2 p =
    let
        val r = g f1 f2
    in
        case p of
            Wildcard          => f1 ()
          | Variable x        => f2 x
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | ConstructorP(_,p) => r p
          | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
             | UnitT
             | IntT
             | TupleT of typ list
             | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
val only_capitals =
    List.filter (fn str => (Char.isUpper o String.sub) (str, 0))

(* 2 *)
val longest_string1 =
    List.foldl (fn (str, acc) =>
                   if (String.size str) > (String.size acc)
                   then str
                   else acc)
               ""

(* 3 *)
val longest_string2 =
    List.foldl (fn (str, acc) =>
                   if (String.size str) >= (String.size acc)
                   then str
                   else acc)
               ""

(* 4 *)
fun longest_string_helper f =
    List.foldl (fn (str, acc) =>
                   if f (String.size str, String.size acc)
                   then str
                   else acc)
               ""

val longest_string3 = longest_string_helper (fn (a, b) => a > b)
val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

(* 5 *)
val longest_capitalized = longest_string3 o only_capitals

(* 6 *)
val rev_string = String.implode o rev o String.explode

(* 7 *)
fun first_answer _ [] = raise NoAnswer
  | first_answer f (x::xs) =
    case f x
     of SOME v => v
      | NONE => first_answer f xs

(* 8 *)
fun all_answers f xs =
    let fun loop (acc, xs) =
            case xs
             of [] => SOME acc
              | x::xs => case f x
                           of NONE => NONE
                            | SOME y => loop(acc @ y, xs)
    in loop ([], xs)
    end

(* 9.a *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(* 9.b *)
val count_wild_and_variable_lengths = g (fn _ => 1) String.size

(* 9.c *)
fun count_some_var (str, pat) =
    g (fn _ => 0)
      (fn var => if var = str
                 then 1
                 else 0)
      pat

(* 10 *)
fun check_pat pat =
    let fun get_strings pat =
            case pat
             of Variable var => [var]
              | TupleP pats => List.foldl (fn (pat, acc) => acc @ (get_strings pat))
                                          []
                                          pats
              | ConstructorP (_, pat) => get_strings pat
              | _ => []
        fun is_unique lst =
            case lst
             of [] => true
              | x::xs => if List.exists (fn y => y = x) xs
                         then false
                         else is_unique xs
    in (is_unique o get_strings) pat
    end

(* 11 *)
fun match (value, pat) =
    case pat of
        Wildcard => SOME []
      | UnitP => (case value
                   of Unit => SOME []
                    | _ => NONE)
      | Variable var => SOME [(var, value)]
      | ConstP x => (case value
                      of Const y => if x = y then SOME [] else NONE
                       | _ => NONE)
      | TupleP pats => (case value
                         of Tuple values =>
                            (all_answers match (ListPair.zip (values, pats))
                             handle UnequalLengths => NONE)
                          | _ => NONE)
      | ConstructorP (str, pat) => (case value
                                       of Constructor (str_v, value_v) =>
                                          if str = str_v
                                          then match (value_v, pat)
                                          else NONE
                                        | _ => NONE)

fun match_SAMPLE_SOLUTION (value, pat) =
    case (valu,pat) of
        (_,Wildcard)    => SOME []
      | (_,Variable(s)) => SOME [(s,valu)]
      | (Unit,UnitP)    => SOME []
      | (Const i, ConstP j)    => if i=j then SOME [] else NONE
      | (Tuple(vs),TupleP(ps)) => if length vs = length ps
                                  then all_answers match (ListPair.zip(vs,ps))
                                  else NONE
      | (Constructor(s1,v), ConstructorP(s2,p)) => if s1=s2
                                                   then match(v,p)
                                                   else NONE
      | _ => NONE

(* 12 *)
fun first_match value pats =
    SOME (first_answer (fn pat => match (value, pat)) pats)
    handle NoAnswer => NONE

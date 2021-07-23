(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1(a) *)

fun is_some intoption =
    case intoption of
        NONE => false
      | SOME i => true

exception MyOptionException of string
exception MyUndesirableArgument

fun val_of intoption =
    case intoption of
        NONE    => raise MyUndesirableArgument
      | SOME [] =>  raise MyOptionException("SOME option values can not be []")
      | SOME e  => e

fun all_except_option (s, xs) =
    let fun leftSide (s, ls, x, rs) =
            case (s, ls, x, rs ) of
                (s, ls, x, x' :: xs') => if same_string(s, x) then SOME (ls @ (x'::xs')) else leftSide(s, ls @ [x], x', xs' )
              | (s, ls, x, []) => if same_string(s, x) then SOME ls  else NONE
    in case xs of
           [] => NONE
         | x :: [] => if same_string(s, x) then SOME [] else NONE
         | x :: xs' => leftSide(s, [], x, xs')
    end





(* 1(b) *)
(*   string list list,  string -> string list

    if (is_some l) then (val_of l) @ get_substitutions1(ys, s)  else get_substitutions1(ys, s)
    if (is_some l) then subs2(e, s, ls @ val_of l) else subs2(ys, s, ls)

 *)
fun get_substitutions1 (e, s) =
    case e of
        [] => []
      | (xs :: ys) => let val l = all_except_option(s, xs)
                      in
                          case l of
                              SOME i => i @ get_substitutions1(ys, s)
                            | NONE => get_substitutions1(ys, s)

                      end

(* 1(c) *)
(*   string list list,  string -> string list *)
fun get_substitutions2 (e, s) =
    let fun subs2 (e,s, acc) =
            case e of
                [] => acc
              | (xs :: ys) => let val l = all_except_option(s, xs)
                              in
                                  case l of
                                      SOME i => subs2(ys, s, i @ acc)
                                    | NONE  => subs2(ys, s, acc)

                              end
    in
        subs2(e,s, [])
    end

(* 1(c) *)
(* {first:string,middle:string,last:string} and returns a list of full names type {first:string,middle:string,last:string} list. *)
fun change_list(name,ls, e) =
    let val {first = f, middle = m, last = l} = name
    in
        case e of
            [] => []
          | x::[] => ls @ [{first = x, middle = m, last = l}]
          | x::xs => change_list( name, ls @ [{first = x, middle = m, last = l}] , xs)
    end

(* general solution for string list list input
fun sub_names (name, e) =
    let val {first = f, middle = m, last = l} = name
    in
    case e of
    [] => []
    | [[]] => []
            | (x::xs) :: ys => change_list(name, [], x::xs) @ sub_names(name, ys)
            | [] :: ((x::xs)::ys) =>  change_list(name, [],x::xs) @ sub_names(name, ys)
            | xs :: ys => []
    end
 *)

fun similar_names (e, {first = f, middle = m, last = l}) =
    let val  name =  {first = f, middle = m, last = l}
        val  names = get_substitutions1(e, f)
    in
        name :: change_list(name, [], names)
    end



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black

(* put your solutions for problem 2 here *)



(* 2(a) *)
(* card -> color *)
datatype rank = Jack | Queen | King | Ace | Num of int
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype color = Black | Red
type card = suit * rank

fun card_color (s:suit, r:rank) =
    case (s, r) of
        (Clubs,r)   => Black
      | (Spades,r) => Black
      | (s,r) => Red
(* 2(b) *)
fun card_value (s:suit, r:rank) =
    case (s,r) of
        (_, Num i) => i
      | (_, Ace) => 11
      | (_, _) => 10
(* 2(c) *)

fun remove_card (cs,(s:suit, r:rank) , ex) =
    let fun new_list (cs, (s, r), acc) =
            case cs of
                [] => (false, [])
              | x::[] => if x = (s, r) then (true, acc) else (false, x :: acc)
              | x::xs => if x = (s, r) then (true, acc @ xs) else new_list(xs, (s, r), [x] @ acc )
    in
        case cs of
            [] => raise ex
          | x::xs => let val ls = new_list(cs, (s, r), [])
                     in
                         case ls of
                             (false, xs) => raise ex
                           | (true, xs) => xs
                     end
    end

(* 2(d) *)
fun all_same_color cs =
    case cs of
        [] => true
      | _ :: [] => true
      | first::(second::rest) => if rest = [] then (card_color(first) = card_color(second)) else
                                 (card_color(first) = card_color(second)) andalso all_same_color(second::rest)




(* 2(e) *)
fun sum_cards cs =
    let fun add_cards (cs, acc) =
            case cs of
                [] => acc
              | x::xs => add_cards(xs, acc + card_value(x))
    in
        add_cards(cs, 0)
    end

(* 2(f) *)
fun score (cs, g) =
    let val sum = sum_cards(cs)
        fun prelim_score (sum, g) =
            if sum > g then  (sum -g ) * 3 else g - sum
    in
        if all_same_color(cs) then prelim_score(sum, g) div 2
        else prelim_score(sum, g)
    end

(* 2(g) *)
(*  • The game starts with the held-cards being the empty list.
    • The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
    • If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove exception.
    • If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues with a larger held-cards and a smaller card-list.  *)

(*
 ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw], 42)
game ends if:
 mvs is empty
player discards card he doesn't have -> raise exception
drawing on empty cs
drawing causes sum ( score?)

*)
exception IllegalMove
datatype moves = Draw | Discard of suit * rank

fun exceeds_goal (hcs, g) =
    if sum_cards(hcs) > g then true else false

fun officiate (cs, mvs, g) =
    let fun do_move (cs, mvs, hcs, g) =
            case (cs, mvs, hcs, g) of
                ([], Draw::mvs', hcs, g) => score(hcs, g)
              | (cs, [], hcs, g) => score(hcs, g)
              | (x::xs, Draw::mvs', hcs, g) => if exceeds_goal(x::hcs, g) then score(x::hcs, g) else do_move(xs, mvs',x::hcs, g)
              | (cs, Discard(c)::mvs', hcs, g) => let val new_hcs = remove_card(hcs, c, IllegalMove)
                                                  in do_move(cs, mvs',new_hcs, g) end
    in
        case (cs, mvs, g) of
            (cs, [], g) => do_move(cs, [],[], g)
          | ([], m::mvs', g) => do_move([], m::mvs', [], g)
          | (cs, m::mvs', g) =>  do_move(cs, m::mvs', [], g)
    end

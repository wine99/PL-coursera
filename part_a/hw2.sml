(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (_, []) = NONE
  | all_except_option (target, x::xs) =
    case (same_string(target, x), all_except_option(target, xs))
     of (true, _) => SOME xs
      | (false, NONE) => NONE
      | (false, SOME ys) => SOME (x::ys)

fun get_substitutions1 ([], _) = []
  | get_substitutions1 (subs::more_subs, sub) =
    case all_except_option (sub, subs)
     of NONE => get_substitutions1 (more_subs, sub)
      | SOME result => result @ get_substitutions1 (more_subs, sub)

fun get_substitutions2 (all_subs, sub) =
    let fun aux ([], accu) = accu
          | aux (subs::more_subs, accu) =
            case all_except_option (sub, subs)
             of NONE => aux (more_subs, accu)
              | SOME result => aux (more_subs, accu @ result)
    in aux (all_subs, [])
    end

fun similar_names (all_subs, {first=f, middle=m, last=l}) =
    let fun substitute (choices) =
            case choices
             of [] => []
              | x::xs => {first=x, middle=m, last=l} :: substitute(xs)
    in substitute (f :: get_substitutions2(all_subs, f))
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color card =
    case card
     of (Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red
      | (Spades, _) => Black

fun card_value card =
    case card
     of (_, Num value) => value
      | (_, Ace) => 11
      | _ => 10

fun remove_card ([], card, e) = raise e
  | remove_card (c::cs, card, e) =
    case c = card
     of true => cs
      | false => c :: remove_card (cs, card, e)

fun all_same_color [] = true
  | all_same_color (c::[]) = true
  | all_same_color (c1::c2::rest) =
    (card_color c1 = card_color c2) andalso all_same_color (c2::rest)

fun sum_cards cards =
    let fun sum_after_proc (lst, proc, accu) =
            case lst
             of [] => accu
              | x::xs => sum_after_proc (xs, proc, accu + (proc x))
    in sum_after_proc (cards, card_value, 0)
    end

fun score (held_cards, goal) =
    let val sum = sum_cards held_cards
        val preliminary_score = if sum > goal
                                then 3 * (sum - goal)
                                else goal - sum
        val score = if all_same_color held_cards
                    then preliminary_score div 2
                    else preliminary_score
    in score
    end

fun officiate (card_list, move_list, goal) =
    let fun end_game held_cards = score (held_cards, goal)
        fun next_round (card_list, move_list, held_cards) =
            if sum_cards (held_cards) > goal
            then end_game (held_cards)
            else case (card_list, move_list)
                  of ([], _) => end_game held_cards
                   | (_, []) => end_game held_cards
                   | (_, (Discard c) :: rest_moves) =>
                     next_round (card_list, rest_moves, remove_card(held_cards, c, IllegalMove))
                   | (c :: rest_cards, Draw :: rest_moves) =>
                     next_round (rest_cards, rest_moves, c::held_cards)
    in next_round (card_list, move_list, [])
    end

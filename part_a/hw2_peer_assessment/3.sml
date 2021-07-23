(* I have taken it from provided.sml, as was advised *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (s, sl) =
    let fun all_except_option_with_acc ([], acc) = NONE
          | all_except_option_with_acc (shd::stl, acc) = if same_string(shd, s)
                                                         then SOME(acc @ stl)
                                                         else all_except_option_with_acc (stl, acc @ [shd])
    in
        all_except_option_with_acc (sl, [])
    end

fun get_substitutions1 ([], s) = []
  | get_substitutions1 (shd::stl, s) = case all_except_option (s, shd) of
                                           NONE => get_substitutions1 (stl, s)
                                         | SOME(sl) => sl @ get_substitutions1 (stl, s)

fun get_substitutions2 (sl, s) =
    let fun g_s_helper ([], []) = []
          | g_s_helper ([], acc) = acc
          | g_s_helper (shd::stl, acc) = case all_except_option (s, shd) of
                                             NONE => g_s_helper (stl, acc)
                                           | SOME(sl) => g_s_helper (stl, acc @ sl)
    in
        g_s_helper (sl, [])
    end

fun similar_names (nl, {first = f_n, last = l_n, middle = m_n}) =
    let
        fun iter ([], acc) = acc
          | iter (nlhd::nltl, acc) = iter (nltl, acc @ [{first = nlhd, last = l_n, middle = m_n}])
    in
        iter (get_substitutions2(nl, f_n), [{first = f_n, last = l_n, middle = m_n}])
    end


(* These are provided datatypes and type from provided.sml *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

fun card_color (Clubs, _) = Black
  | card_color (Spades, _) = Black
  | card_color (_, _) = Red

fun card_value (_, Ace) = 11
  | card_value (_, Num(n)) = n
  | card_value (_, _) = 10

fun remove_card (cs, c, e) =
    let fun remove_card_helper ([], _) = raise e
          | remove_card_helper (cshd::cstl, acc) = if cshd = c
                                                   then acc @ cstl
                                                   else remove_card_helper (cstl, acc @ [cshd])
    in
        remove_card_helper (cs, [])
    end

fun all_same_color (c1::c2::[]) = card_color(c1) = card_color(c2)
  | all_same_color (c1::[]) = true
  | all_same_color (c1::c2::tl) = card_color(c1) = card_color(c2) andalso all_same_color(c2::tl)

fun sum_cards (cs) =
    let fun sum_cards_helper ([], acc) = acc
          | sum_cards_helper (cshd::cstl, acc) = sum_cards_helper (cstl, acc + card_value (cshd))
    in
        sum_cards_helper (cs, 0)
    end

fun score (hcs, goal) =
    let val sum = sum_cards(hcs)
        val pscore = if sum > goal
                     then 3 * (sum - goal)
                     else goal - sum
    in
        if all_same_color (hcs)
        then pscore div 2
        else pscore
    end

fun officiate (cards, moves, goal) =
    let fun helper (held, c1::ctl, Draw::mtl) = if sum_cards (c1::held) > goal
						                                    then score (c1::held, goal)
						                                    else helper (c1::held, ctl, mtl)
	        | helper (held, [], Draw::mtl) = score (held, goal)
	        | helper (held, _, []) = score (held, goal)
	        | helper (held, cards, Discard (c)::mtl) = helper (remove_card (held, c, IllegalMove), cards, mtl)
    in
	      helper ([], cards, moves)
    end

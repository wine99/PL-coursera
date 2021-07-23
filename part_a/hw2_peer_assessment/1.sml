(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)

fun same_string (s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (str, xs) =
    case xs of
        []     => NONE
      | x::xs' =>
        if same_string(str, x)
        then SOME xs'
        else case all_except_option(str, xs') of
                 NONE    => NONE
               | SOME ys => SOME (x::ys)


fun get_substitutions1 (subs, s) =
    case subs of
        []    => []
      | x::xs => case all_except_option(s, x) of
                     NONE    => get_substitutions1(xs, s)
                   | SOME ys => ys @ get_substitutions1(xs, s)


fun get_substitutions2 (subs, s) =
    let fun make_list(subs, acc) =
            case subs of
                []    => acc
              | x::xs => case all_except_option(s, x) of
                             NONE    => make_list(xs, acc)
                           | SOME ys => ys @ make_list(xs, acc)
    in
        make_list(subs, [])
    end


fun similar_names (subs, name) =
    let
        val {first=x, middle=y, last=z} = name
        fun create_names xs =
            case xs of
                [] => []
              | x'::xs' => {first=x', middle=y, last=z}::(create_names(xs'))
    in
        name::create_names(get_substitutions2(subs, x))
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

fun card_color c =
    case c of
        (Clubs, _)    => Black
      | (Diamonds, _) => Red
      | (Hearts, _)   => Red
      | (Spades, _)   => Black

fun card_value c =
    case c of
        (_, Jack)  => 10
      | (_, Queen) => 10
      | (_, King)  => 10
      | (_, Ace)   => 11
      | (_, Num x) => x

fun remove_card (cs, c, e) =
    case cs of
        []    => raise e
      | x::xs => if x = c
                 then xs
                 else x :: remove_card(xs, c, e)

fun all_same_color cs =
    case cs of
        []    => true
      | _::[] => true
      | x::y::rest => card_color x = card_color y andalso all_same_color(y::rest)

fun sum_cards cs =
    let
        fun add_card_values (xs, acc) =
            case xs of
                []     => acc
              | x::xs' => card_value x + add_card_values (xs', acc)
    in
        add_card_values (cs, 0)
    end

fun score (cs, goal) =
    let
        val sum = sum_cards cs
        fun prelim_score (sum, goal) =
            if sum > goal
            then 3 * (sum - goal)
            else (goal - sum)
    in
        if all_same_color cs
        then prelim_score (sum, goal) div 2
        else prelim_score (sum, goal)
    end

fun officiate (cs, moves, goal) =
    let
        fun game (cards_left, hand, moves_left) =
            case moves_left of
                [] => score (hand, goal)
              | (Discard c)::rest =>
                game (cards_left, remove_card(hand, c, IllegalMove), rest)
              | Draw::rest =>
                case cards_left of
                    [] => score(hand, goal)
                  | c::cs' => if sum_cards (c::hand)> goal
                              then score(c::hand, goal)
                              else game (cs', c::hand, rest)
    in
        game (cs, [], moves)
    end

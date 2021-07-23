(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* Problem A *)
(* Write a function all_except_option, which takes a string and a string list. Return NONE if the string is not in the list, else return SOME lst where lst is identical to the argument list except the string is not in it. You may assume the string is in the list at most once. Use same_string, provided to you, to compare strings. Sample solution is around 8 lines. *)
fun all_except_option(str, str_list) =
    case str_list of
        [] => NONE
        | cur_str::str_list' => if same_string(cur_str, str) then SOME str_list'
                                else case all_except_option(str, str_list') of
                                    NONE => NONE
                                    | SOME str_list' => SOME (cur_str :: str_list')

(* Problem B *)
(* Write a function get_substitutions1, which takes a string list list (a list of list of strings, the substitutions) and a string s and returns a string list. The result has all the strings that are in some list in substitutions that also has s, but s itself should not be in the result. Example: get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") (* answer: ["Fredrick","Freddie","F"] *) Assume each list in substitutions has no repeats. The result will have repeats if s and another string are both in more than one list in substitutions. Example: get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") (* answer: ["Jeffrey","Geoff","Jeffrey"] *) Use part (a) and ML’s list-append (@) but no other helper functions. Sample solution is around 6 lines. *)
fun get_substitutions1(str_list_list, str) =
    case str_list_list of
        [] => []
        | str_list::str_list_list' =>
        case all_except_option(str, str_list) of
            NONE => get_substitutions1(str_list_list', str)
            | SOME str_list_except_name => str_list_except_name @ get_substitutions1(str_list_list', str)

(* Problem C *)
(* Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive local helper function. *)
fun get_substitutions2(str_list_list, str) =
    let
        fun aux(str_list_list, acc) =
            case str_list_list of
                [] => acc
                | str_list::str_list_list' =>
                case all_except_option(str, str_list) of
                    NONE => aux(str_list_list', acc)
                    | SOME str_list_except_name => aux(str_list_list', acc @ str_list_except_name)
    in
        aux(str_list_list, [])
    end

(* Problem D *)
(* Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and (c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full names (type {first:string,middle:string,last:string} list). The result is all the full names you can produce by substituting for the first name (and only the first name) using substitutions and parts (b) or (c). The answer should begin with the original name (then have 0 or more other names). *)
fun similar_names(str_list_list, {first=first, middle=middle, last=last}) =
    let
        val first_names = get_substitutions2(str_list_list, first)

        fun aux(first_names, acc) =
            case first_names of
                [] => acc
                | first::first_names' => aux(first_names', acc @ [{first=first, middle=middle, last=last}])
    in
        {first=first, middle=middle, last=last} :: aux(first_names, [])
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
(* Problem A *)
(* Write a function card_color, which takes a card and returns its color (spades and clubs are black, diamonds and hearts are red). Note: One case-expression is enough. *)
fun card_color(suit, card) =
    case suit of
        Clubs => Black
        | Spades => Black
        | Diamonds => Red
        | Hearts => Red

(* Problem B *)
(* Write a function card_value, which takes a card and returns its value (numbered cards have their number as the value, aces are 11, everything else is 10). Note: One case-expression is enough. *)
fun card_value(suit, card) =
    case card of
        Ace => 11
        | Num i => i
        | _ => 10

(* Problem C *)
(* Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a list that has all the elements of cs except c. If c is in the list more than once, remove only the first one. If c is not in the list, raise the exception e. You can compare cards with =. *)
fun remove_card(cards, card, e) =
    case cards of
        [] => raise e
        | cur_card::cards' => if cur_card = card then cards'
                              else cur_card :: remove_card(cards', card, e)

(* Problem D *)
(* Write a function all_same_color, which takes a list of cards and returns true if all the cards in the list are the same color. Hint: An elegant solution is very similar to one of the functions using nested pattern-matching in the lectures. *)
fun all_same_color(cards) =
    case cards of
        [] => true
        | _::[] => true
        | card1::(card2::cards') => if card_color card1 = card_color card2 then all_same_color(card2::cards')
                                    else false

(* Prbolem E *)
(* Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally defined helper function that is tail recursive. (Take “calls use a constant amount of stack space” as a requirement for this problem.) *)
fun sum_cards(cards) =
    let
        fun aux(cards, sum) =
            case cards of
                [] => sum
                | card::cards' => aux(cards', sum + card_value card)
    in
        aux(cards, 0)
    end

(* Problem F *)
(* Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes the score as described above. *)
fun score(cards, goal) =
    let
        val sum = sum_cards(cards)

        val preliminary_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in
        if all_same_color(cards) then preliminary_score div 2 else preliminary_score
    end

(* Problem G *)
(* Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list (what the player “does” at each point), and an int (the goal) and returns the score at the end of the game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive helper function that takes several arguments that together represent the current state of the game. As described above: *)
fun officiate(cards, moves, goal) =
    let
        fun aux(cards, moves, held_cards) =
            case moves of
                [] => score(held_cards, goal)
                | move::moves' =>
                case move of
                    Discard card => aux(cards, moves', remove_card(held_cards, card, IllegalMove))
                    | Draw =>
                    case cards of
                        [] => score(held_cards, goal)
                        | card::cards' =>
                        let
                            val held_cards' = card :: held_cards
                            val score_after_drawing = score(held_cards', goal)
                        in
                            if score_after_drawing > goal then score_after_drawing
                            else aux(cards', moves', held_cards')
                        end
    in
        aux(cards, moves, [])
    end
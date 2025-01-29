(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Maya El Zein                       *)
(* Time spent on HW6: 4                     *)

(* Collaborators and references: chatGpt    *)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)
(* This function takes a list and returns true
 * if the list is empty and false otherwise *)

fun mynull [] = true
  | mynull (_::_) = false;

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true
  
val () =
    Unit.checkExpectWith Bool.toString "mynull [1, 2] should be false"
    (fn () => mynull [1, 2])
    false

(**** Problem B ****)
(* This function takes a list of lower-case letters 
 * and returns true if the first character is a vowel 
 * and false otherwise *)

fun firstVowel [] = false
  | firstVowel (#"a"::_) = true  
  | firstVowel (#"e"::_) = true
  | firstVowel (#"i"::_) = true
  | firstVowel (#"o"::_) = true
  | firstVowel (#"u"::_) = true
  | firstVowel _ = false;

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ck' should be false"
    (fn () => firstVowel [#"c",#"k"])
    false
  
val () =
    Unit.checkExpectWith Bool.toString "firstVowel '' should be false"
    (fn () => firstVowel [])
    false

(**** Problem C ****)
(* This function takes a list and returns a new list 
 * with the elements in reverse order *)

fun reverse xs = foldl (fn (x, acc) => x :: acc) [] xs;

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [] should be []"
  (fn () => reverse [])
  []

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [5,4,3,2,1] should be [1,2,3,4,5]"
  (fn () => reverse [5,4,3,2,1])
  [1,2,3,4,5]

(**** Problem D ****)
(* This function returns the smallest element of a 
 * nonempty list of integers. An empty list of 
 * integers with throw an exception *)

fun minlist [] = raise Match
  | minlist (x::xs) = foldl Int.min x xs;

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0

val () =
  Unit.checkExpectWith Int.toString
  "minlist [12,8,9,4] should be 4"
  (fn () => minlist [12,8,9,4])
  4

(**** Problem E ****)
(* This function takes a pair of lists of equal length 
 * and returns the equivalent list of pairs. An exception 
 * Mismatch will be raised if the lengths don't match *)

exception Mismatch

fun zip([], []) = []
  | zip (x::xs, y::ys) = (x, y) :: zip (xs, ys)
  | zip (_, _) = raise Mismatch;

val () =
    Unit.checkExnWith (Unit.listString (fn (x: int, y: int) => "(" ^ Int.toString x ^ ", " ^ Int.toString y ^ ")"))
    "zip ([1,2], [4]) should raise Mismatch"
    (fn () => zip ([1,2], [4]))

val () =
    Unit.checkExpectWith (Unit.listString (fn (x: int, y: int) => "(" ^ Int.toString x ^ ", " ^ Int.toString y ^ ")"))
    "zip ([], []) should be []"
    (fn () => zip ([], []))
    []

val () =
    Unit.checkExpectWith (Unit.listString (fn (x: int, y: int) => "(" ^ Int.toString x ^ ", " ^ Int.toString y ^ ")"))
    "zip ([1,2,3], [4,5,6]) should be [(1,4), (2,5), (3,6)]"
    (fn () => zip ([1,2,3], [4,5,6]))
    [(1,4), (2,5), (3,6)]

(**** Problem F ****)
(* This function takes a list of lists and returns a single 
 * list containing all the elements in the correct order *)

fun concat [] = []
  | concat (xs::xxs) = xs @ concat(xxs);

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[1], [2, 3, 4], [], [5, 6]] should be [1, 2, 3, 4, 5, 6]"
  (fn () => concat [[1], [2, 3, 4], [], [5, 6]])
  [1, 2, 3, 4, 5, 6]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[], [], [4], []] should be [4]"
  (fn () => concat [[], [], [4], []])
  [4]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[], [], [], []] should be []"
  (fn () => concat [[], [], [], []])
  []

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[0, 9, 5], [1], [12], [7]] should be [0, 9, 5, 1, 12, 7]"
  (fn () => concat [[0, 9, 5], [1], [12], [7]])
  [0, 9, 5, 1, 12, 7]

(**** Problem G ****)
(* This function takes a single character and returns true 
 * if the character is a digit and false otherwise *)

fun isDigit #"0" = true
  | isDigit #"1" = true
  | isDigit #"2" = true
  | isDigit #"3" = true
  | isDigit #"4" = true
  | isDigit #"5" = true
  | isDigit #"6" = true
  | isDigit #"7" = true
  | isDigit #"8" = true
  | isDigit #"9" = true
  | isDigit _ = false;

val () =
    Unit.checkExpectWith Bool.toString "isDigit '0' should be true"
    (fn () => isDigit #"0")
    true

val () =
    Unit.checkExpectWith Bool.toString "isDigit 'a' should be false"
    (fn () => isDigit #"a")
    false

val () =
    Unit.checkExpectWith Bool.toString "isDigit '9' should be true"
    (fn () => isDigit #"9")
    true

(**** Problem H ****)
(* This function takes a single character and returns true if the 
 * character is an alphabetical letter and false otherwise *)

fun isAlpha c = Char.ord c >= Char.ord #"a" andalso Char.ord c <= Char.ord #"z"
  orelse Char.ord c >= Char.ord #"A" andalso Char.ord c <= Char.ord #"Z";

val () =
    Unit.checkExpectWith Bool.toString "isAlpha 'a' should be true"
    (fn () => isAlpha #"a")
    true

val () =
    Unit.checkExpectWith Bool.toString "isAlpha 'Y' should be true"
    (fn () => isAlpha #"Y")
    true

val () =
    Unit.checkExpectWith Bool.toString "isAlpha '9' should be false"
    (fn () => isAlpha #"9")
    false
  
val () =
    Unit.checkExpectWith Bool.toString "isAlpha 'z' should be true"
    (fn () => isAlpha #"z")
    true

val () =
    Unit.checkExpectWith Bool.toString "isAlpha 'A' should be true"
    (fn () => isAlpha #"A")
    true

(**** Problem I ****)
(* This function takes a tuple of four values and returns a 
 * properly formatted SVG string representing a circle element *)

fun svgCircle (cx, cy, r, fill) = "<circle cx=\"" ^ Int.toString cx ^ "\" cy=\"" 
  ^ Int.toString cy ^ "\" r=\"" ^ Int.toString r ^ "\" fill=\"" ^ fill ^ "\" />";

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (120, 150, 60, \"white\") should return <circle cx=\"120\" cy=\"150\" r=\"60\" fill=\"white\" />"
  (fn () => svgCircle (120, 150, 60, "white"))
  "<circle cx=\"120\" cy=\"150\" r=\"60\" fill=\"white\" />";

(**** Problem J ****)
(* This function takes takes a predicate function and 
 * a list, and splits the list into a list that contains 
 * elements that satisfy the predicate and into a list of 
 * elements that don't *) 

fun partition _ [] = ([], [])
  | partition p (x :: xs) =
    let
      val (t, f) = partition p xs
    in
      if p x then (x :: t, f)
      else (t, x :: f)
    end;

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x > 0) [~3, ~2, 0, 1, 2] should return ([1, 2], [~3, ~2, 0])"
  (fn () => partition (fn x => x > 0) [~3, ~2, 0, 1, 2])
  ([1, 2], [~3, ~2, 0]);


(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)

(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Maya El Zein                       *)
(* Time spent on HW6: 1 + 1
*)

(* Collaborators and references: chatGpt
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)

fun mynull []       = true
  | mynull (_::_)   = false;

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true
  
val () =
    Unit.checkExpectWith Bool.toString "mynull [1, 2] should be false"
    (fn () => mynull [1, 2])
    false


(**** Problem B ****)

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
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be false"
    (fn () => firstVowel [#"c",#"k"])
    false
  
val () =
    Unit.checkExpectWith Bool.toString "firstVowel '' should be false"
    (fn () => firstVowel [])
    false

(**** Problem C ****)

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


(**** Problem E ****)
(*
exception Mismatch

fun zip (x::xs, y::ys) = 

*)
(**** Problem F ****)
(*
fun concat xs = xs
*)
(**** Problem G ****)
(*
fun isDigit _    = false;
*)
(**** Problem H ****)
(*
fun isAlpha c = false
*)
(**** Problem I ****)
(*
fun svgCircle (cx, cy, r, fill) = "NOT IMPLEMENTED YET"

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";
*)
(**** Problem J ****)
(*
fun partition p (x :: xs) = ([],[])

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);
*)

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)

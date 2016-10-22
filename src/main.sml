structure Main =
struct

  val pl = SimplePlayer.play

  val _ = Hanabi.newGame [pl,pl,pl]

  val ngames = 200
  val _ =
    (print ("Average score +/- 2 * standard error (range) over " ^
            Int.toString ngames ^ " games:\n");
    List.tabulate (4, fn n =>
    let val scores = Hanabi.newGames ngames (List.tabulate (n+2, fn i => pl)) in
      print
           (Int.toString (n+2) ^ " players: " ^
           Real.fmt (StringCvt.FIX (SOME 3)) (Util.mean scores) ^ " +/- " ^
           Real.fmt (StringCvt.FIX (SOME 3)) (2.0 * Util.stdDev scores) ^ " (" ^
           Int.toString (Util.findMin (fn i => i) scores) ^ "-" ^
           Int.toString (Util.findMax (fn i => i) scores) ^ "). Example scores: " ^
           String.concatWith "," (map Int.toString (List.take (scores, 10))) ^ ".\n")
    end))

  val x : int ref = ref 0
  val _ = (print (Int.toString (!x)); x := !x + 1; print (Int.toString (!x)))

  val foo : unit =
  let val z : int ref = ref 0
  in z := !z + 1; print (Int.toString (!z)) end

  val bar1 : unit -> unit =
  let val z = ref 0
  in fn u => (z := !z + 1; print (Int.toString (!z))) end

  fun bar2 (u : unit) : unit =
  let val z : int ref = ref 0
  in z := !z + 1; print (Int.toString (!z)) end

  fun bar3 (b : bool) : unit -> unit =
  let val z : int ref = ref 0
  in fn u => (z := !z + 1; print (Int.toString (!z))) end

  val bar4 : bool -> unit -> unit =
  fn b => let val z : int ref = ref 0
  in fn u => (z := !z + 1; print (Int.toString (!z))) end

  val bar5 : unit -> bool -> unit =
  let val z : int ref = ref 0
  in fn u => fn b => (z := !z + 1; print (Int.toString (!z))) end

  val _ = (print "\n";foo;foo;foo)
  val _ = (print "\n";bar1 (); bar1 (); bar1 ())
  val _ = (print "\n";bar2 (); bar2 (); bar2 ())
  val _ = (print "\n";bar3 true (); bar3 true (); bar3 true ())
  val _ = (print "\n";bar3 false (); bar3 false (); bar3 false ())
  val _ = (print "\n";bar4 true (); bar4 true (); bar4 true ())
  val _ = (print "\n";bar4 false (); bar4 false (); bar4 false ())
  val _ = (print "\n";bar5 () true; bar5 () true; bar5 () true)
  val _ = (print "\n";bar5 () false; bar5 () false; bar5 () false)

end

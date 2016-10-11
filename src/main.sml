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

end

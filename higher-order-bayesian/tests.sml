(*  COMP 212 Homework 8:  Higher-order programming.
*   
*   N. Danner
*)

structure Tests =
struct

  structure U = UnitTest
  structure TR = TestRunner

  type category = Hw8.category
  type doc = Hw8.doc
  type wordcounts = Hw8.wordcounts
  type ctycounts = Hw8.ctycounts
  type wordprobs = Hw8.wordprobs
  type ctyprobs = Hw8.ctyprobs

  (* ************************************************************************ 
  *  UTILITY FUNCTIONS
  *
  *  A number of the functions return ('a * 'b) lists for some 'a and 'b,
  *  but the order does not matter.  These utility functions are used to sort
  *  the returned lists into a canonical order, which in turn makes checking
  *  correctness simpler.
  *
  *  ************************************************************************ 
  *)


  (*  stringGt(w, w') = true,  w occurs after w' lexicographically,
  *                     false, o/w.
  *)
  fun stringGt (w : string, w' : string) : bool =
    case String.compare(w, w') of
         GREATER => true
       | _ => false

  (*  wcGt ((w, _), (w', _)) = stringGt(w, w').
  *)
  fun wcGt ((w, _) : string*'a, (w', _) : string*'a) : bool =
    stringGt(w, w')

  fun ccGt (
      (c, _) : category*'a, 
      (c', _) : category*'a) : bool =
    stringGt(c, c')

  (*  sortByCty ccs = ccs', where ccs' is obtained by sorting ccs 
  *   by categories, and sorting each word count for each category
  *   according to the words.
  *)
  fun sortByCty (ccs : (string*((string*'a) list)) list) :
    (string*((string*'a) list)) list =
    ListMergeSort.sort
      ccGt
      (map (fn (c, wcs) => (c, ListMergeSort.sort wcGt wcs)) ccs)

  (*  ************************************************************************ 
  *   STRINGIFY FUNCTIONS.
  *
  *   These functions return a string representation of their arguments,
  *   used when printing out information.
  *
  *   ************************************************************************
  *)

  val cdsToString : (category*doc) list -> string =
    ListFormat.listToString (
      fn (c, d) =>
        String.concat [
          "(",
          c,
          ", ",
          ListFormat.listToString 
            String.toString 
            (List.take(d, Int.min(length d, 15))),
          ")"
        ]
    )

  val wcsToString : wordcounts -> string =
    ListFormat.listToString
      (fn (w, c) => "(" ^ w ^ ", " ^ (Int.toString c) ^ ")")

  val ccsToString : ctycounts -> string =
    ListFormat.listToString
      (fn (c, wcs) => "<" ^ c ^ ", " ^ (wcsToString wcs) ^ ">")

  val wpsToString : wordprobs -> string =
    ListFormat.listToString
      (fn (w, p) => "(" ^ w ^ ", " ^ (Real.toString p) ^ ")")

  val cpsToString : ctyprobs -> string =
    ListFormat.listToString
      (fn (c, wps) => "<" ^ c ^ ", " ^ (wpsToString wps) ^ ">")

  (*  ************************************************************************ 
  *   TESTING FUNCTIONS.
  *
  *   These are the functions that return tests of various functions in HW8.
  *
  *   ************************************************************************
  *)

  (*  testCounts(keyws, cds, exp) is a test that succeeds if
  *     sortByCty(Hw4.count(keyws, cds)) = sortByCty(exp).
  *)
  fun testCounts (
      keyws : string list,
      cds : (category*doc) list,
      exp : ctycounts) : U.test =
    U.assertEq(
      String.concatWith " " [
        ListFormat.listToString String.toString keyws,
        ": ",
        cdsToString cds
      ],
      fn () => sortByCty (Hw8.count(keyws, cds)),
      sortByCty (exp),
      ccsToString
    )

  (*  testCtyProbs(ccs, exp) is a test that succeeds if
  *     sortByCty(Hw4.makeCtyProbs(ccs)) = sortByCty(exp).
  *)
  fun testCtyProbs (ccs : ctycounts, exp : ctyprobs) : U.test =
  let
    fun eq(cps : ctyprobs, cps' : ctyprobs) : bool =
      ListPair.allEq (
        fn ((c, wps), (c', wps')) => 
          c = c' andalso ListPair.allEq(
            fn ((w, p), (w', p')) => w = w' andalso Real.==(p, p')
          ) (wps, wps')
      ) (cps, cps')
  in
    U.makeTestEq(
      ccsToString(ccs),
      fn () => sortByCty (Hw8.makeCtyProbs ccs),
      sortByCty exp,
      eq,
      cpsToString
    )
  end

  (*  testLL(K, wps, d, exp) is a test that succeeds if 
  *     abs(Hw4.computeLL(K, d, wps) - exp) < 1.0e~6.
  *)
  fun testLL(K : string list, wps : wordprobs, d : doc, exp : real) : U.test =
    U.assertAlmostEqReal(
      wpsToString(wps),
      fn () => Hw8.computeLL(K, d, wps),
      exp,
      1.0e~6)




  (*  The values keyws1, cds1, counts1, probs1, and d1 correspond to the running
  *  example from the assignment.
  *)
  val keyws1 = ["Fred", "George", "Sam"]

  (*  Each element of the list component of this value is a test of the form
  *  testCounts(keyws, cds, ccs).  The test succeeds if Hw8.count(keyws, cd)
  *  returns a ctycounts value that is equivalent to ccs (so order doesn't
  *  matter).
  *)
  val countTests = ("count", [
    testCounts(keyws1, 
        [
          ("cat1", ["Fred"]), 
          ("cat2", ["George"]), 
          ("cat1", ["Fred", "George"])
        ], 
        [
          ("cat1", [("Fred", 2), ("George", 1), ("Sam", 0)]),
          ("cat2", [("Fred", 0), ("George", 1), ("Sam", 0)])
        ])
  ])

  (*  Each element of the list component of this value is a test of the form
  *  testCtyProbs(ccs, wps).  The test succeeds if Hw8.makeCtyProbs(ccs) returns
  *  a value that is equivalent to wps (so order doesn't matter).
  *)
  val probTests = ("makeCtyProbs", [
    testCtyProbs(
        [
          ("cat1", [("Fred", 2), ("George", 1), ("Sam", 0)]),
          ("cat2", [("Fred", 0), ("George", 1), ("Sam", 0)])
        ],
        [
          ("cat1", [("Fred", 2.0/3.0), ("George", 1.0/3.0), ("Sam", 1.0/3.0)]),
          ("cat2", [("Fred", 1.0), ("George", 1.0), ("Sam", 1.0)])
        ]
    )
  ])

  (*  Each element of the list component of this value is a test of the form
  *  testLL(keyws, wps, d, ll).  The test succeeds if Hw8.computeLL(keyws, d,
  *  wps) == ll, where == means that the absolute difference is <1.0e~6.
  *)
  val llTests = ("computeLL", [
    testLL(keyws1,
        [("Fred", 2.0/3.0), ("George", 1.0/3.0), ("Sam", 1.0/3.0)],
        ["Fred", "George", "Sam", "Fred"],
        ~1.30642502755),
    testLL(keyws1,
        [("Fred", 2.0/7.0), ("George", 4.0/7.0), ("Sam", 1.0/7.0)],
        ["Fred", "George", "Sam", "Fred"],
        ~2.1762721774)
  ])

  val allTests = [
    countTests,
    probTests,
    llTests
  ]

  fun runTests() =
    TR.runTimedTestSuites(allTests, 60, true)

  fun main(_ : string, _ : string list) : int =
    runTests()
    handle e =>
    let
      val () = print ("Exception raised:  " ^ (exnName e) ^ "\n")
      val () = print ("\t" ^ (exnMessage e) ^ "\n")
      val () = print ("Backtrace:\n\t")
      val () = print(
        ListFormat.fmt 
          {init="", sep="\n", final="", fmt=String.toString}
          (SMLofNJ.exnHistory e)
      )
      val () = print "\n"
    in
      1
    end


end

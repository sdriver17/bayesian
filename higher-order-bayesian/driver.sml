(*  COMP 212 Homework 8:  Higher-order functions.
*   
*   N. Danner
*
*   See assignment for details on how to use this program.
*)

structure Driver =
struct

  structure T = TextIO
  structure S = T.StreamIO

  type category = Hw8.category
  type doc = Hw8.doc

  fun printnl (s : string) : unit =
    print (s ^ "\n")

  val usage = String.concatWith "\n" [
    "driver keywfile traindir unknowndir",
    "\tkeywfile:  file with list of keywords (one per line)",
    "\ttraindir:  directory of training data",
    "\tunknowndir:  directory of files to be classified"
  ]

  (*  splitFile(p, f) = ws, where ws is the list of tokens in the file f.  A
  *  token is a consecutive sequence of characters delimited by any character c
  *  such that p(c) = true.
  *)
  fun splitFile (p : char -> bool) (f : string) : string list =
  let
    val ins = T.getInstream(T.openIn f)
    val (contents, _) = S.inputAll ins
  in
    String.tokens p contents before S.closeIn ins
  end

  (*  getKeyWords(f) = ws, where ws is the list of lines in the file f.
  *)
  val getKeyWords = splitFile (fn c => c = #"\n")

  (*  isws(c) = true if c is a whitespace character.
  *)
  fun isws (c : char) : bool =
    c = #" " orelse c = #"\t" orelse c = #"\n" orelse c = #"\r"

  (*  fileToDoc(f) = ws, where ws is the list of words in the file f, where a
  *  "word" is a consecutive sequence of non-whitespace characters delimited by
  *  whitespace.
  *)
  val fileToDoc = splitFile isws

  fun getCategories (ds : OS.FileSys.dirstream) : category list =
    case OS.FileSys.readDir ds of
         NONE => []
       | SOME f => if String.sub(f, 0) = #"." then getCategories ds
                   else f :: getCategories ds

  fun getDocs (d : string) : doc list =
  let
    fun getDocsFromDirstream (ds : OS.FileSys.dirstream) : doc list =
      case OS.FileSys.readDir ds of
           NONE => []
         | SOME f => if String.sub(f, 0) = #"." then getDocsFromDirstream ds
                     else fileToDoc (d ^ "/" ^ f) :: getDocsFromDirstream ds
    val ds = OS.FileSys.openDir d
  in
    getDocsFromDirstream ds before OS.FileSys.closeDir ds
  end

  fun getCatsDocs (trainDir : string) : (category*doc) list =
  let
    val cats = getCategories (OS.FileSys.openDir trainDir)
    val catsdocs : (category*(doc list)) list = 
      map (fn c => (c, getDocs (trainDir ^ "/" ^ c))) cats
    val catsdocs : (category*doc) list list =
      map (fn (c, ds) => map (fn d => (c, d)) ds) catsdocs
  in
    List.concat catsdocs
  end

  fun getUnknownDocs (unknownDir : string) : (string*doc) list =
  let
    fun getUnknowns (ds : OS.FileSys.dirstream) : (string*doc) list =
      case OS.FileSys.readDir ds of
           NONE => []
         | SOME f => if String.sub(f, 0) = #"." then getUnknowns ds
                     else (f, fileToDoc (unknownDir ^ "/" ^ f)) :: 
                          getUnknowns ds
    val ds = OS.FileSys.openDir unknownDir
  in
    getUnknowns ds before OS.FileSys.closeDir ds
  end

  fun probsToString (probs : (string*((category*real) list)) list) : string =
    ListFormat.fmt {
      init="",
      sep="\n",
      final="\n",
      fmt=fn (s, crs) => (s ^ ":\n" ^ 
        ListFormat.fmt {
          init="",
          sep="\n",
          final="\n",
          fmt=fn (c, r) => "\t" ^ c ^ ": " ^ (Real.toString r)
        } crs
      )
    } probs

  fun main(arg0 : string, args : string list) : int =
  let
    val rtTotal = Timer.startRealTimer()

    val [keywFile, trainDir, unknownDir] = args

    val keyws : string list = getKeyWords keywFile

    val trainingData : (category*doc) list = getCatsDocs trainDir

    (*  Notice how the declaration and use of classifier has changed from the
    *  corresponding definition and use in the HW 4 driver.
    *  - In HW 4, classifier is a function, so its declaration takes no time to
    *  execute.  However, the call to (classifier(d)) results in the computation
    *  of the category probability distributions from the training data *every
    *  time it is evaluated*.  This is wasteful, because those distributions
    *  really only need to be computed once (they don't depend on the document
    *  being analyzed).
    *  - In HW 8, classifier is a value.  If you have implemented it correctly,
    *  then evaluating makeClassifier(keyws, trainingData) should do all the
    *  work of computing the category probability distributions for the training
    *  data.  The call to (classifier d) below should result in computing only
    *  the log-likelihoods for the document d.
    *
    *  As a result, you should see that computing the probabilities for a
    *  non-trivial data set takes significantly less time with your HW 8
    *  implementation than with your HW 4 implementation.
    *)

    val () = print "Building classifier..."
    val rt = Timer.startRealTimer ()
    val classifier : doc -> (category*real) list =
      Hw8.makeClassifier(keyws, trainingData)
    val elapsed = Timer.checkRealTimer rt
    val () = printnl (
      "done (" ^ (IntInf.toString (Time.toMilliseconds elapsed)) ^ " ms.)."
    )

    val unknownData : (string*doc) list = getUnknownDocs unknownDir

    val () = print "Computing probabilities..."
    val rt = Timer.startRealTimer ()
    val probs : (string*((category*real) list)) list =
      map (fn (s, d) => (s, classifier d)) unknownData
    val elapsed = Timer.checkRealTimer rt
    val () = printnl (
      "done (" ^ (IntInf.toString (Time.toMilliseconds elapsed)) ^ " ms.)."
    )

    val () = printnl (probsToString probs)

    val elapsedTotal = Timer.checkRealTimer rtTotal
    val () = printnl (
      "Total time: " ^ (IntInf.toString (Time.toMilliseconds elapsedTotal)) ^ " ms."
    )

  in
    0
  end
  handle Bind => (printnl usage ; 1)
    | e => (
        printnl (String.concatWith " " ["Exception raised: ", exnMessage e])
        ; 1
      )

end

(*
*
    Samuel Driver
*)

structure Hw8 =
struct

  exception Unimplemented

  (*  Naming conventions.
  *   - c : category
  *   - d : doc
  *   - ccs : ctycounts
  *   - wcs : wordcounts
  *   - wps : wordprobs
  *   - keyws : string list (list of keywords)
  *)

  (*  These type declarations just let us use meaningful names for the types
  *  that we use here.
  *)

  type category = string
  type doc = string list

  (*  Think of a value wcs : wordcounts as a function from strings to ints.
  *   If wcs = [(s_0, i_0),...,(s_n, i_n)], then the value wcs on string s
  *   is computed as follows:
  *   - if s = s_j, then the value is i_j
  *   - if s <> s_j for any j, then the value is undefined.
  *   In the documentation below, we will write wcs(s) for the value of wcs
  *   on string s.
  *
  *   Think of each of the list types below in the same way.
  *)
  type wordcounts = (string*int) list

  (*  The type of functions from categories to wordcounts.
  *)
  type ctycounts = (category*wordcounts) list

  (*  A type of functions from strings to reals.
  *)
  type wordprobs = (string*real) list

  (*  A type of functions from categories to wordprobs.
  *)
  type ctyprobs = (category*wordprobs) list

  (*  ********************
  *   Un-Curried list functionals.
  *)

  (*  filter0(p, [x_0,...,x_{n-1}]) = ys, where ys is the sublist of xs
  *  consisting of those elements x of xs such that p(x) = true.
  *
  *
  *)
  fun filter0(p : 'a -> bool, xs : 'a list) : 'a list =
    case xs of
         [] => []
       | y :: ys => if p y then y :: filter0(p, ys) else filter0(p, ys)

  (*  find0(0, [x_0,...,x_{n-1}]) = SOME(x_i), if p(x_i) = true and
  *                                            p(x_j) = false for j < i
  *                               = NONE,      if there is no i such that
  *                                            p(x_i) = true
  *)
  fun find0(p : 'a -> bool, xs : 'a list) : 'a option =
    case xs of
         [] => NONE
       | y :: ys => if p y then SOME y else find0(p, ys)

  (* JUST FOR TESTS *)
  fun exists0(p: 'a -> bool, xs: 'a list) : bool =
    case xs of
         [] => false
       | y::ys => if p y then true else exists0(p, ys)

  (*  map0(f, [x_0,...,x_{n-1}]) = [f(x_0),...,f(x_{n-1})].
  *)
  fun map0(f : 'a -> 'b, xs : 'a list) : 'b list =
    case xs of
         [] => []
       | y :: ys => f(y) :: map0(f, ys)

  (*  fold0(f, b, [x_0,...,x_{n-1}]) =
  *     f(x_0, f(x_1, f(..., f(x_{n-1}, b)...))).
  *)
  fun fold0(f : 'a*'b -> 'b, b : 'b, xs : 'a list) : 'b =
    case xs of
         [] => b
       | y :: ys => f(y, fold0(f, b, ys))

  (*checkDupe(cds) = cd2 where cd2 is cds without any duplicate elements *)
  fun checkDupe(cds:(category*doc) list):(category*doc) list =
  let
    fun dupe((cd,l):((category*doc)*((category*doc) list))):(category*doc) list =
    let
      fun checkDupe'(cd1:(category*doc)):bool =
        if #1(cd1) = #1(cd) then true else false
    in
      if List.exists checkDupe' l then l else cd::l
    end
  in
     foldr dupe [] cds
  end

  (*docFreq(word,doc) = n where n is the number of occurences of word in the
  * document*)
  fun docFreq(word:string,doc:string list):int =
  let
    fun wordMatch((d,n):string*int):int =
      if d=word then n+1 else n
  in
    foldr wordMatch 0 doc
  end

  (*checkFreq(cds, c, word) = n where n is the number of occurences of word in
    * all docs in category c within cds*)
  fun checkFreq(cds:(category*doc) list, cat:category, word:string):int =
  let
    fun getFreq((cd,n):(category*doc)*int) =
      if #1(cd) = cat then n+docFreq(word,#2(cd)) else n
  in
    foldr getFreq 0 cds
  end

  (*makeWcs(keyws,cat,cds) = wcs where wcs is a wordcounts value for the
  * category c from cds*)
  fun makeWcs(keyws:string list, cat: category,
                                 cds:(category*doc) list): wordcounts =
  let
    fun makeWcs'(word:string):string*int =
      (word,checkFreq(cds,cat,word))
  in
    map makeWcs' keyws
  end

  (*  count (keyws, cds) = ccs, where ccs(c) is the word count for all documents
  *   in cds with category c.
  *)
  fun count (keyws : string list, cds : (category*doc) list) : ctycounts =
  let
    val cd2:(category*doc) list = checkDupe(cds) (* cds without dupes *)
    fun initCtyCounts((cd,l):(category*doc)*((category*wordcounts)list))
      :ctycounts = (#1(cd),makeWcs(keyws,#1(cd),cds))::l
  in
    foldl initCtyCounts [] cd2
  end

  (*getTotal(wcs) = n where n is the sum total of all int values in the wordcounts
  * list*)
  fun getTotal(wcs:wordcounts):real =
  let
    fun getReals((w,r):(string*int)*real):real = Real.fromInt(#2(w)) + r
  in
    foldr getReals 0.0 wcs
  end

  (*makeWps(wcs) = wps where wps is wcs but the integer portion of it is
  * converted to a real and then divided by getTotal(wcs)*)
  fun makeWps(wcs:wordcounts):wordprobs =
  let
    fun makeWps'((w,n):(string*int)):(string*real) =
      if n = 0 then (w,1.0/getTotal(wcs))
               else (w,(Real.fromInt(n)/getTotal(wcs)))
  in
    map makeWps'(wcs)
  end



  (*  makeCtyProbs ccs = cps, where cps(c) = makeWordProbs(ccs(c))
  *)
  fun makeCtyProbs(ccs : ctycounts) : ctyprobs =
  let
    fun makeWordProb((c,wcs):(category*wordcounts)):category*wordprobs =
      (c, makeWps(wcs))
  in
    map makeWordProb ccs
  end

  fun findProb(wps:wordprobs, word:string):(string*real) option =
  let
    fun findProb'((w,r):(string*real)):bool =
      if w = word then true else false
  in
    List.find findProb' wps
  end

  (*  computeLL (keyws, d, wps) = the log-likelihood of the document d
  *   being produced by the model wps.
  *)
  fun computeLL (keyws : string list, d : doc, wps : wordprobs) : real  =
  let
    fun makeLL((word,n):(string*real)):real =
      Real.fromInt(docFreq(word,d))*
      Math.log10(#2(Option.valOf((findProb(wps,word))))) + n
  in
    foldr makeLL 0.0 keyws
  end

  (*  makeClassifier (keyws, cds) = cl, where cl(d) = [...,(c, r),...],
  *   where the list ranges over pairs (c, r) such that c is a category
  *   in cds and r is the log-likelihood that the document d is produced
  *   by the model computed from c.
  *)
  fun makeClassifier
      (keyws : string list, cds : (category*doc) list)
      : doc -> (category*real) list =
  let
    val ctyCounts:ctycounts = count(keyws,cds)
    val ctyProbs:ctyprobs = makeCtyProbs(ctyCounts)
  in
    fn (d:doc) =>
      let
        fun makeClassifier'((c,wps):(category*wordprobs)):(category*real) =
          (c,computeLL(keyws,d,wps))
      in
        map makeClassifier' ctyProbs
      end
  end


end

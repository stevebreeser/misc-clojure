;;;;
;;;; Fib and Lucas foo
;;;;
;;
;; Note:  Lucas sequence :  analogous to Fibs but start with 1, 3 (, 4, 7, etc.)
;;
;; Fwiw:
;; Limit of Fibs and closed form:  
;;    http://jwilson.coe.uga.edu/EMAT6680Su07/Francisco/Assignment12/fibonacci.html
;;

(defn floatdiv[a b] (float (/ a b)))
(defn doublediv[a b] (double (/ a b)))
(defn make-slopes [stream] (map doublediv stream (rest stream)))
(defn make-pairs-stream [stream] (map vector stream (rest stream)))

(def fibs (lazy-cat [0N 1N] (map + fibs (rest fibs))))
(def lucases (lazy-cat [1N 3N] (map + lucases (rest lucases))))

(def fibpairs (make-pairs-stream fibs))
(def fibpairslopes (make-slopes fibs))
(def fibpairslopeslopes (make-slopes (make-slopes fibs)))

(def lucaspairs (make-pairs-stream lucases))
(def lucaspairslopes (make-slopes lucases))
(def lucaspairslopeslopes (make-slopes (make-slopes lucases)))

(defn make-binary-string [ls-bools]
	(apply str (map #(if % 1 0) ls-bools)))

(defn make-binary-string-of-len-maker [stream] 
    (fn foo-mbsolm 
	  ([n] (foo-mbsolm 0 n))
	  ([d n] 
	 	(make-binary-string (take (- n d) (drop d (map #(> % 1) stream)))))))

(def binary-of-fib-slope-slopes 
  (make-binary-string-of-len-maker fibpairslopeslopes))

(def binary-of-lucas-slope-slopes
  (make-binary-string-of-len-maker lucaspairslopeslopes))

(defn fib-and-lucas-limits-equal? 
	"note: limit of f(n) / f(n+1), where f is fib and lucas.  Uses 45th element."
	[]
	(apply = (map #(nth % 45) (list fibpairslopes lucaspairslopes))))

(defn oscillating? [binarystring] 
	(if (< (count binarystring) 2) 
		true
		(reduce #(and %1 %2) (map not= binarystring (rest binarystring)))))

(defn tests-oscillating? []
  (pprint
    (map 
      oscillating?
      '(
		""
		"0"
		"1"
		"00"
		"01"
		"10"
		"11"
		"000"
		"001"
		"010"
		"011"
		"100"
		"101"
		"110"
		"111"
	  ))))

(oscillating? (binary-of-fib-slope-slopes 40))  ;; ==> true 
(oscillating? (binary-of-fib-slope-slopes 50))  ;; ==> false because of precision 

(oscillating? (binary-of-lucas-slope-slopes 40))  ;; ==> true 
(oscillating? (binary-of-lucas-slope-slopes 50))  ;; ==> false because of precision 


;;
;; Prove:
;; Given (a, b) two sequential fibs and (x, y) also two sequential fibs, 
;; Prove ax + by is a fib
;;
;; Note, this does not hold for Lucas, as is obvious from inspection and also shown below.
;;


;; the for[] creates redundant permuations
(defn ax-plus-by [pairs]
  (fn [n]
	(for [[a b] (take n pairs) 
		  [x y] (take n pairs)]
	  (+ (* (bigint a) x) (* (bigint b) y)))))

(def fibs-ax-plus-by
	(ax-plus-by fibpairs))

(def lucas-ax-plus-by
	(ax-plus-by lucaspairs))

(defn proof [theseq ax-plus-by-examples]
  (fn [n] 
    (true? 
    	(reduce #(and %1 %2) (map (fn [x] (some #(= % x) theseq)) (ax-plus-by-examples n))))))

(def fibs-proof
	(proof (take 1000 fibs) fibs-ax-plus-by)) ;; so we eventually stop trying

(def lucas-proof
	(proof (take 1000 lucases) lucas-ax-plus-by)) ;; so we eventually stop trying

(defn tests-prove-ax-plus-by-is-in-seq 
    "Resulting fn[] takes a minute to run."
	[seq-proof]
	(fn []
	  (pprint
		(map 
			seq-proof
			'(
				1     ;; ==> true
			    46    ;; ==> true 
			    47    ;; ==> true ;; if not BigInt, then ==> ArithmeticException integer overflow
			    100   ;; ==> true ;; ditto
			    500   ;; ==> true ;; ditto
			    501   ;; ==> false ;; !! because we limited the proof fn to stop after (take 1000 fibs)
			    )))))

(def tests-fibs-prove-etc 
	(tests-prove-ax-plus-by-is-in-seq fibs-proof))
; ==> (true true true true true false)

(def tests-lucas-prove-etc 
	(tests-prove-ax-plus-by-is-in-seq lucas-proof))
; ==> (false false false false false false)

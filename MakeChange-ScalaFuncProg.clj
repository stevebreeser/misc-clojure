;; Problem posed in M. Odesky "Functional Programming in Scala" course
;;
;; Function that determines permutations for "making change" for a given amount of money 
;; Parameters:
;;   amount -- a given amount of money
;;   denominations -- a given list of available denominations
;;

;; A few different solutions below.

;;
;; a = amount to change
;; ds = list of denominations, ordered ascending
;; acc == accumulator for permuations
;;
(defn makechange [a ds acc] 
	(cond
		(empty? ds)  '()
		(< a (first ds))  '()
		(= a (first ds))  (list (cons (first ds) acc))
		'else  (concat 
					(makechange a (rest ds) acc)
					(makechange (- a (first ds)) ds (cons (first ds) acc)))))

(defn mkchg [a ds]
	(let [combos (makechange a ds '())]
		(vector (count combos) combos)))

;;
;; a = amount to change
;; ds = list of denominations, ordered ascending
;; acc == accumulator for permuations
;;
(defn makechange-cps [a ds acc cc] 
	(cond
		(empty? ds)  (cc '())
		(< a (first ds))  (cc '())
		(= a (first ds))  (cc (list (cons (first ds) acc)))
		'else  (makechange-cps a (rest ds) acc 
						(fn [x]
							(makechange-cps (- a (first ds)) ds (cons (first ds) acc) 
								(fn [y]
									(cc (concat x y))))))))

(defn mkchg-cps [a ds]
	(let [combos (makechange-cps a (sort ds) '() (fn [x] x))]
		(vector (count combos) combos)))

;;
;;  Hmm, this isn't quite right
;;
(defn makechange-cps-tail-rec [a ds acc cc]
  (loop [	a 	a
  			ds 	ds
  			acc acc
  			cc 	cc] 
	(cond
		(empty? ds)  (cc '())
		(< a (first ds))  (cc '())
		(= a (first ds))  (cc (list (cons (first ds) acc)))
		'else  (recur a (rest ds) acc 
						(fn [x]
							;(recur ...  but it complains that recur should only have one argument ?!
							;            presumably because of the f[x] continuation
							;
							(makechange-cps-tail-rec (- a (first ds)) ds (cons (first ds) acc) 
								(fn [y] 
									(cc (concat x y)))))))))

(defn mkchg-cps-tail-rec [a ds]
	(let [combos (makechange-cps-tail-rec a (sort ds) '() identity)]
		(vector (count combos) combos)))


(defn mkchg-num-combos [a ds]
	(cond 
		(empty? ds)  0
		(< a (first ds))  0
		(= a (first ds))  1
		'else (+ (mkchg-num-combos a (rest ds)) (mkchg-num-combos (- a (first ds)) ds))))


;
; Some test examples
;
;; N.B.  If the given amount is one of the available denominations, 
;;       then a single token of that denomination is a valid answer. 
;;       e.g.  Giving a 5 as change for a 5.  

;(mkchg 5 '(5 10 20 50))  
;(mkchg-cps 5 '(5 10 20 50))  
;(mkchg-cps-tail-rec 5 '(5 10 20 50))  
;(mkchg-num-combos 5 '(5 10 20 50))  


(defn tests []
  (pprint 
  (map 
  	(fn [args] 
	  (map 
	  	(fn [f] (apply f args))
	    (list mkchg 
	    	  mkchg-cps 
	    	  mkchg-cps-tail-rec 
	    	  mkchg-num-combos) ))
    '(  (5 (5 10 20 50))
	    (30 (5 10 20 50))
	    (60 (5 10 20 50)) 
	    (65 (5 10 20 50)) 
	    ))))

(tests)

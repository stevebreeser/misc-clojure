;
; contains-summing-pair determines whether there are two elements in a sorted array which sum to x
;
; sortedvec:  sorted vector of numbers
; x:  number
;

(defn contains-summing-pair [sortedvec x]
  (loop [v sortedvec]
  	(if (< (count v) 2)
  	  nil
      (let [a (first v)
	     	z (last v)
		    sum  (+ a z)]
        (cond 
	      (= sum x)  (list a z)
	      (< sum x)  (recur (rest v))
	      (> sum x)  (recur (drop-last v)))))))

(defn contains-summing-pair2 [v x]
	(let [sums 
		  (for [a v
		        b v]
	  	    (if (not= a b) 
	  		  { (+ a b) (list a b) } ))]
	  ((apply merge (filter (complement nil?) sums)) x)))


(defn tests []
  (pprint 
  (map 
  	(fn [args] 
	  (map 
	  	(fn [f] (apply f args))
	    (list 
	    	contains-summing-pair 
	    	contains-summing-pair2
	    	  )))
    '(  ([1 2 3 4 5 27 29] 6)
    	([1 2 3 4 5 27 29] 17)
    	([1 3 5 11 13 27 29] 16)
    	([1 3 5 11 13 27 29] 17)
    	([1 3 5 11 13 27 29] 30)
    	([1 3 5 11 13 27 29] 31)
    	([1 3 5 11 13 27 29] 1)
    	([1 3 5 11 13 27 29] 60)

	 ))))


(tests)


;;
;; map-reduce
;;

 
;; Map-(Combine-Prereduce-)Reduce
;;
;; 1) A simple warm-up with the identity function
;; 2) A Map-Reduce combinator (make-mr)
;; 

;;
;; The Warm-up -- a set of functions which just performs the identity function
;;
;; id- <blah> ==> identity functions
;;
;;
(defn id-mr-map [key vals]
	(map (fn [x] (list x x)) vals))

(defn id-mr-combine [ls-kvs]
	ls-kvs)

(defn id-mr-reduce-sort-etc [ls-kvs]
	(loop [ls  ls-kvs
		   acc  '{}]
		(if (empty? ls)
			acc
			(let [[k v]  (first ls)
				  newval (if (contains? acc k)
				  			 (cons v (get acc k))
				  			 (cons v '()))]
				(recur (rest ls) (assoc acc k newval))))))

(defn id-mr-reduce [mappp]
	(zipmap (keys mappp) (map identity (vals mappp))))


; Steps:
;    - First three steps all happen on the hypothetical "mapper" machines
; 1) Map
;    - Steps 2 and 3 are just optimizations.  Not applicable for some reduce functions.  (Only works for commutative associative reducers.)
; 2) Combine
; 3) Reduce some, sort, etc.
;    - Fourth step happens on hypothetical "reducer" machines
; 4) Reduce
;
; The full recipe for a simple identity function map-reduce
(defn id-mr [key vals]
	(id-mr-reduce (id-mr-reduce-sort-etc (id-mr-combine (id-mr-map key vals)))))



;;
;;
;; Generalized Map-Reduce maker functions
;;
;;

;; this has become trivial
(defn make-mr-map [f]
	(fn [key vals]
		(f key vals)))

;; this has become trivial
(defn make-mr-combine [f]
	(fn [ls-kvs]
		(f ls-kvs)))

(defn helper--if-in-map-etc [f base] 
	(fn [m k v]
		(if (contains? m k)
			(f v (get m k))
			(f v base))))

(defn make-mr-pre-reduce []
	(fn [ls-ls-kvs]
		(loop [ls  (apply concat ls-ls-kvs)
			   acc  '{}]
			(if (empty? ls)
				acc
				(let [[k v]  (first ls)
				  	  newval ((helper--if-in-map-etc cons '()) acc k v)]
					(recur (rest ls) (assoc acc k newval)))))))

(defn make-mr-reduce [f]
	(fn [mappp]
		(zipmap (keys mappp) (map f (vals mappp)))))

(defn make-mr [f-map f-combine f-reduce]
	(fn [doc-map]
		(let [reducer (make-mr-reduce f-reduce)
			  pre-reducer (make-mr-pre-reduce)
			  combiner (make-mr-combine f-combine)
			  mapper (make-mr-map f-map)]
			 (reducer (pre-reducer
				(map (fn [[k vs]] (combiner (mapper k vs))) doc-map))))))


; Example "documents" a, b, c
(def docs1 (zipmap '(a b c) 
				(map (fn [str] (clojure.string/split str #"\s")) 
					'("the milk is dubious"  
			 		  "the brown milk came from the brown cow" 
					  "the very dubious milk cow is brown"))))

;;
;; Example:  Use the maker functions to make the identity map-reduce functions (as above)
;;
(def cool-id-mr 
	(make-mr 
		(fn [k vs] (map (fn [x] (list x x)) vs)) 
		identity 
		identity))

(defn mr-example-thunk-id []
	(cool-id-mr docs1))

;;
;; More exmaples:  canonical word count and some variations thereon
;;
(defn make-kvs-combiner [f base]
	(fn [ls-kvs]
		(reduce (fn [m [k v]] (assoc m k ((helper--if-in-map-etc f base) m k v))) '{} ls-kvs)))

(def cool-wc-mr 
		(make-mr 
			(fn [k vs] (map (fn [x] (list x 1)) vs)) 
			(make-kvs-combiner + 0)
			(fn [ls] (apply + ls))))

(defn mr-example-thunk-wc []
	(cool-wc-mr docs1))

;; NOTE:  adds :LTE-to-3 symbol as key to accumulate count of words of size <=3
(def count-words-longer-than-three-chars
	(make-mr
		(fn [k vs] (map (fn [s] (if (< 3 (count s)) (list s 1) (list :LTE-to-3 1))) vs))
		(make-kvs-combiner + 0)
		(fn [ls] (apply + ls))))

(defn mr-example-thunk-wc-gt3 []
	(count-words-longer-than-three-chars docs1))

(def mr-max-occur-in-docs
	(make-mr
		(fn [k vs] (map (fn [x] (list x 1)) vs))
		(make-kvs-combiner + 0)
		(fn [ls] (apply max ls))))

(defn mr-example-thunk-max-occur-in-docs []
	(mr-max-occur-in-docs docs1))

;  Same as above, but note which doc has highest word occurrance
;  In theory this should be done with some types, etc
(def mr-max-occur-in-which-doc
	(make-mr
		(fn [k vs] (map (fn [x] (list (list x k) 1)) vs))
		(fn [ls-kvs] (reduce 
						(fn [m [k v]] (let [a (first k) b (second k)] (assoc m a (list v b))))
						'{}
						((make-kvs-combiner + 0) ls-kvs)))
		(fn [ls-pairs] (reduce (fn [a b] (if (> (first b) (first a)) b a)) '(0 nil) ls-pairs))))

(defn mr-example-thunk-max-occur-in-which-doc []
	(mr-max-occur-in-which-doc docs1))

(defn tests []
	(pprint
	(map 
		(fn [f] (apply f '()))
		(list 
			mr-example-thunk-id
			mr-example-thunk-wc
			mr-example-thunk-wc-gt3
			mr-example-thunk-max-occur-in-docs
			mr-example-thunk-max-occur-in-which-doc
		))))

(tests)


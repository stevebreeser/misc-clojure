; Notes transcribed from The Seasoned Schemer.  All credit to the authors.

;;;;; We interrupt this program for 
;;;;; A message from ...
;;;;; Lambda the Ultimate

(def cdr rest)
(def add1 inc)
(def sub1 dec)

(defn rember-f [test?]
  (fn [a ls]
    (cond
      (empty? ls) '()
      (test? a (first ls)) (rest ls)
      'else (cons (first ls) ((rember-f test?) a (rest ls))))))


(defn insertL-f [test?]
  (fn [old new ls]
    (cond
      (empty? ls) '()
      (test? old (first ls)) (cons new (cons old (rest ls)))
      'else (cons (first ls) ((insertL-f test?) old new (rest ls))))))


(defn insertR-f [test?]
  (fn [old new ls]
    (cond
      (empty? ls) '()
      (test? old (first ls)) (cons old (cons new (rest ls)))
      'else (cons (first ls) ((insertL-f test?) old new (rest ls))))))


(defn insert-f-maker [inserter]
  (fn [test?]
    (fn [old new ls]
      (cond
        (empty? ls) '()
        (test? old (first ls)) (inserter old new (rest ls))
        'else (cons (first ls) (((insert-f-maker inserter) test?) old new (rest ls)))))))

(def insertL-f
  (insert-f-maker 
   (fn [old new ls]
     (cons new (cons old ls)))))

(def insertR-f
  (insert-f-maker 
   (fn [old new ls]
     (cons old (cons new ls)))))

(def subst-f
  (insert-f-maker
   (fn [old new ls]
     (cons new ls))))

(defn rember-f [a ls]
  ((((insert-f-maker (fn [old new ls] ls)) =)
   a 
   false 
   ls)))
 

(def atom? (complement list?))

(defn multirember [a ls]
  (cond 
    (empty? ls)  '()
    (= a (first ls))  (multirember a (rest ls))
    'else (cons (first ls) (multirember a (rest ls)))))

(defn mulitinsertLR&co [new oldL oldR ls col]
  (cond
    (empty? ls) (col '() 0 0)
    (= (first ls) oldL) 
    (mulitinsertLR&co new oldL oldR (cdr ls) 
      (fn [newls countL countR]
        (col 
          (cons new (cons oldL newls)) 
          (add1 countL) countR)))
    (= (first ls) oldR) 
    (mulitinsertLR&co new oldL oldR (cdr ls) 
      (fn [newls countL countR]
        (col 
          (cons oldR (cons new newls)) 
          countL (add1 countR))))
    'else 
    (mulitinsertLR&co new oldL oldR (cdr ls)
      (fn [newls countL countR]
        (col 
          (cons (first ls) newls) 
          countL countR)))))



(defn evens-only* [ls]
  (cond
    (empty? ls)   '()
    (list? (first ls))   (cons (evens-only* (first ls)) (evens-only* (rest ls)))
    (even? (first ls))   (cons (first ls) (evens-only* (rest ls)))
    'else   (evens-only* (rest ls))))


;; Parameters: 
;;   ls  -  a list of numbers
;;   col -  a function which collects the return vals:
;;     1)  list of evens only 
;;     2)  product of evens
;;     3)  sum of odds
;;
(defn evens-only*&co [ls col]
  (cond
    (empty? ls)   (col '() 1 0)
    
    (list? (first ls))   
    ;(cons (evens-only* (first ls)) (evens-only* (rest ls)))
    (evens-only*&co (first ls)
      (fn [newls prod-of-evens sum-of-odds]
        (evens-only*&co (rest ls)
          (fn [newls2 poe2 soo2]
            (col (cons newls newls2) 
                  (* prod-of-evens poe2)
                  (+ sum-of-odds soo2))))))
    
    (even? (first ls))   
    (evens-only*&co (rest ls)
      (fn [newls prod-of-evens sum-of-odds]
        (col (cons (first ls) newls) (* (first ls) prod-of-evens) sum-of-odds)))
    
    'else   
    (evens-only*&co (rest ls) 
      (fn [newls prod-of-evens sum-of-odds]
        (col newls prod-of-evens (+ (first ls) sum-of-odds))))
      ))


(defn tests []
  (evens-only*&co '(1 2 4 5 7 8) list))

(tests)



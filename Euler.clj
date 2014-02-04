; 
; Project Euler problems (and other miscellany, no doubt)
;
; http://projecteuler.net/problems
;
;


; functions to create a stream of primes

;take n
;take list of primes
;accumulator

; guard x > 1

(defn f [n restofprimes acc]
(let [a (first restofprimes)]
  (cond
     (= n 1N)  acc
     (< n a)  '("YIKES")
     (= 0N (mod n a))  (f (/ n a) restofprimes (cons a acc)) 
     'else  (f n (rest restofprimes) acc)
   ))) 


(defn prime-helper [n a]
  (cond
    (= 0N (mod n a))  false
    (> (* a a) n)  true
    'else (prime-helper n (+ 1N a))))

(defn prime? [n]
  (cond 
    (= 0 n)  false
    (= 1 n)  false
    (= 2 n)  true
    'else (prime-helper n 2N)))

  
;
; Stream of primes
;
(def primes (filter prime? (iterate inc 0)))




;
;(defn lfactor [n]
;  (lfactor-helper n))
;
;(defn prime? [n]
;  (if (< n 2)
;      false
;      (not (not (= 1 (count (lfactor n)))))))



(defn foo [n a acc]
  (cond
    (< n a)  "Yikes"
    (= n a)  n
    (< n (* a a))  n
    (= 0 (mod n a))  (foo (/ n a) a a)
    'else  (foo n (+ 1 a) acc)
    ))



(defn sm [n i]
   (cond
  	  (> i (apply * (range 2N (+ 1 n))))  "YIKES"
      (reduce #(and %1 %2) (map #(= 0 (mod i %1)) (range 2N (inc n))))  i
      'else (sm n (inc i))))


(defn sm [n]
   (loop [i 1]
     (cond
  	  (> i (apply * (range 2N (+ 1 n))))  "YIKES"
      (reduce #(and %1 %2) (map #(= 0 (mod i %1)) (range 2N (inc n))))  i
      'else (recur (inc i)))))




;(defn sm [n i acc]
;  (loop [n n 
;         acc
;         (cond
;    (> i n)  acc
;    'else (let [ ]
;               (sm n (inc i) (assoc ))))]

;(sm 10 1 {})


(defn factorial [n]
  (loop [n n
        acc 1]
  (case n
    0 acc 
    1 acc
    (recur (dec n) (* n acc)))))


;ls = (2 2 2 5 7)
;acc = {}

(defn factor-ls-to-map [ls]
  (loop [ls ls
         acc {}]
    (if 
      (empty? ls)
      acc   
      (let [a (first ls)] 
        (let [newval (if (contains? acc a)
                       (inc (acc a))
                       1)]
          (recur (rest ls) (assoc acc a newval)))))))

(defn factor-union [m1 m2]
  (if
    (empty? m1)  
    m2
    (loop [m m2
           acc m1]
       (if
         (empty? m)  
         acc
         (let [[k v] (first m)
               newacc 
               (if
                 (or (not (contains? acc k)) (> v (acc k)))  
                 (assoc acc k v)
                 acc)]
             (recur (rest m) newacc))))))



;---
;prob 7
;
; find 10001th prime



(defn findnthprime [n]
  (if (< n 1)   
    "YIKES"
    (loop [x 2
           acc 1]
      (if
        (= n acc)   
        x
        (recur (inc x) (if (prime? (inc x)) (inc acc) acc))))))

(defn find10001prime [] (findnthprime 10001))


;---
;prob 8

; Largest product in a series
;
; Find the greatest product of five consecutive digits in the 1000-digit number.



(def big1000digitnumber 
"73167176531330624919225119674426574742355349194934969835203127745063262395783180169848018694788518438586
156078911294949545950173795833195285320880551112540698747158523863050715693290963295227443043557668966489
504452445231617318564030987111217223831136222989342338030813533627661428280644448664523874930358907296290
491560440772390713810515859307960866701724271218839987979087922749219016997208880937766572733300105336788
122023542180975125454059475224352584907711670556013604839586446706324415722155397536978179778461740649551
492908625693219784686224828397224137565705605749026140797296865241453510047482166370484403199890008895243
450658541227588666881164271714799244429282308634656748139191231628245861786645835912456652947654568284891
288314260769004224219022671055626321111109370544217506941658960408071984038509624554443629812309878799272
442849091888458015616609791913387549920052406368991256071760605886116467109405077541002256983155200055935
72972571636269561882670428252483600823257530420752963450")        



(defn product-of-digits [s]
  (reduce * (map #(Integer/parseInt (str %1)) (seq s))))

(defn g5dp [s]
  (let [len (count s)]
    (loop [i 0
           acc 0]
      (if (> (+ i 5) len) 
          acc
          (let [prod (product-of-digits (subs s i (+ i 5)))
                newacc (if (> prod acc) prod acc)]
            (recur (inc i) newacc))))))


;---
;prob 9

; Special Pythagorean triplet

; Given a + b + c = 1000
; and   a^2 + b^2 = c^2
; Find  a*b*c

(defn square [n] (* n n))

(defn pythag? [[a b c]] (= (square c) (+ (square a) (square b))))

(defn triplets-sum-to [n]
  (filter (fn [x] (not (nil? x))) 
    (for [a (range 1 (- n 3)) 
        b (range (inc a) (- n a))] 
    (let [c (- n a b)]
      (if (and (> c 0) (> c a) (> c b))
        [a b c]
        nil)))))

(def special-pythag-triplets--1000
  (filter (fn [trip] (pythag? trip)) (triplets-sum-to 1000)))

(defn test-special-pythag-triplet []
  (pprint
    (let [trips special-pythag-triplets--1000]
      [(count trips) trips])))



;---
;prob 10

; Summation of primes

; Find the sum of all the primes below two million.

; WARNING:  this might take half a minute
(defn test-summation-of-primes-below-2000000 []
  (reduce + (filter prime? (range 2000000))))

;---
;prob 11

; Largest product in a grid

; What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) 
;   in the 20×20 grid?

(def str-grid20x20
"08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"
)

(defn trim0 [str] (if (= "0" (subs str 0 1)) (if (= str "0") str (trim0 (subs str 1))) str))

(def grid20x20 
  (map (fn [x] (map (fn [y] (read-string (trim0 y))) (clojure.string/split x #" "))) 
    (clojure.string/split-lines str-grid20x20))
)

(def a20x20 (to-array-2d grid20x20))
(def a a20x20)
(def vertical-prods
  (for [i (range 17) j (range 20)] (reduce * (map (fn [x] (aget a (+ i x) j)) (range 4))))
)

(def horizontal-prods
  (for [i (range 20) j (range 17)] (reduce * (map (fn [x] (aget a i (+ j x))) (range 4))))
)

(def down-diag-prods
  (for [i (range 17) j (range 17)] (reduce * (map (fn [x] (aget a (+ i x) (+ j x))) (range 4)))) 
)

(def up-diag-prods
  (for [i (range 3 20) j (range 17)] (reduce * (map (fn [x] (aget a (- i x) (+ j x))) (range 4))))  
)

(def answer 
  (reduce (fn [a b] (if (> a b ) a b)) (concat vertical-prods horizontal-prods down-diag-prods up-diag-prods)))

;---
;prob 12

; Highly divisible triangular number

; triangle numbers (sum of natural numbers 1 to n)
; Find first triangle number with over 500 divisors.


(defn triangle-num [n] 
  (reduce + (range 1 (inc n))))

(def triangle-nums (map triangle-num (iterate inc 1)))

;; sort of a cheap memoization I suppose;  
;; not used
;;(defn get-triangle-num [n] (last (take n triangle-nums)))


(defn optimized-factoring-increment [n]
  ; (find-next-prime n)
  (+ 1 n))

(defn lfactor-helper [n a acc]
  (cond
    (< n a)  "Yikes"
    (= n a)  (cons n acc)
    (< n (* a a))  (cons n acc)
    (= 0 (mod n a))  (lfactor-helper (/ n a) a (cons a acc))
    'else  (lfactor-helper n (optimized-factoring-increment a) acc)))

(defn lfactor [n]
  (cond
    (< n 1)  '()
    (= n 1)  '(1)
    'else  (lfactor-helper n 2 '(1))))


(defn all-factors-helper [n a acc]
  (loop [a a acc acc stop (- n 1)]
    (cond
      (<= stop a) acc
      (= n (* a a)) (recur (inc a) (cons a acc) (/ n a))
      (= 0 (mod n a)) (recur (inc a) (cons a (cons (/ n a) acc)) (/ n a))
      'else (recur (inc a) acc stop))))

(defn all-factors [n]
  (cond 
    (< n 1) '()
    (= n 1) '(1)
    'else (sort (all-factors-helper n 2 (list 1 n)))))



(defn highly-divisible-triangle-num [more-than-n-divisors]
  (loop [nums triangle-nums]
    (let [a (first nums)
          c (count (all-factors a))]
      (if (> c more-than-n-divisors)
          (vector a c)
          (recur (rest nums))))))

(defn test-highly-divisible-triangle-num--500 []
  (highly-divisible-triangle-num 500))

;-----
; prob 14

; Collatz sequences
; n is even   n -> n/2
; n is odd    n -> 3n + 1
;
; all chains end in 1 (in theory)
; which is longest chain starting with number under 1 million

(defn collatz-func [n]
  (cond
    (< n 1)  "Yikes"
    (even? n) (/ n 2)
    'else-odd (+ 1 (* 3 n))))

(defn seq-leng-until-stopval [s stopval]
  (loop [s s c 1]
    (if (= stopval (first s))
        c
        (recur (rest s) (inc c)))))

(defn collatz-seq-leng [s]
  (seq-leng-until-stopval s 1))

(defn make-collatz-seq [n]
  (iterate collatz-func n))

(defn second-element-bigger-comparator [v w]
  (let [a (second v)
        b (second w)]
      (if (> a b) v w)))

(defn longest-collatz-seq [upto-n]
  (reduce 
    second-element-bigger-comparator
    (map (fn [s] (vector (first s) (collatz-seq-leng s))) (map make-collatz-seq (range 1 (inc upto-n))))
    ))

;-----
; prob 15

; number of taxicab routes from corner to corner on 20x20 grid
; essentially this is "40 choose 20"
; To wit, each move is either horizontal or vertical.  
; You need to make 40 moves, 20 of which are horizontal (the other 20 being vertical)
; Another way of thinking of this is the number of 40-bit numbers that have 20 bits set to '1'


; n * (n-1) * ... (n - k + 1)
; ---------------------------
;           k!
;
(defn choose [n k]
  (if (< n k)
      "Yikes"
      (/ (apply * (range (+ 1N (- n k)) (+ 1N n)))
          (apply * (range 1N (+ 1 k)))
          )))

(defn grid-routes [gridsize]
  (choose (* 2 gridsize) gridsize))



;-----
; prob 16

; Sum of digits of 2^1000
;

;; Ugly and wrong -- doesn't quite work because of precision issues
(def TwoTo1000 (Math/pow 2 1000))

(defn split-num-into-vec [n]
  (clojure.string/split (str n) #"[/.E]"))

(defn sum-str-digits [str]
  (apply + (map (fn [c] (Character/digit c 10)) str)))

(defn sum-digits [n]
  (loop [n n
         acc 0]
    (let [[a b c] (split-num-into-vec n)]
      (if (not (nil? c))
          (recur
            (- n (read-string (str a "E" c)))
            (+ acc (read-string a)))
          ;(doall 
          ;  (list n acc (vector a b c)) 
          (if (not (= "0" b))
              (str "yikes  --  " a " " b " " c " ")
              (+ acc (sum-str-digits a)))
          ;)
          ))))

;; end ugly and wrong

; represent numbers as a list of integers 0-9; do math by hand appending to list
; start with 1; double; iterate 1000 times

(defn double-by-hand [ls-digits carry-bit]
  (if (empty? ls-digits)
      (if (= 0 carry-bit)
          '()
          '(1))
      (let [x (+ carry-bit (* 2 (first ls-digits)))]
        (if (< x 10)
            (cons x (double-by-hand (rest ls-digits) 0))
            (cons (mod x 10) (double-by-hand (rest ls-digits) 1))))))

(defn find-sum-digits-power-of-2 [n]
  (loop [i 1
         acc '(1)]
    (if (> i n)
      (vector (apply + acc) acc) 
      (recur (inc i) (double-by-hand acc 0)))))


; -----
; prob 17

; number letter counts for all numbers 1 thru 1000
;
; one two three four five six seven eight nine
; ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen 
; twenty twenty-one ...
; thirty ... forty ... fifty ... sixty ... seventy ... eighty ... ninety ...
; one hundred
; one hundred and one ...
; two hundred
; two hundred and one ...
; ...
; one thousand

(defn my-char-count [str]
  (reduce + (map count (clojure.string/split str #"[ ]"))))

(def one-thru-nine-str "one two three four five six seven eight nine")
(def first-nine-total (my-char-count one-thru-nine-str))
(def teens-total (my-char-count "ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen"))
(defn some-ies [str]
  (+ (* 10 (count str)) 
     first-nine-total))
(def twenties-thru-nineties-total
  (reduce + (map some-ies (clojure.string/split "twenty thirty forty fifty sixty seventy eighty ninety" #"[ ]"))))
(def first-ninety-nine-total
  (+ first-nine-total teens-total twenties-thru-nineties-total))

(defn undreds [str]
  (+ (* 100 (+ (count str)
               (count "hundred")))
     (* 99 (count "and"))
     first-ninety-nine-total))

(def undreds-total 
  (reduce + (map undreds (clojure.string/split one-thru-nine-str #"[ ]"))))

(def one-thru-one-thousand-total
  (+ first-ninety-nine-total
     undreds-total
     (my-char-count "one thousand")))


; -----
; prob 18

;
;
(def str-tricky-triangle
"75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23")


(defn trim0 [str] (if (= "0" (subs str 0 1)) (if (= str "0") str (trim0 (subs str 1))) str))

(def tricky-triangle 
  (map (fn [x] (map (fn [y] (read-string (trim0 y))) (clojure.string/split x #" "))) 
    (clojure.string/split-lines str-tricky-triangle))
)

(def array-tricky-triangle (to-array-2d tricky-triangle))

; given coordinate, and depth, return n options
;  NOT SURE THIS IS CORRECT
; algorithm just returns sum of each side of triangle.  Chooses greater of the two below.
(defn get-options [arr [x y]]
  (let [opt1 (reduce + (map (fn [i] (aget arr (+ x i) y)) (range 1 (- 15 x))))
        opt2 (reduce + (map (fn [i] (aget arr (+ x i) (+ y i))) (range 1 (- 15 x))))]
    (vector opt1 opt2)))

(defn triangle-driver [arr depth]
  (loop [i depth
         coord '[0 0]
         acc '()]
    (if (= i 1)
        (cons (aget arr (coord 0) (coord 1)) acc)
        (let [options (get-options arr coord)
              choice (if (> (options 0) (options 1)) 0 1)]
            (recur 
              (dec i) 
              (vector (+ 1 (coord 0)) (+ choice (coord 1)))
              (cons (aget arr (coord 0) (coord 1)) acc))))))

(def  answer-tricky-triangle
  (let [ans (reverse (triangle-driver array-tricky-triangle 15))]
    (vector (apply + ans) ans)))


; ------
; prob 19

; Counting Sundays
;
; How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

(def month-lengths
  { :sep 30 :apr 30 :jun 30 :nov 30 :feb 28 :feb-leap 29 :jan 31 :mar 31 :may 31 :jul 31 :aug 31 :oct 31 :dec 31})

(def normal-year
  '(31 28 31 30 31 30 31 31 30 31 30 31))

(def leap-year
  '(31 29 31 30 31 30 31 31 30 31 30 31))

(defn get-new-year [year]
  (if (or (and (= 0 (mod year 4)) (not (= 0 (mod year 100))))
          (= 0 (mod year 400)))
      leap-year
      normal-year))

; dow (day of week)  mon 0 --> sun 6
(defn calculate-sundays [start-year dow-year-starts-on end-year]
    (loop [acc 0
           dow dow-year-starts-on    
           year start-year
           remaining-months-ls (get-new-year year)]
      (cond
        (= (+ 1 end-year) year)
            acc
        (empty? remaining-months-ls) 
            (recur acc dow (inc year) (get-new-year (inc year)))    
        'else
            (let [days-in-month (first remaining-months-ls)
                  sundays (quot (+ dow days-in-month) 7)
                  new-dow (mod (+ dow days-in-month) 7)]
              (recur 
                (if (= 6 dow) (inc acc) acc)
                ; raw number of sundays ; (+ sundays acc) 
                new-dow 
                year 
                (rest remaining-months-ls))))))


; dow (day of week)  mon 0 --> sun 6
(defn answer-prob-19 [] (calculate-sundays 1901 1 2000)) ; start on Tues


; -----
; prob 20

; Factorial digit sum
;
; sum of digits of 100! 
;

;; easy way
(defn factorial [n]
  (loop [n n 
         acc 1N]
    (cond
      (= 0 n)  1
      (= 1 n)  acc
      'else  (recur (dec n) (* n acc)))))

(defn sum-of-digits [n]
  (apply + (map (fn [c] (Character/digit c 10)) (str n))))

(defn answer-prob-20 [] (sum-of-digits (factorial 100)))

;; hard way would require reimplementing multiplication a la problem 16


; ------
; prob 21

; Amicable Numbers : d(x) is sum of proper divisors of x;  d(a) = b, d(b) = a, and a != b 
; find sum of all amicable numbers under 10000

; n.b.  proper divisors of 284 are 1, 2, 4, 71, and 142
; 
; e.g. 220 & 284 are amicable
;

(defn properdivisors [n]
  (loop [ i 2 
          top (- n 1)
          acc '(1)]
    (cond
      (>= i top)   (sort acc)
      (= n (* i i))   (recur (inc i) (- (/ n i) 1) (cons i acc))
      (= 0 (mod n i))  (recur (inc i) (- (/ n i) 1) (cons i (cons (/ n i) acc)))
      'else   (recur (inc i) top acc) )))

(defn amicable? [n]
  (let [pds (properdivisors n)
        sum-pds (apply + pds)
        pds2 (properdivisors sum-pds)
        sum-pds2 (apply + pds2)]
      (if (and (= n sum-pds2) (not (= n sum-pds)))
          (vector n sum-pds)
          false)))

(defn find-amicables [n]
  (filter (complement not) (map amicable? (range 2 n))))





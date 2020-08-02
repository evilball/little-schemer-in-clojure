;;http://juliangamble.com/blog/2012/07/20/the-little-schemer-in-clojure/

(ns com.videodemons.clojure.little_schemer)

(declare equal?)
(declare insert-g)
(declare seqL)
(declare seqR)

(defn nextChapter [n]
  (println "")
  (println "=========")
  (println (str "ch" (str n)))
  (println "=========")
  (println ""))

;;ch1
(nextChapter 1)

(defn atom? [a]
  (not (seq? a)))

(defn null? [a]
  (or
    (nil? a)
    (= () a)))

(println "(atom? 'a) = " (atom? 'a))
(println "(atom? '(a)) = " (atom? '(a)))
(println "")

(println "(null? nil) = " (null? nil))
(println "(null? 'a) =  " (null? 'a))
(println "(null? '(a)) = " (null? '(a)))
(println "(null? '()) = " (null? '()))

;; ch3
(nextChapter 3)

(def lat?
  (fn [l]
    (cond
      (null? l) true
      (and (seq? l)
           (atom? (first l)))
      (lat? (rest l))
      true false)))

(defn member? [a lat]
  (cond
    (null? lat) false
    true (or
           (= (first lat) a)
           (member? a (rest lat)))))

(defn firsts [l]
  (cond
    (null? l) '()
    true (cons (first (first l))
               (firsts (rest l)))))

(defn seconds [l]
  (cond
    (null? l) '()
    true (cons (rest (first l))
               (seconds (rest l)))))

(defn insert-g [seq]
  (fn [new old l]
    (cond
      (null? l) '()
      (= (first l) old) (seq new old (rest l))
      true (cons (first l)
                 ((insert-g seq) new old (rest l))))))

(def insertR
  (insert-g (fn [new old l]
                (cons old (cons new l)))))

(def insertL
  (insert-g (fn [new old l]
                (cons new (cons old l)))))

(def subst
  (insert-g (fn [new old l]
              (cons new l))))

(defn subst2 [new o1 o2 lat]
  (cond
    (null? lat) '()
    true (cond
           (or (= (first lat) o1)
               (= (first lat) o2)) (cons new (rest lat))
           true (cons (first lat)
                      (subst2 new o1 o2 (rest lat))))))



(println (firsts '((large burger) (fries coke) (chocolate sundae))))
(println (firsts '((large burger) () (chocolate sundae))))

(println "(insertR 'e 'd '(a b c d f g h)) =" (insertR 'e 'd '(a b c d f g h)))
(println "(insertL 'e 'f '(a b c d f g h)) =" (insertL 'e 'f '(a b c d f g h)))
(println "(subst 'e 'f '(a b c d f g h)) =" (subst 'e 'f '(a b c d f g h)))
(println (subst2 'e 'b 'f '(a b c d f g h)))


(defn multirember [a lat]
  (cond
    (null? lat) '()
    true (cond
           (= (first lat) a) (multirember a (rest lat))
           true (cons (first lat)
                      (multirember
                        a (rest lat))))))

(defn multiinsertR [new old lat]
  (cond
    (null? lat) '()
    true (cond
           (= (first lat) old) (cons old
                                     (cons new (multiinsertR new old (rest lat))))
           true (cons (first lat)
                      (multiinsertR new old
                                    (rest lat))))))

(defn multiinsertL [new old lat]
  (cond
    (null? lat) '()
    true (cond
           (= (first lat) old) (cons new
                                     (cons old
                                           (multiinsertL new old (rest lat))) )
           true (cons (first lat)
                      (multiinsertL new old
                                    (rest lat))))))

(defn multisubst [new old lat]
  (cond
    (null? lat) '()
    true (cond
           (= (first lat) old) (cons new (multisubst new old (rest lat)))
           true (cons (first lat)
                      (multisubst new old (rest lat))))))

(println (multirember 'a '(a b c a d a)))
(println (multiinsertR 'b 'a '(a b c a d a)))
(println (multiinsertL 'a 'b '(a b c b d b)))
(println (multisubst 'a 'b '(a b c b d b)))

;; ch4
(nextChapter 4)

(defn add1 [n]
  (+ 1 n))

(defn sub1 [n]
  (- n 1))

(defn o+ [n m]
  (cond
    (zero? m) n
    true (add1 (o+ n (sub1 m)))))

(defn o- [n m]
  (cond
    (zero? m) n
    true (sub1 (o- n (sub1 m)))))

(defn addtup [tup]
  (cond
    (null? tup) 0
    true (o+ (first tup) (addtup (rest tup)))))

(defn o* [n m]
  (cond
    (zero? m) 0
    true (o+ n (o* n (sub1 m)))))

(defn tup+ [tup1 tup2]
  (cond
    (null? tup1) tup2
    (null? tup2) tup1
    true (cons (o+ (first tup1) (first tup2))
               (tup+ (rest tup1) (rest tup2)))))

(defn gt [n m]
  (cond
    (zero? n) false
    (zero? m) true
    true (gt (sub1 n) (sub1 m))))

(defn lt [n m]
  (cond
    (zero? m) false
    (zero? n) true
    true (lt (sub1 n) (sub1 m))))

(defn equal [n m]
  (cond
    (> n m) false
    (< n m) false
    true true))

(defn expt [n m]
  (cond
    (zero? m) 1
    true (o* n (expt n (sub1 m)))))

(defn quotient [n m]
  (cond
    (< n m) 0
    true (add1 (quotient (o- n m) m))))

(defn length [lat]
  (cond
    (null? lat) 0
    true (add1 (length (rest lat)))))

(defn pick [n lat]
  (cond
    (zero? (sub1 n)) (first lat)
    true (pick (sub1 n) (rest lat))))


(defn one? [n]
  (equal 1 n))

(defn rempick [n lat]
  (cond
    (one? n) (rest lat)
    true (cons (first lat) (rempick (sub1 n) (rest lat)))))

(defn no-nums [lat]
  (cond
    (null? lat) '()
    true (cond
           (number? (first lat)) (no-nums (rest lat))
           true (cons (first lat) (no-nums (rest lat))))))

(defn all-nums [lat]
  (cond
    (null? lat) '()
    true (cond
           (number? (first lat)) (cons (first lat) (all-nums (rest lat)))
           true (all-nums (rest lat)))))

(defn eqan? [a1 a2]
  (cond
    (and (number? a1) (number? a2)) (equal a1 a2)
    (or (number? a1) (number? a2)) false
    true (= a1 a2)))

(defn occur [a lat]
  (cond
    (null? lat) 0
    true (cond
           (eqan? a (first lat)) (add1 (occur a (rest lat)) )
           true (occur a (rest lat)))))


(println "(add1 67) = " (add1 67))
(println "(sub1 5) = " (sub1 5))
(println "(zero? 0) = " (zero? 0))
(println "(o+ 46 12) = " (o+ 46 12))
(println "(o- 17 9) = " (o- 17 9))
(println "(addtup (1 2 3) = " (addtup '(1 2 3)))
(println "(o* 5 6) = " (o* 5 6))
(println "(tup+ '(3 6 9 11 4) '(8 5 2 0 7)) = " (tup+ '(3 6 9 11 4) '(8 5 2 0 7)))
(println "(tup+ '(3 6 9) '(8 5 2 0 7)) = " (tup+ '(3 6 9) '(8 5 2 0 7)))
(println "(tup+ '(3 6 9 11 4) '(8 5 2)) = " (tup+ '(3 6 9 11 4) '(8 5 2)))
(println "(gt 12 133) = " (gt 12 133))
(println "(gt 120 11) = " (gt 120 11))
(println "(gt 120 120) = " (gt 120 120))

(println "(lt 12 133) = " (lt 12 133))
(println "(lt 120 11) = " (lt 120 11))
(println "(lt 120 120) = " (lt 120 120))

(println "(equal 120 120) = " (equal 120 120))

(println "(expt 2 3) = " (expt 2 3))
(println "(expt 5 3) = " (expt 5 3))

(println "(quotient 10 3) =" (quotient 10 3))

(println "(length '(hotdogs with mustard sauerkraut and pickles)) =" (length '(hotdogs with mustard sauerkraut and pickles)))
(println "(pick 4 '(lasagna spaghetti ravioli macaroni meatball)) =" (pick 4 '(lasagna spaghetti ravioli macaroni meatball)))
(println "(rempick 3 '(hotdogs with hot mustard)) =" (rempick 3 '(hotdogs with hot mustard)))

(println "(number? 10) =" (number? 10))

(println "(no-nums '(5 pears 6 prunes 9 dates)) =" (no-nums '(5 pears 6 prunes 9 dates)))
(println "(all-nums '(5 pears 6 prunes 9 dates)) =" (all-nums '(5 pears 6 prunes 9 dates)))

(println "(eqan? 1 2) =" (eqan? 1 2))
(println "(eqan? 1 1) =" (eqan? 1 1))
(println "(eqan? \"1\" \"1\") =" (eqan? "1" "1"))

(println "(occur 1 '(1 2 3 4 1 23 3 1)) =" (occur 1 '(1 2 3 4 1 23 3 1)))

(println "(one? 1) =" (one? 1))
(println "(one? 2) =" (one? 2))

;; ch5
(nextChapter 5)

(defn rember* [a l]
  (cond
    (null? l) '()
    (atom? (first l)) (cond
                        (eqan? a (first l)) (rember* a (rest l))
                        true (cons (first l) (rember* a (rest l))))
    true (cons (rember* a (first l))
               (rember* a (rest l)))))

(defn insertR* [new old l]
  (cond
    (null? l) '()
    (atom? (first l)) (cond
                        (eqan? old (first l)) (cons old
                                                (cons new
                                                      (insertR* new old (rest l))))
                        true (cons (first l) (insertR* new old (rest l))))
    true (cons (insertR* new old (first l))
               (insertR* new old (rest l)))))

(defn occur* [a l]
  (cond
    (null? l) 0
    (atom? (first l)) (cond
                        (eqan? a (first l)) (add1 (occur* a (rest l)))
                        true (occur* a (rest l)))
    true (+ (occur* a (first l))
           (occur* a (rest l)))))

(defn subst* [new old l]
  (cond
    (null? l) '()
    (atom? (first l)) (cond
                        (eqan? old (first l)) (cons new
                                                  (subst* new old (rest l)))
                        true (cons (first l) (subst* new old (rest l))))
    true (cons (subst* new old (first l))
               (subst* new old (rest l)))))

(defn insertL* [new old l]
  (cond
    (null? l) '()
    (atom? (first l)) (cond
                        (eqan? old (first l)) (cons new
                                                    (cons old
                                                          (insertL* new old (rest l))))
                        true (cons (first l) (insertL* new old (rest l))))
    true (cons (insertL* new old (first l))
               (insertL* new old (rest l)))))

(defn member* [a l]
      (cond
        (null? l) false
        (atom? (first l)) (or
                            (eqan? a (first l))
                            (member* a (rest l)))
        true (or (member* a (first l))
                 (member* a (rest l)))))

(defn leftmost [l]
  (cond (atom? (first l)) (first l)
        true (leftmost (first l))))

(defn eqlist? [l1 l2]
      (cond 
        (and (null? l1) (null? l2)) true
        (or (null? l1)
            (null? l2)) false
        true (and (equal? (first l1) (first l2))
                  (eqlist? (rest l1) (rest l2)))))

(defn equal? [s1 s2]
  (cond
    (and (atom? s1) (atom? s2)) (eqan? s1 s2)
    (or (atom? s1) (atom? s2)) false
    true (eqlist? s1 s2)))

(defn rember [a lat]
  (cond
    (null? lat) '()
    true (cond
           (equal? (first lat) a) (rest lat)
           true (cons (first lat)
                      (rember a (rest lat))))))

(println "(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)) ="
         (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)) "\n")

(println "(insertR* = \n" (insertR* 'roast 'chuck '((how much (wood))
                                                 could
                                                 ((a (wood) chuck))
                                                 (((chuck)))
                                                 (if (a) ((wood chuck)))
                                                 could chuck wood)) "\n")

(println "occur* =" (occur* 'banana '((banana)
                                       (split ((((banana ice)))
                                                (cream (banana))
                                                sherbet))
                                       (banana)
                                       (bread)
                                       (banana brandy))) "\n")

(println "subst* =" (subst* 'orange 'banana '((banana)
                                       (split ((((banana ice)))
                                                (cream (banana))
                                                sherbet))
                                       (banana)
                                       (bread)
                                       (banana brandy))) "\n")

(println "(insertL* = \n" (insertL* 'pecker 'chuck '((how much (wood))
                                                     could
                                                     ((a (wood) chuck))
                                                     (((chuck)))
                                                     (if (a) ((wood chuck)))
                                                     could chuck wood)) "\n")

(println "(member* 'chips '((potato) (chips ((with) fish) (chips)))) ="
         (member* 'chips '((potato) (chips ((with) fish) (chips)))) "\n")

(println "(leftmost '(((hot) (tuna (and))) cheese)) =" (leftmost '(((hot) (tuna (and))) cheese)))

(println "(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) =" (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))))

(println (rember 'banana '(apple banana orange)))

;; ch6
(nextChapter 6)

(defn numbered? [aexp]
  (cond
    (atom? aexp) (number? aexp)
    true (and
           (numbered? (first aexp))
           (numbered? (first (rest (rest aexp)))))))

(defn first-sub-exp [aexp]
  (first (rest aexp)))

(defn second-sub-exp [aexp]
  (first (rest (rest aexp))))

(defn operator [aexp]
  (first aexp))

(def atom-to-function
  (fn [x]
    (cond
      (= x '+) +
      (= x '*) *
      true expt)))

(defn value [nexp]
  (cond
    (atom? nexp) nexp
    true ((atom-to-function (operator nexp))
           (value (first-sub-exp nexp))
           (value (second-sub-exp nexp)))))

(println "(value '(+ 2 3)) =" (value '(+ 2 3)))
;(println "(value '(↑ 2 (+ 2 (* 2 2)))) =" (value '(↑ 2 (+ 2 (* 2 2)))))

;; ch7
(nextChapter 7)

(defn set2? [lat]
  (cond
    (null? lat) true
    (member? (first lat) (rest lat)) false
    true (set2? (rest lat))))

(defn makeset [lat]
  (cond
    (null? lat) '()
    (member? (first lat) (rest lat)) (makeset (rest lat))
    true (cons (first lat) (makeset (rest lat)))))

(defn subset? [set1 set2]
  (cond
    (null? set1) true
    true (and
           (member? (first set1) (set2))
           (subset? (rest set1) (set2)))))

(defn eqset? [set1 set2]
  (and (subset? set1 set2)
       (subset? set2 set1)))

(defn intersect? [set1 set2]
  (null? set1) false
  (or (member? (first set1) set2)
      (intersect? (rest set1) set2)))

(defn intersect [set1 set2]
  (cond
    (null? set1) '()
    (member? (first set1) set2) (cons
                                  (first set1)
                                  (intersect (rest set1) set2))
    true (intersect (rest set1) set2)))

(defn union [set1 set2]
  (null? set1) set2
  (member? (first set1) set2) (union (rest set1) set2)
  true (cons
         (first set1)
         (union (rest set1) set2)))

(defn intersectall [lset]
  (cond
    (null? (rest lset)) (first lset)
    true (intersect (first lset)
               (intersectall (rest lset)))))

(defn a-pair? [x] 
  (cond
    (atom? x) false
    (null? x) false
    (null? (rest x)) false
    (null? (rest (rest x))) true
    true false))

(defn build [s1 s2]
  (cons s1 (cons s2 '())))

(defn fun? [rel]
  (set2? (firsts rel)))

(defn revpair [pair]
  (build (second pair)
         (first pair)))

(defn revrel [rel]
  (cond
    (null? rel) '()
    true (cons (revpair (first rel))
               (revrel (rest rel)))))

(defn fullfun? [fun]
  (fun? (revrel fun)))


(println "(set2? '(1 2 2 3) =" (set2? '(1 2 2 3)))
(println "(makeset '(apple peach pear pear peach plum apple lemon peach) ="
         (makeset '(apple peach pear pear peach plum apple lemon peach)))

(println "intersectall '((a b c) (c a d e) (e f g h a b)) ="
         (intersectall '((a b c) (c a d e) (e f g h a b))))

(println "revrel '((a b) (b c) (c d)) =" (revrel '((a b) (b c) (c d))))

(println "fullfun? (fullfun? '((1 2) (3 4))) = " (fullfun? '((1 2) (3 4))))
(println "fullfun? (fullfun? '((1 2) (3 2))) = " (fullfun? '((1 2) (3 2))))

;; ch8
(nextChapter 8)

(defn rember-f [test?]
  (fn [a l]
    (cond
      (null? l) '()
      (test? a (first l)) (rest l)
      true (cons
             (first l)
             ((rember-f test?) a (rest l))))))

(defn eq?-c [a]
  (fn [x]
    (= x a)))

(defn eq?-salad [k]
  ((eq?-c 'salad) k))

(def eq?-tuna
  (eq?-c 'tuna))

(defn insertL-f [test?]
  (fn [new old l]
    (cond
      (null? l) '()
      (test? (first l) old) (cons new (cons old (rest l)))
      true (cons (first l)
                 ((insertL-f test?) new old (rest l))))))

(defn insertR-f [test?]
  (fn [new old l]
    (cond
      (null? l) '()
      (test? (first l) old) (cons
                              (cons old new)
                              (rest l))
      true (cons (first l)
                 ((insertL-f test?) new old (rest l))))))

(defn multirember-f [test?]
  (fn [a lat]
    (cond
      (null? lat) '()
      (test? a (first lat)) ((multirember-f test?)
                              a (rest lat))
      true (cons (first lat)
                 ((multirember-f test?) a (rest lat))))))

(defn multiremberT [test? lat]
  (cond
    (null? lat) '()
    (test? (first lat)) (multiremberT test?(rest lat))
    true (cons (first lat)
               (multiremberT test? (rest lat)))))

(def multirember-eq?
  (multirember-f =))

(defn a-friend [x y]
  (null? y))

(defn new-friend [newlat seen]
  (a-friend newlat (cons 'tuna seen)))

(defn last-friend [x y]
  (length x))

;(multirember&co 'tuna '(strawberries tuna and swordfish) last-friend))
(defn multirember&co [a lat col]
  (cond
    (null? lat) (col '() '())
    (= (first lat) a) (multirember&co a
                                      (rest lat)
                                      (fn [newlat seen]
                                        (col
                                          newlat
                                          (cons (first lat) seen))))
    true (multirember&co a
                         (rest lat)
                         (fn [newlat seen]
                           (col 
                             (cons (rest lat) newlat)
                             seen)))))

(defn multiinsertLR [new oldL oldR lat]
  (cond
    (null? lat) '()
    (= oldL (first lat)) (cons new
                               (cons oldL
                                     (multiinsertLR new oldL oldR (rest lat))))
    (= oldR (first lat)) (cons oldR
                               (cons new (multiinsertLR new oldL oldR (rest lat))))
    true (cons (first lat)
               (multiinsertLR new oldL oldR (rest lat)))))

(defn multiinsertLR&co [new oldL oldR lat col]
  (cond
    (null? lat) (col '() 0 0)
    (= oldL (first lat)) (multiinsertLR&co new oldL oldR (rest lat)
                                           (fn [newlat L R]
                                             (col (cons new
                                                        (cons oldL newlat))
                                                  (add1 L) R)))
    (= oldR (first lat)) (multiinsertLR&co new oldL oldR (rest lat)
                                         (fn [newlat L R]
                                           (col (cons oldR
                                                      (cons new newlat))
                                                L (add1 R))))
    true (multiinsertLR&co new oldL oldR lat
                           (fn [newlat L R]
                             (col (cons (first lat) newlat) L R)))))

(defn evens-only* [l]
  (cond
    (null? l) '()
    (atom? (first l)) (cond
                        (even? (first l)) (cons (first l)
                                                (evens-only* (rest l)))
                        true (evens-only* (rest l)))
    true (cons (evens-only* (first l))
               (evens-only* (rest l)))))

(defn evens-only*&co [l col]
  (cond
    (null? l) (col '() 1 0)
    (atom? (first l)) (cond
                        (even? (first l)) (evens-only*&co (rest l)
                                                          (fn [newl p s]
                                                            )))))

(println "(rember-f = 1 '(1 2 3)) =" ((rember-f =) 1 '(1 2 3)))
(println "(eq?-salad 'salad) = " (eq?-salad 'salad))
(println "(eq?-salad 'salad) = " (eq?-salad 'tuna))
(println "((insertR-f =) 'e 'd '(a b c d f g h)) =" ((insertR-f =) 'e 'd '(a b c d f g h)))
(println "(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna)) =" (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna)))
(println "(multirember&co 'tuna '(strawberries tuna and swordfish) last-friend) =" (multirember&co 'tuna '(strawberries tuna and swordfish) last-friend))
(println "(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)) = " (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)))

;; ch9
(nextChapter 9)

(defn keep-looking [a sorn lat]
  (cond
    (number? sorn) (keep-looking a (pick sorn lat) lat)
    true (= sorn a)))

(defn shift [pair]
  (build (first (first pair))
         (build (second (first pair))
                (second pair))))

(defn align [pora]
  (cond (atom? pora) pora)
  (a-pair? (first pora)) (align (shift pora))
  true (build (first pora)
              (align (second pora))))

(defn length* [pora]
  (cond
    (atom? pora) 1
    true (+ (length* (first pora))
            (length* (second pora)))))

(defn weight* [pora]
  (cond
    (atom? pora) 1
    true (+ (* (weight* (first pora)) 2)
            (weight* (second pora)))))

(defn shuffle* [pora]
  (cond
    (atom? pora) pora
    (a-pair? (first pora)) (shuffle* (revpair pora))
    true (build (first pora)
                (shuffle* (second pora)))))

(defn C [n]
  (cond
    (one? n) 1
    true (cond
           (even? n) (C (/ n 2))
           true (C (add1 (* 3 n))))))

(defn A [n m]
  (cond
    (zero? n) (add1 m)
    (zero? m) (A (sub1 n) 1)
    true (A (sub1 n)
            (A n (sub1 m)))))

(defn eternity [x]
  (eternity (x)))

;((fn [le]
;   ((fn [mk-length]
;      (mk-length mk-length))
;     (fn [mk-length]
;       (le (fn [x]
;             ((mk-length mk-length) x))))))
;  (fn [length]
;    (fn [l]
;      (cond
;        (null? l) 0
;        true (add1 (length (rest l)))))))

(def length-mind-blowing
  (fn [mk-length]
    (mk-length mk-length)
    (fn [mk-length]
      (fn [length]
        (fn [l]
          (cond
            (null? l) 0
            true (add1 (length (rest l)))))
        (mk-length mk-length)))))

(def Y
  (fn [le]
    ((fn [f] (f f))
      (fn [f]
        (le (fn [x] ((f f) x)))))))

(println "(shift (a (b (c (d))))) =" (shift '((a b) (c d))))

(println "(weight* '((a b) c)) =" (weight* '((a b) c)))
(println "(weight* '(a (b c))) =" (weight* '(a (b c))))
(println "(shuffle '(a (b c))) =" (shuffle* '(a (b c))))
;(println "(shuffle '((a b) (c d))) =" (shuffle* '((a b) (c d)))) StackOverflowError

(println "(C 10) =" (C 10))
(println "(A 1 2) =" (A 1 2))
;(println "(A 4 3) =" (A 4 3)) StackOverflowError

;(println "Y =" (Y Y)) StackOverflowError
;(println "(length-mind-blowing '(apples)) =" (length-mind-blowing '(apples)))

;; ch10
(nextChapter 10)

(defn lookup-in-entry-help [name names values entry-f]
  (cond
    (null? names) (entry-f name)
    (= (first names) name) (first values)
    true (lookup-in-entry-help name
                               (rest names)
                               (rest values)
                               entry-f)))

(def extend-table cons)

(defn lookup-in-entry [name entry entry-f]
  (lookup-in-entry-help name
                        (first entry)
                        (second entry)
                        entry-f))

(defn lookup-in-table [name table table-f]
  (cond
    (null? table) (table-f name)
    true (lookup-in-entry name
                          (first table)
                          (fn [name]
                            (lookup-in-table name
                                             (rest table)
                                             table-f)))))

(def initial-table
  (fn [name]
    (cond
      (= name 't) true
      (= name 'nil) nil
      true (build 'primitive name))))

(def *identifier
  (fn [e table]
    (lookup-in-table
      e table initial-table)))

(def *self-evaluating
  (fn [e table]
    e))

(def atom-to-action
  (fn [e]
    (cond
      (number? e) *self-evaluating
      true *identifier)))

(def text-of-quotation second)

(def *quote
  (fn [e table]
    (text-of-quotation e)))

(def *lambda
  (fn [e table]
    (build 'non-primitive
           (cons table (rest e)))))

(def question-of first)
(def answer-of second)


(declare *cond)
(declare *application)

(def list-to-action
  (fn [e]
    (cond
      (atom? (first e)) (cond
                          (= (first e) 'quote) *quote
                          (= (first e) 'lambda) *lambda
                          (= (first e) 'cond) *cond
                          true *application)
      true *application)))

(def expression-to-action
  (fn [e]
    (cond
      (atom? e) (atom-to-action e)
      true (list-to-action e))))

(def meaning
  (fn [e table]
    ((expression-to-action e) e table)))

(def evcon
  (fn [lines table]
    (cond
      (meaning
        (question-of (first lines)) table)
      (meaning
        (answer-of (first lines)) table)
      true (evcon (rest lines) table))))

(def cond-lines rest)

(def *cond
  (fn [e table]
    (evcon (cond-lines e) table)))

(def value
  (fn [e]
    (meaning e '())))

(def apply-primitive
  (fn [name vals]
    (cond
      (= name 'car ) (first (first vals))
      (= name 'cdr ) (rest (first vals))
      (= name 'cons ) (cons (first vals) (second vals))
      (= name 'eq ) (= (first vals) (second vals))
      (= name 'atom? ) (atom? (first vals))
      (= name 'not ) (not (first vals))
      (= name 'null? ) (null? (first vals))
      (= name 'number? ) (number? (first vals))
      (= name 'zero? ) (zero? (first vals))
      (= name 'add1 ) (add1 (first vals))
      (= name 'sub1 ) (sub1 (first vals)))))

(def evlis
  (fn [args table]
    (cond
      (null? args) '()
      true (cons (meaning (first args) table)
                 (evlis (rest args) table)))))

(println "(extend-table 'a '(a b))" (extend-table 'a '(a b)))
(println (*lambda '(*lambda (b) (println b)) '()))
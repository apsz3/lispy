;; aliases
(defn + add)
(defn - sub)
(defn / div)
(defn * mul)
(defn % mod)
(defn ^ pow)
(defn = eq)
(defn != neq)
(defn >= gte)
(defn <= lte)
(defn < lt)
(defn > gt)

;; testing
(defn asseq (lambda (a b) (ass (eq a b))))
    (asseq 1 1)
;;(defn assneq (lambda (f) (not f))) Defining this as NOT ASSEQ is a
;; candidate for starting to work on macros.
(defn check (lambda (fn input expected) (asseq (fn input) expected)))

;; list
(defn empty? (lambda (ls) (eq ls '())))

(defn len
    (defn _len (lambda (ls acc)
        (if (empty? ls 0) 0
        (if (eq (tl ls) '())
        (add 1 acc) ;; only the tl is empty, so have to add for hd elem.
        (_len (tl ls) (add 1 acc))))))
    (lambda (ls) (_len ls 0)))


    (asseq (len '()) 0)
    (asseq (len (list 1)) 1)
    (asseq (len (cons 1 (cons 1 (cons 1 '())))) 3)

;; comparison -- return `a` if fn(a,b) is #true else `b`
(defn cmp (lambda (fn a b) (if (fn a b) a b)))

    (asseq (cmp (lambda () #true) 0 1) 0)
    (asseq (cmp > 2 1) 2)

(defn is?
    ;; We define `is?` to be used to compare Symbols specifically,
    ;; even though it is just an alias of `eq`.
    ;; This is because naive equality checks, such as between
    ;; an integer and a Symbol, will error, in our implementation.
    ;; TODO-- type handle this better.
    (lambda (a b) (eq a b)))
(defn cond
    ;; (cond ( ... (cond-expr, expr) ... (default-expr)))
    (lambda (lst)
    ;; expect no case for the last element, because that's the default.
    (if (eq (len lst) 1) (hd lst) ;; it's null-terminated, so pop the head off.
    (if (hd (hd lst)) (tl (hd lst))
    (cond (tl lst))))))
(check cond (list (list (eq #true #false) 1) 2) 2)



;; math
(defn max (lambda (a b) (cmp gt a b))) (asseq (max 0 1) 1)
(defn min (lambda (a b) (cmp lt a b))) (asseq (min 0 1) 0)
(defn inc (lambda (a)      (add 1 a))) (asseq (inc 1)   2)
(defn dec (lambda (a)     (add -1 a))) (asseq (dec 1)   0)

;; application
(defn map (lambda (fn ls)
    (if (empty? ls) '()
    (cons (fn (hd ls)) (map fn (tl ls))))))
        (defn ls (list 1 2 3))
    (asseq (map inc ls) (list 2 3 4))


;; Getting higher-level, we can make
;; a procedure that will automatically
;; give us this ending lambda method.
;; Note the use of `begin` here to let us
;; `eval` into the environment
(defn make-pkg
    (lambda (body)
    (begin
        (eval body)
        (lambda (method) (eval method)))))

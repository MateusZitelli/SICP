(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (inc x) (+ x 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (integral f a b dx)
  (define (add-dx a) (+ a dx))
  (*
    (sum f (+ a (/ dx 2)) add-dx b)
    dx))

(integral cube 0 10 0.0001)

;ex.: 1.29

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k)
    (cond ((or (= k 0) (= k n)) 1)
          ((odd? k) 4)
          (else 2)))
  (define (new-f k)
    (* (term k) (y k)))
  (* (/ h 3) (sum new-f 0 inc n)))

(simpson-integral cube 0 1 100) ; 1/4
(simpson-integral cube 0 1 1000) ; 1/4
(integral cube 0 1 0.01) ; 0.2499875
(integral cube 0 1 0.001) ; 0.249999875000001

;ex.: 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))

;ex.: 1.31
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (wallis-pi n)
  (define (term x)
    (/
      (* 4 (square x))
      (- (* 4 (square x)) 1)))
  (* 2 (product term 1 inc n)))

(wallis-pi 10000)

;1.32 For a recursive version just change the reducer
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
        (accumulate combiner null-value term (next a) next b))))

;1.33
(define (filtered-accumulate combiner null-value term a next b filter-proc)
  (define (iter a result)
    (if (filter-proc a)
     (if (> a b)
      result
      (iter (next a) (combiner (term a) result)))
     (iter (next a) result)))
  (iter a null-value))

; fast-prime from 1.20
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (prime? n)
  (fast-prime? n 10))
; End primes

(define (prime-squared-sum a b)
  (filtered-accumulate + 0 square a inc b prime?))

(prime-squared-sum 1 100)

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (relative-primes-sum n)
  (define (relative-prime x)
    (= (gcd x n) 1))
  (filtered-accumulate + 0 identity 1 inc n relative-prime))

(relative-primes-sum 5)

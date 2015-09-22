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
      null-value
      (iter (next a) (combiner (term a) result))))
  (iter a start))

(define (product term a next b)
  (reducer * 1 term a next b))

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

(define (square x) (* x x))

(define (wallis-pi n)
  (define (term x)
    (/
      (* 4 (square x))
      (- (* 4 (square x)) 1)))
  (* 2 (product term 1 inc n)))

(wallis-pi 10)

; For a recursive version just change the reducer
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
        (reducer op start term (next a) next b))))

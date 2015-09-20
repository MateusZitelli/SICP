(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

; Exercise 1.6: Is not a good idea to use an custom function with a cond
; statement in place of the if because when a function is evaluated all the 
; args will be evaluated too, making an infinite series of calls.

(define (improve guess x)
  (average guess (/ x guess)))

(define (improve-cubic guess x)
  (/ (+ (* 2 guess) (/ x (square guess))) 3.0))

(define (average x y)
  (/ (+ x y) 2))

; Ex: 1.7
; This function is not very efficient because the precision is fixed not
; adapting to the desired value
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (good-enough-adaptive? x f guess)
  (let ((ratio (/ x (f guess))))
   (and (< ratio 1) (> ratio 0.9999))))

(define (sqrt-iter guess x f improve)
  (if (good-enough-adaptive? x f guess)
    guess
    (sqrt-iter (improve guess x) x f improve))) 

(define (sqrt x) (sqrt-iter 1.0 x square improve))

(define (square x) (* x x))
(define (cubic x) (* x x x))

; Ex: 1.8
(define (sqrt3 x) (sqrt-iter 1.0 x cubic improve-cubic))

; Ex. 1.9

; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
; This process is recursive

; (+ 3 6)
; (+ 4 5)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
; And this is iterative

;Ex. 1.10

; (A 1 10)
; (A 0 (A 1 9))
; (A 0 (A 0 (A 1 8)))
; (A 0 (A 0 (A 0 (A 1 7))))
; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
; ...
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
; ...
; 1024
; 
; (A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 4))
; ...
; (A 1 16)
; ...
; 65536
; 
; (A 3 3)
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 (A 1 (A 2 1)))
; (A 2 (A 1 2))
; (A 2 (A 0 (A 1 1)))
; (A 2 (A 0 2))
; (A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 (A 0 2)))
; (A 1 (A 1 4))
; (A 1 (A 0 (A 1 3)))
; (A 1 (A 0 (A 0 (A 1 2))))
; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
; (A 1 (A 0 (A 0 (A 0 2))))
; ...
; (A 1 16)
; ...
; 65536
; 
; (= (A 1 4) (A 2 3))
; (= (A 3 3) (A 2 4) (A 1 16))
; 
; (= (f n) (* 2 n))
; (= (f n) (pow 2 n))
; (= (f n) (pow 2 (pow 2 (- n 1))))

; 1.11
(define (f n)
  (cond ((< n 3) n)
        (else
          (+ (f (- n 1)) (f (- n 2)) (f (- n 3))))))

(define (f-iter n a b c)
  (cond ((= n 3) c)
        ((= n 2) b)
        ((= n 1) a)
        (else (f-iter (- n 1) b c (+ a b c)))))

(define (f2 n) (f-iter n 1 2 3))
  
; 1.12
(define (pascal l c)
  (cond ((or (< l 0) (< c 0) (>= c l)) 1)
        (else (+ (pascal (- l 1) (- c 1)) (pascal (- l 1) c)))))

(pascal 2 1)

; 1.15
; a)
; 5 times because if n is the number of steps so 12.15 / 3 ^ n = 0.1 =>
; =>n = log3(12.15 / 0.1) = 4.369...
; b)
; The number of steps and space has the growth rate of Theta(log(n)).

; 1.16 
(define (exp-iter b n a)
  (cond ((= n 0) a)
        ((odd? n)
         (exp-iter (square b)
                   (/ (- n 1) 2)
                   (* a b)))
        (else
          (exp-iter (square b)
                    (/ n 2)
                    a))))

(define (exp b n) (exp-iter b n 1))

(exp 64 13)

; 1.17
(define (double a) (+ a a))
(define (halve a) (/ a 2))

(define (fast-* a b)
  (cond ((= b 0) 0)
        ((even? b)
         (double (fast-* a (halve b))))
        (else
          (+ a
             (fast-* a (- b 1))))))

(fast-* 2 20)

;1.18
(define (iter-* a b c)
  (cond ((= b 0) c)
        ((even? b) (iter-* (double a) (halve b) c))
        (else (iter-* a (- b 1) (+ c a)))))

(define (new-* a b) (iter-* a b 0))
(new-* 42 24)

; 1.19
(define (fib n) (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (* p p)
                   (+ (* q q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b q) (* a q))
                        p
                        q
                        (- count 1)))))

(fib-iter 1 0 1 1 4)

; 1.20
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

; Running it as (gcd 206 40)
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd
  (remainder 206 40)
  (remainder 40 (remainder 206 40)))
(gcd
  (remainder 40 (remainder 206 40))
  (remainder 
    (remainder 206 40)
    (remainder 40 (remainder 206 40))))
(gcd 
  (remainder 
    (remainder 206 40)
    (remainder 40 (remainder 206 40)))
  (remainder
    (remainder 40 (remainder 206 40))
    (remainder 
      (remainder 206 40)
      (remainder 40 (remainder 206 40))))) #| In this case the b = 0 |# 

; Just a is evaluated
(gcd (remainder 
       (remainder 206 40)
       (remainder 40 (remainder 206 40))))

; So just 4 remainder operations are evaluated in normal-mode operation or lazy evaluation.

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

; Ex. 1.21
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199) ;199 
(smallest-divisor 1999) ;1999 
(smallest-divisor 19999) ;7 

;1.22
(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (current-milliseconds) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (cond 
    ((> start end) 
     (newline))
    ((odd? start)
     (timed-prime-test start)
     (search-for-primes (+ start 2) end))
    (else (search-for-primes (+ start 1) end))))

(search-for-primes 1e11 (+ 1e11 70))
#|
100000000003 *** 180.0
100000000019 *** 152.0
100000000057 *** 149.0
|#
(search-for-primes 1e12 (+ 1e12 70))
#|
1000000000039 *** 485.0
1000000000061 *** 483.0
1000000000063 *** 476.0
|#
(search-for-primes 1e13 (+ 1e13 100))
#|
10000000000037.0 *** 1535.0
10000000000051.0 *** 1522.0
10000000000099.0 *** 1564.0
|#
(define mean1e11 (/ (+ 180 152 149) 3))
(define mean1e12 (/ (+ 485 483 476) 3))
(define mean1e13 (/ (+ 1535 1522 1564) 3))

(/ (/ mean1e12 mean1e11) (sqrt 10)) ; 0.949340736233501
(/ (/ mean1e13 mean1e12) (sqrt 10)) ; 1.01197265011344 

; 1.23

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next-divisor test-divisor)
  (if (= test-divisor 2)
    3
    (+ test-divisor 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))


(search-for-primes 1e11 (+ 1e11 70))
#|
100000000003.0 *** 99.0
100000000019.0 *** 96.0
100000000057.0 *** 91.0
|#
(search-for-primes 1e12 (+ 1e12 70))
#|
1000000000039.0 *** 301.0
1000000000061.0 *** 288.0
1000000000063.0 *** 286.0
|#
(search-for-primes 1e13 (+ 1e13 100))
#|
10000000000037.0 *** 924.0
10000000000051.0 *** 915.0
10000000000099.0 *** 917.0
|#

(define mean1e11-2 (/ (+ 99 96 91) 3))
(define mean1e12-2 (/ (+ 301 288 286) 3))
(define mean1e13-2 (/ (+ 924 915 917) 3))

(/ mean1e11-2 mean1e11) ; 0.594594594594595
(/ mean1e12-2 mean1e12) ; 0.60595567867036 
(/ mean1e13-2 mean1e13) ; 0.596407703960182 

#|
I think that the reason for the ratio don't be 0.5 is some extra costs because 
of the extra next function calling and the if inside it be more expensive than
the + operation.
|#

; 1.24

(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
    (report-prime (- (current-milliseconds) start-time))))
#|
Seing that the fermat method calculates primes with complexity O(log n)
my expectation is that the ration between then will be log(1e18) / log(1e5) =
= 3.6...
|#

(search-for-primes 1000000000000000000 (+ 1000000000000000000 100))
#|
1000000000000000003 *** 116.0
1000000000000000009 *** 111.0
1000000000000000031 *** 109.0
1000000000000000079 *** 114.0
|#
(search-for-primes 100000 (+ 100000 100))
#|
100019 *** 31.0
100043 *** 30.0
100057 *** 30.0
100069 *** 34.0
|#

(define mean1e18 (/ (+ 116 111 109 114) 4))
(define mean1e5 (/ (+ 31 30 30 34) 4))
(inexact (/ mean1e18 mean1e5)) ; Exactly 3.6


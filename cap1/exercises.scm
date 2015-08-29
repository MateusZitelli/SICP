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
                   (/ count 2))
         (else (fib-iter (+ (* b q) (* a q) (* a p))
                         (+ (* b q) (* a q))
                         p
                         q
                         (- count 1))))))

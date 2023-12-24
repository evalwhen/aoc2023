;; chez scheme version
;; --- Day 1: Trebuchet?! ---

(import
 ;; (rnrs)
 ;; (rnrs hashtables)
 (utils)
 (chezscheme)
 )

(define digits-a '(
                   ("1" 1) ("2" 2) ("3" 3) ("4" 4) ("5" 5) ("6" 6) ("7" 7) ("8" 8) ("9" 9)
                   ))

(define digits-b '(
                   ("one" 1) ("two" 2) ("three" 3) ("four" 4) ("five" 5) ("six" 6) ("seven" 7) ("eight" 8) ("nine" 9)
                   ("1" 1) ("2" 2) ("3" 3) ("4" 4) ("5" 5) ("6" 6) ("7" 7) ("8" 8) ("9" 9)
                   ))


(define digits (make-parameter digits-b))

(define (first lst)
  (car lst))

(define (last lst)
  (let loop ([lst lst])
    (cond
     [(null? lst) '()]
     [(null? (cdr lst)) (car lst)]
     [else (loop (cdr lst))])))

(define (is-digits? str)
  (assoc str (digits)))

;; eightwothree
;; get all nums from string
;; this function can be modified to get all substrings of string.
(define (iter-string str)
  (let ([len (string-length str)])
    (let loop ([i 0] [j 0] [sub ""] [nums '()])
      ;; (displayln sub)
      ;; (displayln i)
      (cond
       [(<= len i) (reverse nums)]
       ;; [(is-digits? sub) (loop j j "" (cons sub nums))]
       [(is-digits? sub) (loop (add1 i) (add1 i) "" (cons sub nums))]
       [(<= len j) (loop (add1 i) (add1 i) "" nums)]
       [else (loop i
                   (add1 j)
                   (string-append sub (string (string-ref str j)))
                   nums)]))))



(define (get-numbers str)
  (map (lambda (e) (cadr (assoc e (digits)))) (iter-string str)))

(define (combine-digit str)
  (let ([nums (get-numbers str)])
    ;; (displayln nums)
    (+ (* (first nums) 10)
       (last nums))))

(define (sum strlst)
  (apply + (map combine-digit strlst)))

(define (sum2 strlst)
  (map combine-digit strlst))

;; (displayln (sum2 (get-lines "input.txt")))
(displayln (sum (get-lines "input.txt")))
;; (displayln (sum2 (get-lines "part-b.txt")))

;; (displayln (iter-string "eightwothree"))
(displayln (combine-digit "5nss"))

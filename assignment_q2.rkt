#lang racket

;This is an example implementation of ins_beg,
;It obviously doesn't do what it should, so you
;can edit this function to get started.
;
;Please note the provide function is necessary
;for the unit tests to work. Please include a
;(provide) for each function you write in your
;submitted assignment.
;
;You may delete these comments!

(provide ins_beg)
(provide ins_end)
(provide count_top_level)

(define (ins_beg el l)
  (cons el l))
(ins_beg 'a '(b c d))
(ins_beg '(a b) '(b c d))

(define (ins_end el lst)
  (append lst (list el)))
(ins_end 'a '(b c d))
(ins_end '(a b) '(b c d))

(define (count_top_level lst)
  (if (null? lst)
      0
      (+ 1 (count_top_level (cdr lst)))))

;D
(define (count_instances item 1st)
  (cond [(empty? 1st) 0]
        [(equal? item (car 1st))(+ 1(count_instances item(cdr 1st)))]
        [else (count_instances item (cdr 1st))]))
(printf "Instances of 'a':~a~n" (count_instances 'a '(a b a c a d)))
;E
(define (count_instances_tr item lst)
  (helper_tr item lst 0))
(define (helper_tr item lst total)
  (cond
    [(empty? lst) total]
    [(equal? item (car lst)) (helper_tr item (cdr lst) (+ 1 total))]
    [else (helper_tr item (cdr lst) total)]))
(printf "Tail recursion - Instances of 'b':~a~n" (count_instances_tr 'b '(a b a b a d)))

;F
(define (count_instances_deep item lst)
  (cond
    [(empty? lst) 0]
    [(equal? item (car lst)) (+ 1 (count_instances_deep item (cdr lst)))]
    [(list? (car lst)) (+ (count_instances_deep item (car lst)) (count_instances_deep item (cdr lst)))]
    [else (count_instances_deep item (cdr lst))]))
(printf "Sub lists - Instances of 'c':~a~n" (count_instances_deep 'c '((a b c) a (c a) (b a d))))
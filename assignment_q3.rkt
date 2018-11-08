#lang racket
(define tree '(((() 1 ()) 5 (() 16 ())) 21 ((() 25 ()) 39 (() 42 ()))))
(define to_Sort '(43 5 21 32 10 27 46 14))

(define (left_child tree)
  (car tree))

(define (right_child tree)
  (caddr tree))

;A 
(define (sortTree tree);sort left then sort right
 (begin(cond [(not (empty?(left_child tree))) (sortTree (left_child tree))])
   (printf "~a " (cadr tree));
   (cond [(not (empty?(right_child tree))) (sortTree (right_child tree))])))

;B 
(define (search_tree el tree)
(cond
  [(empty? tree) #f]
  [(equal? el (cadr tree)) #t]
  [(< el (cadr tree)) (search_tree el (left_child tree))]
  [else (search_tree el (right_child tree))]
  )
  )

;C 
(define (insert_item el tree)
  (cond [(empty? tree) (list '() el '())]
        [(equal? el (cadr tree)) tree]
        [(< el (cadr tree))
         (list ( insert_item el (left_child tree)) (cadr tree) (right_child tree))]
        [else (list (left_child tree) (cadr tree) (insert_item el (right_child tree)))]))



;D 
(define (add_list lst tree)
  (if (empty? lst) tree
      (add_list (cdr lst) (insert_item (car lst) tree))))

(define (higher_order_add_list lst tree left)
  (if (empty? lst) tree
      (higher_order_add_list (cdr lst) (higher_order_addItem (car lst) tree left) left)))



;E 
(define (tree_sort lst)
  (sortTree (add_list lst '())))

;F 
(define (higher_order_tree_sort lst orderFunc)
 (sortTree (higher_order_add_list lst '() orderFunc)))

(define (higher_order_addItem item tree left)
  (cond [(empty? tree) (list '() item '())]
        [(equal? item (cadr tree)) tree]
        [(left item (cadr tree))
         (list (higher_order_addItem item (left_child tree) left) (cadr tree) (right_child tree))]
        [else (list (left_child tree) (cadr tree) (higher_order_addItem item (right_child tree) left))]))



(define (ascending_last_digit a b)
  (< (remainder a 10) (remainder b 10)))

(display "display sorted list:\n")
(sortTree tree)

(display "\nsearch tree:\n")
(search_tree 21 tree)
(search_tree 20 tree)

(display "insert number 12\n")
(insert_item 12 tree)

(display "add to list:\n")
(add_list '(4 19 88 99 65) tree)

(display "sort tree from input list:\n")
(tree_sort to_Sort)

(display "\nHigher order tree sort:\n")
(display "ASCENDING:\n")
(higher_order_tree_sort to_Sort <)
(display "\nDescending:\n")
(higher_order_tree_sort to_Sort >)
(display "\nAscending based on last digit:\n")
(higher_order_tree_sort to_Sort ascending_last_digit)
#lang racket

(provide tree)
(provide to_Sort)
(provide left_child)
(provide right_child)
(provide sortTree)
(provide search_tree)
(provide insert_item)
(provide add_list)
(provide higher_order_add_list)
(provide tree_sort)
(provide higher_order_tree_sort)
(provide higher_order_addItem)

(define tree '(((() 1 ()) 5 (() 16 ())) 21 ((() 25 ()) 39 (() 42 ()))))
(define to_Sort '(43 5 21 32 10 27 46 14))

(define (left_child tree)
  (car tree))

(define (right_child tree)
  (caddr tree))

;A Display in sorted order the contents of a binary search tree
(define (sortTree tree);sort left then sort right
 (begin(cond [(not (empty?(left_child tree))) (sortTree (left_child tree))])
   (printf "~a " (cadr tree));
   (cond [(not (empty?(right_child tree))) (sortTree (right_child tree))])))

;B Return #t or #f if a given item is present or absent in a tree or not
(define (search_tree el tree)
(cond
  [(empty? tree) #f]
  [(equal? el (cadr tree)) #t]
  [(< el (cadr tree)) (search_tree el (left_child tree))]
  [else (search_tree el (right_child tree))]
  )
  )

;C Insert an item correctly into a list representing a binary search tree
(define (insert_item el tree)
  (cond [(empty? tree) (list '() el '())]
        [(equal? el (cadr tree)) tree]
        [(< el (cadr tree))
         (list ( insert_item el (left_child tree)) (cadr tree) (right_child tree))]
        [else (list (left_child tree) (cadr tree) (insert_item el (right_child tree)))]))



;D Take a list of items and insert them into a binary search tree
(define (add_list lst tree)
  (if (empty? lst) tree
      (add_list (cdr lst) (insert_item (car lst) tree))))

(define (higher_order_add_list lst tree left)
  (if (empty? lst) tree
      (higher_order_add_list (cdr lst) (higher_order_addItem (car lst) tree left) left)))



;E Implement a tree-sort algorithm
(define (tree_sort lst)
  (sortTree (add_list lst '())))

;F Implement a higher order version of the tree-sort function that takes a list and a
;function that determines the sorted order
;sort the list in ascending, descending and ascending based on last digit
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

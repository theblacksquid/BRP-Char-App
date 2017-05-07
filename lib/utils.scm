

(define -> js-invoke)

(define char->string
  (lambda (char)
    (list->string (cons char '()))))


(define char-upcase
  (lambda (char)
    (let ((int (char->integer char)))
      (integer->char (+ int 32)))))

(define char-downcase
  (lambda (char)
    (let ((int (char->integer char)))
      (integer->char (- int 32)))))
      
(define range
  (lambda (start end)
    (let loop ((index 0))
      (if 
        (eq? (+ index start) end)
        (cons end '())
        (cons 
          (+ start index)
          (loop (+ index 1)))))))

(define in-range?
  (lambda (num start end)
    (let loop ((rg (range start end)))
      (cond 
        ((null? rg) #f)
        ((eqv? num (car rg)) #t)
        (else (loop (cdr rg)))))))

(define char-upper-case?
  (lambda (char)
    (in-range? 
      (char->integer char)
      (char->integer #\A)
      (char->integer #\Z))))

; takes an alphabetic character, 
; returns #tl if its lower-case
(define char-lower-case?
  (lambda (char)
    (in-range? 
      (char->integer char)
      (char->integer #\a)
      (char->integer #\z))))

; returns a new string in which all lowercase
; characters are capitalized.
(define capitalize-string
  (lambda (str)
    (list->string
      (let loop ((ls (string->list str)))
        (if 
          (null? ls)
          '()
          (cons
            (if 
              (char-lower-case? (car ls))
              (char-downcase (car ls))
              (car ls))
            (loop (cdr ls))))))))

; returns a number from a character
(define char->number 
  (lambda (char)
    (string->number (char->string char))))

; returns a list of numbers
(define number->digits
  (lambda (num)
    (let loop ((ls (string->list (number->string num))))
      (if 
        (null? ls)
        '()
        (cons (char->number (car ls)) (loop (cdr ls)))))))

(define in-list? 
  (lambda (ls item)
    (cond
      ((null? ls) #f)
      ((eqv? (car ls) item) #t)
      (else (in-list? (cdr ls) item)))))

(define remove-duplicates
  (lambda (ls)
    (let loop ((ls-loop ls)
               (unq '()))
      (cond
        ((null? ls-loop) (reverse unq))
        ((in-list? unq (car ls-loop)) 
          (loop (cdr ls-loop) unq))
        (else 
          (loop (cdr ls-loop) (cons (car ls-loop) unq)))))))
          
(define remove-ls-elem
  (lambda (ls elem)
    (let loop ((ls-loop ls)
               (output '()))
      (cond 
        ((null? ls-loop) (reverse output))
        ((not (eqv? (car ls-loop) elem)) 
          (loop (cdr ls-loop) (cons (car ls-loop) output)))
        (else 
          (loop (cdr ls-loop) output))))))

(define tree-ref
  (lambda (tree key)
    (cond 
      ((null? tree) #f)
      ((eqv? (car (car tree)) key) (cdr (car tree)))
      ((pair? (cdr (car tree))) 
        (or 
          (tree-ref (cdr tree) key)
          (tree-ref (cdr (car tree)) key)))
      (else (tree-ref (cdr tree) key)))))
    
(define has-key?
  (lambda (node)
    (if
      (pair? (car node))
      #f
      (car node))))
    
(define get-keys
  (lambda (tree)
    (let ((result '()))
      (let loop ((ls tree))
        (cond
          ((null? ls) (reverse result))
          ((has-key? (car ls)) 
            (begin
              (set! result (cons (has-key? (car ls)) result))  
              (if 
                (pair? (cdr (car ls)))
                (or (loop (cdr ls)) (loop (cdr (car ls))))
                (loop (cdr ls)))))
          ((pair? (cdr (car ls))) 
            (or (loop (cdr ls)) (loop (cdr (car ls)))))
          (else (loop (cdr ls))))))))
          
; Usage: 
; (object-ref object key)
; given an associative list (object), it returns
; the value of `key`, if the value of `key` 
; is a function, it returns it as a function.
(define object-ref 
  (lambda (object key)
    (let ((sym (tree-ref object key)))
      (if 
        (procedure? (eval sym))
        (eval sym)
        sym))))

(define => object-ref)

(define divisible?
  (lambda (num1 num2)
    (zero? (mod num1 num2))))

(define prime-factorize
  (lambda (num)
    (let loop ((index 2) 
               (n num))
      (cond
        ((eqv? n index) (cons index '()))
        ((divisible? n index) 
          (cons index (loop index (/ n index))))
        (else 
          (loop (+ index 1) n))))))

(define remove-special-chars
  (lambda (str)
    (list->string
      (let loop ((ls (string->list str))
                 (alphanumeric (string->list 
                 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890 ")))
       (cond 
         ((null? ls) '())
         ((in-list? alphanumeric (car ls)) 
          (cons (car ls)
                (loop (cdr ls) alphanumeric)))
         (else 
          (loop (cdr ls) alphanumeric)))))))

(define sanitize-name
  (lambda (name)
    (let loop ((ls (string-split (remove-special-chars name) " "))
               )
      (if (null? (cdr ls))
          (car ls)
          (string-append (car ls) "-" (loop (cdr ls)))))))

(define flatten
  (lambda (ls)
    (let loop ((lst ls))
      (cond
        ((null? lst) '())
        ((pair? (car lst)) 
         (append (loop (car lst))
                 (loop (cdr lst))))
        (else
          (cons (car lst)
                (loop (cdr lst))))))))




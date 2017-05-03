(define rand-int
  (lambda (start end)
    (js-call (js-eval "randInt") start end)))

(define dice
  (lambda (num sides modifier)
    (if (zero? num)
        modifier
        (+ (rand-int 1 sides) (dice (- num 1) sides modifier)))))

(define profile-items
  '("Name" "Age" "Race" "Gender" 
    "Handedness" "Job" "Description"
    "Biography"))

(define stats
  '("STR" "SIZ" "CON" "INT" "DEX" "POW" "APP"))

(define roll-stat
  ; stat-name should be a string
  ; dice-vals is a list of three numbers, see 'dice'
  (lambda (stat-name dice-vals)
    (list stat-name (dice (list-ref dice-vals 0)
                          (list-ref dice-vals 1)
                          (list-ref dice-vals 2)))))

(define make-stat-table
  (lambda (dice-vals)
    (let loop ((ls stats))
      (if (null? ls)
          '()
          (cons (roll-stat (car ls) dice-vals)
                (loop (cdr ls)))))))

(define derived-stats
  '("Health" "Magick" "Effort" "Dmg Bonus" "Stamina" 
    "Idea" "Agility" "Luck" "Charisma"))

(define derived-stats-pairs
  '(("Effort" "STR")
    ("Stamina" "CON")
    ("Idea" "INT")
    ("Agility" "DEX")
    ("Luck" "POW")
    ("Charisma" "APP")))

(define calculate-dmg-bonus
  ; num is a number that is the sum of 
  ; STR and SIZ
  (lambda (num)
    (cond
      ((< num 24) "None")
      ((and (>= num 24)
            (< num 33)) "1d4")
      ((and (>= num 33)
            (< num 41)) "1d6")
      ((> num 41) 
        (string-append (number->string (floor (/ num 16))) "d6")))))

(define assoc-ref
  (lambda (val ls)
    (cadr (assoc val ls))))

(define calculate-derived-stats
  ; stat-table is an associative list of the form
  ; '(x . y) where x is a string of one of the
  ; stat names (see 'stats') and y is the stat 
  ; value. 
  (lambda (stat-table)
    (let loop ((ls derived-stats)
               (prod '()))
      (cond 
        ((null? ls) prod)
        ((eqv? (car ls) "Health") 
         (loop (cdr ls)
               (cons (cons (car ls)
                           (+ (assoc-ref "SIZ" stat-table)
                              (assoc-ref "CON" stat-table)))
                     prod)))
        ((eqv? (car ls) "Magick")
         (loop (cdr ls)
               (cons (cons (car ls)
                           (+ (assoc-ref "POW" stat-table)
                              (assoc-ref "CON" stat-table)))
                     prod)))
        ((eqv? (car ls) "Dmg Bonus") 
         (loop (cdr ls)
               (cons (cons (car ls)
                           (calculate-dmg-bonus 
                             (+ (assoc-ref "STR" stat-table)
                                (assoc-ref "SIZ" stat-table))))
                     prod)))
        (else 
          (loop (cdr ls)
                (cons (cons (car ls)
                            (* (assoc-ref (assoc-ref (car ls) 
                                                     derived-stats-pairs) 
                                          stat-table) 
                               5))
                      prod)))))))




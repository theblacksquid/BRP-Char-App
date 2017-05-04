
(define get-stat-values
  (lambda ()
    (let loop ((ls stats))
      (if (null? ls)
          '()
          (cons (list (car ls)
                      (-> ($ (string-append "#" (car ls) "-val")) 
                          'val))
                (loop (cdr ls)))))))

(define update-derived
  (lambda ()
    (let loop ((ls derived-stats)
               (ht (get-stat-values)))
      (cond 
        ((null? ls) '())
        ((eqv? (car ls) "Dmg Bonus") 
         (-> ($ "#Dmg-Bonus") 
             'val 
             (calculate-dmg-bonus (+ (string->number (assoc-ref "STR" ht))
                                     (string->number (assoc-ref "SIZ" ht)))))
         (loop (cdr ls) ht))
        ((or (eqv? (car ls) "Health")
             (eqv? (car ls) "Magick")) 
         (-> ($ (string-append "#" (car ls)))
             'val 
             (+ (string->number (assoc-ref "CON" ht))
                (string->number (assoc-ref (if (eqv? (car ls) "Health") 
                                               "SIZ" 
                                                "POW") 
                                           ht))))
         (loop (cdr ls) ht))
        (else
         (-> ($ (string-append "#" (car ls)))
             'val 
             (* (string->number 
                  (case (car ls) 
                    (("Effort") (assoc-ref "STR" ht))
                    (("Idea") (assoc-ref "INT" ht))
                    (("Stamina") (assoc-ref "CON" ht))
                    (("Agility") (assoc-ref "DEX" ht))
                    (("Luck") (assoc-ref "POW" ht))
                    (("Charisma") (assoc-ref "APP" ht))) 
                )
                5))
          (loop (cdr ls) ht))))))

(define stat-roll-callback
  (lambda (stat-name)
    (begin
      (-> ($ (string-append "#" stat-name "-val")) 
          'val 
          (dice 2 6 6))
      (update-derived)
    )))

(define randomize-callback
  (lambda ()
    (begin
      (let loop ((ls stats)
                 (ht (make-stat-table '(2 6 6))))
        (if (null? ls)
            '()
            (begin
              (-> ($ (string-append "#" (car ls) "-val")) 
                  'val 
                  (assoc-ref (car ls) ht))
              (loop (cdr ls) ht)))
      )
      (update-derived)
    )))

(-> ($ "#randomize")
    'click
    (js-closure
      (lambda ()
        (randomize-callback))))
    
(let loop ((ls stats))
  (if (null? ls)
      '()
      (begin
        (-> ($ (string-append "#" (car ls) "-btn"))
            'click
           (js-closure
             (lambda ()
               (stat-roll-callback (car ls)))))
        (loop (cdr ls))
      )))    
    
    
    

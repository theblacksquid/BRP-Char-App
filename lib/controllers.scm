
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

(define skill-point-callback
  (lambda (skill-name mode)
    (let ((name (string-append "#" (sanitize-name skill-name))))
      (if (zero? (-> ($ "#skillpoints") 'val))
          '()
          (begin
            (-> ($ name) 'val ((if (eqv? mode 'add) + -) 
                               (string->number (-> ($ name) 'val))
                               1))
            (-> ($ "#skillpoints") 'val ((if (mode 'add) - +)
                                         (-> ($ "#skillpoints") 'val)
                                         1))
          )))))

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

(define update-skills
  (lambda ()
    '()))
    
(define nav-btn-callback
  (lambda (to-show to-hide)
    (if (pair? to-hide)
        (let loop ((ls to-hide))
          (if (null? ls)
              '()
              (begin
                (-> ($ (string-append "#" to-show)) 
                    'css "display" "block")
                (-> ($ (string-append "#" (car ls))) 
                    'css "display" "none")
                (loop (cdr ls))
              )))
        (begin
          (-> ($ (string-append "#" to-show)) 'css "display" "inline-block")
          (-> ($ (string-append "#" to-hide)) 'css "display" "none")
        ))))    

(-> ($ "#profile-btn") 'click
  (js-closure
    (lambda ()
      (nav-btn-callback "profile-info" "skills-overview"))))    
    
    
(-> ($ "#skills-btn") 'click
  (js-closure
    (lambda ()
      (nav-btn-callback "skills-overview" "profile-info"))))    
    
    

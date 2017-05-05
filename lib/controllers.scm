
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

(define update-skillpoints
  (lambda ()
    (-> ($ "#skillpoints") 'val
      (let ((ht (get-stat-values)))
        (+ (let loop ((ls stats)
                      (prod 0))
             (cond 
                ((null? ls) prod)
                ((eqv? (car ls) "INT") 
                 (loop (cdr ls) prod))
                (else
                  (loop (cdr ls) 
                        (+ (* (string->number (assoc-ref (car ls) ht))
                              5) 
                           prod)))))
           (* (string->number (assoc-ref "INT" ht)) 10))))))

(define update-skills
  (lambda ()
    (let loop ((ls skill-list)
               (ht (get-stat-values)))
      (cond
        ((null? ls) '())
        ((pair? (assoc-ref (car (car ls)) ls)) 
         (begin
           (console-log (car (car ls)))
           (console-log (cadr (assoc-ref (car (car ls)) ls)))
           (console-log (string-append "#" (sanitize-name (car (car ls)))))
           (console-log "----")
           (-> ($ (string-append "#" (sanitize-name (car (car ls))))) 'val 
             (if (string? (cadr (assoc-ref (car (car ls)) ls)))
                 (+ (string->number (assoc-ref (car (assoc-ref (car (car ls)) ls)) ht))
                    (string->number (assoc-ref (cadr (assoc-ref (car (car ls)) ls)) ht)))
                 (* (string->number (assoc-ref (car (assoc-ref (car (car ls)) ls)) ht)) 
                    (cadr (assoc-ref (car (car ls)) ls))
                    )))
           (loop (cdr ls) ht)
         ))
        (else
          (loop (cdr ls) ht))))))

(define stat-roll-callback
  (lambda (stat-name)
    (begin
      (-> ($ (string-append "#" stat-name "-val")) 
          'val 
          (dice 2 6 6))
      (update-derived)
      (update-skillpoints)
      (update-skills)
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
      (update-skillpoints)
      (update-skills)
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
    
   
   
   
   
   
   

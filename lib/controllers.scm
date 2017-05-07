
(define get-stat-values
  (lambda ()
    (let loop ((ls stats))
      (if (null? ls)
          '()
          (cons (list (car ls)
                      (-> ($ (string-append "#" (car ls))) 
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
      (-> ($ (string-append "#" stat-name)) 
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
              (-> ($ (string-append "#" (car ls))) 
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
      (if (zero? (string->number (-> ($ "#skillpoints") 'val)))
          '()
          (begin
            (-> ($ name) 'val ((if (eqv? mode 'add) + -) 
                               (string->number (-> ($ name) 'val))
                               1))
            (-> ($ "#skillpoints") 'val ((if (eqv? mode 'add) - +)
                                         (string->number (-> ($ "#skillpoints") 'val))
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
                    'css "display" "inline-block")
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
      (nav-btn-callback "profile-info" '("skills-overview" "stats-overview")))))    
    
    
(-> ($ "#skills-btn") 'click
  (js-closure
    (lambda ()
      (nav-btn-callback "skills-overview" '("profile-info" "stats-overview")))))    
    
(-> ($ "#stats-btn") 'click
  (js-closure
    (lambda ()
      (nav-btn-callback "stats-overview" '("profile-info" "skills-overview")))))
   
(define get-profile-info
  (lambda ()
    (let loop ((ls (flatten 
                     (list (js-array->list ($ "#profile-info input"))
                           (js-array->list ($ "#profile-info select"))
                           (js-array->list ($ "#profile-info textarea"))))))
      (if (null? ls)
          '()
          (begin
            ;(console-log (car ls))
            (cons (list (js-ref (car ls) "id") (js-ref (car ls) "value"))
                  (loop (cdr ls))))))))

(define get-stats-info
  (lambda ()
    (let loop ((ls stats))
      (if (null? ls)
          '()
          (cons (list (car ls) 
                      (-> ($ (string-append "#" (car ls))) 'val))
                (loop (cdr ls)))))))

(define get-derived-stats-info
  (lambda ()
    (let loop ((ls (js-array->list ($ "#derived-stats-panel input"))))
      (if (null? ls)
          '()
          (cons (list (js-ref (car ls) "id") (js-ref (car ls) "value"))
                (loop (cdr ls)))))))

(define get-skills-info
  (lambda ()
    (let loop ((ls (js-array->list ($ "#skills-panel input"))))
      (if (null? ls)
          '()
          (cons (list (js-ref (car ls) "id")
                      (js-ref (car ls) "value"))
                (loop (cdr ls)))))))

(define get-weapon-skills-info
  (lambda ()
    (let loop ((ls (js-array->list ($ "#weapon-skills-panel input"))))
      (if (null? ls)
          '()
          (cons (list (js-ref (car ls) "id")
                      (js-ref (car ls) "value"))
                (loop (cdr ls)))))))

;(console-log (flatten (list (js-array->list ($ "#profile-info input"))
;                           (js-array->list ($ "#profile-info select"))
;                           (js-array->list ($ "#profile-info textarea")))))

(define collate-info
  (lambda ()
    `(("Profile" ,(get-profile-info)) 
      ("Stats" ,(get-stats-info)) 
      ("Derived Stats" ,(get-derived-stats-info))
      ("Skills" ,(get-skills-info)
      ("Weapon Skills" ,(get-weapon-skills-info))))))



(-> ($ "#save-character") 'click
  (js-closure 
    (lambda ()
      (console-log (format-info (collate-info))))))



   

   
   

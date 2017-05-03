
(define render
  (lambda (elem tmpl mode)
    (-> ($ elem) mode tmpl)))
    
(define rmv
  (lambda (elem)
    (js-closure
      (lambda ()
        (element-remove! elem)))))

(define make-stat-roller
  (lambda (stat-name)
    (element-new 
      `(div
        class "w3-container w3-row"
        style "padding: 0.35em 16px;"
        (input 
          type "text"
          placeholder ,stat-name
          class "w3-input w3-grey"
          style "display: inline-block;width:30%"
          id ,(string-append stat-name "-val")
          disabled)
        (button
          id ,(string-append stat-name "-btn")
          style "display: inline-block;width:50%"
          class "w3-btn"
          "ROLL!")))))

(define make-derived-stat-box
  (lambda (stat-name)
    (element-new
      `(div
        class "w3-container"
        style "padding: 0.35em 16px;width: 60%"
        (input
          type "text"
          id ,stat-name
          class "w3-input w3-grey"
          style "width 30%"
          placeholder ,stat-name
          disabled)))))

; make sure to research about drop-down menus
(define profile-elem
  (lambda (elem-name)
	(cond 
		((or (eqv? elem-name "Description")
		     (eqv? elem-name "Biography")) 
		 (element-new
			`(textarea
				id ,elem-name
				class "w3-grey w3-input"
				style "padding: 0.35em 16px"
				placeholder ,elem-name)))
		((or (eqv? elem-name "Handedness")
		     (eqv? elem-name "Gender"))
		 (let ((check (lambda (val then els) 
					(if (eqv? val "Gender")
					    then
					    els))))
			(element-new 
				`(select
					id ,elem-name
					class "w3-select w3-grey"
					style "padding: 0.35em 16px"
					(option
						value ""
						,(check elem-name "Gender" "Handedness"))
					(option
						value ,(check elem-name "Male" "Right")
						,(check elem-name "Male" "Right"))
					(option
						value ,(check elem-name "Female" "Left")
						,(check elem-name "Female" "Left"))
					(option
						value "Other"
						"Other")))))
		(else
		  (element-new
			`(input
				id ,elem-name
				class "w3-input w3-grey"
				style "padding: 0.60em 16px"
				placeholder ,elem-name))))))

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
    (let loop ((ls (string-split name " "))
               )
      (if (null? (cdr ls))
          (car ls)
          (string-append (car ls) "-" (loop (cdr ls)))))))

(define skill-box
  (lambda (skill-name skill-base)
    (element-new 
      `(div
        class "w3-container"
        id ,(string-append skill-name "-div")
		(p
			class "w3-third"
			,(string-append skill-name ":   "))
        (input
			    type "text"
			    id ,(sanitize-name skill-name)
			    class "w3-input w3-third w3-grey w3-center"
			    style "display: inline-block;width:30%"
			    value ,(if (pair? skill-base)
			               (if (string? (cadr skill-base))
			                   (string-append (car skill-base)
			                                  "+"
			                                  (cadr skill-base))
			                   (string-append (car skill-base)
			                                  "*"
			                                  (number->string (cadr skill-base))))
			               skill-base)
			    disabled)
		(div
			class "w3-third"
			style "display: inline-block;"
			;style "float: right;"
			(button
				class "w3-btn"
				style "display: inline-block;"
				id ,(string-append (sanitize-name skill-name) "-add")
				"+")
			(button
				class "w3-btn"
				style "display: inline-block;"
				id ,(string-append (sanitize-name skill-name) "-subtract")
				"-"
				)
		)
        ))))

(define weapon-skill-box
  (lambda (skill-name skill-base dmg-val)
    (element-new 
      `(div
        class "w3-container"
        id ,(string-append skill-name "-div")
		    (p
			    class "w3-rest"
			    ,(string-append skill-name ":   "))
            (input
              id ,(sanitize-name skill-name) 
			        type "text"
			        class "w3-input w3-third w3-grey w3-center"
			        style "display: inline-block;width:30%"
			        value ,(if (pair? skill-base)
			                   (if (string? (cadr skill-base))
			                       (string-append (car skill-base)
			                                      "+"
			                                      (cadr skill-base))
			                       (string-append (car skill-base)
			                                      "*"
			                                      (number->string (cadr skill-base))))
			                   skill-base)
			        disabled)
		        (div
			        class "w3-third"
			        style "display: inline-block;"
			        ;style "float: right;"
			        (button
				        class "w3-btn"
				        style "display: inline-block;"
				        id ,(string-append (sanitize-name skill-name) "-add")
				        "+")
			        (button
				        class "w3-btn"
				        style "display: inline-block;"
				        id ,(string-append (sanitize-name skill-name) "-subtract")
				        "-"
				        )
		        )
		        (p 
			        class "w3-third"
			        ,dmg-val)
        ))))

(let loop ((ls stats))
  (if (null? ls)
      '()
      (begin
        (render "#stats-panel" (make-stat-roller (car ls)) 'append)
        (loop (cdr ls))
      )))

(render "#stats-panel" 
        (element-new
          '(div
            class "w3-container"
            style "padding: 0.35em 16px;"
            (button
              id "randomize"
              style "width: 80%"
              class "w3-btn-block"
              "RANDOMIZE!!!"))) 
        'append)

(let loop ((ls profile-items))
	(if (null? ls)
		'()
		(begin
			(render "#profile-panel" (profile-elem (car ls)) 'append)
			(loop (cdr ls))
		)))

(let loop ((ls derived-stats))
  (if (null? ls)
      '()
      (begin
        (render "#derived-stats-panel" 
                (make-derived-stat-box (car ls)) 
                'append)
        (loop (cdr ls))
      )))

(render "#main-skills-panel"
        (element-new
          '(div
            class "w3-container w3-rest"
            (p
              style "display:inline-block"
            "Total Skill Points:  ")
             (input 
                type "text"
                style "width:80%"
                class "w3-input w3-center w3-grey"
                value "000"
                disabled)))
        'append)

(let loop ((ls skill-list))
	(if (null? ls)
	    '()
	    (begin
	    (render "#skills-panel" 
	            (skill-box (car (car ls)) (cadr (car ls)))
	            'append)
	    (loop (cdr ls))
	    )))

(render "#weapon-skills-panel" 
        (weapon-skill-box "Brawl" 25 "1d4+db") 
        'append)

(render "#weapon-skills-panel" 
        (weapon-skill-box "Grapple" 25 "1d4+db") 
        'append)

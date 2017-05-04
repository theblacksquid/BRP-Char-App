
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
        ,(string-append stat-name ":  ")
        (button
          id ,(sanitize-name (string-append stat-name "-btn"))
          style "display: inline-block;float:right"
          class "w3-btn"
          "ROLL!")
        (input 
          type "text"
          placeholder ,stat-name
          class "w3-grey"
          style "display: inline-block;float:right"
          id ,(sanitize-name (string-append stat-name "-val"))
          disabled)))))

(define make-derived-stat-box
  (lambda (stat-name)
    (element-new
      `(div
        class "w3-container"
        style "padding: 0.35em 16px;"
        ,(string-append stat-name ":  ")
        (input
          type "text"
          id ,(sanitize-name stat-name)
          class "w3-grey"
          style "float:right"
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
				class "w3-grey"
				style "padding: 0.35em 16px;display:block;width:100%"
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
					class "w3-grey"
					style "padding: 0.35em 16px;display:block;width:100%"
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
				class "w3-grey"
				style "padding: 0.60em 16px;display:block;width:100%"
				placeholder ,elem-name))))))

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
			    class "w3-third w3-grey w3-center"
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
          style "width:25%;display:inline-block;text-align:center"
          ,skill-name)
        (input
          id ,(sanitize-name skill-name) 
	        type "text"
	        class "w3-grey w3-center"
	        style ";width:25%"
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
	        style "display: inline-block;width:25%"
	        ;style "float: left;"
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
	        ;class "w3-third"
		      style "display: inline-block;width:25"
	        ,dmg-val)
        ))))

(render "#app"
        (element-new
          `(div
            style "margin:auto;"
            (div
              id "profile-panel"
              style "display:block")
            (div
              id "stats-container" 
              style "text-align:center;display:block"
              (div 
               id "stats-panel"
               style "display:inline-block;"
               )
              (div 
               id "derived-stats-panel"
               style "display:inline-block;"
               )
            )
            (div 
              id "skills-panel"
              style "text-align:center")
            (div
              id "weapon-skills-panel"
              style "text-align:center"
              ))
        )
        'html)

(let loop ((ls profile-items))
	(if (null? ls)
		'()
		(begin
			(render "#profile-panel" (profile-elem (car ls)) 'append)
			(loop (cdr ls))
		)))

(render "#skills-panel"
        (element-new
          '(div
            class "w3-container w3-rest"
            style "text-align:center;"
            (p
              style "text-align:center;"
            "Total Skill Points:  ")
             (input 
                type "text"
                style "width:80%;margin:auto"
                class "w3-center w3-grey"
                value "000"
                disabled)))
        'append)

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
              ;style "width: 80%"
              class "w3-btn-block"
              "RANDOMIZE!!!"))) 
        'append)

(let loop ((ls derived-stats))
  (if (null? ls)
      '()
      (begin
        (render "#derived-stats-panel" 
                (make-derived-stat-box (car ls)) 
                'append)
        (loop (cdr ls))
      )))

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

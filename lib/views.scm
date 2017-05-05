
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
        style "padding: 0.35em 16px;text-align:left"
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
        style "padding: 0.35em 16px;text-align:left"
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
        ;style "display:inline-block"
        id ,(string-append skill-name "-div")
	      (p
		      class "w3-third"
		      style "text-align:left"
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
          style "width:25%;display:inline-block;text-align:left"
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
              id "main-navbar"
              style "display:block"
              (h1 "BRP Character Generator")
              (button
                id "character-panel-btn"
                class "w3-btn-bar"
                "Character Profile")
              (button
                id "character-list-btn"
                class "w3-btn-bar"
                "Character List")
              (button 
                id "settings-panel-btn"
                "Settings"))
             (div
              id "character-panel"
              style "display:block"
              (div
                style "display:inline-block;width:15%;vertical-align:top;"
                (button
                  id "profile-btn"
                  class "w3-btn-block"
                  style "display:block"
                  "Profile Information")
                (button
                  id "skills-btn"
                  class "w3-btn-block"
                  style "display:block"
                  "Skills Overview"))
              (div
                style "display:inline-block;width:85%"
                id "profile-info"
                (div
                  id "profile-panel"
                  style "display:inline-block;text-align:center;vertical-align:top")
                (div
                  id "stats-container" 
                  style "text-align:center;display:inline-block"
                  (div 
                   id "stats-panel"
                   style "display:inline-block;"
                   )
                  (div 
                   id "derived-stats-panel"
                   style "display:inline-block;"
                   )
                 )
              
               )
               (div
                  id "skills-overview"
                  style "display:none"
                  (div 
                    id "skill-pt-panel")
                  (div 
                    id "skills-panel"
                    style "text-align:center;display:inline-block;overflow:auto;height:500px")
                  (div
                    id "weapon-skills-panel"
                    style "text-align:center;display:inline-block;vertical-align:top")
               )
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

(render "#skill-pt-panel"
        (element-new
          '(div
            class "w3-container w3-rest"
            style "text-align:center;vertical-align:top"
            (p
              style "text-align:left;display:inline-block"
            "Total Skill Points:  ")
             (input 
                type "text"
                id "skillpoints"
                style "width:30%;margin:auto;display:inline-block"
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

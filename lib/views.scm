
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
        style "padding: 0.35em 16px;text-align:left;"
        ,(string-append stat-name ":  ")
        (button
          id ,(string-append stat-name "-btn")
          style "display:inline-block;float:right"
          class "w3-btn"
          "ROLL!")
        (input 
          type "text"
          placeholder ,stat-name
          class "w3-grey"
          style "display: inline-block;float:right"
          id ,stat-name 
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
        ;style "vertical-align:top"
        id ,(string-append skill-name "-div")
	      (p
		      ;class "w3-third"
		      style "text-align:left;display:inline-block;width:30%"
		      ,(string-append skill-name ":   "))
        (input
			    type "text"
			    id ,(sanitize-name skill-name)
			    class "w3-grey w3-center"
			    style "width:30%;"
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
          style "display: inline-block;width:30%"
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

(element-remove! ($ "#gear"))

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
                class "w3-btn"
                "Character Profile")
              (button
                id "character-list-btn"
                class "w3-btn"
                "Character List")
              (button 
                id "settings-panel-btn"
                class "w3-btn"
                "Settings"))
             (div
              id "character-panel"
              style "display:block"
              (div
                style "display:inline-block;width:15%;vertical-align:top;margin-top:1%;"
                (button
                  id "profile-btn"
                  class "w3-btn-block"
                  style "display:block"
                  "Profile Overview")
                (button
                  id "stats-btn"
                  class "w3-btn-block"
                  style "display:block"
                  "Stats Overview")
                (button
                  id "skills-btn"
                  class "w3-btn-block"
                  style "display:block"
                  "Skills Overview"))
              (div
                style "display:inline-block;width:85%;margin-top:1%"
                class "w3-container"
                id "profile-info"
                (div
                  id "profile-panel"
                  style "display:inline-block;vertical-align:top")
                  (div
                    style "margin-left:1%;overflow:auto;height:500px;width:35%;display:inline-block"
                    (textarea 
                      id "charsheet"
                      class "w3-grey"
                      placeholder "Character Info goes here when you click 'Character Sheet'."
                      style "display:inline-block;width:100%;height:90%")
                    (div
                      (button
                        id "Clear"
                        class "w3-btn"
                        style "display:inline-block;width:50%"
                        "Clear") 
                      (button
                        id "Print"
                        class "w3-btn"
                        style "display:inline-block;width:50%"
                        "Print"))
                  )
               )
               (div 
                 id "stats-overview"
                 style "display:none;width:85%;margin-top:1%"
                (div 
                  id "stats-panel"
                  style "display:inline-block;"
                  )
                (div 
                  id "derived-stats-panel"
                  style "display:inline-block;"
                  )
                (div
                  id "stats-container" 
                  style "text-align:center;display:inline-block"
                 )
               )
               (div
                  id "skills-overview"
                  class "w3-container"
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

(render "#profile-panel" 
        (element-new
          '(div
            (button
              id "save-character"
              class "w3-btn"
              style "width:100%"
              "Save Character")
             (button
              id "get-character-sheet"
              class "w3-btn"
              style "width:100%"
              "Character Sheet")
             (button
              id "get-codes"
              class "w3-btn"
              style "width:100%"
              "Get Codes"))) 
        'append)

(render "#skill-pt-panel"
        (element-new
          '(div
            class "w3-container"
            style ";vertical-align:top;display:block"
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

(render "#skill-pt-panel"
        (element-new
          '(div
            class "w3-container"
            style "display:block"
            (button
              id "add-skill-btn"
              class "w3-btn"
              style "display:inline-block"
              "Add Skill")
            (button
              id "reset-skills-btn"
              class "w3-btn"
              style "display:inline-block"
              "Reset Skills")))
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
	      (-> ($ (string-append "#" (sanitize-name (car (car ls))) "-add")) 
	          'click
	          (js-closure 
	            (lambda ()
	              (skill-point-callback (car (car ls)) 'add))))
	      (-> ($ (string-append "#" (sanitize-name (car (car ls))) "-subtract")) 
	          'click
	          (js-closure 
	            (lambda ()
	              (skill-point-callback (car (car ls)) 'subtract))))
	      (loop (cdr ls))
	    )))

(define create-list-elem
  (lambda (ls)
    (string-append 
      "<ul>"
      (let loop ((lst ls))
        (if (null? lst)
            ""
            (string-append "<li>"
                           (car lst) ": "
                           (-> ($ (string-append "#" (sanitize-name (car lst))))
                               'val)
                           "</li>"
                           (loop (cdr lst)))))
      "</ul>")))

(render "#weapon-skills-panel" 
        (weapon-skill-box "Brawl" 25 "1d4+db") 
        'append)

(render "#weapon-skills-panel" 
        (weapon-skill-box "Grapple" 25 "1d4+db") 
        'append)

(define popup
  (lambda ()
    '()))


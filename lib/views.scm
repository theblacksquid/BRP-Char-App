
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
        class "w3-container w3-row stat-roller"
        ,(string-append stat-name ":  ")
        (button
          id ,(string-append stat-name "-btn")
          class "w3-btn"
          "ROLL!")
        (input 
          type "text"
          placeholder ,stat-name
          class "w3-grey"
          id ,stat-name 
          disabled)))))

(define make-derived-stat-box
  (lambda (stat-name)
    (element-new
      `(div
        class "w3-container derived-stat-box"
        ,(string-append stat-name ":  ")
        (input
          type "text"
          id ,(sanitize-name stat-name)
          class "w3-grey"
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
				class "w3-grey profile-elem"
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
					class "w3-grey profile-elem"
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
				class "w3-grey profile-elem"
				placeholder ,elem-name))))))

(define skill-box
  (lambda (skill-name skill-base)
    (element-new 
      `(div
        class "w3-container skill-box"
        ;style "vertical-align:top"
        id ,(string-append skill-name "-div")
	      (p ,(string-append skill-name ":   "))
        (input
			    type "text"
			    id ,(sanitize-name skill-name)
			    class "w3-grey w3-center"
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
          (button
            class "w3-btn"
            id ,(string-append (sanitize-name skill-name) "-add")
            "+")
          (button
            class "w3-btn"
            id ,(string-append (sanitize-name skill-name) "-subtract")
            "-"
            )
        )
        ))))

(define weapon-skill-box
  (lambda (skill-name skill-base dmg-val)
    (element-new 
      `(div
        class "w3-container weapon-skill-box"
        id ,(string-append skill-name "-div")
        (p ,skill-name)
        (input
          id ,(sanitize-name skill-name) 
	        type "text"
	        class "w3-grey w3-center"
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
          class "weapon-skill-box-panel"
	        (button
		        class "w3-btn"
		        id ,(string-append (sanitize-name skill-name) "-add")
		        "+")
	        (button
		        class "w3-btn"
		        id ,(string-append (sanitize-name skill-name) "-subtract")
		        "-"
		        )
        )
        (p ,dmg-val)
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
                id "char-navbar"
                (button
                  id "profile-btn"
                  class "w3-btn-block"
                  "Profile Overview")
                (button
                  id "stats-btn"
                  class "w3-btn-block"
                  "Stats Overview")
                (button
                  id "skills-btn"
                  class "w3-btn-block"
                  "Skills Overview"))
              (div
                class "w3-container"
                id "profile-info"
                (div
                  id "profile-panel")
                  (div
                    id "char-results"
                    (textarea 
                      id "charsheet"
                      class "w3-grey"
                      placeholder "Character Info goes here when you click 'Character Sheet'.
                      Or get the codes for your character by clicking on 'Get Codes'.")
                    (div
                      (button
                        id "Clear"
                        class "w3-btn"
                        "Clear") 
                      (button
                        id "Print"
                        class "w3-btn"
                        "Print"))
                  )
               )
               (div 
                 id "stats-overview"
                (div 
                  id "stats-panel"
                  )
                (div 
                  id "derived-stats-panel"
                  )
                (div
                  id "stats-container" 
                 )
               )
               (div
                  id "skills-overview"
                  class "w3-container"
                  style ""
                  (div 
                    id "skill-pt-panel")
                  (div 
                    id "skills-panel")
                  (div
                    id "weapon-skills-panel")
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
              id "profile-btn-panel"
            (button
              id "save-character"
              class "w3-btn"
              "Save Character")
             (button
              id "get-character-sheet"
              class "w3-btn"
              "Character Sheet")
             (button
              id "get-codes"
              class "w3-btn"
              "Get Codes"))) 
        'append)

(render "#skill-pt-panel"
        (element-new
          '(div
            class "w3-container"
            (p
              id "skill-pt-label"
            "Total Skill Points:  ")
             (input 
                type "text"
                id "skillpoints"
                class "w3-center w3-grey"
                value "000"
                disabled)))
        'append)

(render "#skill-pt-panel"
        (element-new
          '(div
            id "skill-pt-btns"
            class "w3-container"
            ;style "display:block"
            (button
              id "add-skill-btn"
              class "w3-btn"
              ;style "display:inline-block"
              "Add Skill")
            (button
              id "reset-skills-btn"
              class "w3-btn"
              ;style "display:inline-block"
              "Reset Skill Points")))
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

(-> ($ "#Brawl-add") 'click
  (js-closure
    (lambda ()
      (skill-point-callback "Brawl" 'add))))
      
(-> ($ "#Brawl-subtract") 'click
  (js-closure
    (lambda ()
      (skill-point-callback "Brawl" 'subtract))))

(render "#weapon-skills-panel" 
        (weapon-skill-box "Grapple" 25 "1d4+db") 
        'append)

(-> ($ "#Grapple-add") 'click
  (js-closure
    (lambda ()
      (skill-point-callback "Grapple" 'add))))

(-> ($ "#Grapple-subtract") 'click
  (js-closure
    (lambda ()
      (skill-point-callback "Grapple" 'subtract))))


(define add-skill-box
  (element-new 
    '(div
      class "add-skill-box"
      (p "Add Skill")
      (div
        (p "Skill Name: ")
        (input 
          type "text"
          class "w3-grey"
          id "new-skill-name"
          placeholder "Name"))
      (div 
        (p "Skill Base: ")
        (input
          type "text"
          class "w3-grey"
          id "new-skill-base"
          placeholder "Base %"))
      (div 
        (p "Weapon Skill?: ")
        (input
          class "w3-grey"
          type "checkbox"
          name "Weapon"
          id "is-weapon-skill"))
      (div 
        (p "Weapon Damage: ")
        (input 
          type "text"
          class "w3-grey"
          id "new-weapon-dmg"
          placeholder "Damage"))
      (div 
        (button
          class "w3-btn add-new-skill"
          "OK")
        (button
          class "w3-btn cancel-add-skill"
          "Cancel"))
      )))





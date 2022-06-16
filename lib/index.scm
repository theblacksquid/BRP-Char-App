(load "lib/utils.scm")
(load "lib/local-forage.scm")
(load "lib/skill-list.scm")
(load "lib/brp.scm")
(load "lib/views.scm")
(load "lib/controllers.scm")

(element-remove! ($ "#gear"))

(render "#app"
        (element-new app-template)
        'html)

(render "#profile-panel" 
        (element-new profile-panel-template) 
        'append)

(let loop ((ls profile-items))
	(if (null? ls)
		'()
		(begin
			(render "#profile-panel" (profile-elem (car ls)) 'append)
			(loop (cdr ls)))))

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


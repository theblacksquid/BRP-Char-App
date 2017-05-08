(define local-forage
  (js-eval "localforage"))

;; IMPORTANT NOTE
;; please ensure that proc is enclosed in a 
;; js-closure and has enough params to accomodate
;; the args that will be passed to it
;; Refer to localforage docs for more details.

(define set-item!
  (lambda (key val proc)
    (-> local-forage 'setItem key val proc)))

(define get-item
  (lambda (key proc)
    (-> local-forage 'getItem key proc)))



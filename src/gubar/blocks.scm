(define-module (gubar blocks)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module (ice-9 match)
  #:export (date-time
            simple-label))

(define* (date-time #:key (format "%c") (interval 60))
  "Creates a new date-time gublock."
  (make-gublock
   (scm->block '(("full_text" . *unspecified*)))
   interval
   (lambda (block)
     (scm->block
      `(("full_text" .
         ,(strftime format (localtime (current-time)))))))
   #f))

(define (simple-label text)
  (let ((initial-block-scm
         `(("full_text" . ,text)
           ("name" . "label")
           ("instance" . "make me random"))))
    (make-gublock
     (scm->block initial-block-scm) 'persistant #f
     (lambda (event block)
       (scm->block
        (assoc-set! initial-block-scm "background"
                    (match (block-background block)
                      ("#FF0000" "")
                      (_ "#FF0000"))))))))

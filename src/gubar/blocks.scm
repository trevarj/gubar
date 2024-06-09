(define-module (gubar blocks)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
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
  (make-gublock
   (scm->block `(("full_text" . ,text)
                 ("name" . "label")
                 ("instance" . "make me random")))
   'persistant
   #f
   (lambda (event block)
     ;; this is dumb
     (scm->block `(("full_text" . "clicked")
                   ("name" . "label")
                   ("instance" . "make me random"))))))

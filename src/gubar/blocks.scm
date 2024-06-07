(define-module (gubar blocks)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:export (date-time))

(define* (date-time #:key (format "%c") (interval 60))
  "Creates a new date-time gublock."
  (make-gublock
   (scm->block '(("full_text" . *unspecified*)))
   interval
   (lambda (block)
     (scm->block
      `(("full_text" .
         ,(strftime format (localtime (current-time)))))))))

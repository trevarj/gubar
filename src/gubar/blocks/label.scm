(define-module (gubar blocks label)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:export (label))

(define* (label text #:key color)
  "Creates a label that turns urgent on click."
  (let ((initial-block-scm
         `(("full_text" . ,text)
           ("name" . "label")
           ("instance" . "make me random")
           ("color" . ,color))))
    (gublock
     #:block initial-block-scm
     #:click-handler
     (lambda (event block)
       (scm->block
        (assoc-set! initial-block-scm
                    "urgent"
                    (not (block-urgent block))))))))

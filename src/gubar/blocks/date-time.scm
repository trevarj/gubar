(define-module (gubar blocks date-time)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module (ice-9 textual-ports)
  #:export (date-time))

(define* (date-time #:key (format "%c") (interval 1))
  "Creates a new date-time gublock."
  (gublock
   #:interval interval
   #:procedure
   (lambda (block)
     (set-block-full-text!
      block
      (strftime format (localtime (current-time))))
     block)))

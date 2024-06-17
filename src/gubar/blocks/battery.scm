(define-module (gubar blocks battery)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module ((ice-9 format) #:prefix fmt:)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (battery))

(define* (battery #:key (format "~a ~a%") (nerd-icons #f))
  (let ((bat "/sys/class/power_supply/BAT0/capacity")
        (ac "/sys/class/power_supply/AC/online")
        (icons '(󰁺 󰁻 󰁼 󰁽 󰁾 󰁿 󰂀 󰂁 󰂂 󰁹)))
    (gublock
     #:block '(("name" . "battery") ("full_text" . "N/A"))
     #:interval 3
     #:procedure
     (lambda (block)
       (let* ((level (string->number (get-line (open-input-file bat))))
              (ac (string->number (get-line (open-input-file ac))))
              (label
               (cond
                ((= ac 1) (if nerd-icons "󰂄" "CHRG:"))
                (nerd-icons (list-ref icons (truncate-quotient level 10)))
                (else "BAT:")))
              (block-alist (block->scm block)))
         (scm->block
          (fold
           (lambda (pair alist) (assoc-set! alist (car pair) (cdr pair)))
           block-alist `(("full_text" . ,(fmt:format #f format label level))
                         ("urgent" . ,(<= level 10))))))))))

(define-module (gubar blocks battery)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module ((ice-9 format) #:prefix fmt:)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (battery))

(define power-supply-path "/sys/class/power_supply/")

(define* (find-power-supply prefix #:optional default-dev)
  (let ((bat (scandir power-supply-path
                      (lambda (f) (string-match prefix f)))))
    (format #f "~a~a" power-supply-path
            (or (and (pair? bat) (car bat))
                (format #f "~a~a" prefix default-dev)))))

(define* (battery #:key (format "~a ~a%") (nerd-icons #f)
                  (bat #f) (ac #f))
  (let ((bat
         (string-append (or bat (find-power-supply "BAT" 0)) "/capacity"))
        (ac
         (string-append (or ac (find-power-supply "AC")) "/online"))
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
         (set-block-full-text! block (fmt:format #f format label level))
         (set-block-urgent! block (<= level 10))
         block)))))

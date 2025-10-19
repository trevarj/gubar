(define-module (gubar blocks brightness)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module ((ice-9 format) #:prefix fmt:)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (brightness))


(define brightness-icons '(󰃠 󰃝 󰃟 󰃞))

(define (brightness->icon level)
  (list-ref brightness-icons (truncate-quotient level 26)))

(define (get-brightness-status)
  (let* ((current-input
          (open-input-pipe
           "brightnessctl get"))
         (current-level (get-line current-input))
         (max-input
          (open-input-pipe
           "brightnessctl max"))
         (max (get-line max-input))
         (percentage
          (truncate-quotient (* 100 (/ (string->number current-level)
                                       (string->number max))) 1)))
    (close-pipe current-input)
    (close-pipe max-input)
    (list percentage (brightness->icon percentage))))

(define* (brightness #:key (format "~a ~a%") (nerd-icons #f) (interval 0.1))
  (gublock
   #:block '(("name" . "brightness") ("full_text" . "N/A"))
   #:interval interval
   #:procedure
   (lambda (block)
     (let* ((status (get-brightness-status))
            (level (car status))
            (label (if nerd-icons (cadr status) "BRT:"))
            (block-alist (block->scm block)))
       (set-block-full-text! block (fmt:format #f format label level))
       block))))

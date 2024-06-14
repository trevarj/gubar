(define-module (gubar blocks)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 format) #:select ((format . str-format)))
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (gublock
            date-time
            battery
            simple-label))

(define* (gublock #:key
                  (block '())
                  (interval 'persistent)
                  (procedure (lambda (block) block))
                  (click-handler #f)
                  (signal #f))
  (make-gublock
   (scm->block block) interval procedure click-handler signal))

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
           block-alist `(("full_text" . ,(str-format #f format label level))
                         ("urgent" . ,(<= level 10))))))))))

(define* (date-time #:key (format "%c") (interval 1))
  "Creates a new date-time gublock."
  (gublock
   #:interval interval
   #:procedure
   (lambda (block)
     (scm->block
      `(("full_text" .
         ,(strftime format (localtime (current-time)))))))))

;; (define (volume-pulseaudio)
;;   (gublock
;;    #:interval 2
;;    #:procedure
;;    (lambda (block)
;;      (let* ((vol-port (open-input-pipe
;;                        "pactl list sinks | tr ' ' '\n' | grep -m1 '%'"))
;;             (vol (get-line vol-port)))
;;        (close-pipe vol-port)
;;        (scm->block (assoc-set! (block->scm block) "full_text" vol))))))

(define* (simple-label text #:key color)
  "Creates a label that turns red on click."
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

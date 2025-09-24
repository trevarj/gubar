(define-module (gubar gublock)
  #:use-module (fibers)
  #:use-module (fibers operations)
  #:use-module (fibers channels)
  #:use-module ((fibers timers) #:select ((sleep . fsleep)))
  #:use-module (gubar swaybar-protocol)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:export (<gublock>
            make-gublock
            gublock
            gublock?
            gublock-block
            gublock-interval
            gublock-procedure
            set-gublock-block!
            set-gublock-procedure!
            gublock-run
            gublock-handle-click))

(define-record-type <gublock>
  (make-gublock block interval procedure click-handler signal)
  gublock?
  ;; Swaybar-protocol "body" object
  (block gublock-block set-gublock-block!)
  ;; Amount of seconds to wait until updating
  (interval gublock-interval)
  ;; A lambda that takes the gublock's block and returns a new block
  (procedure gublock-procedure set-gublock-procedure!)
  ;; A lambda that takes (click-event, block) and returns a new block
  (click-handler gublock-click-handler)
  ;; The number of SIGRTMIN+N to register on and perform an update upon
  (signal gublock-signal))

(define* (gublock #:key
                  (block '())
                  (interval 'persistent)
                  (procedure (lambda (block) block))
                  (click-handler #f)
                  (signal #f))
  "Helper to define a custom gublock. The initial block can be defined using an
assoc list."
  (make-gublock
   (scm->block block) interval procedure click-handler signal))

(define (do-procedure gublock update-chan)
  (let ((procedure (gublock-procedure gublock))
        (block (gublock-block gublock)))
    (when (procedure? procedure)
      (set-gublock-block! gublock (procedure block))
      (put-message update-chan #t))))

(define (gublock-handle-click gublock event update-chan)
  (let ((handler (gublock-click-handler gublock))
        (block (gublock-block gublock)))
    (unless (equal? handler #f)
      (set-gublock-block! gublock (handler event block))
      (put-message update-chan #t))))

(define (gublock-run gublock update-chan)
  ;; Register on signal
  (let ((signal (gublock-signal gublock)))
    (when signal
      (sigaction (+ signal SIGRTMIN)
        (lambda (_) (do-procedure gublock update-chan)))))
  ;; First run
  (do-procedure gublock update-chan)
  (unless (equal? 'persistent (gublock-interval gublock))
    (let loop ()
      (fsleep (gublock-interval gublock))
      (do-procedure gublock update-chan)
      (loop))))

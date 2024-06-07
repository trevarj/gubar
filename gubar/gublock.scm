(define-module (gubar gublock)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module ((fibers timers)
                #:select ((sleep . fsleep)))
  #:use-module (srfi srfi-9)
  #:export (<gublock>
            make-gublock
            gublock?
            gublock-block
            set-gublock-block!
            gublock-interval
            gublock-procedure
            set-gublock-procedure!
            gublock-run))
                
(define-record-type <gublock>
  (make-gublock block interval procedure)
  gublock?
  ;; Swaybar-protocol "body" object
  (block gublock-block set-gublock-block!)
  ;; Amount of seconds to wait until updating
  (interval gublock-interval)
  ;; A lambda that takes the gublock's block and outputs a new block
  (procedure gublock-procedure set-gublock-procedure!))

(define (gublock-run gublock channel)
  (let loop ()
    (set-gublock-block! gublock ((gublock-procedure gublock)
                                 (gublock-block gublock)))
    (put-message channel #t)
    (fsleep (gublock-interval gublock))
    (loop)))

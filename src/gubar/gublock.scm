(define-module (gubar gublock)
  #:use-module (fibers)
  #:use-module (fibers operations)  
  #:use-module (fibers channels)
  #:use-module (fibers timers)
  #:use-module (gubar swaybar-protocol)
  #:use-module (ice-9 match)
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
  (make-gublock block interval procedure click-handler)
  gublock?
  ;; Swaybar-protocol "body" object
  (block gublock-block set-gublock-block!)
  ;; Amount of seconds to wait until updating
  (interval gublock-interval)
  ;; A lambda that takes the gublock's block and returns a new block
  (procedure gublock-procedure set-gublock-procedure!)
  ;; A lambda that takes (click-event, block) and returns a new block
  (click-handler gublock-click-handler))

(define (gublock-run gublock update-chan click-chan)
  (let loop ()
    (match-let* ((($ <gublock> block interval procedure click-handler) gublock)
                 (sleep-op (sleep-operation interval))
                 (new-block (procedure block)))
      (set-gublock-block! gublock new-block)
      (put-message update-chan #t)
      (perform-operation sleep-op))
      ;; (perform-operation
      ;;  (choice-operation
      ;;   sleep-op
      ;;   (get-operation click-chan))))
      ;; (let ((result (perform-operation
      ;;                (choice-operation
      ;;                 (sleep-operation interval)
      ;;                 (get-operation click-chan)))))
      ;;   (when (and (click-event? result)
      ;;              (eqv? (block-name block)
      ;;                    (click-event-name result)))
      ;;     (click-handler result block))))
    (loop)))

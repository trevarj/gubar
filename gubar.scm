(use-modules (fibers)
             (fibers channels)
             ((fibers timers)
              #:select ((sleep . fsleep)))
             (gubar swaybar-protocol)
             (ice-9 format)
             (ice-9 match)
             (srfi srfi-9))

(define-record-type <gublock>
  (make-gublock block interval procedure)
  gublock?
  (block gublock-block set-gublock-block!)
  (interval gublock-interval)
  (procedure gublock-procedure set-gublock-procedure!))

(define (gublock-run gublock channel)
  (let loop ()
    ;; TODO: check result of procedure first and log error?
    (set-gublock-block! gublock (gublock-procedure gublock))
    (put-message channel #t)
    (fsleep (gublock-interval gublock))
    (loop)))

(define (parse-config)
  '())

;; TODO:
;; Have the bar register a simple "block" (module)
;; The main loop will wait on a channel receiver for any of the blocks to wake
;; up and write on the other end of the channel.
(define (run-gbar)
  ;; Create update channel for all blocks to send update notifications to
  (let ((ch (make-channel))
        (gublocks (parse-config)))
    ;; The swaybar-protocol requires the header to be written to stdout followed by
    ;; a new line and then the opening bracket '[' of an infinite json array.
    ;; After that we will start dumping out our gubar blocks inner block items
    ;; that are the "body" object described in the swaybar-protocol.
    (format #t "~a\n" (header->json header-without-clicks))
    (format #t "[[],\n")

    ;; Initialize and start mods with channel
    (for-each (lambda (gb)
                (spawn-fiber
                 (lambda () (gublock-run gb ch))
                 #:parallel? #t))
              gublocks)

    ;; TODO: Spawn stdin listener for click events

    ;; The update listener will write the current blocks to stdout
    ;; Check if last update time is less than 1 second ago to not update too
    ;; often
    (let ((update (get-message ch)))
      ;; Wrap all blocks in an array
      (format #t "~a,\n"
              (scm->json
               (list->vector
                (map-in-order
                 (lambda (gb)
                   (block->scm (gublock-block gb)))
                 gublocks))))
      (pk update))))

(define (main)
  (run-fibers run-gbar))

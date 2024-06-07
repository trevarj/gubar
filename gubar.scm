#!/usr/bin/env -S guile -e main -s
!#
(use-modules (fibers)
             (fibers channels)
             (gubar blocks)
             (gubar gublock)
             (gubar swaybar-protocol)
             (ice-9 format)
             (ice-9 match)
             (json))

;; TODO: Actually parse this from ~/.config/gubar/config.scm
(define (parse-config)
  (list (date-time #:format "%b %e %Y %l:%M %p" #:interval 60)
        (date-time #:interval 1)))

(define (update-listener gublocks channel)
  "Listens on the channel for updates sent from any of the block fibers. When an
update is received, output all gublock inner swaybar-protocol blocks to stdout
in json format."
  (let loop ((update '()))
    ;; All blocks should be wrapped in an array, per protocol spec
    (format #t "~a,\n"
            (scm->json-string
             (list->vector
              (map-in-order
               (lambda (gb)
                 (block->scm (gublock-block gb)))
               gublocks))))
    (loop (get-message channel))))

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
     
    (update-listener gublocks ch)))

(define (main args)
  (run-fibers run-gbar))

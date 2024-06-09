#!/usr/bin/env -S guile -e '((@ (gubar) main))' -s
!#

(define-module (gubar)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers io-wakeup)
  #:use-module (gubar blocks)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:export (main))

;; TODO: Actually parse this from ~/.config/gubar/config.scm
(define (parse-config)
  (list (date-time #:interval 1)
        (simple-label "unclicked")))

(define (update-listener gublocks ch)
  "Listens on the channel for updates sent from any of the block fibers. When an
update is received, output all gublock inner swaybar-protocol blocks to stdout
in json format."
  (let loop ()
    (let ((_ (get-message ch)))
      ;; All blocks should be wrapped in an array, per protocol spec
      (format #t "~a,\n"
              (scm->json-string
               (list->vector
                (map-in-order
                 (lambda (gb)
                   (block->scm (gublock-block gb)))
                 gublocks))))
      (flush-all-ports))
    (loop)))

(define (click-listener gublocks)
  "Listens on standard input for click events reported by swaybar, then parses
the <click-event> and sends it to the click handler channel."
  (let* ((stdin (current-input-port))
         (flags (fcntl stdin F_GETFL)))
    ;; Read first '[' of infinite array
    (get-line stdin)

    ;; Non-blocking I/O for stdin so we can do other stuff when waiting for clicks
    (fcntl stdin F_SETFL (logior O_NONBLOCK flags))

    (let loop ()
      ;; TODO: Truncate ',' before json string
      (let* ((event (json->click-event (get-line stdin))))
        (let ((found (find (lambda (g)
                             (equal? (block-name (gublock-block g))
                                     (click-event-name event)))
                           gublocks)))
          (unless (eq? #f found)
            (gublock-handle-click found event))))
      (loop))))

(define (run-gubar)
  (let ((update-chan (make-channel))
        (gublocks (parse-config)))

    ;; The swaybar-protocol requires the header to be written to stdout followed by
    ;; a new line and then the opening bracket '[' of an infinite json array.
    ;; After that we will start dumping out our gubar blocks inner block items
    ;; that are the "body" object described in the swaybar-protocol.
    (format #t "~a\n[[]," (header->json header-with-clicks))
    (flush-all-ports)

    ;; Initialize and start mods with channel
    (for-each (lambda (gb)
                (spawn-fiber
                 (lambda () (gublock-run gb update-chan))
                 #:parallel? #t))
              gublocks)

    ;; Fiber for click handling
    (spawn-fiber
     (lambda () (click-listener gublocks))
     #:parallel? #t)

    ;; Start listening for updates from gublocks
    (update-listener gublocks update-chan)))

(define (main)
  (run-fibers run-gubar))

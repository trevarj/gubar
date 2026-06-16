(define-module (gubar blocks date-time)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module (ice-9 textual-ports)
  #:use-module (fibers)
  #:use-module ((fibers timers) #:select ((sleep . fsleep)))
  #:export (date-time))

(define (make-second-boundary-port)
  "Return an input port that emits a newline on each second boundary."
  (let* ((ports (pipe))
         (read-port  (car ports))
         (write-port (cdr ports)))
    (spawn-fiber
     (lambda ()
       (let loop ()
         (let* ((now (gettimeofday))
                (remaining (- 1.0 (/ (cdr now) 1000000.0))))
           (fsleep remaining)
           (write-char #\newline write-port)
           (force-output write-port)
           (loop))))
     #:parallel? #t)
    read-port))

(define* (date-time #:key (format "%c") (interval 1))
  "Display the current date and time.
Pass #:interval 'persistent for event-driven per-second updates,
or #:interval N for polling every N seconds (default: 1)."
  (if (eqv? interval 'persistent)
      (gublock
       #:interval interval
       #:event-source make-second-boundary-port
       #:procedure
       (lambda (block)
         (set-block-full-text!
          block
          (strftime format (localtime (current-time))))
         block))
      ;; else revert to previous interval-based specification
      (gublock
       #:interval interval
       #:procedure
       (lambda (block)
         (set-block-full-text!
          block
          (strftime format (localtime (current-time))))
         block))))

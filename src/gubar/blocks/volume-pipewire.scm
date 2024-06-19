(define-module (gubar blocks volume-pipewire)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:export (volume-pipewire))

(define-json-type <volume-channel>
  (value-percent "value_percent"))

(define-json-type <volume>
  (front-left "front-left" <volume-channel>)
  (front-right "front-right" <volume-channel>))

(define-json-type <sink>
  (name)
  (volume "volume" <volume>)
  (mute))

(define icons '(󰕿 󰖀 󰕾))

(define (volume->icon volume)
  (list-ref icons (truncate-quotient volume 34)))

(define (get-default-sink)
  (let* ((sinks-pipe (open-input-pipe "pactl -f json list sinks"))
         (default-sink-pipe (open-input-pipe "pactl get-default-sink"))
         (sinks (json->scm sinks-pipe))
         (default-sink (get-line default-sink-pipe)))
    (close-pipe sinks-pipe)
    (close-pipe default-sink-pipe)
    (find (lambda (s) (equal? (sink-name s) default-sink))
          (map scm->sink (array->list sinks)))))

(define (display-sink sink)
  (when sink
    (let* ((volume (string-trim-right (volume-channel-value-percent
                                       (volume-front-left (sink-volume sink)))
                                      #\%))
           (icon (if sink-mute "󰖁" (volume->icon volume))))
      (format #f "~a ~a" icon volume))))

(define* (volume-pipewire #:key (signal 2))
  (gublock
   #:interval 'persistent
   #:signal signal
   #:procedure
   (lambda (block)
     (scm->block
      (assoc-set! (block->scm block)
                  "full_text"
                  (display-sink (get-default-sink)))))))

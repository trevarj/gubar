(define-module (gubar blocks network-manager-wifi)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:export (network-manager-wifi))

(define icons '(󰤟 󰤢 󰤥 󰤨))

(define (parse-line line)
  (if (string? line)
      (filter (compose not string-null?)
              (string-split line #\:))
      '()))

(define (signal->icon signal)
  (list-ref icons (truncate-quotient signal 26)))

(define (get-wifi-status)
  (let ((input
         (open-input-pipe
          "nmcli -t -f SSID,IN-USE,SIGNAL device wifi list --rescan auto")))
    (let has-wifi? ((line (get-line input)))
      (match (parse-line line)
        (() (close-pipe input) #f)
        ((ssid "*" signal) (list ssid (signal->icon (string->number signal))))
        (_ (has-wifi? (get-line input)))))))

(define* (network-manager-wifi
          #:key
          (ssid #f))
  (gublock
   #:interval 10
   #:procedure
   (lambda (block)
     (scm->block
      (assoc-set!
       (block->scm block)
       "full_text" (match (get-wifi-status)
                     (#f "󰤭")
                     ((_ssid signal)
                      (if ssid
                          (format #f "~a (~a)" signal _ssid)
                          signal))))))))

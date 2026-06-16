(define-module (gubar blocks network-manager)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:export (network-manager))

(define wifi-icons '(󰤟 󰤢 󰤥 󰤨))
(define ethernet-icon "󰈀")
(define disconnected-icon "󰤭")

(define (parse-line line)
  "Split LINE on ':' and filter empty strings.
Returns an empty list if LINE is not a string."
  (if (string? line)
      (filter (compose not string-null?)
              (string-split line #\:))
      '()))

(define (signal->icon signal)
  "Map a wifi SIGNAL strength integer (0-100) to a nerd font icon.
Divides the range into four bands of 26, selecting from wifi-icons."
  (list-ref wifi-icons (truncate-quotient signal 26)))

(define (get-ethernet-status)
  "Return #t if an ethernet device is currently connected, #f otherwise.
Queries nmcli for device type and state."
  (let ((input (open-input-pipe
                "nmcli -t -f TYPE,STATE device status")))
    (let loop ((line (get-line input)))
      (match (parse-line line)
        (() (close-pipe input) #f)
        (("ethernet" "connected" . _)
         (close-pipe input) #t)
        (_ (loop (get-line input)))))))

(define (get-wifi-status)
  "Return the active wifi connection as (ssid icon) or #f if not connected.
Queries nmcli for the in-use wifi device, returning its SSID and
a signal strength icon."
  (let ((input (open-input-pipe
                "nmcli -t -f SSID,IN-USE,SIGNAL device wifi list")))
    (let loop ((line (get-line input)))
      (match (parse-line line)
        (() (close-pipe input) #f)
        ((ssid "*" signal)
         (close-pipe input)
         (list ssid (signal->icon (string->number signal))))
        (_ (loop (get-line input)))))))

(define (get-status)
  "Return the current network status as a tagged value.
Prefers wifi over ethernet. Returns one of:
  (wifi ssid icon) — connected to wifi
  ethernet         — connected via ethernet only
  disconnected     — no active connection"
  (cond
   ((get-wifi-status)     => (lambda (s) (cons 'wifi s)))
   ((get-ethernet-status) 'ethernet)
   (else                  'disconnected)))

(define (status->text status ssid?)
  "Format STATUS as a display string for the swaybar block.
If SSID? is #t, appends the SSID to the wifi signal icon."
  (match status
    ('ethernet ethernet-icon)
    (('wifi ssid signal)
     (if ssid?
         (format #f "~a (~a)" signal ssid)
         signal))
    ('disconnected disconnected-icon)))

(define* (network-manager #:key (ssid #f))
  "Display current network connection status.
Shows ethernet icon when connected via ethernet, wifi signal
strength icon when on wifi, or disconnected icon otherwise.
Optional #:ssid #t displays the SSID alongside the signal icon."
  (gublock
   #:block '(("name" . "network-manager") ("full_text" . "…"))
   #:interval 'persistent
   #:event-source (lambda () (open-input-pipe "nmcli monitor"))
   #:procedure
   (lambda (block)
     (set-block-full-text!
      block
      (status->text (get-status) ssid))
     block)))

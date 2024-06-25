;; Example binding within ~/.config/sway/config:
;; bindsym --release $mod+Control_R exec swaymsg input type:keyboard \
;;                                       xkb_switch_layout next && \
;;                                       pkill -SIGRTMIN+3 -f -n gubar.scm

(define-module (gubar blocks xkb-layout)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:export (xkb-layout))

(define-json-type <current-layout>
  (name "xkb_active_layout_name")
  (index "xkb_active_layout_index"))

(define (apply-alias alias-pairs device)
  "Returns the alias for the current layout, or just xkb-active-layout-name."
  (or (assoc-ref alias-pairs (current-layout-name device))
      (assoc-ref alias-pairs (current-layout-index device))
      (current-layout-name device)))

(define (current-layout)
  (let* ((pipe (open-input-pipe "swaymsg -rt get_inputs"))
         (inputs (map scm->current-layout (array->list (json->scm pipe)))))
    (close-pipe pipe)
    (find (lambda (dev) (current-layout-index dev)) inputs)))

(define* (xkb-layout #:key (signal 3) (alias-pairs '()))
  "Creates a module that displays the current xkb layout."
  (gublock
   #:signal signal
   #:procedure
   (lambda (block)
     (set-block-full-text! block (apply-alias alias-pairs (current-layout)))
     block)))

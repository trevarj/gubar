(define-module (gubar swaybar-protocol)
  #:use-module (json record)
  #:export (make-block
            block->json
            block->scm
            make-header
            header-with-clicks
            header-without-clicks
            header->json
            header->scm
            json->click-event))

(define-json-type <header>
  (click-events "click_events")
  (cont-signal "cont_signal")
  (stop-signal "stop_signal")
  (version))

(define header-without-clicks
  (make-header #f *unspecified* *unspecified* 1))
(define header-with-clicks
  (make-header #t *unspecified* *unspecified* 1))

(define-json-type <block>
  (align)
  (background)
  (border)
  (border-bottom "border_bottom")
  (border-left "border_left")
  (border-right "border_right")
  (border-top "border_top")
  (color)
  (full-text "full_text")
  (instance)
  (markup)
  (min-width "min_width")
  (name)
  (separator)
  (separator-block-width "separator_block_width")
  (short-text "short_text")
  (urgent))

(define-json-type <click-event>
  (button)
  (event)
  (height)
  (instance)
  (name)
  (relative-x "relative_x")
  (relative-y "relative_y")
  (width)
  (x)
  (y))

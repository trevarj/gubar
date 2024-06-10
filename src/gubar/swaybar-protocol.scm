(define-module (gubar swaybar-protocol)
  #:use-module (json record)
  #:export (<block>
            make-block
            block->json
            block->scm
            scm->block
            block-name
            block-instance
            block-full-text
            block-short-text
            block-color
            block-background
            block-align
            block-separator
            block-separator-block-width
            block-min-width
            block-border
            block-border-top
            block-border-bottom
            block-border-left
            block-border-right
            block-urgent
            block-markup
            <header>
            make-header
            header-with-clicks
            header->json
            header->scm
            <click-event>
            json->click-event
            click-event?
            click-event-button
            click-event-name
            click-event-instance
            click-event-x
            click-event-relative-x
            click-event-y
            click-event-relative-y
            click-event-event
            click-event-width
            click-event-height))

(define-json-type <header>
  ;; The protocol version to use. Currently, this must be 1
  (version)
  ;; Whether to receive click event information to standard input
  (click-events "click_events")
  ;; The signal that swaybar should send to continue processing
  (cont-signal "cont_signal")
  ;; The signal that swaybar should send to stop processing
  (stop-signal "stop_signal"))

(define header-with-clicks
  (make-header 1 #t *unspecified* *unspecified*))

(define-json-type <block>
  ;; If the text does not span the full width of the block, this specifies how
  ;; the text should be aligned inside of the block. This can be left (default),
  ;; right, or center.
  (align)
  ;; The background color for the block in #RRGGBBAA or #RRGGBB notation
  (background)
  ;; The border color for the block in #RRGGBBAA or #RRGGBB notation
  (border)
  ;; The height in pixels of the bottom border. The default is 1
  (border-bottom "border_bottom")
  ;; The width in pixels of the left border. The default is 1
  (border-left "border_left")
  ;; The width in pixels of the right border. The default is 1
  (border-right "border_right")
  ;; The height in pixels of the top border. The default is 1
  (border-top "border_top")
  ;; The text color to use in #RRGGBBAA or #RRGGBB notation
  (color)
  ;; The text that will be displayed.
  (full-text "full_text")
  ;; The instance of the name for the block. This is only used to identify the
  ;; block for click events. If set, each block should have a unique name and
  ;; instance pair.
  (instance)
  ;; The type of markup to use when parsing the text for the block. This can
  ;; either be pango or none (default).
  (markup)
  ;; The minimum width to use for the block. This can either be given in pixels
  ;; or a string can be given to allow for it to be calculated based on the
  ;; width of the string.
  (min-width "min_width")
  ;; A name for the block. This is only used to identify the block for click
  ;; events. If set, each block should have a unique name and instance pair.
  (name)
  ;; Whether the bar separator should be drawn after the block. See sway-bar(5)
  ;; for more information on how to set the separator text.
  (separator)
  ;; The amount of pixels to leave blank after the block. The separator text
  ;; will be displayed centered in this gap. The default is 9 pixels.
  (separator-block-width "separator_block_width")
  ;; If given and the text needs to be shortened due to space, this will be
  ;; displayed instead of full_text
  (short-text "short_text")
  ;; Whether the block should be displayed as urgent. Currently swaybar utilizes
  ;; the colors set in the sway config for urgent workspace buttons. See
  ;; sway-bar(5) for more information on bar color configuration.
  (urgent))

(define-json-type <click-event>
  ;; The x11 button number for the click. If the button does not have an x11
  ;; button mapping, this will be 0.
  (button)
  ;; The name of the block, if set
  (name)
  ;; The event code that corresponds to the button for the click
  (event)
  ;; The height of the block in pixels
  (height)
  ;; The instance of the block, if set
  (instance)
  ;; The x location of the click relative to the top-left of the block
  (relative-x "relative_x")
  ;; The y location of the click relative to the top-left of the block
  (relative-y "relative_y")
  ;; The width of the block in pixels
  (width)
  ;; The x location that the click occurred at
  (x)
  ;; The y location that the click occurred at
  (y))

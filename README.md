# gubar

A hackable Swaybar generator, written and configured with Guile Scheme.

## Try it using Guix shell

```sh
cd gubar/
guix shell
autoreconf -vif && ./configure
./pre-inst-env gubar
```

## Installing

### Guix

Gubar is available in the main Guix channel.

```sh
guix package -i gubar
```

### With manifest

The repository includes the package manifest for manual installation with Guix,
useful for hacking on gubar.

```sh
cd gubar/
guix package -f guix.scm
```

### Dependencies
- Guile 3
- [guile-fibers](https://github.com/wingo/fibers/)
- [guile-json](https://github.com/aconchillo/guile-json)

## Integrating with swaybar
Once gubar is installed, add the following to your Sway config:
```
bar {
    ...
    status_command gubar
    ...
```

## Configuration
Gubar will look for a config file in `~/.config/gubar/config.scm` before falling
back to the default config, which is just the system time.

The `config.scm` file must return a list of gublocks in the order in which you
want them displayed, from left to right. You may use the bundled blocks in
(`blocks/`)[./src/gubar/blocks/] or create your own using the helper procedure
`gublocks` that exists in the `gubar gublock` module.

### `gublock`
A `gublock` is the fundamental building block of `Gubar`.

| :warning: **Subject to Change**                                 |
|-----------------------------------------------------------------|
| The `gublock` interface is unstable and may change at any point |

```scheme
(gublock 
    #:block [block] 
    #:interval [interval]
    #:procedure [procedure]
    #:click-handler [click-handler]
    #:signal [signal]
    #:event-source [event-source])
```
The effects of the keys are as follows:

| Key           | Description                                                                                                | Example                                         |
|---------------|------------------------------------------------------------------------------------------------------------|-------------------------------------------------|
| block         | Initial block represented as an assoc list.                                                                | `'(("full_text" . "foo"))`                      |
| interval      | Time in seconds in which this block updates or 'persistent for event-driven blocks                         | seconds or `'persistent`                        |
| procedure     | The main procedure that will be run after `interval`. Receives a `<block>` and returns a `<block>` record. | `(lambda (block) block)`                        |
| click-handler | The procedure that is run when this block is clicked. The block must have its name field set.              | `(lambda (event block) (block))`                |
| signal        | SIGRTMIN offset to listen on. Trigger with `pkill -SIGRTMIN+signal gubar`.                                 | `2`                                             |
| event-source  | A thunk returning an input port to monitor events.                                                         | `(lambda () (open-input-pipe "nmcli monitor"))` |



#### Examples

All keys are optional, so the most basic block with no text at all is
`(gublock)`. Here's a block that displays the number of seconds it has been active:

```scheme
(gublock
 #:interval 1 ; update each second
 #:procedure
 (let ((counter 0))
   (lambda (block)
     (set! counter (+ counter 1))
     (set-block-full-text! block (number->string counter))
     block)))
```

And here's a block that shows a random number every time you click it:

```scheme
(gublock
 #:block (("full_text" . "Click me!")
          ("name" . "randomizer"))
 #:click-handler
 (lambda (_ block)
   (set-block-full-text! block (number->string (random 1000000)))
   block))
```

### Built-in Blocks

| Block                | Declaration                                                       | Info                                                                                 |
|----------------------|-------------------------------------------------------------------|--------------------------------------------------------------------------------------|
| battery              | `(battery #:key (format "~a ~a%") (nerd-icons #f))`               | Reads `/sys/class/power_supply/AC/online` and `BAT0/capacity`                        |
| date-time            | `(date-time #:key (format "%c") (interval 1))`                    | Pass `'persistent` for event-driven per-second updates                               |
| label                | `(label text #:key color)`                                        | `color` is an RGB string `"#RRGGBB"`                                                 |
| network-manager      | `(network-manager #:key (ssid #f))`                               | Detects ethernet and wifi via `nmcli monitor`                                        |
| volume               | `(volume-pipewire #:key (signal 2))`                              | Uses `pactl -f json list sinks` and `pactl get-default-sink`                         |
| brightness           | `(brightness #:key (format "~a ~a%") (nerd-icons #f) (signal 4))` | Uses `brightnessctl`. Triggered via `SIGRTMIN+4`                                     |
| xkb-layout           | `(xkb-layout #:key (signal 3) (alias-pairs '()))`                 | Uses `swaymsg -rt get_inputs`. `alias-pairs` maps layout index to display alias      |

### Example Config

```scheme
(use-modules (gubar gublock)
             (gubar swaybar-protocol)
             (gubar blocks label)
             (gubar blocks xkb-layout)
             (gubar blocks network-manager)
             (gubar blocks brightness)
             (gubar blocks volume-pipewire)
             (gubar blocks battery)
             (gubar blocks date-time))

(list
 (label "some text")
 (xkb-layout #:alias-pairs '((0 . "lang1") (1 . "lang2")))
 (network-manager #:ssid #t)
 (brightness #:nerd-icons #t)
 (volume-pipewire)
 (battery #:nerd-icons #t)
 (date-time #:format "%a %b %d %Y %-I:%M %p" #:interval 'persistent))
```
 
#### Corresponding swayconfig
For the modules that require signals:
 
```
# Volume control
bindsym --locked  XF86AudioMute exec pactl set-sink-mute @DEFAULT_SINK@ toggle
bindsym --release XF86AudioMute exec pkill -SIGRTMIN+2 gubar
bindsym --locked  XF86AudioLowerVolume exec pactl set-sink-volume @DEFAULT_SINK@ -5%
bindsym --release XF86AudioLowerVolume exec pkill -SIGRTMIN+2 gubar
bindsym --locked  XF86AudioRaiseVolume exec pactl set-sink-volume @DEFAULT_SINK@ +5%
bindsym --release XF86AudioRaiseVolume exec pkill -SIGRTMIN+2 gubar

# Brightness control
bindsym --locked  XF86MonBrightnessDown exec brightnessctl set 5%-
bindsym --release XF86MonBrightnessDown exec pkill -SIGRTMIN+4 gubar
bindsym --locked  XF86MonBrightnessUp   exec brightnessctl set 5%+
bindsym --release XF86MonBrightnessUp   exec pkill -SIGRTMIN+4 gubar

# Switch layout
bindsym --release $mod+Control_R exec swaymsg input type:keyboard \
                                      xkb_switch_layout next && \
                                      pkill -SIGRTMIN+3 gubar
```

Bar declaration:

```
bar {
    ...
    status_command gubar
    ...
```

## Contributing

This project uses [Guile Hall](https://gitlab.com/a-sassmannshausen/guile-hall)
to generate the files necessary for the GNU Build System.
If you are contributing new source files, e.g., new gublock definitions,
the files must be added to `hall.scm` in the `(files (libraries ...` section
and then regenerated as follows.

```
# Rebuild GNU Build System files
hall build -xf
```

## TODO
- [ ] Make configuration more ergonomic with some kind of syntax for defining
      blocks, instead of having to use assoc lists.
- [ ] Try to use channels to send click updates to a block instead of finding
      the block and updating manually (maybe)
- [ ] Better error handling or assertions
- [ ] Fix guix.scm to install with gnu-build-system so that we can install
      the script as a program with default configs, docs, etc.

# gubar

A hackable Swaybar generator, written and configured with Guile Scheme.

## Try it using Guix shell

```sh
cd gubar/
guix shell guile -f guix.scm -- guile -e "((@ (gubar) main))" 
```

## Installing

### Dependencies
- Guile 3
- [guile-fibers](https://github.com/wingo/fibers/)
- [guile-json](https://github.com/aconchillo/guile-json)

### Guix

This will install gubar and the require dependencies into your Guix profile:
```sh
cd gubar/
guix package -f guix.scm
```

Then you can run using your own profile:

```sh
guile -e "((@ (gubar) main))
```

## Integrating with swaybar
Create a script called `gubar.sh` in `~/.config/sway/`:

```sh
#!/bin/sh

# If you installed just take the text after "--"
guix shell guile -f ~/Workspace/gubar/guix.scm -- guile -c "((@ (gubar) main))"
```

Then, in your `~/.config/sway/config`, execute the script:
```
bar {
    ...
    status_command ~/.config/sway/gubar.sh
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
**Subject to change!!!**

```scheme
(gublock 
    #:block [block] 
    #:interval [interval]
    #:procedure [procedure]
    #:click-handler [click-handler]
    #:signal [signal])
```

| Key | Description | Example 
|-----|-------------|--------
| block | Initial block represented as an assoc list. | `'(("full_text" . "foo"))` |
| interval | Time in seconds in which this block updates | seconds or `'persistent` |
| procedure | The main procedure that will be run after `interval`. Receives a <block> and returns a `<block>` record. | `(lambda (block) block)`|
| click-handler | The procedure that is run when this block is clicked. The block must have its name field set. | `(lambda (event block) (block))`|
| signal | SIGRTIMIN offset that this block will be listening on and refresh on. Can be triggered by sending a SIGRTMIN+signal to the guile process running gubar -- `pkill -SIGRTMIN+signal -f -n gubar.scm`  | `2` |

### Built-in Blocks

| Block                | Declaration                                         | Info                                                                                                                     |
|----------------------|-----------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------|
| battery              | `(battery #:key (format "~a ~a%") (nerd-icons #f))` | Uses `/sys/class/power_supply/AC/online` and `/sys/class/power_supply/BAT0/capacity`                                     |
| date-time            | `(date-time #:key (format "%c") (interval 1))`      | Defaults: format `%c`, info `1`                                                                                          |
| label                | `(label text #:key color)`                          | Color should be an RGB string "#RRGGBB"                                                                                  |
| network-manager-wifi | `(network-manager-wifi #:key (ssid #f))`            | Uses `nmcli -t -f SSID,IN-USE,SIGNAL device wifi list --rescan auto` to gather info                                      |
| volume               | `(volume-pipewire #:key (signal 2))`                | Uses `pactl -f json list sinks` and `pactl get-default-sink` to gather info                                              |
| xkb-layout           | `(xkb-layout #:key (signal 3) (alias-pairs '()))`   | Uses `swaymsg -rt get_inputs` to gather info. `alias-pairs` are for mapping the layout index to an alias to be displayed |

### Example Config

```scheme
(use-modules (gubar gublock)
             (gubar blocks date-time)
             (gubar blocks battery)
             (gubar blocks label)
             (gubar blocks volume-pipewire)
             (gubar blocks network-manager-wifi)
             (gubar blocks xkb-layout)
             (gubar swaybar-protocol))

(list
 (date-time #:interval 60 #:format "%a %b %d %Y %-I:%M %p")
 (network-manager-wifi)
 (volume-pipewire)
 (xkb-layout #:alias-pairs '((0 . "lang1") (1 . "lang2")))
 (battery #:nerd-icons #t)
 (label "some text"))
```
 
#### Corresponding swayconfig
For the modules that require signals:
 
```
# Volume control
bindsym --locked  XF86AudioMute exec pactl set-sink-mute @DEFAULT_SINK@ toggle
bindsym --release XF86AudioMute exec pkill -SIGRTMIN+2 -f -n gubar.scm
bindsym --locked  XF86AudioLowerVolume exec pactl set-sink-volume @DEFAULT_SINK@ -5%
bindsym --release XF86AudioLowerVolume exec pkill -SIGRTMIN+2 -f -n gubar.scm
bindsym --locked  XF86AudioRaiseVolume exec pactl set-sink-volume @DEFAULT_SINK@ +5%
bindsym --release XF86AudioRaiseVolume exec pkill -SIGRTMIN+2 -f -n gubar.scm

# Switch layout
bindsym --release $mod+Control_R exec swaymsg input type:keyboard \
                                      xkb_switch_layout next && \
                                      pkill -SIGRTMIN+3 -f -n gubar.scm
```

Bar declaration:

```
bar {
    ...
    status_command "guix shell guile -f /path/to/gubar/guix.scm -- /path/to/gubar/src/gubar.scm"
    ...
}
```
 
## TODO
- [ ] Make configuration more ergonomic with some kind of syntax for defining
      blocks, instead of having to use assoc lists.
- [ ] Try to use channels to send click updates to a block instead of finding
      the block and updating manually (maybe)
- [ ] Better error handling or assertions
- [ ] Fix guix.scm to install with gnu-build-system so that we can install
      the script as a program with default configs, docs, etc.

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

## TODO
- [ ] Make configuration more ergonomic with some kind of syntax for defining
      blocks, instead of having to use assoc lists.
- [ ] Try to use channels to send click updates to a block instead of finding
      the block and updating manually (maybe)
- [ ] Better error handling or assertions
- [ ] Fix guix.scm to install with gnu-build-system so that we can install
      the script as a program with default configs, docs, etc.

# gubar

Swaybar generator, written in Guile Scheme.

## Try it using Guix shell

```sh
cd gubar/
guix shell guile -f guix.scm -- guile -e "((@ (gubar) main))" 
```

## Installing

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

# sonic-pi-mode

A minor mode to send code to [Sonic Pi](https://sonic-pi.net/) from an Emacs buffer (so that you can live code in Emacs).

## Requirements

* Sonic Pi 4.x. This mode won't work with Sonic Pi 2 or 3.x (but [sonic-pi.el](https://github.com/repl-electric/sonic-pi.el), the package on which this one is based, will)
* The [`osc`](https://elpa.gnu.org/packages/osc.html) Emacs package. Not sure which version is required 🙄

## Installation

I might consider to submit this package to MELPA, but for now you have to install it from this repository, in the way your package manager allows it. If you need to do it manually, clone this repo and add this to your `init.el`:

```elisp
(add-to-list 'load-path "~/<your-local-copy>")
(require 'sonic-pi-mode)
```

## Usage

<kbd>M-x</kbd> `sonic-pi-mode` to enable the following commands (with a default keybinding):

| Keybinding         | Command                   |                                                                                                                |
|--------------------|---------------------------|----------------------------------------------------------------------------------------------------------------|
| <kbd>C-c , c</kbd> | `sonic-pi-connect`        | Starts the Sonic Pi daemon on the background and connects to it.                                               |
| <kbd>C-c , r</kbd> | `sonic-pi-send-buffer`    | Sends the contents of the current buffer to the running Sonic Pi (equivalent to <kbd>Alt+S</kbd> on Sonic Pi). |
| <kbd>C-c , s</kbd> | `sonic-pi-stop`           | Sends a `stop-all-jobs` command to the running Sonic Pi (equivalent to <kbd>Alt+S</kbd> on Sonic Pi)           |
| <kbd>C-c , v</kbd> | `sonic-pi-control-volume` | Opens a menu to control Sonic Pi volume.                                                                       |
| <kbd>C-c , d</kbd> | `sonic-pi-disconnect`     | Closes the current connection to Sonic Pi                                                                      |

To use a different prefix than <kbd>C-c ,</kbd>, assign a different key binding to `sonic-pi-mode-prefix-map`.

## Configuration

You have to set the `sonic-pi-daemon-command` variable, with the path to the ruby file of the daemon. On a standard Sonic Pi install, it should be:

```elisp
(setq sonic-pi-daemon-command "<path-to-sonic-pi>/app/server/ruby/bin/daemon.rb")
```

If you installed Sonic Pi via Flatpak, it's a bit more complicated:

```elisp
(setq sonic-pi-daemon-command "flatpak run --command=\"/app/app/server/ruby/bin/daemon.rb\" net.sonic_pi.SonicPi")
```

## Credits

This work is based on Joseph Wilk's original [sonic-pi.el](https://github.com/repl-electric/sonic-pi.el), and particularly the logging code (the messages window) is directly taken from it. Thank you!

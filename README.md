# sonic-pi-mode

A minor mode to send code to [Sonic Pi](https://sonic-pi.net/) from an Emacs
buffer (so that you can live code in Emacs).

I wrote this after a failed attempt to fix
[sonic-pi.el](https://github.com/repl-electric/sonic-pi.el) (which is broken in
Sonic Pi 4.x due to multiple changes in its API). It is modest and feature-poor
in comparison though: in principle you can just send the whole buffer (like in
the original Sonic Pi code editor).

_After writing it_ I understand the problem better, so I may be able to fix the
original mode now, and I'll try to do that. In the meantime, here is this code
that Works On My Machine (tm).

## Requirements

* Sonic Pi 4.x. This mode won't work with Sonic Pi 2 or 3.x (but [sonic-pi.el](https://github.com/repl-electric/sonic-pi.el) will)
* The [`osc`](https://elpa.gnu.org/packages/osc.html) Emacs package. Not sure which version is required 🙄

## Installation

I might consider to submit this package to MELPA, but for now you have to install it from this repository, in the way your package manager allows it. If you need to do it manually, clone this repo and add this to your `init.el`:

```elisp
(add-to-list 'load-path "~/<your-local-copy>")
(require 'sonic-pi-mode)
```

## Usage

<kbd>M-x</kbd> `sonic-pi-mode` to enable the following commands (with a default keybinding):

| Keybinding       | Command                |                                                                                                                |
|------------------|------------------------|----------------------------------------------------------------------------------------------------------------|
| <kbd>C-c c</kbd> | `sonic-pi-connect`     | Starts the Sonic Pi daemon on the background and connects to it.                                               |
| <kbd>C-c r</kbd> | `sonic-pi-send-buffer` | Sends the contents of the current buffer to the running Sonic Pi (equivalent to <kbd>Alt+S</kbd> on Sonic Pi). |
| <kbd>C-c s</kbd> | `sonic-pi-stop`        | Sends a `stop-all-jobs` command to the running Sonic Pi (equivalent to <kbd>Alt+S</kbd> on Sonic Pi)           |
| <kbd>C-c d</kbd> | `sonic-pi-disconnect`  | Closes the current connection to Sonic Pi                                                                      |

## Roadmap

- [ ] Better error feedback when it's not possible to connect to Sonic Pi (because it doesn't start properly, or any other problem).
- [x] Manage the launch of the Sonic Pi daemon instead of relying on it being running.
- [x] Get logs back from Sonic Pi and display them on a buffer.
- [ ] Attempt to contribute all this back to [sonic-pi.el](https://github.com/repl-electric/sonic-pi.el) on a backwards-compatible way (so that it works with both Sonic Pi 3 and 4).

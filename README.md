# inhibit-mouse.el - Deactivate mouse input in Emacs
![Build Status](https://github.com/jamescherti/inhibit-mouse.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/inhibit-mouse.el)
![](https://raw.githubusercontent.com/jamescherti/inhibit-mouse.el/main/.images/made-for-gnu-emacs.svg)

The **inhibit-mouse** package allows the disabling of mouse input in Emacs using `inhibit-mouse-mode`.

Instead of modifying the keymap of its own mode (as the *disable-mouse* package does), enabling `inhibit-mouse-mode` only modifies `input-decode-map` to disable mouse events, making it simpler and faster. Additionally, the *inhibit-mouse* package allows for the restoration of mouse input when `inhibit-mouse-mode` is disabled.

This concept of utilizing `input-decode-map` to disable the mouse was introduced by Stefan Monnier in an emacs-devel mailing list [thread](https://lists.gnu.org/archive/html/emacs-devel/2024-11/msg00013.html) initiated by Daniel Radetsky, who proposed a patch to the Emacs developers.

## Installation

### Install using straight

To install the `inhibit-mouse` using `straight.el`:

1. It if hasn't already been done, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package inhibit-mouse
  :ensure t
  :straight (inhibit-mouse
             :type git
             :host github
             :repo "jamescherti/inhibit-mouse.el")
  :config
  (inhibit-mouse-mode))
```

# Frequently Asked Question

### What is the difference between the disable-mouse and inhibit-mouse packages?

The *inhibit-mouse* package is a simpler and faster alternative to the *disable-mouse* package, as it only modifies *input-decode-map* to disable mouse events.

In contrast, *disable-mouse* applies mouse events to its own mode, and sometimes the user has to apply it to other modes that are not affected by the *disable-mouse* mode (e.g, evil-mode).

Additionally, *inhibit-mouse* allows re-enabling the mouse when the mode is disabled, which is not supported by *disable-mouse*.

### Is there an alternative package that utilizes `input-decode-map`?

Daniel Radetsky, the person who sent an email to emacs-devel about disabling the mouse, developed a [similar package](https://github.com/dradetsky/ignore-mouse) that deactivates the mouse using `input-decode-map`. Coincidentally, we had the same idea at about the same time after reading Stefan Monnier's email.

### What motivates the author to disable the mouse in Emacs?

The author disables the mouse in Emacs:
- To prevent accidental clicks or cursor movements that can change the cursor position unexpectedly.
- To reinforce a keyboard-centric workflow, helping to avoid the habit of relying on the mouse for navigation.

Some may suggest that the author could modify the touchpad settings at the OS level. However, he prefers not to disable the touchpad entirely, as it remains useful in other applications, such as web browsers.

## Author and License

The `inhibit-mouse` Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [inhibit-mouse.el @GitHub](https://github.com/jamescherti/inhibit-mouse.el)

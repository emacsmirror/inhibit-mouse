# inhibit-mouse.el - Deactivate mouse input in Emacs
![Build Status](https://github.com/jamescherti/inhibit-mouse.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/inhibit-mouse.el)
![](https://raw.githubusercontent.com/jamescherti/inhibit-mouse.el/main/.images/made-for-gnu-emacs.svg)

The inhibit-mouse package allows deactivating mouse input during editing in Emacs.

This package is a simpler and more efficient alternative to the `disable-mouse` package, as it only modifies `input-decode-map` to disable mouse events with a single configuration. In contrast, `disable-mouse` applies mouse events to its own mode and sometimes to other modes, such as Evil.

The concept of using `input-decode-map` was introduced by Stefan Monnier, who proposed an intuitive solution for disabling mouse input in Emacs. This discussion was initiated by Daniel Radetsky, who suggested a C patch to address the issue of mouse functionality in Emacs.

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
             :repo "jamescherti/inhibit-mouse.el"))
```

## Author and License

The `inhibit-mouse` Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [inhibit-mouse.el @GitHub](https://github.com/jamescherti/inhibit-mouse.el)

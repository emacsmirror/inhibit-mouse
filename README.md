# inhibit-mouse.el - Deactivate mouse input in Emacs
[![MELPA](https://melpa.org/packages/outline-indent-badge.svg)](https://melpa.org/#/inhibit-mouse)
![Build Status](https://github.com/jamescherti/inhibit-mouse.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/inhibit-mouse.el)
![](https://raw.githubusercontent.com/jamescherti/inhibit-mouse.el/main/.images/made-for-gnu-emacs.svg)

The **inhibit-mouse** package allows the disabling of mouse input in Emacs using `inhibit-mouse-mode`.

Instead of modifying the keymap of its own mode as the disable-mouse package does, enabling inhibit-mouse-mode only modifies input-decode-map to disable mouse events, making it more efficient and faster than disable-mouse.

Additionally, the *inhibit-mouse* package allows for the restoration of mouse input when `inhibit-mouse-mode` is disabled.

This concept of utilizing `input-decode-map` to disable the mouse was introduced by Stefan Monnier in an emacs-devel mailing list [thread](https://lists.gnu.org/archive/html/emacs-devel/2024-11/msg00013.html) initiated by Daniel Radetsky, who proposed a patch to the Emacs developers.

## Installation

To install `outline-indent` from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package inhibit-mouse
  :ensure t
  :config
  (inhibit-mouse-mode))
```

# Frequently Asked Question

### What is the difference between the disable-mouse and inhibit-mouse packages?

The *inhibit-mouse* package is a simpler and faster alternative to the *disable-mouse* package, as it only modifies *input-decode-map* to disable mouse events.

In contrast, *disable-mouse* applies mouse events to its own mode, and sometimes the user has to apply it to other modes that are not affected by the *disable-mouse* mode using the `disable-mouse-in-keymap` function (e.g, evil-mode, tab-bar...).

Additionally, *inhibit-mouse* allows re-enabling mouse functionality when the mode is disabled, which is not supported by *disable-mouse* when the `disable-mouse-in-keymap` function is used. The `disable-mouse-in-keymap` function overwrites the key mappings of other modes (e.g., evil, tab-bar), and there is no straightforward way to make `disable-mouse` restore them.

### Is there an alternative package that utilizes input-decode-map?

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
- [inhibit-mouse.el @MELPA](https://melpa.org/#/inhibit-mouse)

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.

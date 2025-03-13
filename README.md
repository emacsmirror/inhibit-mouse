# inhibit-mouse.el - Deactivate mouse input in Emacs (Alternative to disable-mouse)
![Build Status](https://github.com/jamescherti/inhibit-mouse.el/actions/workflows/ci.yml/badge.svg)
[![MELPA](https://melpa.org/packages/outline-indent-badge.svg)](https://melpa.org/#/inhibit-mouse)
[![MELPA Stable](https://stable.melpa.org/packages/inhibit-mouse-badge.svg)](https://stable.melpa.org/#/inhibit-mouse)
![License](https://img.shields.io/github/license/jamescherti/inhibit-mouse.el)
![](https://raw.githubusercontent.com/jamescherti/inhibit-mouse.el/main/.images/made-for-gnu-emacs.svg)

The **inhibit-mouse** package allows the disabling of mouse input in Emacs using `inhibit-mouse-mode`.

Instead of modifying the keymap of its own mode as the *disable-mouse* package does, enabling inhibit-mouse-mode only modifies input-decode-map to disable mouse events, making it more efficient and faster than *disable-mouse*.

Additionally, the *inhibit-mouse* package allows for the restoration of mouse input when `inhibit-mouse-mode` is disabled.

## Installation

To install `outline-indent` from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package inhibit-mouse
  :ensure t
  :config
  (inhibit-mouse-mode 1))
```

## Customization

### Preventing help from being displayed when hovering over items?

To prevent help from being displayed when hovering over items (e.g., tooltips):

```elisp
;; Disable help display when hovering over items
(setq show-help-function nil)
```

### Customizing the mouse buttons disabled by inhibit-mouse?

The *inhibit-mouse* custom variables allow you to fine-tune which mouse interactions are disabled.

You can use the following configuration to specify which mouse buttons and events you want to disable:
``` emacs-lisp
;; This variable specifies which mouse buttons should be inhibited from
;; triggering events.
(setq inhibit-mouse-button-numbers '(1 2 3 4 5))

;; List of mouse button events to be inhibited.
(setq inhibit-mouse-button-events '("mouse"
                                    "up-mouse"
                                    "down-mouse"
                                    "drag-mouse"))

;; List of miscellaneous mouse events to be inhibited.
(setq inhibit-mouse-misc-events '("wheel-up"
                                  "wheel-down"
                                  "wheel-left"
                                  "wheel-right"
                                  "pinch"))

;; List of mouse multiplier events to be inhibited.
(setq inhibit-mouse-multipliers '("double" "triple"))

;; List of key modifier combinations to be inhibited for mouse events.
(setq inhibit-mouse-key-modifiers '((control)
                                    (meta)
                                    (shift)
                                    (control meta shift)
                                    (control meta)
                                    (control shift)
                                    (meta shift)))
```

### Disabling mouse highlighting over clickable text (e.g., URLs, hyperlinks, etc.)

The `inhibit-mouse-adjust-mouse-highlight` variable controls whether clickable text, such as URLs or hyperlinks, is highlighted when the mouse hovers over them. By default, it is set to t (enabled), but you can disable it by setting the variable to nil.

Default value:
``` emacs-lisp
(setq inhibit-mouse-adjust-mouse-highlight t)
```

### Enabling/Disabling the context menu

To enable or disable the context menu based on the state of `inhibit-mouse-mode`, the following code dynamically toggles `context-menu-mode` accordingly:

```elisp
(add-hook 'inhibit-mouse-mode-hook
          #'(lambda()
              ;; Enable or disable the context menu based on the state of
              ;; `inhibit-mouse-mode', the following code dynamically toggles
              ;; `context-menu-mode' accordingly.
              (when (fboundp 'context-menu-mode)
                (if (bound-and-true-p inhibit-mouse-mode)
                    (context-menu-mode -1)
                  (context-menu-mode 1)))))
```

This ensures that the context menu is disabled when `inhibit-mouse-mode` is active and enabled when it is inactive.

## Frequently Asked Question
### What motivates the author to disable the mouse in Emacs?

The author disables the mouse in Emacs:
- To prevent accidental clicks or cursor movements that can change the cursor position unexpectedly.
- To reinforce a keyboard-centric workflow, helping to avoid the habit of relying on the mouse for navigation.

Some may suggest that the author could modify the touchpad settings at the OS level. However, he prefers not to disable the touchpad entirely, as it remains useful in other applications, such as web browsers.

### What is the difference between the disable-mouse and inhibit-mouse packages?

The *inhibit-mouse* package is a efficient alternative to the *disable-mouse* package, as it only modifies *input-decode-map* to disable mouse events.

In contrast, *disable-mouse* applies mouse events to its own mode, and sometimes the user has to apply it to other modes that are not affected by the *disable-mouse* mode using the `disable-mouse-in-keymap` function (e.g, evil-mode, tab-bar...).

Additionally, *inhibit-mouse*:
- Allows re-enabling mouse functionality when the mode is disabled, which is not supported by *disable-mouse* when the `disable-mouse-in-keymap` function is used. The `disable-mouse-in-keymap` function overwrites the key mappings of other modes (e.g., evil, tab-bar), and there is no straightforward way to make *disable-mouse* restore them.
- It resolves issues that *disable-mouse* does not, such as the "C-c C-x <mouse-wheel-down> is not bound" problem, where the user intended to enter C-c C-x j but accidentally touched the touchpad.

This concept of utilizing `input-decode-map` to disable the mouse was introduced by Stefan Monnier in an emacs-devel mailing list [thread](https://lists.gnu.org/archive/html/emacs-devel/2024-11/msg00013.html) initiated by Daniel Radetsky, who proposed a patch to the Emacs developers. Additionally, here is an interesting discussion on GitHub: [Add recipe for inhibit-mouse](https://github.com/melpa/melpa/pull/9229).

## Author and License

The `inhibit-mouse` Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024-2025 James Cherti

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
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el): An Emacs package that prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking their execution if they would break the parenthetical structure.

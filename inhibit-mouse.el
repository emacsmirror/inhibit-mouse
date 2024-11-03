;;; inhibit-mouse.el --- Deactivate mouse input during editing in Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/inhibit-mouse.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Deactivate mouse input during editing in Emacs.

;;; Code:

(defgroup inhibit-mouse nil
  "Non-nil if inhibit-mouse mode mode is enabled."
  :group 'inhibit-mouse
  :prefix "inhibit-mouse-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/inhibit-mouse.el"))

(defconst inhibit-mouse-multipliers '("double" "triple"))
(defconst inhibit-mouse-button-numbers '(1 2 3 4 5 6 7 8 9 10))
(defconst inhibit-mouse-button-events
  '("mouse" "up-mouse" "down-mouse" "drag-mouse"))
(defvar inhibit-mouse-wheel-events
  '("wheel-up" "wheel-down" "wheel-left" "wheel-right"))

(defun inhibit-mouse--suppress-input-event (modifier base)
  "Suppress a specific input event in Emacs.

This function disables an input event defined by the combination of
MODIFIER and BASE. It modifies the `input-decode-map` to ensure that
the specified event is not processed, effectively preventing any
action associated with it.

MODIFIER: The modifier key (e.g., control, meta) to be used in conjunction
          with BASE.
BASE: The base input event (e.g., mouse-1, wheel-up) to be suppressed.

This function is useful for disabling unwanted mouse events during
editing or other operations in Emacs, allowing users to maintain
focus on keyboard input without interruption from mouse actions."
  (define-key input-decode-map
              (vector (event-convert-list (list modifier base)))
              (lambda (_prompt) [])))

;;;###autoload
(define-minor-mode inhibit-mouse-mode
  "Toggle `inhibit-mouse-mode'."
  :global t
  :lighter " inhibit-mouse"
  :group 'inhibit-mouse
  (if inhibit-mouse-mode
      (progn
        (dolist (modifier '(control meta nil))
          (dolist (base inhibit-mouse-wheel-events)
            (inhibit-mouse--suppress-input-event modifier (intern base))))

        ;; Disable mouse button events with modifiers
        (dolist (modifier '(control meta nil))
          (dolist (button inhibit-mouse-button-numbers)
            (dolist (event inhibit-mouse-button-events)
              (let ((base (format "%s-%d" event button)))
                (inhibit-mouse--suppress-input-event modifier (intern base)))))))
    ;; TODO Implement disable mouse
    t))

(provide 'inhibit-mouse)
;;; inhibit-mouse.el ends here

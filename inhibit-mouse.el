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

(defcustom inhibit-mouse-mode-lighter " InhibitMouse"
  "Mode-line lighter for `inhibit-mouse-mode'."
  :group 'inhibit-mouse
  :type 'string)

(defconst inhibit-mouse-multipliers '("double" "triple"))
(defconst inhibit-mouse-button-numbers '(1 2 3 4 5 6 7 8 9 10))
(defconst inhibit-mouse-button-events
  '("mouse" "up-mouse" "down-mouse" "drag-mouse"))
(defvar inhibit-mouse-wheel-events
  '("wheel-up" "wheel-down" "wheel-left" "wheel-right"))

(defvar inhibit-mouse--ignored-events nil
  "The mouse events that have been ignored.")

(defun inhibit-mouse--suppress-input-event (modifier base)
  "Suppress a specific input event.

This function disables an input event defined by the combination of
MODIFIER and BASE. It modifies the `input-decode-map` to ensure that
the specified event is not processed.

MODIFIER: The modifier key (e.g., control, meta).
BASE: The base input event (e.g., wheel-up) to be suppressed.

This function is useful for disabling unwanted mouse events during editing or
other operations, allowing users to maintain focus on keyboard input without
interruption from mouse actions."
  (define-key input-decode-map
              (vector (event-convert-list (list modifier base)))
              (lambda (_prompt) [])))

(defun inhibit-mouse--restore-input-event (modifier base)
  "Restore the input event defined by MODIFIER and BASE."
  (define-key input-decode-map
              (vector (event-convert-list (list modifier base)))
              nil))

;;;###autoload
(define-minor-mode inhibit-mouse-mode
  "Toggle `inhibit-mouse-mode'."
  :global t
  :lighter inhibit-mouse-mode-lighter
  :group 'inhibit-mouse
  (if inhibit-mouse-mode
      (progn
        (setq inhibit-mouse--ignored-events nil)
        (dolist (modifier '(control meta nil))
          (dolist (base inhibit-mouse-wheel-events)
            (push (cons modifier base) inhibit-mouse--ignored-events)
            (inhibit-mouse--suppress-input-event modifier (intern base)))

          (dolist (button inhibit-mouse-button-numbers)
            (dolist (event inhibit-mouse-button-events)
              (let ((base (format "%s-%d" event button)))
                (push (cons modifier base) inhibit-mouse--ignored-events)
                (inhibit-mouse--suppress-input-event modifier (intern base)))))))
    ;; Remove the ignored events when disabling the mode
    (dolist (ignored-event inhibit-mouse--ignored-events)
      (let ((modifier (car ignored-event))
            (base (cdr ignored-event)))
        (inhibit-mouse--restore-input-event modifier (intern base))))
    ;; Clear the list after restoring
    (setq inhibit-mouse--ignored-events nil)))

(provide 'inhibit-mouse)
;;; inhibit-mouse.el ends here

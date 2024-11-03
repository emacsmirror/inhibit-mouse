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

;;;###autoload
(define-minor-mode inhibit-mouse-mode
  "Toggle `inhibit-mouse-mode'."
  :global t
  :lighter " inhibit-mouse"
  :group 'inhibit-mouse
  (if inhibit-mouse-mode
      t
    t))

(provide 'inhibit-mouse)
;;; inhibit-mouse.el ends here

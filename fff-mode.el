;;; fff-mode.el --- Fat Finger Forgiveness for auto-revert and save-some-buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Le Wang

;; Author: Le Wang <lewang.dev.26@gmail.com>
;; URL: https://github.com/lewang/fff-mode
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Auto-handle buffers with trivially small accidental modifications.
;;
;; Two problems solved:
;; 1. `auto-revert-mode' skips modified buffers even if the modification was
;;    an accidental keypress.  fff-mode teaches auto-revert to proceed when
;;    the changes are trivially small.
;; 2. `save-some-buffers' nags about fat-fingered buffers.  fff-mode silently
;;    skips the save prompt for those buffers.

;;; Code:

(require 'cl-lib)

;;;###autoload
(defgroup fff nil
  "Fat Finger Forgiveness -- auto-handle trivially modified buffers."
  :group 'convenience
  :prefix "fff-")

;;;###autoload
(defcustom fff-max-characters 4
  "Max total characters inserted+deleted to qualify as fat-fingered."
  :type 'integer
  :group 'fff)

;;;###autoload
(defcustom fff-max-changes 1
  "Max distinct change groups (undo boundaries) to qualify as fat-fingered."
  :type 'integer
  :group 'fff)

;;;###autoload
(defcustom fff-features '(auto-revert save-some-buffers)
  "Which forgiveness features to enable.
`auto-revert'       -- let auto-revert handle fat-fingered buffers
`save-some-buffers' -- silently revert fat-fingered buffers before save prompts"
  :type '(set (const auto-revert) (const save-some-buffers))
  :group 'fff)

(defun fff--fat-finger-p ()
  "Return non-nil when current buffer's modifications are below both thresholds.
Walks `buffer-undo-list' from head to first (t . TIME) marker, counting
characters from insertions/deletions and change groups from nil boundaries."
  (and (buffer-modified-p)
       (not (eq buffer-undo-list t))
       (let ((chars 0)
             (changes 0)
             (tail buffer-undo-list)
             (seen-change nil))
         (while (and tail (not (and (consp (car tail))
                                    (eq (car-safe (car tail)) t))))
           (let ((entry (car tail)))
             (cond
              ;; nil = undo boundary
              ((null entry)
               (when seen-change
                 (cl-incf changes)
                 (setq seen-change nil)))
              ;; (BEG . END) = insertion
              ((and (consp entry)
                    (integerp (car entry))
                    (integerp (cdr entry)))
               (cl-incf chars (abs (- (cdr entry) (car entry))))
               (setq seen-change t))
              ;; (TEXT . POS) = deletion
              ((and (consp entry)
                    (stringp (car entry))
                    (integerp (cdr entry)))
               (cl-incf chars (length (car entry)))
               (setq seen-change t))))
           (setq tail (cdr tail)))
         ;; Count final change group if no trailing boundary
         (when seen-change
           (cl-incf changes))
         (and (<= chars fff-max-characters)
              (<= changes fff-max-changes)))))

(defun fff--buffer-stale-function (&optional noconfirm)
  "Like `buffer-stale--default-function' but also considers fat-fingered buffers.
When the default says \"not stale\" (buffer modified), fallback checks:
file changed on disk AND fat-fingered? -> return non-nil -> auto-revert proceeds."
  (or (buffer-stale--default-function noconfirm)
      (and (memq 'auto-revert fff-features)
           buffer-file-name
           (file-readable-p buffer-file-name)
           (not (verify-visited-file-modtime))
           (fff--fat-finger-p))))

(defun fff--save-some-buffers-predicate ()
  "Return nil for fat-fingered buffers, skipping the save prompt.
Changes accumulate until they exceed the threshold."
  (not (and (memq 'save-some-buffers fff-features)
            (fff--fat-finger-p))))

(defvar fff--saved-stale-function nil
  "Saved default value of `buffer-stale-function' before `fff-mode' activation.")

(defvar fff--saved-predicate nil
  "Saved value of `save-some-buffers-default-predicate' before `fff-mode' activation.")

;;;###autoload
(define-minor-mode fff-mode
  "Fat Finger Forgiveness -- auto-handle buffers with trivially small modifications."
  :global t
  :lighter " fff"
  (if fff-mode
      (progn
        (setq fff--saved-stale-function (default-value 'buffer-stale-function))
        (setq-default buffer-stale-function #'fff--buffer-stale-function)
        (setq fff--saved-predicate save-some-buffers-default-predicate)
        (setq save-some-buffers-default-predicate #'fff--save-some-buffers-predicate))
    (setq-default buffer-stale-function fff--saved-stale-function)
    (setq save-some-buffers-default-predicate fff--saved-predicate)))

(provide 'fff-mode)
;;; fff-mode.el ends here

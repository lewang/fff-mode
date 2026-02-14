;;; fff-mode-test.el --- Tests for fff-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for fff-mode.  All tests construct `buffer-undo-list'
;; structures directly — no user interaction required.

;;; Code:

(require 'ert)
(require 'fff-mode)

;;; --- fff--undo-metrics ---------------------------------------------------

(ert-deftest fff-test-metrics-empty-list ()
  "Empty undo list → zero chars, zero changes."
  (should (equal (fff--undo-metrics nil) '(0 . 0))))

(ert-deftest fff-test-metrics-single-insertion ()
  "One insertion of 3 chars: (1 . 4) means positions 1–4."
  (should (equal (fff--undo-metrics '((1 . 4)))
                 '(3 . 1))))

(ert-deftest fff-test-metrics-single-deletion ()
  "One deletion: (\"abc\" . 5) → 3 chars, 1 change."
  (should (equal (fff--undo-metrics '(("abc" . 5)))
                 '(3 . 1))))

(ert-deftest fff-test-metrics-boundary-separates-groups ()
  "Two edits separated by nil boundary → 2 change groups."
  (let ((undo '((1 . 2)                ; insert 1 char  — group 1
                nil                     ; boundary
                ("x" . 3))))            ; delete 1 char  — group 2
    (should (equal (fff--undo-metrics undo) '(2 . 2)))))

(ert-deftest fff-test-metrics-consecutive-nils ()
  "Adjacent nil boundaries don't inflate the count."
  (let ((undo '((1 . 2) nil nil nil ("x" . 3))))
    (should (equal (fff--undo-metrics undo) '(2 . 2)))))

(ert-deftest fff-test-metrics-stops-at-save-marker ()
  "Entries after a (t . TIME) save marker are ignored."
  (let ((undo `((1 . 4)                ; 3 chars — counted
                nil
                (t . ,(current-time))   ; save marker — stop
                (1 . 100))))            ; should NOT be counted
    (should (equal (fff--undo-metrics undo) '(3 . 1)))))

(ert-deftest fff-test-metrics-mixed-insert-delete-one-group ()
  "Insert + delete in the same group sums chars, counts 1 change."
  (let ((undo '((1 . 3) ("ab" . 1))))
    (should (equal (fff--undo-metrics undo) '(4 . 1)))))

(ert-deftest fff-test-metrics-ignores-unknown-entries ()
  "Property-change and marker entries are silently skipped."
  (let ((undo `((1 . 2)
                ;; property change entry (ignored)
                (nil font-lock-face bold 10 15)
                ;; marker entry (ignored)
                (,(make-marker) . 1))))
    ;; Only the (1 . 2) insertion counts
    (should (equal (car (fff--undo-metrics undo)) 1))))

(ert-deftest fff-test-metrics-leading-boundary ()
  "Leading nil before any edit doesn't create a phantom change."
  (let ((undo '(nil (1 . 2))))
    (should (equal (fff--undo-metrics undo) '(1 . 1)))))

;;; --- fff--fat-finger-p (integration with buffer state) -------------------

(ert-deftest fff-test-predicate-unmodified-buffer ()
  "Unmodified buffer → nil regardless of undo list."
  (with-temp-buffer
    (setq buffer-undo-list '((1 . 2)))
    ;; temp buffer starts unmodified
    (should-not (fff--fat-finger-p))))

(ert-deftest fff-test-predicate-undo-disabled ()
  "Undo disabled (buffer-undo-list = t) → nil."
  (with-temp-buffer
    (set-buffer-modified-p t)
    (setq buffer-undo-list t)
    (should-not (fff--fat-finger-p))))

(ert-deftest fff-test-predicate-within-thresholds ()
  "Small edit within both thresholds → non-nil."
  (with-temp-buffer
    (set-buffer-modified-p t)
    (setq buffer-undo-list '((1 . 2)))
    (let ((fff-max-characters 4)
          (fff-max-changes 1))
      (should (fff--fat-finger-p)))))

(ert-deftest fff-test-predicate-exceeds-char-threshold ()
  "Edit exceeding character threshold → nil."
  (with-temp-buffer
    (set-buffer-modified-p t)
    (setq buffer-undo-list '((1 . 10)))       ; 9 chars
    (let ((fff-max-characters 4)
          (fff-max-changes 1))
      (should-not (fff--fat-finger-p)))))

(ert-deftest fff-test-predicate-exceeds-change-threshold ()
  "Edit exceeding change-group threshold → nil."
  (with-temp-buffer
    (set-buffer-modified-p t)
    (setq buffer-undo-list '((1 . 2) nil (3 . 4)))  ; 2 change groups
    (let ((fff-max-characters 4)
          (fff-max-changes 1))
      (should-not (fff--fat-finger-p)))))

(provide 'fff-mode-test)
;;; fff-mode-test.el ends here

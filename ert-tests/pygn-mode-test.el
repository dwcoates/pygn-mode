;; -*- lexical-binding: t -*-

(require 'pygn-mode)

(setq pygn-mode-test-containing-directory
      (file-name-directory
       (or load-file-name
           (bound-and-true-p byte-compile-current-file)
           (buffer-file-name (current-buffer)))))

(setq pygn-mode-test-input-directory
      (expand-file-name "test-input" pygn-mode-test-containing-directory))

(setq pygn-mode-test-output-directory
      (expand-file-name "test-output" pygn-mode-test-containing-directory))

;;; pygn-mode-pgn-at-pos

;; todo: Ideally, PGN-at-pos tests would cover every position in this and
;; several other input files. How to organize that with clarity not clutter?
(ert-deftest pygn-mode-pgn-at-pos-01 nil
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "test-01.pgn" pygn-mode-test-input-directory))
    (pygn-mode)
    (should (equal
             "[Event \"?\"]\n"
             (pygn-mode-pgn-at-pos (point-min))))))

;;
;; Emacs
;;
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;

;;; pygn-mode-test.el ends here

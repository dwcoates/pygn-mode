;;; pgn-mode.el --- Simple syntax highlighting for chess PGN files -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019 Dodge Coates and Roland Walker
;;
;; Author: Dodge Coates and Roland Walker
;; Homepage: https://github.com/dwcoates/pgn-mode
;; URL: https://raw.githubusercontent.com/dwcoates/pgn-mode/master/pgn-mode.el
;; Version: 0.0.4
;; Last-Updated:  4 Nov 2019
;; Package-Requires:
;; Keywords:
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; To turn on fontification of line-wrapped parenthesized variations, set
;;
;;     (setq font-lock-maximum-decoration t)
;;
;; before loading the mode.  This feature is hidden behind a variable because
;; the speed cost when inserting text is enormous.
;;
;; No keys are bound by default.  Consider
;;
;;     (eval-after-load "pgn-mode"
;;       (define-key pgn-mode-map (kbd "M-f") 'pgn-mode-next-move)
;;       (define-key pgn-mode-map (kbd "M-b") 'pgn-mode-previous-move))
;;
;; Customization
;;
;;     M-x customize-group RET pgn-mode RET
;;
;; See Also
;;
;;     http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
;;
;; Prior Art
;;
;;     https://github.com/jwiegley/emacs-chess
;;
;; Notes
;;
;; Bugs
;;
;;     Fontification of multi-line variations is unreliable without
;;
;;         (setq font-lock-maximum-decoration t)
;;
;;     which is also slow (see below.)
;;
;;     `pgn-mode-after-change-function' and `pgn-mode-font-lock-extend-region'
;;     are still too slow.  `pgn-mode-font-lock-extend-region' causes considerable
;;     lag when typing.  Compare to speed of typing after
;;
;;         (remove-hook 'font-lock-extend-region-functions 'pgn-mode-font-lock-extend-region)
;;
;; TODO
;;
;;     Support line-comments with %.  Partial try, which doesn't mix with {}:
;;     (setq font-lock-syntactic-keywords '(("^\\(%\\).+?\\(\n\\)" (1 "<") (2 ">"))))
;;
;;     FEN-at-point.
;;
;;     Board-image-at-point.
;;
;;; License
;;
;; Simplified BSD License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;   1. Redistributions of source code must retain the above
;;      copyright notice, this list of conditions and the following
;;      disclaimer.
;;
;;   2. Redistributions in binary form must reproduce the above
;;      copyright notice, this list of conditions and the following
;;      disclaimer in the documentation and/or other materials
;;      provided with the distribution.
;;
;; This software is provided by the authors "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall the authors or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of the authors.
;;
;;; Code:
;;

;;; imports

(require 'cl-lib)

;;; declarations

(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end))

;;; customizable variables

;;;###autoload
(defgroup pgn-mode nil
  "Simple syntax highlighting for chess PGN files."
  :version "0.0.4"
  :prefix "pgn-mode-")

;;;###autoload
(defgroup pgn-mode-faces nil
  "Faces used by pgn-mode."
  :group 'pgn-mode)

(defface pgn-mode-tagpair-key-face
   '((t (:inherit font-lock-keyword-face)))
  "pgn-mode face for tagpair (header) keys."
  :group 'pgn-mode-faces)

(defface pgn-mode-nag-face
   '((t (:inherit font-lock-comment-face)))
  "pgn-mode face for Numeric Annotation Glyphs."
  :group 'pgn-mode-faces)

(defface pgn-mode-variation-face
   '((t (:inherit font-lock-string-face)))
  "pgn-mode face for variations."
  :group 'pgn-mode-faces)

(defface pgn-mode-result-face
   '((t (:inherit font-lock-builtin-face)))
  "pgn-mode face for result codes."
  :group 'pgn-mode-faces)

(defface pgn-mode-tagpair-bracket-face
   '((t (:foreground "Gray50")))
  "pgn-mode face for tagpair square brackets."
  :group 'pgn-mode-faces)

;;; variables

;;; syntax table

(defvar pgn-mode-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (with-syntax-table st
      (modify-syntax-entry ?{ "<")
      (modify-syntax-entry ?} ">")
      (modify-syntax-entry ?\\ "\\")
      (modify-syntax-entry ?\" "\"")
      (modify-syntax-entry ?| "w")
      (modify-syntax-entry ?+ "w")
      (modify-syntax-entry ?- "w")
      (modify-syntax-entry ?/ "w")
      (modify-syntax-entry ?± "w")
      (modify-syntax-entry ?– "w")
      (modify-syntax-entry ?‼ "w")
      (modify-syntax-entry ?⁇ "w")
      (modify-syntax-entry ?⁈ "w")
      (modify-syntax-entry ?⁉ "w")
      (modify-syntax-entry ?↑ "w")
      (modify-syntax-entry ?→ "w")
      (modify-syntax-entry ?⇆ "w")
      (modify-syntax-entry ?⇔ "w")
      (modify-syntax-entry ?⇗ "w")
      (modify-syntax-entry ?∆ "w")
      (modify-syntax-entry ?− "w")
      (modify-syntax-entry ?∓ "w")
      (modify-syntax-entry ?∞ "w")
      (modify-syntax-entry ?⊥ "w")
      (modify-syntax-entry ?⌓ "w")
      (modify-syntax-entry ?□ "w")
      (modify-syntax-entry ?✕ "w")
      (modify-syntax-entry ?\⟪ "w")
      (modify-syntax-entry ?\⟫ "w")
      (modify-syntax-entry ?⟳ "w")
      (modify-syntax-entry ?⨀ "w")
      (modify-syntax-entry ?⩱ "w")
      (modify-syntax-entry ?⩲ "w")
      (modify-syntax-entry ?= "w"))
    st)
  "Syntax table used while in `pgn-mode'.")

;;; keymaps

(defvar pgn-mode-map
  (let ((map (make-sparse-keymap)))
    ;; for example
    ;;
    ;; (define-key map (kbd "C-c C-n") 'pgn-mode-next-game)
    ;; (define-key map (kbd "C-c C-p") 'pgn-mode-previous-game)
    ;; (define-key map (kbd "M-f")     'pgn-mode-next-move)
    ;; (define-key map (kbd "M-b")     'pgn-mode-previous-move)
    ;;
    ;; and note that `down-list'/`backward-up-list' already works to
    ;; enter/exit a parenthesized variation
    map)
  "Keymap for `pgn-mode'.")

;;; lighter

;; todo: the lighter menu is inheriting useless items from `text-mode'

;;; utility functions

(defun pgn-mode-inside-comment-p ()
  "Whether the point is inside a PGN comment."
  (nth 4 (syntax-ppss)))

(defun pgn-mode-inside-variation-p ()
  "Whether the point is inside a PGN variation."
  (when (> (nth 0 (syntax-ppss)) 0)
    (nth 0 (syntax-ppss))))

(defun pgn-mode-inside-variation-or-comment-p ()
  "Whether the point is inside a PGN comment or a variation."
  (or (pgn-mode-inside-comment-p)
      (pgn-mode-inside-variation-p)))

(defun pgn-mode-looking-at-legal-move ()
  "Whether the point is looking at a legal chess move.

Leading move numbers are allowed, and ignored."
  (looking-at "[0-9.…\s-]*\\<\\([RNBQK][a-h]?[1-8]?x?[a-h][1-8]\\|[a-h]x?[1-8]=?[RNBQ]?\\|O-O\\|O-O-O\\)"))

(defun pgn-mode-game-start-position (&optional pos)
  "Start position for the PGN game which contains position POS.

POS defaults to `point'."
  (callf or pos (point))
  (save-excursion
    (goto-char pos)
    (unless (looking-at "\\[Event ")
      (re-search-backward "^\\[Event " nil t))
    (forward-line 0)
    (point)))

(defun pgn-mode-game-end-position (&optional pos)
  "End position for the PGN game which contains position POS.

POS defaults to `point'."
  (callf or pos (point))
  (save-excursion
    (goto-char pos)
    (goto-char (line-end-position))
    (if (re-search-forward "^\\[Event " nil t)
        (forward-line 0)
      ;; else
      (goto-char (point-max)))
    (re-search-backward "\\S-" nil t)
    (forward-line 1)
    (point)))

;; todo maybe shouldn't consult looking-at here, but it works well for the
;; purpose of pgn-mode-next-move
(defun pgn-mode-forward-exit-variations-and-comments ()
  "However deep in nested variations and comments, exit and skip forward."
  (while (or (> (nth 0 (syntax-ppss)) 0)
             (nth 4 (syntax-ppss))
             (looking-at "\\s-*(")
             (looking-at "\\s-*{"))
    (cond
      ((> (nth 0 (syntax-ppss)) 0)
       (up-list (nth 0 (syntax-ppss))))
      ((nth 4 (syntax-ppss))
       (skip-syntax-forward "^>")
       (forward-char 1))
      ((looking-at "\\s-*(")
       (skip-syntax-forward "-")
       (forward-char 1))
      ((looking-at "\\s-*{")
       (skip-syntax-forward "-")
       (forward-char 1)))
    (skip-syntax-forward "-")
    (when (looking-at "$")
      (forward-line 1)
      (skip-syntax-forward "-"))))

;; todo maybe shouldn't consult looking-back here, but it works well for the
;; purpose of pgn-mode-previous-move
(defun pgn-mode-backward-exit-variations-and-comments ()
  "However deep in nested variations and comments, exit and skip backward."
  (while (or (> (nth 0 (syntax-ppss)) 0)
             (nth 4 (syntax-ppss))
             (looking-back ")\\s-*" 10)
             (looking-back "}\\s-*" 10))
    (cond
      ((> (nth 0 (syntax-ppss)) 0)
       (up-list (- (nth 0 (syntax-ppss)))))
      ((nth 4 (syntax-ppss))
       (skip-syntax-backward "^<")
       (backward-char 1))
      ((looking-back ")\\s-*" 10)
       (skip-syntax-backward "-")
       (backward-char 1))
      ((looking-back "}\\s-*" 10)
       (skip-syntax-backward "-")
       (backward-char 1)))
    (skip-syntax-backward "-")
    (when (looking-at "^")
      (forward-line -1)
      (goto-char (line-end-position))
      (skip-syntax-backward "-"))))

;;; font-lock

(defun pgn-mode-after-change-function (beg end old-len)
  "Help refontify multi-line variations during edits."
  (let ((syn (syntax-ppss beg)))
    (if (> 0 (nth 0 syn))
        (progn
          (setq beg (max (point-min) (- (nth 1 (syntax-ppss)) 1)))
          (setq end (save-excursion
                      (goto-char beg)
                      (forward-list 1)
                      (point))))
      ;; else guess
      (setq beg (save-excursion
                  (goto-char beg)
                  (forward-line -1)
                  (point)))
      (setq end (save-excursion
                  (goto-char end)
                  (forward-line 2)
                  (point))))
    (cons beg end)))

(defun pgn-mode-font-lock-extend-region ()
  "Extend the search region to help fontify multi-line variations."
  (let ((syn (syntax-ppss font-lock-beg)))
    (if (> 0 (nth 0 syn))
        (save-excursion
          (goto-char (nth 1 syn))
          (setq font-lock-beg (point))
          (forward-list 1)
          (setq font-lock-end (point)))
      ;; else by block, which may be inefficient
      (save-excursion
        (save-match-data
          (goto-char font-lock-beg)
          (when (re-search-backward "\n\n" nil t)
            (setq font-lock-beg (point)))
          (goto-char font-lock-end)
          (when (re-search-forward "\n\n" nil t)
            (setq font-lock-end (point))))))))

(font-lock-add-keywords
 'pgn-mode
 '(
   ;; tagpair keys. values are handled by the syntax table
   ("^\\[\\(\\S-+\\)\\s-+\".*\"\\]" 1 'pgn-mode-tagpair-key-face)
   ;; tagpair open-brackets
   ("^\\[" . 'pgn-mode-tagpair-bracket-face)
   ;; tagpair close-brackets
   ("\\]\\s-*$" . 'pgn-mode-tagpair-bracket-face)
   ;; numeric NAGs
   ("\\<$[0-9]+" . 'pgn-mode-nag-face)
   ;; unicode NAGs and annotations
   ("±\\|–\\|‼\\|⁇\\|⁈\\|⁉\\|↑\\|→\\|⇆\\|⇔\\|⇗\\|∆\\|−\\|∓\\|∞\\|⊥\\|⌓\\|□\\|✕\\|⟪\\|⟫\\|⟳\\|⨀\\|⩱\\|⩲" . 'pgn-mode-nag-face)
   ;; NAGs preceded by space
   ("\\s-\\(\\+-?\\|-\\|=\\)" 1 'pgn-mode-nag-face)
   ;; annotations not preceded by space
   ("\\?\\|!" . 'pgn-mode-nag-face)
   ;; result codes
   ("\\(1\\s-*-\\s-*0\\|0\\s-*-\\s-*1\\|1/2\\s-*-\\s-*1/2\\|\\*\\)\\s-*$" 1 'pgn-mode-result-face)
   ;; variation text. append or keep is very important here.
   ("([^()]*?)" 0 'pgn-mode-variation-face append)))

;;; major-mode definition

;;;###autoload
(define-derived-mode pgn-mode text-mode "PGN"
 "Simple syntax highlighting for chess PGN files."
 :syntax-table pgn-mode-syntax-table
 :group 'pgn-mode
 (setq-local comment-start "{")
 (setq-local comment-end "}")
 (setq-local comment-continue " ")
 (setq-local comment-multi-line t)
 (setq-local comment-style 'plain)
 (when font-lock-maximum-decoration
   (setq-local font-lock-multiline t)
   (setq-local font-lock-extend-after-change-region-function 'pgn-mode-after-change-function)
   ;; especially slow
   (add-hook 'font-lock-extend-region-functions 'pgn-mode-font-lock-extend-region t t)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[pP][gG][nN]\\'" . pgn-mode))

;;; interactive commands

(defun pgn-mode-next-game (arg)
  "Advance to the next game in a multi-game PGN buffer.

With numeric prefix ARG, advance ARG games."
  (interactive "p")
  (cl-callf or arg 1)
  (when (looking-at "\\[Event ")
    (goto-char (line-end-position)))
  (if (re-search-forward "^\\[Event " nil t arg)
      (goto-char (line-beginning-position))
    ;; else
    (error "No next game.")))

(defun pgn-mode-previous-game (arg)
  "Move back to the previous game in a multi-game PGN buffer.

With numeric prefix ARG, move back ARG games."
  (interactive "p")
  (cl-callf or arg 1)
  (unless (looking-at "\\[Event ")
    (re-search-backward "^\\[Event " nil t))
  (if (re-search-backward "^\\[Event " nil t arg)
      (goto-char (line-beginning-position))
    ;; else
    (error "No previous game.")))

(defun pgn-mode-next-move (arg)
  "Advance to the next move in a PGN game.

Treats move numbers purely as punctuation.  If the point is on a move
number, it considered to be on the move followed by that move number.
But the advancing motion will skip over move numbers when possible.

With numeric prefix ARG, advance ARG moves forward."
  (interactive "p")
  (cl-callf or arg 1)
  (save-restriction
    (narrow-to-region (pgn-mode-game-start-position)
                      (pgn-mode-game-end-position))
    (let ((last-point -1)
          (start (point))
          (thumb (point)))
      (when (or (looking-at "[^\n]*\\]")
                (and (looking-at "\\s-*$") (looking-back "\\]\\s-*" 10)))
        (re-search-forward "\n\n" nil t))
      (dotimes (counter arg)
        (when (pgn-mode-looking-at-legal-move)
          (setq thumb (point))
          (skip-chars-forward "0-9.…\s-")
          (forward-char 1))
        (while (and (not (= (point) last-point))
                    (or (not (pgn-mode-looking-at-legal-move))
                        (pgn-mode-inside-variation-or-comment-p)))
          (setq last-point (point))
          (cond ((pgn-mode-inside-variation-or-comment-p)
                 (pgn-mode-forward-exit-variations-and-comments))
                (t
                 (forward-sexp 1)))))
      (skip-chars-forward "0-9.…\s-")
      (unless (pgn-mode-looking-at-legal-move)
        (goto-char thumb)
        (when (eq thumb start)
          (error "No more moves."))))))

(defun pgn-mode-previous-move (arg)
  "Move back to the previous move in a PGN game.

Treats move numbers purely as punctuation.  If the point is on a move
number, it considered to be on the move followed by that move number.
But the backward motion will skip over move numbers when possible.

With numeric prefix ARG, move ARG moves backward."
  (interactive "p")
  (cl-callf or arg 1)
  (save-restriction
    (narrow-to-region (pgn-mode-game-start-position)
                      (pgn-mode-game-end-position))
    (let ((last-point -1)
          (start (point))
          (thumb (point)))
      (when (or (looking-at "[^\n]*\\]")
                (and (looking-at "\\s-*$") (looking-back "\\]\\s-*" 10)))
        (re-search-backward "\n\n" nil t))
      (dotimes (counter arg)
        (when (pgn-mode-looking-at-legal-move)
          (setq thumb (point))
          (skip-chars-backward "0-9.…\s-")
          (backward-char 1))
        (while (and (not (= (point) last-point))
                    (or (not (pgn-mode-looking-at-legal-move))
                        (pgn-mode-inside-variation-or-comment-p)))
          (setq last-point (point))
          (cond ((pgn-mode-inside-variation-or-comment-p)
                 (pgn-mode-backward-exit-variations-and-comments))
                (t
                 (skip-chars-backward "0-9.…\s-")
                 (forward-sexp -1)))))
      (unless (pgn-mode-looking-at-legal-move)
        (goto-char thumb)
        (when (eq thumb start)
          (error "No more moves."))))))

(defun pgn-mode-select-game ()
  "Select current game in a multi-game PGN buffer."
  (interactive)
  (push-mark (pgn-mode-game-start-position) t t)
  (goto-char (pgn-mode-game-end-position))
  (exchange-point-and-mark))

(provide 'pgn-mode)

;;
;; Emacs
;;
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: ARGS alist
;;

;;; pgn-mode.el ends here

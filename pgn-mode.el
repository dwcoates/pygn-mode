;;; pgn-mode.el --- Simple syntax highlighting for chess PGN files -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019 Dodge Coates and Roland Walker
;;
;; Author: Dodge Coates and Roland Walker
;; Homepage: https://github.com/dwcoates/pgn-mode
;; URL: https://raw.githubusercontent.com/dwcoates/pgn-mode/master/pgn-mode.el
;; Version: 0.0.4
;; Last-Updated:  4 Nov 2019
;; Package-Requires: ((emacs "24.3") (nav-flash "1.0.0"))
;; Keywords: data, games, chess
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
(require 'nav-flash nil t)

;;; declarations

(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end))

;;; customizable variables

;;;###autoload
(defgroup pgn-mode nil
  "Simple syntax highlighting for chess PGN files."
  :version "0.0.4"
  :prefix "pgn-mode-"
  :group 'data
  :group 'games)

(defcustom pgn-mode-python-path "python"
  "Path to a Python interpreter with the python-chess library installed."
  :group 'pgn-mode
  :type 'string)

(defcustom pgn-mode-board-size 400
  "Size for graphical board display, expressed as pixels-per-side."
  :group 'pgn-mode
  :type 'int)

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

(defvar pgn-mode-script-directory
  (file-name-directory
   (or load-file-name
       (bound-and-true-p byte-compile-current-file)
       (buffer-file-name (current-buffer))))
  "Directory to find Python helper scripts.")

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
    (define-key map [menu-bar PGN]
      (cons "PGN" (make-sparse-keymap "PGN")))
    (define-key map [menu-bar PGN pgn-mode-select-game]
      '(menu-item "Select Game" pgn-mode-select-game
                  :help "Select the current game"))
    (define-key map [menu-bar PGN pgn-mode-previous-game]
      '(menu-item "Previous Game" pgn-mode-previous-game
                  :help "Navigate to the previous game"))
    (define-key map [menu-bar PGN pgn-mode-next-game]
      '(menu-item "Next Game" pgn-mode-next-game
                  :help "Navigate to the next game"))
    (define-key map [menu-bar PGN sep] menu-bar-separator)
    (define-key map [menu-bar PGN pgn-mode-previous-move]
      '(menu-item "Previous Move" pgn-mode-previous-move
                  :help "Navigate to the previous move"))
    (define-key map [menu-bar PGN pgn-mode-next-move]
      '(menu-item "Next Move" pgn-mode-next-move
                  :help "Navigate to the next move"))

    ;; todo: enable these once fen-at-point and board-at-point are merged
    ;;
    ;; (define-key map [menu-bar PGN sep-2] menu-bar-separator)
    ;; (define-key map [menu-bar PGN pgn-mode-display-fen-at-point]
    ;;   '(menu-item "FEN at point" pgn-mode-display-fen-at-point
    ;;               :help "Display FEN at point in separate window"))
    ;; (define-key map [menu-bar PGN pgn-mode-display-gui-board-at-point]
    ;;   '(menu-item "Board at point" pgn-mode-display-gui-board-at-point
    ;;               :help "Display GUI board at point in separate window"))

    ;; example keystrokes:
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

(defvar pgn-mode--python-process nil "Python process that powers pgn-mode.")
(defvar pgn-mode--python-buffer nil "Buffer to which the pgn-mode Python process sends output.")


;;; lighter

;; todo: the lighter menu is inheriting useless items from `text-mode'

;;; utility functions

(defun pgn-mode--process-running-p ()
  "Return non-nil iff `pgn-mode--python-process' is running."
  (and pgn-mode--python-process (process-live-p pgn-mode--python-process) t))

;; TODO: check for pyhon-chess
;; TODO: generalize script
;; TODO: pipes?
(defun pgn-mode--make-process (&optional force)
  "Initialize pgn-mode `pgn-mode--python-process', optionally FORCE recreation if already exists."
  (when (and (not force) (pgn-mode--process-running-p))
    (error "The pgn-mode Python process already running. Use optional `force' to recreate"))
  (when (not (= 0 (call-process pgn-mode-python-path nil nil nil "-c" "import chess")))
    (error "The Python interpreter at `pgn-mode-python-path' must have the python-chess library available"))
  (message (format "Initializing pgn-mode python process%s." (if force " (forcing)" "")))
  (setq pgn-mode--python-buffer (get-buffer-create "pgn-mode-data-buffer"))
  (setq pgn-mode--python-process
        (make-process :name "pgn-mode-python"
                      :buffer pgn-mode--python-buffer
                      :noquery t
                      :command (list pgn-mode-python-path
                                     (concat pgn-mode-script-directory "pgn_handler.py") "-"))))

(defun pgn-mode--kill-process ()
  "Stop the currently running `pgn-mode--python-process' if it is running."
  (when (pgn-mode--process-running-p)
    (delete-process pgn-mode--python-process)
    (setq pgn-mode--python-process nil)))

(defun pgn-mode--send-process (message)
  "Send MESSAGE to the running `pgn-mode--python-process'."
  (if (pgn-mode--process-running-p)
      (process-send-string pgn-mode--python-process (concat message (string 10) (string 4)))
    (error "Need running Python process to send pgn-mode message")))

;; TODO: divert error using :stderr on make-process, instead of taking only the first line of output
(defun pgn-mode--receive-process (seconds &optional max-time)
  "Wrap `accept-process-output' with SECONDS for `pgn-mode--python-process' for MAX-TIME."
  (when (not (pgn-mode--process-running-p))
    (error "Cannot fetch pgn-mode output without a running process"))
  (when (not (get-buffer pgn-mode--python-buffer))
    (error "Python output buffer does not exist"))
  (with-current-buffer pgn-mode--python-buffer
    (let ((tries 0)
          python-process-output)        ;
      (goto-char (point-min))
      (while (and (progn
                    (accept-process-output pgn-mode--python-process seconds nil 1)
                    (= (buffer-size) 0))
                  (< (* tries seconds) max-time))
        (sit-for 0)
        (cl-incf tries))
      (goto-char (point-min))
      (setq python-process-output
            (buffer-substring-no-properties (point-min) (line-end-position)))
      (erase-buffer)
      python-process-output)))

(defun pgn-mode--query-process (message seconds &optional max-time force)
  "Send MESSAGE to active `pgn-mode--python-process' every SECONDS for MAX-TIME and return response, optionally FORCE a new python process."
  (when (not (pgn-mode--process-running-p))
    (pgn-mode--make-process force))
  (pgn-mode--send-process message)
  (pgn-mode--receive-process seconds (or max-time 0.25)))

(defun pgn-mode--inside-comment-p ()
  "Whether the point is inside a PGN comment."
  (nth 4 (syntax-ppss)))

(defun pgn-mode-inside-variation-p ()
  "Whether the point is inside a PGN variation."
  (when (> (nth 0 (syntax-ppss)) 0)
    (nth 0 (syntax-ppss))))

(defun pgn-mode-inside-variation-or-comment-p ()
  "Whether the point is inside a PGN comment or a variation."
  (or (pgn-mode--inside-comment-p)
      (pgn-mode-inside-variation-p)))

(defun pgn-mode-looking-at-legal-move ()
  "Whether the point is looking at a legal SAN chess move.

Leading move numbers, punctuation and spaces are allowed, and ignored."
  (let ((inhibit-changing-match-data t))
    (and (looking-at-p "[ \t]*[0-9]*[.…\s-]*\\<\\([RNBQK][a-h]?[1-8]?x?[a-h][1-8]\\|[a-h]x?[1-8]=?[RNBQ]?\\|O-O\\|O-O-O\\)\\(\\+\\+?\\|#\\)?")
         (not (looking-back "[A-Za-z]" 1)))))

(defun pgn-mode-game-start-position (&optional pos)
  "Start position for the PGN game which contains position POS.

POS defaults to `point'."
  (cl-callf or pos (point))
  (save-excursion
    (goto-char pos)
    (unless (looking-at-p "\\[Event ")
      (let ((inhibit-changing-match-data t))
        (re-search-backward "^\\[Event " nil t)))
    (forward-line 0)
    (point)))

(defun pgn-mode-game-end-position (&optional pos)
  "End position for the PGN game which contains position POS.

POS defaults to `point'."
  (cl-callf or pos (point))
  (save-excursion
    (save-match-data
      (goto-char pos)
      (goto-char (line-end-position))
      (if (re-search-forward "^\\[Event " nil t)
          (forward-line 0)
        ;; else
        (goto-char (point-max)))
      (re-search-backward "\\S-" nil t)
      (forward-line 1)
      (point))))

;; todo maybe shouldn't consult looking-at here, but it works well for the
;; purpose of pgn-mode-next-move
(defun pgn-mode-forward-exit-variations-and-comments ()
  "However deep in nested variations and comments, exit and skip forward."
  (while (or (> (nth 0 (syntax-ppss)) 0)
             (nth 4 (syntax-ppss))
             (looking-at-p "\\s-*(")
             (looking-at-p "\\s-*{"))
    (cond
      ((> (nth 0 (syntax-ppss)) 0)
       (up-list (nth 0 (syntax-ppss))))
      ((nth 4 (syntax-ppss))
       (skip-syntax-forward "^>")
       (forward-char 1))
      ((looking-at-p "\\s-*(")
       (skip-syntax-forward "-")
       (forward-char 1))
      ((looking-at-p "\\s-*{")
       (skip-syntax-forward "-")
       (forward-char 1)))
    (skip-syntax-forward "-")
    (when (looking-at-p "$")
      (forward-line 1)
      (skip-syntax-forward "-"))))

;; todo maybe shouldn't consult looking-back here, but it works well for the
;; purpose of pgn-mode-previous-move
(defun pgn-mode-backward-exit-variations-and-comments ()
  "However deep in nested variations and comments, exit and skip backward."
  (save-match-data
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
      (when (looking-at-p "^")
        (forward-line -1)
        (goto-char (line-end-position))
        (skip-syntax-backward "-")))))

(defun pgn-mode--send-board (code &optional pos)
  "Get PGN string preceding POS and send a `pgn-mode--python-process' request denoted by CODE."
  (when (not (pgn-mode--process-running-p))
    (pgn-mode--make-process))
  (cl-callf or pos (point))
  (save-excursion
    (let ((pgn (buffer-substring-no-properties (pgn-mode-game-start-position) pos)))
      (pgn-mode--query-process (concat (number-to-string code) " -- " pgn) 0.01 0.51))))

;; todo divert error using :stderr on make-process, instead of taking only the first line of output
(defun pgn-mode-fen-at-pos (&rest pos)
  "Return the FEN corresponding to POS, which defaults to the point."
  (pgn-mode--send-board 1 pos))

(defun pgn-mode-echo-fen-at-point ()
  "Display the FEN corresponding to the point in the echo area."
  (interactive)
  (message "%s" (pgn-mode-fen-at-pos)))

(defun pgn-mode-board-at-pos (&rest pos)
  "Get SVG output for PGN string preceding POS."
  (pgn-mode--send-board 2 pos))

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

(defun pgn-mode-propertize-line-comments (start end)
  "Put text properties on beginnings and ends of line comments.

Intended to be used as a `syntax-propertize-function'."
  (save-excursion
    (save-match-data
      (goto-char start)
      (while (re-search-forward "^\\(%\\)[^\n]*\\(\n\\)" end t)
        (put-text-property (match-beginning 1) (match-end 1)
                           'syntax-table (string-to-syntax "!"))
        (put-text-property (match-beginning 2) (match-end 2)
                           'syntax-table (string-to-syntax "!"))))))

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
(define-derived-mode pgn-mode fundamental-mode "PGN"
 "Simple syntax highlighting for chess PGN files."
 :syntax-table pgn-mode-syntax-table
 :group 'pgn-mode
 (setq-local comment-start "{")
 (setq-local comment-end "}")
 (setq-local comment-continue " ")
 (setq-local comment-multi-line t)
 (setq-local comment-style 'plain)
 (setq-local syntax-propertize-function 'pgn-mode-propertize-line-comments)
 (setq-local parse-sexp-lookup-properties t)
 (setq-local parse-sexp-ignore-comments t)
 (when font-lock-maximum-decoration
   (setq-local font-lock-multiline t)
   (setq-local font-lock-extend-after-change-region-function 'pgn-mode-after-change-function)
   ;; especially slow
   (add-hook 'font-lock-extend-region-functions 'pgn-mode-font-lock-extend-region t t))
 (font-lock-ensure)
 (let ((map (make-sparse-keymap)))
   (set-keymap-parent map (default-value 'mode-line-major-mode-keymap))
   (define-key map (kbd "<mode-line> <mouse-4>")    'pgn-mode-previous-move)
   (define-key map (kbd "<mode-line> <mouse-5>")    'pgn-mode-next-move)
   (define-key map (kbd "<mode-line> <wheel-up>")   'pgn-mode-previous-move)
   (define-key map (kbd "<mode-line> <wheel-down>") 'pgn-mode-next-move)
   (setq-local mode-line-major-mode-keymap map)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[pP][gG][nN]\\'" . pgn-mode))

;;; interactive commands

(defun pgn-mode-next-game (arg)
  "Advance to the next game in a multi-game PGN buffer.

With numeric prefix ARG, advance ARG games."
  (interactive "p")
  (cl-callf or arg 1)
  (save-match-data
    (when (looking-at-p "\\[Event ")
      (goto-char (line-end-position)))
    (if (re-search-forward "^\\[Event " nil t arg)
        (goto-char (line-beginning-position))
      ;; else
      (error "No next game.")))
  (when (fboundp 'nav-flash-show)
    (nav-flash-show)))

(defun pgn-mode-previous-game (arg)
  "Move back to the previous game in a multi-game PGN buffer.

With numeric prefix ARG, move back ARG games."
  (interactive "p")
  (cl-callf or arg 1)
  (save-match-data
    (unless (looking-at-p "\\[Event ")
      (re-search-backward "^\\[Event " nil t))
    (if (re-search-backward "^\\[Event " nil t arg)
        (goto-char (line-beginning-position))
      ;; else
      (error "No previous game.")))
  (when (fboundp 'nav-flash-show)
    (nav-flash-show)))

(defun pgn-mode-next-move (arg)
  "Advance to the next move in a PGN game.

Treats move numbers purely as punctuation.  If the point is on a move
number, it considered to be on the move followed by that move number.
But the advancing motion will skip over move numbers when possible.

With numeric prefix ARG, advance ARG moves forward."
  (interactive "p")
  (cl-callf or arg 1)
  (save-restriction
    (save-match-data
      (narrow-to-region (pgn-mode-game-start-position)
                        (pgn-mode-game-end-position))
      (let ((last-point -1)
            (start (point))
            (thumb (point)))
        (when (or (looking-at-p "[^\n]*\\]")
                  (and (looking-at-p "\\s-*$") (looking-back "\\]\\s-*" 10)))
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
            (cond
              ((pgn-mode-inside-variation-or-comment-p)
               (pgn-mode-forward-exit-variations-and-comments))
              (t
               (forward-sexp 1)))))
        (skip-chars-forward "0-9.…\s-")
        (unless (pgn-mode-looking-at-legal-move)
          (goto-char thumb)
          (when (= thumb start)
            (error "No more moves.")))))))

(defun pgn-mode-previous-move (arg)
  "Move back to the previous move in a PGN game.

Treats move numbers purely as punctuation.  If the point is on a move
number, it considered to be on the move followed by that move number.
But the backward motion will skip over move numbers when possible.

With numeric prefix ARG, move ARG moves backward."
  (interactive "p")
  (cl-callf or arg 1)
  (save-restriction
    (save-match-data
      (narrow-to-region (pgn-mode-game-start-position)
                        (pgn-mode-game-end-position))
      (let ((last-point -1)
            (start (point))
            (thumb (point)))
        (when (or (looking-at-p "[^\n]*\\]")
                  (and (looking-at-p "\\s-*$") (looking-back "\\]\\s-*" 10)))
          (error "No more moves."))
        (dotimes (counter arg)
          (when (pgn-mode-looking-at-legal-move)
            (setq thumb (point))
            (skip-chars-backward "0-9.…\s-")
            (backward-char 1))
          (while (and (not (= (point) last-point))
                      (or (not (pgn-mode-looking-at-legal-move))
                          (pgn-mode-inside-variation-or-comment-p)))
            (setq last-point (point))
            (cond
              ((pgn-mode-inside-variation-or-comment-p)
               (pgn-mode-backward-exit-variations-and-comments))
              (t
               (skip-chars-backward "0-9.…\s-")
               (forward-sexp -1)))))
        (unless (pgn-mode-looking-at-legal-move)
          (goto-char thumb)
          (when (= thumb start)
            (error "No more moves.")))))))

(defun pgn-mode-select-game ()
  "Select current game in a multi-game PGN buffer."
  (interactive)
  (push-mark (pgn-mode-game-end-position) t t)
  (goto-char (pgn-mode-game-start-position)))

(defun pgn-mode-display-fen-at-point ()
  "Display the FEN corresponding to the point in a separate buffer."
  (interactive)
  (let ((fen (pgn-mode-fen-at-pos))
        (buf (get-buffer-create " *pgn-mode-fen*"))
        (win nil))
    (with-current-buffer buf
      (delete-region (point-min) (point-max))
      (insert fen)
      (display-buffer buf '(display-buffer-reuse-window))
      (setq win (get-buffer-window buf))
      (resize-temp-buffer-window win)
      (set-window-dedicated-p win t))))

;; todo ascii board command
(defun pgn-mode-display-gui-board-at-point ()
  "Display the board corresponding to the point in a separate buffer."
  (interactive)
  (let ((svg (pgn-mode-board-at-pos))
        (buf (get-buffer-create " *pgn-mode-board*"))
        (win nil))
    (with-current-buffer buf
      (when (eq major-mode 'image-mode)
        (image-mode-as-text))
      (delete-region (point-min) (point-max))
      (insert svg)
      (image-mode))
    (display-buffer buf '(display-buffer-reuse-window))
    (setq win (get-buffer-window buf))
    (resize-temp-buffer-window win)
    (set-window-dedicated-p win t)))

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

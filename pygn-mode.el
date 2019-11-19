;;; pygn-mode.el --- Simple syntax highlighting for chess PGN files -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019 Dodge Coates and Roland Walker
;;
;; Author: Dodge Coates and Roland Walker
;; Homepage: https://github.com/dwcoates/pygn-mode
;; URL: https://raw.githubusercontent.com/dwcoates/pygn-mode/master/pygn-mode.el
;; Version: 0.0.4
;; Last-Updated:  4 Nov 2019
;; Package-Requires: ((emacs "25.0") (nav-flash "1.0.0"))
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
;;     (eval-after-load "pygn-mode"
;;       (define-key pygn-mode-map (kbd "M-f") 'pygn-mode-next-move)
;;       (define-key pygn-mode-map (kbd "M-b") 'pygn-mode-previous-move))
;;
;; Customization
;;
;;     M-x customize-group RET pygn-mode RET
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
;; Compatibility and Requirements
;;
;;     Python and python-chess are needed for numerous features such as SVG
;;     board images:
;;
;;         https://pypi.org/project/python-chess/
;;
;; Bugs
;;
;;     Fontification of multi-line variations is unreliable without
;;
;;         (setq font-lock-maximum-decoration t)
;;
;;     which is also slow (see below.)
;;
;;     `pygn-mode-after-change-function' and `pygn-mode-font-lock-extend-region'
;;     are still too slow.  `pygn-mode-font-lock-extend-region' causes considerable
;;     lag when typing.  Compare to speed of typing after
;;
;;         (remove-hook 'font-lock-extend-region-functions 'pygn-mode-font-lock-extend-region)
;;
;; TODO
;;
;;     Performance.
;;
;;     Flash current move on selection
;;
;;     pygn-display-mode: minor mode that makes navigation commands
;;     automatically update gui
;;
;;     Copy fen to clipboard?
;;
;; IDEA
;;
;;     pygn-ivy:
;;        Select games with ivy (via header contents)
;;        Select fens with ivy
;;
;;     UCI moves to pgn: UCI position command arguments to pgn and/or graphical display
;;
;;     uci-mode: use comint to make a uci mode and integrate this with pygn?
;;
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

(defconst pygn-mode-version "0.0.4")

;;; Imports

(require 'cl-lib)
(require 'nav-flash nil t)
(require 'svg)

;;; Declarations

(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end))

;;; Customizable variables

;;;###autoload
(defgroup pygn-mode nil
  "Simple syntax highlighting for chess PGN files."
  :version pygn-mode-version
  :prefix "pygn-mode-"
  :group 'data
  :group 'games)

(defcustom pygn-mode-python-path "python"
  "Path to a Python interpreter with the python-chess library installed."
  :group 'pygn-mode
  :type 'string)

(defcustom pygn-mode-board-size 400
  "Size for graphical board display, expressed as pixels-per-side."
  :group 'pygn-mode
  :type 'int)

;;;###autoload
(defgroup pygn-mode-faces nil
  "Faces used by pygn-mode."
  :group 'pygn-mode)

(defface pygn-mode-tagpair-key-face
   '((t (:inherit font-lock-keyword-face)))
  "pygn-mode face for tagpair (header) keys."
  :group 'pygn-mode-faces)

(defface pygn-mode-nag-face
   '((t (:inherit font-lock-comment-face)))
  "pygn-mode face for Numeric Annotation Glyphs."
  :group 'pygn-mode-faces)

(defface pygn-mode-variation-face
   '((t (:inherit font-lock-string-face)))
  "pygn-mode face for variations."
  :group 'pygn-mode-faces)

(defface pygn-mode-result-face
   '((t (:inherit font-lock-builtin-face)))
  "pygn-mode face for result codes."
  :group 'pygn-mode-faces)

(defface pygn-mode-tagpair-bracket-face
   '((t (:foreground "Gray50")))
  "pygn-mode face for tagpair square brackets."
  :group 'pygn-mode-faces)

;;; Variables

(defvar pygn-mode-script-directory
  (file-name-directory
   (or load-file-name
       (bound-and-true-p byte-compile-current-file)
       (buffer-file-name (current-buffer))))
  "Directory to find Python helper scripts.")

(defvar pygn-mode-python-chess-succeeded nil
  "Whether a simple python-chess command has succeeded.")

(defvar pygn-mode--python-process nil
  "Python process that powers pygn-mode.")

(defvar pygn-mode--python-buffer nil
  "Buffer to which the pygn-mode Python process sends output.")

;;; Syntax table

(defvar pygn-mode-syntax-table
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
  "Syntax table used while in `pygn-mode'.")

;;; Keymaps

(defvar pygn-mode-map
  (let ((map (make-sparse-keymap)))
    ;; menu bar and lighter
    (define-key map [menu-bar PyGN]
      (cons "PyGN" (make-sparse-keymap "PyGN")))
    (define-key map [menu-bar PyGN pygn-mode-select-game]
      '(menu-item "Select Game" pygn-mode-select-game
                  :help "Select the current game"))
    (define-key map [menu-bar PyGN pygn-mode-previous-game]
      '(menu-item "Previous Game" pygn-mode-previous-game
                  :help "Navigate to the previous game"))
    (define-key map [menu-bar PyGN pygn-mode-next-game]
      '(menu-item "Next Game" pygn-mode-next-game
                  :help "Navigate to the next game"))
    (define-key map [menu-bar PyGN sep] menu-bar-separator)
    (define-key map [menu-bar PyGN pygn-mode-previous-move]
      '(menu-item "Previous Move" pygn-mode-previous-move
                  :help "Navigate to the previous move"))
    (define-key map [menu-bar PyGN pygn-mode-next-move]
      '(menu-item "Next Move" pygn-mode-next-move
                  :help "Navigate to the next move"))
    (define-key map [menu-bar PyGN sep-2] menu-bar-separator)
    (define-key map [menu-bar PyGN pygn-mode-display-fen-at-point]
      '(menu-item "FEN at point" pygn-mode-display-fen-at-point
                  :help "Display FEN at point in separate window"))
    (define-key map [menu-bar PyGN pygn-mode-display-gui-board-at-point]
      '(menu-item "Board at point" pygn-mode-display-gui-board-at-point
                  :help "Display GUI board at point in separate window"))

    ;; mouse
    (define-key map [mouse-2]        'pygn-mode-mouse-display-gui-board)
    (define-key map [double-mouse-2] 'pygn-mode-mouse-display-gui-board-inclusive)

    ;; example keystrokes:
    ;;
    ;; (define-key map (kbd "C-c C-n") 'pygn-mode-next-game)
    ;; (define-key map (kbd "C-c C-p") 'pygn-mode-previous-game)
    ;; (define-key map (kbd "M-f")     'pygn-mode-next-move)
    ;; (define-key map (kbd "M-b")     'pygn-mode-previous-move)
    ;;
    ;; and note that `down-list'/`backward-up-list' already works to
    ;; enter/exit a parenthesized variation
    map)
  "Keymap for `pygn-mode'.")

;;; Utility functions

(defun pygn-mode--process-running-p ()
  "Return non-nil iff `pygn-mode--python-process' is running."
  (and pygn-mode--python-process (process-live-p pygn-mode--python-process)))

;; TODO: generalize script
;; TODO: pipes?
(defun pygn-mode--make-process (&optional force)
  "Initialize pygn-mode `pygn-mode--python-process', optionally FORCE recreation if already exists."
  (pygn-mode-python-chess-guard)
  (when (and (not force) (pygn-mode--process-running-p))
    (error "The pygn-mode Python process already running. Use optional `force' to recreate"))
  (message (format "Initializing pygn-mode python process%s." (if force " (forcing)" "")))
  (setq pygn-mode--python-buffer (get-buffer-create " *pygn-mode-data-buffer*"))
  (setq pygn-mode--python-process
        (make-process :name "pygn-mode-python"
                      :buffer pygn-mode--python-buffer
                      :noquery t
                      :sentinel #'ignore
                      :command (list pygn-mode-python-path
                                     "-u"
                                     (expand-file-name "pygn_handler.py" pygn-mode-script-directory)
                                     "-"))))

(defun pygn-mode--kill-process ()
  "Stop the currently running `pygn-mode--python-process' if it is running."
  (when (pygn-mode--process-running-p)
    (delete-process pygn-mode--python-process)
    (setq pygn-mode--python-process nil)
    (message "pygn-mode Python service killed.")))

(defun pygn-mode--send-process (message)
  "Send MESSAGE to the running `pygn-mode--python-process'."
  (if (pygn-mode--process-running-p)
      (process-send-string pygn-mode--python-process (concat message (string 10)))
    (error "Need running Python process to send pygn-mode message")))

(defun pygn-mode--receive-process (seconds &optional max-time)
  "Wrap `accept-process-output' with SECONDS for `pygn-mode--python-process' for MAX-TIME."
  (when (not (pygn-mode--process-running-p))
    (error "Cannot fetch pygn-mode output without a running process"))
  (when (not (get-buffer pygn-mode--python-buffer))
    (error "Python output buffer does not exist"))
  (with-current-buffer pygn-mode--python-buffer
    (erase-buffer)
    (let ((tries 0))
      (goto-char (point-min))
      (while (and (not (eq ?\n (char-before (point-max))))
                  (< (* tries seconds) max-time))
        (accept-process-output pygn-mode--python-process seconds nil 1)
        (cl-incf tries))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun pygn-mode--inside-comment-p ()
  "Whether the point is inside a PGN comment."
  (nth 4 (syntax-ppss)))

(defun pygn-mode-inside-variation-p ()
  "Whether the point is inside a PGN variation."
  (when (> (nth 0 (syntax-ppss)) 0)
    (nth 0 (syntax-ppss))))

(defun pygn-mode-inside-variation-or-comment-p ()
  "Whether the point is inside a PGN comment or a variation."
  (or (pygn-mode--inside-comment-p)
      (pygn-mode-inside-variation-p)))

(defun pygn-mode-looking-at-legal-move ()
  "Whether the point is looking at a legal SAN chess move.

Leading move numbers, punctuation and spaces are allowed, and ignored."
  (let ((inhibit-changing-match-data t))
    (and (looking-at-p "[ \t]*[0-9]*[.…\s-]*\\<\\([RNBQK][a-h]?[1-8]?x?[a-h][1-8]\\|[a-h]x?[1-8]=?[RNBQ]?\\|O-O\\|O-O-O\\)\\(\\+\\+?\\|#\\)?")
         (not (looking-back "[A-Za-z]" 1)))))

(defun pygn-mode-game-start-position (&optional pos)
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

(defun pygn-mode-game-end-position (&optional pos)
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
;; purpose of pygn-mode-next-move
(defun pygn-mode-forward-exit-variations-and-comments ()
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
;; purpose of pygn-mode-previous-move
(defun pygn-mode-backward-exit-variations-and-comments ()
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

(defun pygn-mode-python-chess-guard ()
  "Throw an error unless the python-chess library is available."
  (unless pygn-mode-python-chess-succeeded
    (if (zerop (call-process pygn-mode-python-path nil nil nil "-c" "import chess"))
        (setq pygn-mode-python-chess-succeeded t)
      (error "The Python interpreter at `pygn-mode-python-path' must have the python-chess library available."))))

(defun pygn-mode-pgn-as-if-variation (pos &optional inclusive)
  "PGN string as if a variation had been played until position POS.

When INCLUSIVE is non-nil, synthesize a PGN inclusive of any move
on which the point is resting.

Does not work for nested variations."
  (save-excursion
    (goto-char pos)
    (if inclusive
        (progn
          (skip-chars-forward "0-9.…\s-")
          (skip-syntax-forward "^-"))
      (skip-syntax-backward "^-"))
    (let ((pgn (buffer-substring-no-properties
                (pygn-mode-game-start-position)
                (point))))
    (with-temp-buffer
      (insert pgn)
      (when (pygn-mode-inside-variation-p)
        (up-list -1)
        (delete-char 1)
        (delete-region
         (save-excursion (forward-word-strictly -1) (point))
         (point)))
        (goto-char (point-max))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun pygn-mode--query-process (message seconds &optional max-time force)
  "Send MESSAGE to active `pygn-mode--python-process' every SECONDS for MAX-TIME and return response, optionally FORCE a new python process."
  (when (not (pygn-mode--process-running-p))
    (pygn-mode--make-process force))
  (pygn-mode--send-process message)
  (pygn-mode--receive-process seconds (or max-time 0.25)))

(defun pygn-mode--send-board (code &optional pos)
  "Get PGN string preceding POS and send a `pygn-mode--python-process' request denoted by CODE."
  (cl-callf or pos (point))
  (save-excursion
    (let ((pgn (buffer-substring-no-properties (pygn-mode-game-start-position) pos)))
      (setq pgn (replace-regexp-in-string "\n" "\\\\n" pgn))
      (pygn-mode--query-process (concat (number-to-string code) " -- " pgn) 0.01 0.51))))

(defun pygn-mode-fen-at-pos (pos)
  "Return the FEN corresponding to POS, which defaults to the point."
  (when (not (pygn-mode--process-running-p))
    (pygn-mode--make-process))
  (pygn-mode--send-board 1 pos))

(defun pygn-mode-board-at-pos (pos)
  "Get SVG output for PGN string preceding POS."
  (when (not (pygn-mode--process-running-p))
    (pygn-mode--make-process))
  (pygn-mode--send-board 2 pos))

;;; font-lock

(defun pygn-mode-after-change-function (beg end old-len)
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

(defun pygn-mode-font-lock-extend-region ()
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

(defun pygn-mode-propertize-line-comments (start end)
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
 'pygn-mode
 '(
   ;; tagpair keys. values are handled by the syntax table
   ("^\\[\\(\\S-+\\)\\s-+\".*\"\\]" 1 'pygn-mode-tagpair-key-face)
   ;; tagpair open-brackets
   ("^\\[" . 'pygn-mode-tagpair-bracket-face)
   ;; tagpair close-brackets
   ("\\]\\s-*$" . 'pygn-mode-tagpair-bracket-face)
   ;; numeric NAGs
   ("\\<$[0-9]+" . 'pygn-mode-nag-face)
   ;; unicode NAGs and annotations
   ("±\\|–\\|‼\\|⁇\\|⁈\\|⁉\\|↑\\|→\\|⇆\\|⇔\\|⇗\\|∆\\|−\\|∓\\|∞\\|⊥\\|⌓\\|□\\|✕\\|⟪\\|⟫\\|⟳\\|⨀\\|⩱\\|⩲" . 'pygn-mode-nag-face)
   ;; NAGs preceded by space
   ("\\s-\\(\\+-?\\|-\\|=\\)" 1 'pygn-mode-nag-face)
   ;; annotations not preceded by space
   ("\\?\\|!" . 'pygn-mode-nag-face)
   ;; result codes
   ("\\(1\\s-*-\\s-*0\\|0\\s-*-\\s-*1\\|1/2\\s-*-\\s-*1/2\\|\\*\\)\\s-*$" 1 'pygn-mode-result-face)
   ;; variation text. append or keep is very important here.
   ("([^()]*?)" 0 'pygn-mode-variation-face append)))

;;; Major-mode definition

;;;###autoload
(define-derived-mode pygn-mode fundamental-mode "PyGN"
 "Simple syntax highlighting for chess PGN files."
 :syntax-table pygn-mode-syntax-table
 :group 'pygn-mode
 (setq-local comment-start "{")
 (setq-local comment-end "}")
 (setq-local comment-continue " ")
 (setq-local comment-multi-line t)
 (setq-local comment-style 'plain)
 (setq-local syntax-propertize-function 'pygn-mode-propertize-line-comments)
 (setq-local parse-sexp-lookup-properties t)
 (setq-local parse-sexp-ignore-comments t)
 (when font-lock-maximum-decoration
   (setq-local font-lock-multiline t)
   (setq-local font-lock-extend-after-change-region-function 'pygn-mode-after-change-function)
   ;; especially slow
   (add-hook 'font-lock-extend-region-functions 'pygn-mode-font-lock-extend-region t t))
 (font-lock-ensure)
 (let ((map (make-sparse-keymap)))
   (set-keymap-parent map (default-value 'mode-line-major-mode-keymap))
   (define-key map (kbd "<mode-line> <mouse-4>")    'pygn-mode-previous-move)
   (define-key map (kbd "<mode-line> <mouse-5>")    'pygn-mode-next-move)
   (define-key map (kbd "<mode-line> <wheel-up>")   'pygn-mode-previous-move)
   (define-key map (kbd "<mode-line> <wheel-down>") 'pygn-mode-next-move)
   (setq-local mode-line-major-mode-keymap map)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[pP][gG][nN]\\'" . pygn-mode))

;;; Interactive commands

(defun pygn-mode-next-game (arg)
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

(defun pygn-mode-previous-game (arg)
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

(defun pygn-mode-next-move (arg)
  "Advance to the next move in a PGN game.

Treats move numbers purely as punctuation.  If the point is on a move
number, it considered to be on the move followed by that move number.
But the advancing motion will skip over move numbers when possible.

With numeric prefix ARG, advance ARG moves forward."
  (interactive "p")
  (cl-callf or arg 1)
  (save-restriction
    (save-match-data
      (narrow-to-region (pygn-mode-game-start-position)
                        (pygn-mode-game-end-position))
      (let ((last-point -1)
            (start (point))
            (thumb (point)))
        (when (or (looking-at-p "[^\n]*\\]")
                  (and (looking-at-p "\\s-*$") (looking-back "\\]\\s-*" 10)))
          (re-search-forward "\n\n" nil t))
        (dotimes (counter arg)
          (when (pygn-mode-looking-at-legal-move)
            (setq thumb (point))
            (skip-chars-forward "0-9.…\s-")
            (forward-char 1))
          (while (and (not (= (point) last-point))
                      (or (not (pygn-mode-looking-at-legal-move))
                          (pygn-mode-inside-variation-or-comment-p)))
            (setq last-point (point))
            (cond
              ((pygn-mode-inside-variation-or-comment-p)
               (pygn-mode-forward-exit-variations-and-comments))
              (t
               (forward-sexp 1)))))
        (skip-chars-forward "0-9.…\s-")
        (unless (pygn-mode-looking-at-legal-move)
          (goto-char thumb)
          (when (= thumb start)
            (error "No more moves.")))))))

(defun pygn-mode-previous-move (arg)
  "Move back to the previous move in a PGN game.

Treats move numbers purely as punctuation.  If the point is on a move
number, it considered to be on the move followed by that move number.
But the backward motion will skip over move numbers when possible.

With numeric prefix ARG, move ARG moves backward."
  (interactive "p")
  (cl-callf or arg 1)
  (save-restriction
    (save-match-data
      (narrow-to-region (pygn-mode-game-start-position)
                        (pygn-mode-game-end-position))
      (let ((last-point -1)
            (start (point))
            (thumb (point)))
        (when (or (looking-at-p "[^\n]*\\]")
                  (and (looking-at-p "\\s-*$") (looking-back "\\]\\s-*" 10)))
          (error "No more moves."))
        (dotimes (counter arg)
          (when (pygn-mode-looking-at-legal-move)
            (setq thumb (point))
            (skip-chars-backward "0-9.…\s-")
            (backward-char 1))
          (while (and (not (= (point) last-point))
                      (or (not (pygn-mode-looking-at-legal-move))
                          (pygn-mode-inside-variation-or-comment-p)))
            (setq last-point (point))
            (cond
              ((pygn-mode-inside-variation-or-comment-p)
               (pygn-mode-backward-exit-variations-and-comments))
              (t
               (skip-chars-backward "0-9.…\s-")
               (forward-sexp -1)))))
        (unless (pygn-mode-looking-at-legal-move)
          (goto-char thumb)
          (when (= thumb start)
            (error "No more moves.")))))))

(defun pygn-mode-select-game (pos)
  "Select current game in a multi-game PGN buffer.

When called non-interactively, select the game containing POS."
  (interactive "d")
  (goto-char pos)
  (push-mark (pygn-mode-game-end-position) t t)
  (goto-char (pygn-mode-game-start-position)))

(defun pygn-mode-echo-fen-at-point (pos)
  "Display the FEN corresponding to the point in the echo area.

When called non-interactively, display the FEN corresponding to POS."
  (interactive "d")
  (message "%s" (pygn-mode-fen-at-pos pos)))

(defun pygn-mode-display-fen-at-point (pos)
  "Display the FEN corresponding to the point in a separate buffer.

When called non-interactively, display the FEN corresponding to POS."
  (interactive "d")
  (let* ((fen (pygn-mode-fen-at-pos pos))
         (buf (get-buffer-create " *pygn-mode-fen*"))
         (win (get-buffer-window buf)))
    (with-current-buffer buf
      (delete-region (point-min) (point-max))
      (insert fen)
      (goto-char (point-min))
      (display-buffer buf '(display-buffer-reuse-window))
      (unless win
        (setq win (get-buffer-window buf))
        (set-window-dedicated-p win t)
        (resize-temp-buffer-window win)))))

(defun pygn-mode-display-variation-fen-at-point (pos)
  "Respecting variations, display the FEN corresponding to the point.

When called non-interactively, display the board corresponding to POS."
  (interactive "d")
  (let ((pgn (pygn-mode-pgn-as-if-variation pos)))
    (with-temp-buffer
      (insert pgn)
      (pygn-mode-display-fen-at-point (point-max)))))

;; todo ascii board command
(defun pygn-mode-display-gui-board-at-point (pos)
  "Display the board corresponding to the point in a separate buffer.

When called non-interactively, display the board corresponding to POS."
  (interactive "d")
  (let* ((svg-data (pygn-mode-board-at-pos pos))
         (buf (get-buffer-create " *pygn-mode-board*"))
         (win (get-buffer-window buf)))
    (with-current-buffer buf
      (delete-region (point-min) (point-max))
      (insert-image (create-image svg-data 'svg t)))
    (display-buffer buf '(display-buffer-reuse-window))
    (unless win
      (setq win (get-buffer-window buf))
      (set-window-dedicated-p win t)
      (resize-temp-buffer-window win))))

(defun pygn-mode-mouse-display-gui-board (event)
  "Display the board corresponding to the mouse click in a separate buffer."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (goto-char (posn-point (event-start event)))
  (save-excursion
    (skip-syntax-backward "^\\s-")
    (pygn-mode-display-gui-board-at-point (point))))

(defun pygn-mode-mouse-display-gui-board-inclusive (event)
  "Display inclusive board corresponding to the mouse click in a separate buffer.

\"Inclusive\" here means that the board includes any move which contains the
click position."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (goto-char (posn-point (event-start event)))
  (save-excursion
    (skip-syntax-forward "^\\s-")
    (pygn-mode-display-gui-board-at-point (point))))

(defun pygn-mode-display-variation-gui-board-at-point (pos)
  "Respecting variations, display the board corresponding to the point.

When called non-interactively, display the board corresponding to POS."
  (interactive "d")
  (let ((pgn (pygn-mode-pgn-as-if-variation pos)))
    (with-temp-buffer
      (insert pgn)
      (pygn-mode-display-gui-board-at-point (point-max)))))

(defun pygn-mode-previous-move-follow-gui-board (arg)
  "Move back to the previous move and display the updated board.

With numeric prefix ARG, move ARG moves backward."
  (interactive "p")
  (pygn-mode-previous-move arg)
  (pygn-mode-display-gui-board-at-point (point)))

(defun pygn-mode-next-move-follow-gui-board (arg)
  "Advance to the next move and display the updated board.

With numeric prefix ARG, move ARG moves forward."
  (interactive "p")
  (pygn-mode-next-move arg)
  (pygn-mode-display-gui-board-at-point (point)))

(provide 'pygn-mode)

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

;;; pygn-mode.el ends here

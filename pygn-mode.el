;;; pygn-mode.el --- Simple syntax highlighting for chess PGN files -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019 Dodge Coates and Roland Walker
;;
;; Author: Dodge Coates and Roland Walker
;; Homepage: https://github.com/dwcoates/pygn-mode
;; URL: https://raw.githubusercontent.com/dwcoates/pygn-mode/master/pygn-mode.el
;; Version: 0.5.0
;; Last-Updated: 26 Nov 2019
;; Package-Requires: ((emacs "25.0") (nav-flash "1.0.0"))
;; Keywords: data, games, chess
;;
;; Simplified BSD License
;;
;;; Commentary:
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
;;     Emacs 25
;;
;;     Python and the python-chess library are needed for numerous features
;;     such as SVG board images:
;;
;;         https://pypi.org/project/python-chess/
;;
;; Bugs
;;
;;     `pygn-mode-after-change-function' should be made faster
;;
;;     bracketed {comments} inside variations can't contain close-parenthesis
;;
;; TODO
;;
;;     Flash current move on selection
;;
;;     pygn-display-mode: minor mode that makes navigation commands
;;     automatically update gui
;;
;; IDEA
;;
;;     pygn-ivy:
;;        Select games with ivy (via header contents)
;;        Select fens with ivy
;;
;;     UCI moves to pgn: UCI position command arguments to pgn and/or graphical display
;;
;;     uci-mode:
;;        use comint to make a uci mode and integrate this with pygn?
;;
;;     count games in current file? Display in modeline?
;;
;;     evil text objects?
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

(defconst pygn-mode-version "0.5.0")

;;; Imports

(require 'cl-lib)
(require 'nav-flash nil t)

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

(defcustom pygn-mode-python-executable "python"
  "Path to a Python 3.5+ interpreter."
  :group 'pygn-mode
  :type 'string)

(defcustom pygn-mode-pythonpath nil
  "A colon-delimited path to override the $PYTHONPATH environment variable."
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
   '((t (:foreground "Gray50")))
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
  "Directory to find Python server script \"pygn_server.py\".")

(defvar pygn-mode-python-chess-succeeded nil
  "Whether a simple external command using the python-chess library has succeeded.")

(defvar pygn-mode--server-process nil
  "Python-based server which powers many `pygn-mode' features.")

(defvar pygn-mode--server-buffer nil
  "Buffer to which the `pygn-mode' server process is associated.")

(defvar pygn-mode--server-receive-every-seconds 0.01
  "How often `pygn-mode--server-receive' should check the server for output when polling.")

(defvar pygn-mode--server-receive-max-seconds 0.5
  "The maximum amount of time `pygn-mode--server-receive' should check the server for output when polling.")

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
    (define-key map [mouse-2]        'pygn-mode-mouse-display-variation-gui-board)
    (define-key map [double-mouse-2] 'pygn-mode-mouse-display-variation-gui-board-inclusive)

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

(defun pygn-mode--server-running-p ()
  "Return non-nil iff `pygn-mode--server-process' is running."
  (and pygn-mode--server-process (process-live-p pygn-mode--server-process)))

(defun pygn-mode--python-chess-guard ()
  "Throw an error unless the python-chess library is available."
  (unless pygn-mode-python-chess-succeeded
    (let ((process-environment (cl-copy-list process-environment)))
      (when pygn-mode-pythonpath
        (setenv "PYTHONPATH" pygn-mode-pythonpath))
      (if (zerop (call-process pygn-mode-python-executable nil nil nil "-c" "import chess"))
          (setq pygn-mode-python-chess-succeeded t)
        (error "The Python interpreter at `pygn-mode-python-path' must have the python-chess library available")))))

;; TODO: pipes?
(defun pygn-mode--server-start (&optional force)
  "Initialize the `pygn-mode' `pygn-mode--server-process'.

Optionally FORCE recreation if the server already exists."
  (pygn-mode--python-chess-guard)
  (if force
      (pygn-mode--server-kill)
    (when (pygn-mode--server-running-p)
      (error "The pygn-mode server process is already running. Use optional `force' to recreate")))
  (message (format "Initializing pygn-mode server process%s." (if force " (forcing)" "")))
  (let ((process-environment (cl-copy-list process-environment)))
    (when pygn-mode-pythonpath
      (setenv "PYTHONPATH" pygn-mode-pythonpath))
    (setq pygn-mode--server-buffer (get-buffer-create " *pygn-mode-server*"))
    (setq pygn-mode--server-process
          (make-process :name "pygn-mode-server"
                        :buffer pygn-mode--server-buffer
                        :noquery t
                        :sentinel #'ignore
                        :command (list pygn-mode-python-executable
                                       "-u"
                                       (expand-file-name "pygn_server.py" pygn-mode-script-directory)
                                       "-")))))

(defun pygn-mode--server-kill ()
  "Stop the currently running `pygn-mode--server-process'."
  (when (pygn-mode--server-running-p)
    (process-send-eof pygn-mode--server-process)
    (delete-process pygn-mode--server-process)
    (setq pygn-mode--server-process nil)
    (message "pygn-mode server process killed.")))

(defun pygn-mode--server-send (message)
  "Send MESSAGE to the running `pygn-mode--server-process'."
  (unless (pygn-mode--server-running-p)
    (error "The pygn-mode server is not running -- cannot send a message"))
  (process-send-string
   pygn-mode--server-process
   (replace-regexp-in-string "[\n\r]*$" "\n" message)))

(defun pygn-mode--server-receive ()
  "Receive a response after `pygn-mode--server-send'.

Respects the variables `pygn-mode--server-receive-every-seconds' and
`pygn-mode--server-receive-max-seconds'."
  (unless (pygn-mode--server-running-p)
    (error "The pygn-mode server is not running -- cannot receive a response"))
  (unless (get-buffer pygn-mode--server-buffer)
    (error "The pygn-mode server output buffer does not exist -- cannot receive a response"))
  (with-current-buffer pygn-mode--server-buffer
    (erase-buffer)
    (let ((tries 0))
      (goto-char (point-min))
      (while (and (not (eq ?\n (char-before (point-max))))
                  (< (* tries pygn-mode--server-receive-every-seconds) pygn-mode--server-receive-max-seconds))
        (accept-process-output pygn-mode--server-process pygn-mode--server-receive-every-seconds nil 1)
        (cl-incf tries))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun pygn-mode--server-query (message &optional force)
  "Send MESSAGE to `pygn-mode--server-process', await, and return response.

SECONDS and MAX-TIME are as arguments to `pygn-mode--server-receive'.  FORCE
forces a new server process to be created."
  (unless (pygn-mode--server-running-p)
    (pygn-mode--server-start force))
  (pygn-mode--server-send message)
  (pygn-mode--server-receive))

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

(defun pygn-mode--send-board-and-fetch (command &optional pos)
  "Get PGN string preceding POS, send a `pygn-mode--server-process' request denoted by COMMAND, and return the response."
  (cl-callf or pos (point))
  (save-excursion
    (let ((pgn (buffer-substring-no-properties (pygn-mode-game-start-position) pos)))
      (setq pgn (replace-regexp-in-string "\n" "\\\\n" pgn))
      (pygn-mode--server-query (concat (symbol-name command) " -- " pgn)))))

(defun pygn-mode-fen-at-pos (pos)
  "Return the FEN corresponding to POS, which defaults to the point."
  (replace-regexp-in-string
   "\n+\\'" ""
   (pygn-mode--send-board-and-fetch :fen pos)))

(defun pygn-mode-board-at-pos (pos)
  "Get SVG output for PGN string preceding POS."
  (pygn-mode--send-board-and-fetch :board pos))

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
  (let ((syn (syntax-ppss font-lock-beg))
        (initial-beg font-lock-beg)
        (initial-end font-lock-end))
    (when (> 0 (nth 0 syn))
      (save-excursion
        (goto-char (nth 1 syn))
        (setq font-lock-beg (point))
        (forward-list 1)
        (setq font-lock-end (point)))
      (or (/= initial-beg font-lock-beg)
          (/= initial-end font-lock-end)))))

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
   ("^\\[\\(\\S-+\\)\\s-+\"[^\n]*\"\\]" 1 'pygn-mode-tagpair-key-face)
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
 (setq-local font-lock-multiline t)
 (setq-local font-lock-extend-after-change-region-function 'pygn-mode-after-change-function)
 (add-hook 'font-lock-extend-region-functions 'pygn-mode-font-lock-extend-region t t)
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

(defun pygn-mode--next-game-driver (arg)
  "Move point to next game, moving ARG games forward (backwards if negative).

Recenters buffer afterwards."
  (let ((next-game (and (re-search-forward "^\\[Event " nil t arg)
                        (goto-char (line-beginning-position)))))
    (recenter-window-group)
    (when (not next-game)
      (error "No next game")))
  (when (fboundp 'nav-flash-show)
    (nav-flash-show)))

;;; Interactive commands

;;;###autoload
(cl-defun pygn-mode-dependency-check ()
  "Open a buffer describing `pygn-mode' dependencies."
  (interactive)
  (let ((buf (get-buffer-create " *pygn-mode-dependency-check*"))
        (process-environment (cl-copy-list process-environment)))
    (with-current-buffer buf
      (erase-buffer)
      (display-buffer buf '(display-buffer-reuse-window))
      (when pygn-mode-pythonpath
        (setenv "PYTHONPATH" pygn-mode-pythonpath))
      (if (zerop (call-process pygn-mode-python-executable nil nil nil "-c" "pass"))
          (insert (format "[x] Good. We can execute the pygn-mode-python-executable at '%s'\n\n" pygn-mode-python-executable))
        ;; else
        (insert
         (format
          "[ ] Bad. We cannot execute the interpreter '%s'.  Try installing Python 3.5+ and/or customizing the value of pygn-mode-python-executable.\n\n"
          pygn-mode-python-executable))
        (cl-return-from pygn-mode-dependency-check))
      (if (zerop (call-process pygn-mode-python-executable nil nil nil "-c" "import sys; exit(0 if sys.hexversion >= 0x3000000 else 1)"))
          (insert (format "[x] Good. The pygn-mode-python-executable at '%s' is a Python 3 interpreter.\n\n" pygn-mode-python-executable))
        ;; else
        (insert
         (format
          "[ ] Bad. The executable '%s' is not a Python 3 interpreter.  Try installing Python 3.5+ and/or customizing the value of pygn-mode-python-executable.\n\n"
          pygn-mode-python-executable))
        (cl-return-from pygn-mode-dependency-check))
      (if (zerop (call-process pygn-mode-python-executable nil nil nil "-c" "import sys; exit(0 if sys.hexversion >= 0x3050000 else 1)"))
          (insert (format "[x] Good. The pygn-mode-python-executable at '%s' is better than or equal to Python version 3.5.\n\n" pygn-mode-python-executable))
        ;; else
        (insert
         (format
          "[ ] Bad. The executable '%s' is not at least Python version 3.5.  Try installing Python 3.5+ and/or customizing the value of pygn-mode-python-executable.\n\n"
          pygn-mode-python-executable))
        (cl-return-from pygn-mode-dependency-check))
      (if (zerop (call-process pygn-mode-python-executable nil nil nil "-c" "import chess"))
          (insert (format "[x] Good. The pygn-mode-python-executable at '%s' can import the python-chess library.\n\n" pygn-mode-python-executable))
        ;; else
        (insert
         (format
          "[ ] Bad. The executable '%s' cannot import the python-chess library.  Try installing python-chess, and/or customizing the value of pygn-mode-pythonpath.\n\n"
          pygn-mode-python-executable))
        (cl-return-from pygn-mode-dependency-check))
      (insert (format "------------------------------------\n\n"))
      (insert (format "All pygn-mode dependencies verified.\n")))))

(defun pygn-mode-next-game (arg)
  "Advance to the next game in a multi-game PGN buffer.

With numeric prefix ARG, advance ARG games."
  (interactive "p")
  (cl-callf or arg 1)
  (save-match-data
    (when (looking-at-p "\\[Event ")
      (goto-char (line-end-position)))
    (pygn-mode--next-game-driver arg)))

(defun pygn-mode-previous-game (arg)
  "Move back to the previous game in a multi-game PGN buffer.

With numeric prefix ARG, move back ARG games."
  (interactive "p")
  (cl-callf or arg 1)
  (save-match-data
    (unless (looking-at-p "\\[Event ")
      (re-search-backward "^\\[Event " nil t))
    (pygn-mode--next-game-driver (* arg -1))))

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
            (error "No more moves")))))))

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
          (error "No more moves"))
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
            (error "No more moves")))))))

(defun pygn-mode-select-game (pos)
  "Select current game in a multi-game PGN buffer.

When called non-interactively, select the game containing POS."
  (interactive "d")
  (goto-char pos)
  (push-mark (pygn-mode-game-end-position) t t)
  (goto-char (pygn-mode-game-start-position)))

(defun pygn-mode-echo-fen-at-point (pos &optional do-copy)
  "Display the FEN corresponding to the point in the echo area.

When called non-interactively, display the FEN corresponding to POS.

With prefix-arg DO-COPY, copy the FEN to the kill ring, and to the
system clipboard when running a GUI Emacs."
  (interactive "d\nP")
  (let ((fen (pygn-mode-fen-at-pos pos)))
    (when do-copy
      (kill-new fen)
      (when (and (fboundp 'gui-set-selection)
                 (display-graphic-p))
        (gui-set-selection 'CLIPBOARD fen)))
    (message "%s%s" fen (if do-copy (propertize "\t(copied)" 'face '(:foreground "grey33")) ""))))

(defun pygn-mode-display-fen-at-point (pos)
  "Display the FEN corresponding to the point in a separate buffer.

When called non-interactively, display the FEN corresponding to POS."
  (interactive "d")
  (let* ((fen (pygn-mode-fen-at-pos pos))
         (buf (get-buffer-create " *pygn-mode-fen*"))
         (win (get-buffer-window buf)))
    (with-current-buffer buf
      (erase-buffer)
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
      (setq cursor-type nil)
      (erase-buffer)
      (insert-image (create-image svg-data 'svg t)))
    (display-buffer buf '(display-buffer-reuse-window))
    (unless win
      (setq win (get-buffer-window buf))
      (set-window-dedicated-p win t)
      (resize-temp-buffer-window win))))

(defun pygn-mode-mouse-display-variation-gui-board (event)
  "Display the board corresponding to the mouse click in a separate buffer.

The board display respects variations."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (goto-char (posn-point (event-start event)))
  (save-excursion
    (skip-syntax-backward "^\\s-")
    (let ((pgn (pygn-mode-pgn-as-if-variation (point))))
      (with-temp-buffer
        (insert pgn)
        (pygn-mode-display-gui-board-at-point (point))))))

(defun pygn-mode-mouse-display-variation-gui-board-inclusive (event)
  "Display inclusive board corresponding to the mouse click in a separate buffer.

\"Inclusive\" here means that the board includes any move which contains the
click position.

The board display respects variations."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (goto-char (posn-point (event-start event)))
  (save-excursion
    (skip-syntax-forward "^\\s-")
    (skip-syntax-forward "\\s-")
    (let ((pgn (pygn-mode-pgn-as-if-variation (point))))
      (with-temp-buffer
        (insert pgn)
        (pygn-mode-display-gui-board-at-point (point))))))

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

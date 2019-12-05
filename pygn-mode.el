;;; pygn-mode.el --- Major-mode for chess PGN files, powered by Python -*- lexical-binding: t -*-
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
;;     GNU Emacs 25
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
  "A major-mode for chess PGN files, powered by Python."
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

(defcustom pygn-mode-flash-full-game nil
  "If non-nil, flash the entire PGN on game selection actions."
  :group 'pygn-mode
  :type 'boolean)

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

(defvar pygn-mode-fen-buffer-name "*pygn-mode-fen*"
  "Buffer name used to display FENs.")

(defvar pygn-mode-board-buffer-name "*pygn-mode-board*"
  "Buffer name used to display boards.")

(defvar pygn-mode-dependency-check-buffer-name "*pygn-mode-dependency-check*"
  "Buffer name used to display a dependency check.")

(defvar pygn-mode--server-process nil
  "Python-based server which powers many `pygn-mode' features.")

(defvar pygn-mode--server-buffer-name " *pygn-mode-server*"
  "Buffer name used to associate a server process.")

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
      '(menu-item "FEN at Point" pygn-mode-display-fen-at-point
                  :help "Display FEN at point in separate window"))
    (define-key map [menu-bar PyGN pygn-mode-display-board-at-point]
      '(menu-item "Board at Point" pygn-mode-display-board-at-point
                  :help "Display board at point in separate window"))

    ;; mouse
    (define-key map [mouse-2]        'pygn-mode-mouse-display-variation-board)
    (define-key map [double-mouse-2] 'pygn-mode-mouse-display-variation-board-inclusive)

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

(defun pygn-mode--opts-to-argparse (opt-plist)
  "Convert OPT-PLIST into an options string consumable by Python's argparse.

To produce a flag which takes no options, give a plist value of `t'."
  (let ((key-string nil)
        (val-string nil)
        (argparse-string ""))
    (cl-loop for (key value) on opt-plist by (function cddr)
             do (progn
                  (setq key-string
                        (replace-regexp-in-string
                         "^:" "-" (symbol-name key)))
                  (if (eq value t)
                      (setq argparse-string (concat
                                             argparse-string
                                             " " key-string))
                    ;; else
                    (setq val-string (shell-quote-argument
                                      (format "%s" value)))
                    (setq argparse-string (concat
                                           argparse-string
                                           (format " %s=%s" key-string val-string))))))
    argparse-string))

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
    (setenv "PYTHONIOENCODING" "UTF-8")
    (setq pygn-mode--server-buffer (get-buffer-create pygn-mode--server-buffer-name))
    (setq pygn-mode--server-process
          (make-process :name "pygn-mode-server"
                        :buffer pygn-mode--server-buffer
                        :noquery t
                        :sentinel #'ignore
                        :coding 'utf-8
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

(cl-defun pygn-mode--server-send (&key
                                  command
                                  options
                                  payload-type
                                  payload)
  "Send a message to the running `pygn-mode--server-process'.

The server request format is documented more completely at doc/server.md
in the source distribution for `pygn-mode'.

COMMAND should be a symbol such as :pgn-to-fen, which is a command
known by the server.  OPTIONS should be a plist such as (:pixels 400)
in which the keys correspond to argparse arguments known by the server.
PAYLOAD-ID should be a symbol such as :pgn, identifying the type of the
data payload, and PAYLOAD may contain arbitrary data."
  (unless (pygn-mode--server-running-p)
    (error "The pygn-mode server is not running -- cannot send a message"))
  (setq payload (replace-regexp-in-string "\n" "\\\\n" payload))
  (setq payload (replace-regexp-in-string "[\n\r]*$" "\n" payload))
  (process-send-string
   pygn-mode--server-process
   (mapconcat 'identity
              (list
               (symbol-name command)
               (pygn-mode--opts-to-argparse options)
               "--"
               (symbol-name payload-type)
               payload)
              " ")))

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

(cl-defun pygn-mode--server-query (&key
                                   command
                                   options
                                   payload-type
                                   payload
                                   force)
  "Send a request to `pygn-mode--server-process', await, and return the response.

COMMAND, OPTIONS, PAYLOAD-TYPE, and PAYLOAD are as documented at
`pygn-mode--server-send'.

FORCE forces a new server process to be created."
  (unless (pygn-mode--server-running-p)
    (pygn-mode--server-start force))
  (pygn-mode--server-send
   :command      command
   :options      options
   :payload-type payload-type
   :payload      payload)
  (pygn-mode--server-receive))

(defun pygn-mode--parse-response (response)
  "Parse RESPONSE string into a list of payload-id and payload."
  (save-match-data
    (setq response
          (replace-regexp-in-string
           "\n+\\'" ""
           response))
    (unless (string-match
             "\\`\\(:\\S-+\\)\\(.*\\)" response)
      (error "Bad response from `pygn-mode' server"))
    (list
     (intern (match-string 1 response))
     (replace-regexp-in-string
      "\\`\s-*" "" (match-string 2 response)))))

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
         (save-excursion (pygn-mode-backward-exit-variations-and-comments) (point))
         (point))
        (delete-region
         (save-excursion (forward-word-strictly -1) (point))
         (point)))
        (goto-char (point-max))
        (buffer-substring-no-properties (point-min) (point-max))))))

(cl-defun pygn-mode--send-pgn-and-fetch (&key
                                         command
                                         options
                                         pos)
  "Get PGN string preceding POS, send a `pygn-mode--server-process' request denoted by COMMAND, and return the response."
  (cl-callf or pos (point))
  (save-excursion
    (let ((pgn (buffer-substring-no-properties (pygn-mode-game-start-position) pos)))
      (pygn-mode--server-query
       :command       command
       :options       options
       :payload-type :pgn
       :payload       pgn))))

(defun pygn-mode-fen-at-pos (pos)
  "Return the FEN corresponding to POS, which defaults to the point."
  (let ((response (pygn-mode--send-pgn-and-fetch
                   :command :pgn-to-fen
                   :pos     pos)))
    (cl-callf pygn-mode--parse-response response)
    (unless (eq :fen (car response))
      (error "Bad response from `pygn-mode' server"))
    (cadr response)))

(defun pygn-mode-generate-board-at-pos (pos format)
  "Get board representation for PGN string preceding POS."
  (let ((response (pygn-mode--send-pgn-and-fetch
                   :command :pgn-to-board
                   :options `(
                              :pixels       ,pygn-mode-board-size
                              :board_format ,format
                             )
                   :pos     pos)))
    (cl-callf pygn-mode--parse-response response)
    (unless (memq (car response) '(:board-svg :board-text))
      (error "Bad response from `pygn-mode' server"))
    (cadr response)))

(defun pygn-mode-flash-game-at-point ()
  "Use nav-flash to highlight the current game at point."
  (when (fboundp 'nav-flash-show)
    (let ((nav-flash-delay 0.2)
          (beg (if pygn-mode-flash-full-game
                   (pygn-mode-game-start-position)
                 nil))
          (end (if pygn-mode-flash-full-game
                   (pygn-mode-game-end-position)
                 nil)))
      (nav-flash-show beg end))))

;;; Font-lock

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
 "A major-mode for chess PGN files, powered by Python."
 :syntax-table pygn-mode-syntax-table
 :group 'pygn-mode
 (setq-local comment-start "{")
 (setq-local comment-end "}")
 (setq-local comment-continue " ")
 (setq-local comment-multi-line t)
 (setq-local comment-style 'plain)
 (setq-local comment-use-syntax t)
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

;;; Minor-mode definition

;; todo: support inclusive mode
;; todo: support TUI mode
(define-minor-mode pygn-mode-follow-minor-mode
  "Minor mode for pygn-mode.

With a prefix argument ARG, enable mode if ARG is positive, and
disable it otherwise.  If called from Lisp, enable mode if ARG is
omitted or nil.

When turned on, cursor motion in a PyGN buffer causes automatic
display of a GUI board corresponding to the point.  The displayed
board will respect variations."
  :group 'pygn-mode
  :init-value nil
  :lighter " fol"
  (if pygn-mode-follow-minor-mode
      (progn
        (pygn-mode-follow-mode-post-command-hook)
        (add-hook 'post-command-hook #'pygn-mode-follow-mode-post-command-hook nil t))
    (remove-hook 'post-command-hook #'pygn-mode-follow-mode-post-command-hook t)
    (let ((win (get-buffer-window (get-buffer pygn-mode-board-buffer-name))))
      (when (window-live-p win)
        (delete-window win)))))

(defun pygn-mode-follow-mode-post-command-hook ()
  "Driver for `pygn-mode-follow-minor-mode'.

Intended for use in `post-command-hook'."
  (pygn-mode-display-variation-gui-board-at-point (point)))

(defun pygn-mode--next-game-driver (arg)
  "Move point to next game, moving ARG games forward (backwards if negative).

Recenters buffer afterwards."
  (save-match-data
    (let ((next-game (and (re-search-forward "^\\[Event " nil t arg)
                          (goto-char (line-beginning-position)))))
      (recenter-window-group)
      (when (not next-game)
        (error "No next game")))
    (pygn-mode-flash-game-at-point)))

;;; Interactive commands

;;;###autoload
(cl-defun pygn-mode-dependency-check ()
  "Open a buffer describing `pygn-mode' dependencies."
  (interactive)
  (let ((buf (get-buffer-create pygn-mode-dependency-check-buffer-name))
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
  (when (looking-at-p "\\[Event ")
    (goto-char (line-end-position)))
  (pygn-mode--next-game-driver arg))

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
         (buf (get-buffer-create pygn-mode-fen-buffer-name))
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

;; interactive helper
(defun pygn-mode--save-gui-board-at-point (pos)
  "Save the board image corresponding to POS to a file."
  (let* ((pygn-mode-board-size (completing-read "Pixels per side: " nil nil nil nil nil pygn-mode-board-size))
         (filename (read-file-name "SVG filename: "))
         (svg-data (pygn-mode-generate-board-at-pos pos 'svg)))
    (with-temp-buffer
      (insert svg-data)
      (write-file filename))))

;; todo the save feature should be part of pygn-mode-display-board-at-point
(defun pygn-mode-display-gui-board-at-point (pos &optional arg)
  "Display a GUI board corresponding to the point in a separate buffer.

When called non-interactively, display the board corresponding to POS.

With optional universal prefix ARG, write the image to a file, prompting
for image size."
  (interactive "d\nP")
  (if arg
      (pygn-mode--save-gui-board-at-point pos)
    ;; else
    (let* ((svg-data (pygn-mode-generate-board-at-pos pos 'svg))
           (buf (get-buffer-create pygn-mode-board-buffer-name))
           (win (get-buffer-window buf)))
      (with-current-buffer buf
        (setq cursor-type nil)
        (erase-buffer)
        (insert-image (create-image svg-data 'svg t)))
      (display-buffer buf '(display-buffer-reuse-window))
      (unless win
        (setq win (get-buffer-window buf))
        (set-window-dedicated-p win t)
        (resize-temp-buffer-window win)))))

(defun pygn-mode-display-text-board-at-point (pos)
  "Display a text board corresponding to the point in a separate buffer.

When called non-interactively, display the board corresponding to POS."
  (interactive "d")
  (let* ((text-data (pygn-mode-generate-board-at-pos pos 'text))
         (buf (get-buffer-create pygn-mode-board-buffer-name))
         (win (get-buffer-window buf)))
    (with-current-buffer buf
      (erase-buffer)
      (insert (replace-regexp-in-string
               "\\\\n" "\n"
               text-data))
      (goto-char (point-min)))
    (display-buffer buf '(display-buffer-reuse-window))
    (unless win
      (setq win (get-buffer-window buf))
      (set-window-dedicated-p win t)
      (resize-temp-buffer-window win))))

(defun pygn-mode-display-board-at-point (pos &optional format)
  "Display a board corresponding to the point in a separate buffer.

When called non-interactively, display the board corresponding to POS.
FORMAT may be 'svg or 'text, and if nil will be determined based on
`display-graphic-p'."
  (interactive "d")
  (setq format (or format (if (display-graphic-p) 'svg 'text)))
  (cond
    ((eq format 'svg)
     (pygn-mode-display-gui-board-at-point pos))
    ((eq format 'text)
     (pygn-mode-display-text-board-at-point pos))
    (t
     (error "Bad board format: %s" format))))

(defun pygn-mode-mouse-display-variation-board (event)
  "Display the board corresponding to the mouse click in a separate buffer.

The board display respects variations."
  (interactive "@e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (goto-char (posn-point (event-start event)))
  (save-excursion
    (skip-syntax-backward "^\\s-")
    (let ((pgn (pygn-mode-pgn-as-if-variation (point))))
      (with-temp-buffer
        (insert pgn)
        (pygn-mode-display-board-at-point (point))))))

(defun pygn-mode-mouse-display-variation-board-inclusive (event)
  "Display inclusive board corresponding to the mouse click in a separate buffer.

\"Inclusive\" here means that the board includes any move which contains the
click position.

The board display respects variations."
  (interactive "@e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (goto-char (posn-point (event-start event)))
  (save-excursion
    (skip-syntax-forward "^\\s-")
    (skip-syntax-forward "\\s-")
    (let ((pgn (pygn-mode-pgn-as-if-variation (point))))
      (with-temp-buffer
        (insert pgn)
        (pygn-mode-display-board-at-point (point))))))

(defun pygn-mode-display-variation-board-at-point (pos)
  "Respecting variations, display the board corresponding to the point.

When called non-interactively, display the board corresponding to POS."
  (interactive "d")
  (let ((pgn (pygn-mode-pgn-as-if-variation pos)))
    (with-temp-buffer
      (insert pgn)
      (pygn-mode-display-board-at-point (point-max)))))

(defun pygn-mode-previous-move-follow-board (arg)
  "Move back to the previous move and display the updated board.

With numeric prefix ARG, move ARG moves backward."
  (interactive "p")
  (pygn-mode-previous-move arg)
  (pygn-mode-display-board-at-point (point)))

(defun pygn-mode-next-move-follow-board (arg)
  "Advance to the next move and display the updated board.

With numeric prefix ARG, move ARG moves forward."
  (interactive "p")
  (pygn-mode-next-move arg)
  (pygn-mode-display-board-at-point (point)))

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

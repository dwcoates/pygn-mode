;;; uci-mode.el --- Major-mode for chess engine interaction -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019 Dodge Coates and Roland Walker
;;
;; Author: Dodge Coates and Roland Walker
;; Homepage: http://github.com/dwcoates/pygn-mode
;; URL: http://raw.github.com/dwcoates/pygn-mode/master/uci-mode.el
;; Version: 0.5.0
;; Last-Updated: 11 Dec 2019
;; Package-Requires: ((emacs "24.3"))
;; Keywords: data, games, chess
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'uci-mode)
;;     M-x uci-mode-run-engine
;;
;; Explanation
;;
;;     Uci-mode is a comint-derived major-mode for interacting directly with
;;     a UCI chess engine.  Direct UCI interaction is interesting for
;;     programmers who are developing chess engines, or advanced players who
;;     are doing deep analysis on games.  This mode is not useful for simply
;;     playing chess.
;;
;; See Also
;;
;;     M-x customize-group RET uci-mode RET
;;
;;     M-x customize-group RET comint RET
;;
;;     http://github.com/dwcoates/pygn-mode
;;
;;     http://wbec-ridderkerk.nl/html/UCIProtocol.html
;;
;; Notes
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.3 or higher
;;
;; Bugs
;;
;; TODO
;;
;;     Move to separate repository
;;      - change homepage/URL in header
;;      - change url-link in defgroup
;;
;;     Write README.md in separate repository
;;      - features
;;        - font-lock
;;        - persistent history
;;        - pygn-mode integration
;;      - triple-window screenshot
;;      - instructions for engine-over-SSH
;;
;; IDEA
;;
;;     Completions
;;
;;     Support multiple simultaneous engines
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

(defconst uci-mode-version "0.5.0")

;;; Imports

(require 'comint)

;;; Declarations

;;; Customizable variables

;;;###autoload
(defgroup uci-mode nil
  "Major-mode for chess engine interaction."
  :version uci-mode-version
  :link '(url-link :tag "Github" "http://github.com/dwcoates/pygn-mode")
  :prefix "uci-mode-"
  :group 'data
  :group 'games)

(defcustom uci-mode-engine-command '("stockfish")
  "Command to run a UCI chess engine, given as a list.

The first element in the list should be an executable.  Optional
additional elements are arguments to the executable.  The list
form allows access to remote engines over SSH."
  :group 'uci-mode
  :type '(repeat string))

(defcustom uci-mode-engine-setoptions
  '("setoption name MultiPV value 1")
  "List of UCI \"setoption\" commands to issue at engine startup."
  :group 'uci-mode
  :type '(repeat string))

(defcustom uci-mode-command-history-file
  (expand-file-name "uci-mode-history.txt" user-emacs-directory)
  "Filename to store persistent `uci-mode' command history."
  :group 'uci-mode
  :type 'string)

;;;###autoload
(defgroup uci-mode-faces nil
  "Faces used by uci-mode."
  :group 'uci-mode)

(defface uci-mode-depth-face
   '((t (:inherit font-lock-variable-name-face)))
  "uci-mode face for depth in info output."
  :group 'uci-mode-faces)

(defface uci-mode-multipv-1-face
   '((t (:inherit font-lock-variable-name-face)))
  "uci-mode face for MultiPV 1 in info output."
  :group 'uci-mode-faces)

(defface uci-mode-score-face
   '((t (:inherit font-lock-variable-name-face)))
  "uci-mode face for score in info output."
  :group 'uci-mode-faces)

(defface uci-mode-pv-face
   '((t (:inherit font-lock-string-face)))
  "uci-mode face for pv in info output."
  :group 'uci-mode-faces)

(defface uci-mode-option-name-face
   '((t (:inherit font-lock-string-face)))
  "uci-mode face for option names in uci response output."
  :group 'uci-mode-faces)

(defface uci-mode-finished-face
   '((t (:inherit font-lock-builtin-face)))
  "uci-mode face for various final-line outputs."
  :group 'uci-mode-faces)

;;; Variables

(defvar uci-mode-engine-buffer nil
  "The current `uci-mode' engine process buffer.")

(defvar uci-mode-engine-buffer-name "*UCI*"
  "The name of the `uci-mode' engine process buffer.")

(defvar uci-mode-engine-process-name "UCI"
  "The name of the `uci-mode' engine process.")

;;; Syntax table

(defvar uci-mode-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (with-syntax-table st
    st))
  "Syntax table for `uci-mode'.")

;;; Keymaps

(defvar uci-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-c") 'uci-mode-send-stop)
    map)
  "Keymap for `uci-mode'.")

;;; Utility functions

(defun uci-mode-make-comint (command)
  "Create a UCI engine comint buffer.

COMMAND is the engine command to be executed."
  (unless (comint-check-proc uci-mode-engine-buffer-name)
    (let* ((buf (apply
                 #'make-comint-in-buffer
                 uci-mode-engine-process-name
                 uci-mode-engine-buffer-name
                 (car command)
                 nil
                 (cdr command)))
           (proc (get-buffer-process buf)))
      (sleep-for 0.20)
      (process-put proc 'command-basename (file-name-base (car (reverse command))))
      (process-put proc 'command-full command)
      (with-current-buffer buf
        (uci-mode))
      (set-process-query-on-exit-flag proc nil)
      (setq uci-mode-engine-buffer (get-buffer uci-mode-engine-buffer-name))
      (uci-mode-send-setoptions)))
  (display-buffer uci-mode-engine-buffer-name)
  (set-window-scroll-bars
   (get-buffer-window uci-mode-engine-buffer-name) nil nil nil 'bottom)
  uci-mode-engine-buffer)

(defun uci-mode-engine-proc ()
  "Return the `uci-mode' inferior engine process."
  (let ((proc (get-buffer-process
                 uci-mode-engine-buffer)))
    (unless (process-live-p proc)
      (error "No UCI engine process. Try `uci-mode-run-engine' or `uci-mode-restart-engine'"))
    proc))

(defun uci-mode-send-commands (commands)
  "Send COMMANDS (a list of strings) to a running UCI engine."
  (unless uci-mode-engine-buffer
    (error "No UCI engine buffer. Try `uci-mode-run-engine' or `uci-mode-restart-engine'"))
  (unless (process-live-p
           (get-buffer-process uci-mode-engine-buffer))
    (error "No UCI engine process. Try `uci-mode-run-engine' or `uci-mode-restart-engine'"))
  (with-current-buffer uci-mode-engine-buffer
    (dolist (cmd commands)
      (sleep-for 0.05)
      (goto-char (point-max))
      (insert (replace-regexp-in-string "\n+\\'" "" cmd))
      (comint-send-input nil t))))

;; buglet: some specified chatter lines still get through the filter,
;; presumably due to buffering
(defun uci-mode-preoutput-reduce-chatter (str)
  (replace-regexp-in-string
   ;; Komodo
   "^info [^\n]*\\<\\(?:nodes\\|nps\\) [0-9]+\n" ""
   (replace-regexp-in-string
    ;; Stockfish
    "^info [^\n]*\\<\\(?:upperbound\\|lowerbound\\) [^\n]*\n" ""
    (replace-regexp-in-string
     ;; Stockfish
     "^info [^\n]*\\<currmovenumber [0-9]+\n" ""
     str))))

;;; Font-lock

(font-lock-add-keywords
 'uci-mode
 '(
   ;; depth
   ("^info[^\n]*\\s-+\\(depth\\s-+[0-9]+\\)" 1 'uci-mode-depth-face)
   ;; multipv 1
   ("^info[^\n]*\\s-+\\(multipv\\s-+1\\)\\s-" 1 'uci-mode-multipv-1-face)
   ;; score
   ("^info[^\n]*\\s-+\\(\\(?:cp\\|mate\\)\\s-+\\S-+\\)" 1 'uci-mode-score-face)
   ;; pv
   ("^info[^\n]*\\s-+\\(pv\\s-[^\n]+\\)" 1 'uci-mode-pv-face)
   ;; option names
   ("^option\\s-+name\\s-+\\([^\n]*?\\)\\s-+type" 1 'uci-mode-option-name-face)
   ;; finishing strings (a help in the absence of a prompt)
   ("^\\(bestmove\\|uciok\\|readyok\\)[^\n]*" 0 'uci-mode-finished-face)))

;;; Major-mode definition

;;;###autoload
(define-derived-mode uci-mode comint-mode "UCI"
  "Major-mode for chess engine interaction.

Runs a UCI-compatible chess engine as a subprocess of Emacs."
  :syntax-table uci-mode-syntax-table
  :group 'uci-mode
  (setq-local truncate-lines t)
  (setq-local comint-input-ring-file-name uci-mode-command-history-file)
  (comint-read-input-ring 'silent)
  (add-hook 'kill-buffer-hook 'comint-write-input-ring t t)
  (add-hook 'comint-preoutput-filter-functions 'uci-mode-preoutput-reduce-chatter t t)
  (setq-local comint-use-prompt-regexp nil)
  (setq-local comint-input-ignoredups t)
  (setq-local mode-line-process '(":%s"))
  (setq-local comint-output-filter-functions
              '(
                comint-postoutput-scroll-to-bottom
                comint-truncate-buffer
                ))
  ;; modeline
  (when (process-get (get-buffer-process (current-buffer)) 'command-basename)
    (setq-local
     mode-line-buffer-identification
     (propertize
      (process-get (get-buffer-process (current-buffer)) 'command-basename)
      'help-echo (mapconcat
                  #'identity
                  (process-get (get-buffer-process (current-buffer)) 'command-full)
                  " "))))
  (buffer-disable-undo)
  (font-lock-ensure))

;;; Interactive commands

;;;###autoload
(defun uci-mode-run-engine (&optional command)
  "Run an inferior UCI engine process.

COMMAND defaults to `uci-mode-engine-command'.  When called
interactively with universal prefix-arg, the user may edit the
command.  When called with two universal prefix-args, the
user may enter a multi-word command which is split using
`split-string-and-unquote'."
  (interactive "P")
  (setq command (cond
                  ((equal command '(4))
                   (list (read-shell-command
                          "Engine: "
                          (car (reverse uci-mode-engine-command)))))
                  ((equal command '(16))
                   (split-string-and-unquote
                    (read-shell-command
                     "Engine (split): "
                     (mapconcat #'identity uci-mode-engine-command " "))))
                  ((and command (listp command))
                   command)
                  (t
                   uci-mode-engine-command)))
  (get-buffer-process
   (uci-mode-make-comint command)))

(defun uci-mode-restart-engine (&optional command)
  "Restart or replace an inferior UCI engine process.

COMMAND defaults to the path of the currently running engine, or
`uci-mode-engine-command' when that information is not available.
When called interactively with a universal prefix-arg, the user may
edit the command.  When called with two universal prefix-args, the
user may enter a multi-word command which is split using
`split-string-and-unquote'.

When no engine is running, this is equivalent to `uci-mode-run-engine'."
  (interactive "P")
  (let* ((proc (ignore-errors (uci-mode-engine-proc)))
         (command-full (and proc (process-get proc 'command-full))))
    (setq command (cond
                    ((equal command '(4))
                     (list (read-shell-command
                            "Engine: "
                            (car (reverse (or command-full uci-mode-engine-command))))))
                    ((equal command '(16))
                     (split-string-and-unquote
                      (read-shell-command
                       "Engine (split): "
                       (mapconcat #'identity (or command-full uci-mode-engine-command) " "))))
                    ((and command (listp command))
                      command)
                    (t
                     (or command-full uci-mode-engine-command))))
    (when proc
      (delete-process proc))
  (uci-mode-run-engine command)))

(defun uci-mode-send-stop ()
  "Send a \"stop\" message to the UCI engine, without echoing."
  (interactive)
  (let ((proc (uci-mode-engine-proc)))
    (comint-send-string proc "stop\n")))

(defun uci-mode-send-setoptions ()
  "Send `uci-mode-engine-setoptions' to a running UCI engine."
  (interactive)
  (uci-mode-send-commands uci-mode-engine-setoptions))

(provide 'uci-mode)

;;
;; Emacs
;;
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: ARGS alist devel
;;

;;; uci-mode.el ends here

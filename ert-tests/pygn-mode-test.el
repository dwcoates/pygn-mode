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

;; not move number in the chess sense, but ply
(setq pygn-mode-test-01-move-start-positions '(( 1 . 181)
                                               ( 2 . 186)
                                               ( 3 . 277)
                                               ( 4 . 282)
                                               ( 5 . 455)
                                               ( 6 . 459)
                                               ( 7 . 612)
                                               ( 8 . 616)
                                               ( 9 . 795)
                                               (10 . 800)))

;; not move number in the chess sense, but ply
(setq pygn-mode-test-02-move-start-positions '(( 1 .  268)
                                               ( 2 .  271)
                                               ( 3 .  278)
                                               ( 4 .  281)
                                               ( 5 .  287)
                                               ( 6 .  291)
                                               ( 7 .  298)
                                               ( 8 .  301)
                                               ( 9 .  307)
                                               (10 .  310)
                                               (11 .  317)
                                               (12 .  320)
                                               (13 .  328)
                                               (14 .  333)
                                               (15 .  339)
                                               (16 .  613)
                                               (17 .  621)
                                               (18 .  626)
                                               (19 .  635)
                                               (20 .  641)
                                               (21 .  650)
                                               (22 .  748)
                                               (23 . 1100)
                                               (24 . 1171)
                                               (25 . 1181)
                                               (26 . 1186)
                                               (27 . 1235)
                                               (28 . 1452)
                                               (29 . 1576)
                                               (30 . 1739)
                                               (31 . 1747)
                                               (32 . 2209)
                                               (33 . 2217)
                                               (34 . 2404)
                                               (35 . 2413)
                                               (36 . 2418)
                                               (37 . 2514)
                                               (38 . 2518)
                                               (39 . 2525)
                                               (40 . 2684)
                                               (41 . 2692)
                                               (42 . 2697)
                                               (43 . 2706)
                                               (44 . 2711)
                                               (45 . 2800)
                                               (46 . 2861)
                                               (47 . 2868)
                                               (48 . 3052)
                                               (49 . 3061)
                                               (50 . 3065)
                                               (51 . 3177)
                                               (52 . 3181)
                                               (53 . 3190)
                                               (54 . 3195)
                                               (55 . 3203)
                                               (56 . 3206)
                                               (57 . 3277)
                                               (58 . 3280)
                                               (59 . 3287)
                                               (60 . 3292)
                                               (61 . 3301)
                                               (62 . 3396)
                                               (63 . 3404)
                                               (64 . 3408)
                                               (65 . 3417)
                                               (66 . 3482)
                                               (67 . 3917)
                                               (68 . 3921)
                                               (69 . 3929)
                                               (70 . 3933)
                                               (71 . 3941)
                                               (72 . 3945)
                                               (73 . 4041)
                                               (74 . 4045)
                                               (75 . 4053)
                                               (76 . 4057)
                                               (77 . 4065)
                                               (78 . 4069)
                                               (79 . 4111)
                                               (80 . 4115)
                                               (81 . 4123)
                                               (82 . 4127)
                                               (83 . 4135)
                                               (84 . 4139)
                                               (85 . 4171)
                                               (86 . 4300)
                                               (87 . 4308)
                                               (88 . 4312)
                                               (89 . 4468)
                                               (90 . 4538)
                                               (91 . 4546)
                                               (92 . 4550)
                                               (93 . 4603)
                                               (94 . 4676)))

(defmacro pygn-mode-test-with-file (filename &rest body)
  "Evaluate BODY in a `pygn-mode' temp buffer filled with the contents of FILENAME."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((coding-system-for-read 'utf-8))
       (insert-file-contents
        (expand-file-name ,filename pygn-mode-test-input-directory)))
     (goto-char (point-min))
     (pygn-mode)
     ,@body))

;;; pygn-mode-pgn-at-pos

;; TODO: Ideally, PGN-at-pos tests would cover every position in this and
;; several other input files.
(ert-deftest pygn-mode-pgn-at-pos-01 nil
  "Test `pygn-mode-pgn-at-pos' from the first position (a corner case)."
  (pygn-mode-test-with-file "test-01.pgn"
    (should (equal
             "[Event \"?\"]\n"
             (pygn-mode-pgn-at-pos (point-min))))))

;;; pygn-mode-next-move

(ert-deftest pygn-mode-next-move-01 nil
  "Test `pygn-mode-next-move' from `point-min' to first move."
  (pygn-mode-test-with-file "test-01.pgn"
    (pygn-mode-next-move)
    (should (= (point)
               (cdr (assoc 1 pygn-mode-test-01-move-start-positions))))))

(ert-deftest pygn-mode-next-move-02 nil
  "Test two successive calls to `pygn-mode-next-move'."
  (pygn-mode-test-with-file "test-01.pgn"
    (pygn-mode-next-move)
    (pygn-mode-next-move)
    (should (= (point)
               (cdr (assoc 2 pygn-mode-test-01-move-start-positions))))))

(ert-deftest pygn-mode-next-move-03 nil
  "Test integer ARG to `pygn-mode-next-move'."
  (pygn-mode-test-with-file "test-01.pgn"
    (pygn-mode-next-move 2)
    (should (= (point)
               (cdr (assoc 2 pygn-mode-test-01-move-start-positions))))))

(ert-deftest pygn-mode-next-move-04 nil
  "Test negative integer ARG to `pygn-mode-next-move'."
  (pygn-mode-test-with-file "test-01.pgn"
    (pygn-mode-next-move 2)
    (pygn-mode-next-move -1)
    (should (= (point)
               (cdr (assoc 1 pygn-mode-test-01-move-start-positions))))))

(ert-deftest pygn-mode-next-move-05 nil
  "Test `pygn-mode-next-move' for every ARG which leads to a move (test-01.pgn)."
  (pygn-mode-test-with-file "test-01.pgn"
    (dolist (cell pygn-mode-test-01-move-start-positions)
      (let ((moves    (car cell))
            (move-pos (cdr cell)))
        (goto-char (point-min))
        (pygn-mode-next-move moves)
        (should (= (point) move-pos))))))

(ert-deftest pygn-mode-next-move-06 nil
  "Test `pygn-mode-next-move' exhaustively, from every position in the buffer which precedes a move (test-01.pgn)."
  (pygn-mode-test-with-file "test-01.pgn"
    (let ((last-pos (point-min)))
      (dolist (cell pygn-mode-test-01-move-start-positions)
        (let* ((moves             (car cell))
               (move-pos          (cdr cell))
               (leading-positions (number-sequence last-pos (1- move-pos))))
          (dolist (pos leading-positions)
            (goto-char pos)
            (pygn-mode-next-move)
            (should (= (point) move-pos)))
          (setq last-pos move-pos))))))

(ert-deftest pygn-mode-next-move-07 nil
  "Test `pygn-mode-next-move' for every ARG which leads to a move (test-02.pgn)."
  (pygn-mode-test-with-file "test-02.pgn"
    (dolist (cell pygn-mode-test-02-move-start-positions)
      (let ((moves    (car cell))
            (move-pos (cdr cell)))
        (goto-char (point-min))
        (pygn-mode-next-move moves)
        (should (= (point) move-pos))))))

(ert-deftest pygn-mode-next-move-08 nil
  "Test `pygn-mode-next-move' exhaustively, from every position in the buffer which precedes a move (test-02.pgn)."
  (pygn-mode-test-with-file "test-02.pgn"
    (let ((last-pos (point-min)))
      (dolist (cell pygn-mode-test-02-move-start-positions)
        (let* ((moves             (car cell))
               (move-pos          (cdr cell))
               (leading-positions (number-sequence last-pos (1- move-pos))))
          (dolist (pos leading-positions)
            (goto-char pos)
            (pygn-mode-next-move)
            (should (= (point) move-pos)))
          (setq last-pos move-pos))))))

(ert-deftest pygn-mode-next-move-09 nil
  "Test that `pygn-mode-next-move' errors from the last move."
  (pygn-mode-test-with-file "test-01.pgn"
    (let* ((cell     (car (last pygn-mode-test-01-move-start-positions)))
           (moves    (car cell))
           (move-pos (cdr cell)))
      (pygn-mode-next-move moves)
      (should-error (pygn-mode-next-move)))))

(ert-deftest pygn-mode-next-move-10 nil
  "Test that `pygn-mode-next-move' errors from every position after the last move."
  (pygn-mode-test-with-file "test-01.pgn"
    (let* ((cell     (car (last pygn-mode-test-01-move-start-positions)))
           (moves    (car cell))
           (move-pos (cdr cell))
           (trailing-positions (number-sequence move-pos (point-max))))
      (dolist (pos trailing-positions)
        (goto-char pos)
        (should-error (pygn-mode-next-move))))))

;; TODO pygn-mode-next-move behavior when between games

;;; pygn-mode-previous-move

(ert-deftest pygn-mode-previous-move-01 nil
  "Test `pygn-mode-previous-move' from second move to first move."
  (pygn-mode-test-with-file "test-01.pgn"
    (pygn-mode-next-move 2)
    (pygn-mode-previous-move)
    (should (= (point)
               (cdr (assoc 1 pygn-mode-test-01-move-start-positions))))
    (should (looking-at-p "Qe8\\+\\>"))))

(ert-deftest pygn-mode-previous-move-02 nil
  "Test integer ARG to `pygn-mode-previous-move'."
  (pygn-mode-test-with-file "test-01.pgn"
    (pygn-mode-next-move 3)
    (pygn-mode-previous-move 2)
    (should (= (point)
               (cdr (assoc 1 pygn-mode-test-01-move-start-positions))))))

(ert-deftest pygn-mode-previous-move-03 nil
  "Test negative ARG to `pygn-mode-previous-move', which should advance the position."
  (pygn-mode-test-with-file "test-01.pgn"
    (pygn-mode-previous-move -2)
    (should (= (point)
               (cdr (assoc 2 pygn-mode-test-01-move-start-positions))))))

(ert-deftest pygn-mode-previous-move-04 nil
  "Test `pygn-mode-previous-move' for every ARG which leads to a move (test-01.pgn)."
  (pygn-mode-test-with-file "test-01.pgn"
    (dolist (cell (reverse pygn-mode-test-01-move-start-positions))
      (let ((moves    (car cell))
            (move-pos (cdr cell))
            (rescaler (1+ (length pygn-mode-test-01-move-start-positions))))
        (setq moves (- rescaler moves))
        (goto-char (point-max))
        (pygn-mode-previous-move moves)
        (should (= (point) move-pos))))))

(ert-deftest pygn-mode-previous-move-05 nil
  "Test `pygn-mode-previous-move' exhaustively, from every position in the buffer which follows a move (test-01.pgn)."
  (pygn-mode-test-with-file "test-01.pgn"
    (let ((last-pos (point-max)))
      (dolist (cell (reverse pygn-mode-test-01-move-start-positions))
        (let* ((moves              (car cell))
               (move-pos           (cdr cell))
               (trailing-positions (number-sequence last-pos (1+ move-pos) -1)))
          (dolist (pos trailing-positions)
            (goto-char pos)
            (pygn-mode-previous-move)
            (should (= (point) move-pos)))
          (setq last-pos move-pos))))))

(ert-deftest pygn-mode-previous-move-06 nil
  "Test `pygn-mode-previous-move' for every ARG which leads to a move (test-02.pgn)."
  (pygn-mode-test-with-file "test-02.pgn"
    (dolist (cell (reverse pygn-mode-test-02-move-start-positions))
      (let ((moves    (car cell))
            (move-pos (cdr cell))
            (rescaler (1+ (length pygn-mode-test-02-move-start-positions))))
        (setq moves (- rescaler moves))
        (goto-char (point-max))
        (pygn-mode-previous-move moves)
        (should (= (point) move-pos))))))

(ert-deftest pygn-mode-previous-move-07 nil
  "Test `pygn-mode-previous-move' exhaustively, from every position in the buffer which follows a move (test-02.pgn)."
  (pygn-mode-test-with-file "test-02.pgn"
    (let ((last-pos (point-max)))
      (dolist (cell (reverse pygn-mode-test-02-move-start-positions))
        (let* ((moves              (car cell))
               (move-pos           (cdr cell))
               (trailing-positions (number-sequence last-pos (1+ move-pos) -1)))
          (dolist (pos trailing-positions)
            (goto-char pos)
            (pygn-mode-previous-move)
            (should (= (point) move-pos)))
          (setq last-pos move-pos))))))

(ert-deftest pygn-mode-previous-move-08 nil
  "Test that `pygn-mode-previous-move' errors from the first move."
  (pygn-mode-test-with-file "test-01.pgn"
    (pygn-mode-next-move)
    (should-error (pygn-mode-previous-move))))

(ert-deftest pygn-mode-previous-move-09 nil
  "Test that `pygn-mode-previous-move' errors from every position before the first move."
  (pygn-mode-test-with-file "test-01.pgn"
    (let* ((cell     (car pygn-mode-test-01-move-start-positions))
           (moves    (car cell))
           (move-pos (cdr cell))
           (leading-positions (number-sequence (point-min) move-pos)))
      (dolist (pos leading-positions)
        (goto-char pos)
        (should-error (pygn-mode-previous-move))))))

;; TODO pygn-mode-previous-move behavior when between games

;;
;; Emacs
;;
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;

;;; pygn-mode-test.el ends here

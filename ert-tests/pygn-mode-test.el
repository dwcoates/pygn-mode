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
(setq pygn-mode-test-01-move-after-positions '(( 1 . 185)
                                               ( 2 . 189)
                                               ( 3 . 281)
                                               ( 4 . 286)
                                               ( 5 . 458)
                                               ( 6 . 462)
                                               ( 7 . 615)
                                               ( 8 . 620)
                                               ( 9 . 799)
                                               (10 . 802)))

;; not move number in the chess sense, but ply
;; the fen is inclusive of the move itself
(setq pygn-mode-test-01-move-fens '(( 1 . "r1bRQ3/ppp3pp/4p1k1/2b1P1n1/1np1q3/5N2/PPP3PP/5R1K b - - 3 18")
                                    ( 2 . "r1bRQ3/ppp3pp/4p2k/2b1P1n1/1np1q3/5N2/PPP3PP/5R1K w - - 4 19" )
                                    ( 3 . "r1bRQ3/ppp3pp/4p2k/2b1P1N1/1np1q3/8/PPP3PP/5R1K b - - 0 19"   )
                                    ( 4 . "r1bRQ3/ppp3pp/4p3/2b1P1k1/1np1q3/8/PPP3PP/5R1K w - - 0 20"    )
                                    ( 5 . "r1bR4/ppp2Qpp/4p3/2b1P1k1/1np1q3/8/PPP3PP/5R1K b - - 1 20"    )
                                    ( 6 . "r1bR4/ppp2Qpp/4p1q1/2b1P1k1/1np5/8/PPP3PP/5R1K w - - 2 21"    )
                                    ( 7 . "r1b3R1/ppp2Qpp/4p1q1/2b1P1k1/1np5/8/PPP3PP/5R1K b - - 3 21"   )
                                    ( 8 . "r1b3R1/ppp2qpp/4p3/2b1P1k1/1np5/8/PPP3PP/5R1K w - - 0 22"     )
                                    ( 9 . "r1b3R1/ppp2Rpp/4p3/2b1P1k1/1np5/8/PPP3PP/7K b - - 0 22"       )
                                    (10 . "r1b3R1/ppp2R1p/4p1p1/2b1P1k1/1np5/8/PPP3PP/7K w - - 0 23"     )))

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

;; not move number in the chess sense, but ply
(setq pygn-mode-test-02-move-after-positions '(( 1 .  270)
                                               ( 2 .  274)
                                               ( 3 .  280)
                                               ( 4 .  283)
                                               ( 5 .  290)
                                               ( 6 .  294)
                                               ( 7 .  300)
                                               ( 8 .  303)
                                               ( 9 .  309)
                                               (10 .  313)
                                               (11 .  319)
                                               (12 .  324)
                                               (13 .  332)
                                               (14 .  335)
                                               (15 .  341)
                                               (16 .  617)
                                               (17 .  625)
                                               (18 .  630)
                                               (19 .  640)
                                               (20 .  645)
                                               (21 .  653)
                                               (22 .  751)
                                               (23 . 1103)
                                               (24 . 1176)
                                               (25 . 1185)
                                               (26 . 1189)
                                               (27 . 1238)
                                               (28 . 1455)
                                               (29 . 1578)
                                               (30 . 1742)
                                               (31 . 1750)
                                               (32 . 2212)
                                               (33 . 2222)
                                               (34 . 2408)
                                               (35 . 2417)
                                               (36 . 2422)
                                               (37 . 2517)
                                               (38 . 2520)
                                               (39 . 2529)
                                               (40 . 2687)
                                               (41 . 2696)
                                               (42 . 2701)
                                               (43 . 2710)
                                               (44 . 2713)
                                               (45 . 2803)
                                               (46 . 2863)
                                               (47 . 2871)
                                               (48 . 3056)
                                               (49 . 3064)
                                               (50 . 3067)
                                               (51 . 3180)
                                               (52 . 3185)
                                               (53 . 3194)
                                               (54 . 3198)
                                               (55 . 3205)
                                               (56 . 3208)
                                               (57 . 3279)
                                               (58 . 3282)
                                               (59 . 3291)
                                               (60 . 3296)
                                               (61 . 3305)
                                               (62 . 3399)
                                               (63 . 3407)
                                               (64 . 3412)
                                               (65 . 3420)
                                               (66 . 3485)
                                               (67 . 3920)
                                               (68 . 3924)
                                               (69 . 3932)
                                               (70 . 3936)
                                               (71 . 3944)
                                               (72 . 3948)
                                               (73 . 4044)
                                               (74 . 4048)
                                               (75 . 4056)
                                               (76 . 4060)
                                               (77 . 4068)
                                               (78 . 4072)
                                               (79 . 4114)
                                               (80 . 4118)
                                               (81 . 4126)
                                               (82 . 4130)
                                               (83 . 4138)
                                               (84 . 4142)
                                               (85 . 4174)
                                               (86 . 4303)
                                               (87 . 4311)
                                               (88 . 4315)
                                               (89 . 4471)
                                               (90 . 4541)
                                               (91 . 4549)
                                               (92 . 4554)
                                               (93 . 4606)
                                               (94 . 4679)))

;; not move number in the chess sense, but ply
;; the fen is inclusive of the move itself
(setq pygn-mode-test-02-move-fens '(( 1 . "rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR b KQkq - 0 1"         )
                                    ( 2 . "rnbqkb1r/pppppppp/5n2/8/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 1 2"       )
                                    ( 3 . "rnbqkb1r/pppppppp/5n2/8/2PP4/8/PP2PPPP/RNBQKBNR b KQkq - 0 2"       )
                                    ( 4 . "rnbqkb1r/pppp1ppp/4pn2/8/2PP4/8/PP2PPPP/RNBQKBNR w KQkq - 0 3"      )
                                    ( 5 . "rnbqkb1r/pppp1ppp/4pn2/8/2PP4/2N5/PP2PPPP/R1BQKBNR b KQkq - 1 3"    )
                                    ( 6 . "rnbqk2r/pppp1ppp/4pn2/8/1bPP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 2 4"    )
                                    ( 7 . "rnbqk2r/pppp1ppp/4pn2/8/1bPP4/2N2P2/PP2P1PP/R1BQKBNR b KQkq - 0 4"  )
                                    ( 8 . "rnbqk2r/ppp2ppp/4pn2/3p4/1bPP4/2N2P2/PP2P1PP/R1BQKBNR w KQkq - 0 5" )
                                    ( 9 . "rnbqk2r/ppp2ppp/4pn2/3p4/1bPP4/P1N2P2/1P2P1PP/R1BQKBNR b KQkq - 0 5")
                                    (10 . "rnbqk2r/ppp1bppp/4pn2/3p4/2PP4/P1N2P2/1P2P1PP/R1BQKBNR w KQkq - 1 6")
                                    (11 . "rnbqk2r/ppp1bppp/4pn2/3p4/2PPP3/P1N2P2/1P4PP/R1BQKBNR b KQkq - 0 6" )
                                    (12 . "rnbqk2r/ppp1bppp/4pn2/8/2PPp3/P1N2P2/1P4PP/R1BQKBNR w KQkq - 0 7"   )
                                    (13 . "rnbqk2r/ppp1bppp/4pn2/8/2PPP3/P1N5/1P4PP/R1BQKBNR b KQkq - 0 7"     )
                                    (14 . "rnbqk2r/pp2bppp/4pn2/2p5/2PPP3/P1N5/1P4PP/R1BQKBNR w KQkq - 0 8"    )
                                    (15 . "rnbqk2r/pp2bppp/4pn2/2p1P3/2PP4/P1N5/1P4PP/R1BQKBNR b KQkq - 0 8"   )
                                    (16 . "rnbqk2r/pp1nbppp/4p3/2p1P3/2PP4/P1N5/1P4PP/R1BQKBNR w KQkq - 1 9"   )
                                    (17 . "rnbqk2r/pp1nbppp/4p3/2P1P3/2P5/P1N5/1P4PP/R1BQKBNR b KQkq - 0 9"    )
                                    (18 . "rnbqk2r/pp2bppp/4p3/2P1n3/2P5/P1N5/1P4PP/R1BQKBNR w KQkq - 0 10"    )
                                    (19 . "rnbQk2r/pp2bppp/4p3/2P1n3/2P5/P1N5/1P4PP/R1B1KBNR b KQkq - 0 10"    )
                                    (20 . "rnbbk2r/pp3ppp/4p3/2P1n3/2P5/P1N5/1P4PP/R1B1KBNR w KQkq - 0 11"     )
                                    (21 . "rnbbk2r/pp3ppp/4p3/2P1n3/2P5/P1N5/1P2B1PP/R1B1K1NR b KQkq - 1 11"   )
                                    (22 . "rn1bk2r/pp1b1ppp/4p3/2P1n3/2P5/P1N5/1P2B1PP/R1B1K1NR w KQkq - 2 12" )
                                    (23 . "rn1bk2r/pp1b1ppp/4p3/2P1n3/2P5/P1N2N2/1P2B1PP/R1B1K2R b KQkq - 3 12")
                                    (24 . "rn1bk2r/pp1b1ppp/4p3/2P5/2P5/P1N2n2/1P2B1PP/R1B1K2R w KQkq - 0 13"  )
                                    (25 . "rn1bk2r/pp1b1ppp/4p3/2P5/2P5/P1N2B2/1P4PP/R1B1K2R b KQkq - 0 13"    )
                                    (26 . "rn1bk2r/pp3ppp/2b1p3/2P5/2P5/P1N2B2/1P4PP/R1B1K2R w KQkq - 1 14"    )
                                    (27 . "rn1bk2r/pp3ppp/2b1p3/2P5/2P1N3/P4B2/1P4PP/R1B1K2R b KQkq - 2 14"    )
                                    (28 . "rn2k2r/ppb2ppp/2b1p3/2P5/2P1N3/P4B2/1P4PP/R1B1K2R w KQkq - 3 15"    )
                                    (29 . "rn2k2r/ppb2ppp/2b1p3/2P5/1PP1N3/P4B2/6PP/R1B1K2R b KQkq - 0 15"     )
                                    (30 . "r3k2r/ppbn1ppp/2b1p3/2P5/1PP1N3/P4B2/6PP/R1B1K2R w KQkq - 1 16"     )
                                    (31 . "r3k2r/ppbn1ppp/2b1p3/2P5/1PP1N3/P4B2/1B4PP/R3K2R b KQkq - 2 16"     )
                                    (32 . "r3k2r/ppb2ppp/2b1p3/2P1n3/1PP1N3/P4B2/1B4PP/R3K2R w KQkq - 3 17"    )
                                    (33 . "r3k2r/ppb2ppp/2b1p3/2P1n3/1PP1N3/P4B2/1B4PP/2KR3R b kq - 4 17"      )
                                    (34 . "r3k2r/ppb2ppp/2b1p3/2P5/1PP1N3/P4n2/1B4PP/2KR3R w kq - 0 18"        )
                                    (35 . "r3k2r/ppb2ppp/2b1p3/2P5/1PP1N3/P4P2/1B5P/2KR3R b kq - 0 18"         )
                                    (36 . "r3k2r/pp3ppp/2b1p3/2P5/1PP1Nb2/P4P2/1B5P/2KR3R w kq - 1 19"         )
                                    (37 . "r3k2r/pp3ppp/2b1p3/2P5/1PP1Nb2/P4P2/1B5P/1K1R3R b kq - 2 19"        )
                                    (38 . "r3k2r/pp4pp/2b1pp2/2P5/1PP1Nb2/P4P2/1B5P/1K1R3R w kq - 0 20"        )
                                    (39 . "r3k2r/pp4pp/2b1pp2/2P5/1PP1Nb2/P4P2/1B5P/1K1R2R1 b kq - 1 20"       )
                                    (40 . "r6r/pp3kpp/2b1pp2/2P5/1PP1Nb2/P4P2/1B5P/1K1R2R1 w - - 2 21"         )
                                    (41 . "r6r/pp3kpp/2bNpp2/2P5/1PP2b2/P4P2/1B5P/1K1R2R1 b - - 3 21"          )
                                    (42 . "r6r/pp3kpp/2bbpp2/2P5/1PP5/P4P2/1B5P/1K1R2R1 w - - 0 22"            )
                                    (43 . "r6r/pp3kpp/2bRpp2/2P5/1PP5/P4P2/1B5P/1K4R1 b - - 0 22"              )
                                    (44 . "r6r/pp3k1p/2bRpp2/2P3p1/1PP5/P4P2/1B5P/1K4R1 w - - 0 23"            )
                                    (45 . "r6r/pp3k1p/2bRpp2/2P3p1/1PP5/P4P2/1B5P/1K3R2 b - - 1 23"            )
                                    (46 . "r6r/1p3k1p/p1bRpp2/2P3p1/1PP5/P4P2/1B5P/1K3R2 w - - 0 24"           )
                                    (47 . "r6r/1p3k1p/p1bRpp2/2P3p1/1PP5/P4P2/1BK4P/5R2 b - - 1 24"            )
                                    (48 . "r2r4/1p3k1p/p1bRpp2/2P3p1/1PP5/P4P2/1BK4P/5R2 w - - 2 25"           )
                                    (49 . "r2r4/1p3k1p/p1bRpp2/2P3p1/1PP5/P4P2/1B1K3P/5R2 b - - 3 25"          )
                                    (50 . "r2r4/1p3k1p/p1bR1p2/2P1p1p1/1PP5/P4P2/1B1K3P/5R2 w - - 0 26"        )
                                    (51 . "r2r4/1p3k1p/p1bR1p2/2P1p1p1/1PP5/P3KP2/1B5P/5R2 b - - 1 26"         )
                                    (52 . "r7/1p3k1p/p1br1p2/2P1p1p1/1PP5/P3KP2/1B5P/5R2 w - - 0 27"           )
                                    (53 . "r7/1p3k1p/p1bP1p2/4p1p1/1PP5/P3KP2/1B5P/5R2 b - - 0 27"             )
                                    (54 . "r7/1p5p/p1bPkp2/4p1p1/1PP5/P3KP2/1B5P/5R2 w - - 1 28"               )
                                    (55 . "r7/1p5p/p1bPkp2/2P1p1p1/1P6/P3KP2/1B5P/5R2 b - - 0 28"              )
                                    (56 . "r7/7p/ppbPkp2/2P1p1p1/1P6/P3KP2/1B5P/5R2 w - - 0 29"                )
                                    (57 . "r7/7p/ppbPkp2/2P1p1p1/1P5P/P3KP2/1B6/5R2 b - - 0 29"                )
                                    (58 . "r7/8/ppbPkp1p/2P1p1p1/1P5P/P3KP2/1B6/5R2 w - - 0 30"                )
                                    (59 . "r7/8/ppbPkp1p/2P1p1P1/1P6/P3KP2/1B6/5R2 b - - 0 30"                 )
                                    (60 . "r7/8/ppbPkp2/2P1p1p1/1P6/P3KP2/1B6/5R2 w - - 0 31"                  )
                                    (61 . "r7/8/pPbPkp2/4p1p1/1P6/P3KP2/1B6/5R2 b - - 0 31"                    )
                                    (62 . "1r6/8/pPbPkp2/4p1p1/1P6/P3KP2/1B6/5R2 w - - 1 32"                   )
                                    (63 . "1r6/8/pPbPkp2/4p1p1/1P6/P3KP2/1B6/3R4 b - - 2 32"                   )
                                    (64 . "8/8/prbPkp2/4p1p1/1P6/P3KP2/1B6/3R4 w - - 0 33"                     )
                                    (65 . "8/8/prbPkp2/4p1p1/1P6/P4P2/1B3K2/3R4 b - - 1 33"                    )
                                    (66 . "8/8/pr1Pkp2/4p1p1/bP6/P4P2/1B3K2/3R4 w - - 2 34"                    )
                                    (67 . "8/8/pr1Pkp2/4p1p1/bP6/P4P2/1B1R1K2/8 b - - 3 34"                    )
                                    (68 . "1r6/8/p2Pkp2/4p1p1/bP6/P4P2/1B1R1K2/8 w - - 4 35"                   )
                                    (69 . "1r6/8/p2Pkp2/4p1p1/bP6/P4PK1/1B1R4/8 b - - 5 35"                    )
                                    (70 . "2r5/8/p2Pkp2/4p1p1/bP6/P4PK1/1B1R4/8 w - - 6 36"                    )
                                    (71 . "2r5/8/p2Pkp2/4p1p1/bP6/P4P2/1B1R1K2/8 b - - 7 36"                   )
                                    (72 . "8/8/p2Pkp2/4p1p1/bPr5/P4P2/1B1R1K2/8 w - - 8 37"                    )
                                    (73 . "8/8/p2Pkp2/4p1p1/bPr5/P4PK1/1B1R4/8 b - - 9 37"                     )
                                    (74 . "2r5/8/p2Pkp2/4p1p1/bP6/P4PK1/1B1R4/8 w - - 10 38"                   )
                                    (75 . "2r5/8/p2Pkp2/4p1p1/bP6/P4P2/1B1R1K2/8 b - - 11 38"                  )
                                    (76 . "7r/8/p2Pkp2/4p1p1/bP6/P4P2/1B1R1K2/8 w - - 12 39"                   )
                                    (77 . "7r/8/p2Pkp2/4p1p1/bP6/P4PK1/1B1R4/8 b - - 13 39"                    )
                                    (78 . "8/8/p2Pkp2/4p1p1/bP6/P4PK1/1B1R4/7r w - - 14 40"                    )
                                    (79 . "8/8/p2Pkp2/4p1p1/bP6/P4P2/1B1R2K1/7r b - - 15 40"                   )
                                    (80 . "8/8/p2Pkp2/4p1p1/bP6/P4P2/1B1R2K1/1r6 w - - 16 41"                  )
                                    (81 . "8/8/p2Pkp2/4p1p1/bP6/P4PK1/1B1R4/1r6 b - - 17 41"                   )
                                    (82 . "8/8/p1bPkp2/4p1p1/1P6/P4PK1/1B1R4/1r6 w - - 18 42"                  )
                                    (83 . "8/8/p1bPkp2/4p1p1/1P6/P4P2/1B1R1K2/1r6 b - - 19 42"                 )
                                    (84 . "8/3k4/p1bP1p2/4p1p1/1P6/P4P2/1B1R1K2/1r6 w - - 20 43"               )
                                    (85 . "8/3k4/p1bP1p2/4p1p1/1P6/P4PK1/1B1R4/1r6 b - - 21 43"                )
                                    (86 . "8/3k4/p1bP1p2/4p1p1/1P6/P4PK1/1B1R4/5r2 w - - 22 44"                )
                                    (87 . "8/3k4/p1bP1p2/4p1p1/1P6/P2R1PK1/1B6/5r2 b - - 23 44"                )
                                    (88 . "8/8/p1bPkp2/4p1p1/1P6/P2R1PK1/1B6/5r2 w - - 24 45"                  )
                                    (89 . "8/8/p1bPkp2/4p1p1/1P4K1/P2R1P2/1B6/5r2 b - - 25 45"                 )
                                    (90 . "8/8/p1bPkp2/4p1p1/1P4K1/P2R1P2/1B6/1r6 w - - 26 46"                 )
                                    (91 . "8/8/p1bPkp2/4p1p1/1P4K1/P4P2/1B1R4/1r6 b - - 27 46"                 )
                                    (92 . "8/8/p1bPkp2/4p1p1/1P4K1/P4P2/1B1R4/6r1 w - - 28 47"                 )
                                    (93 . "8/8/p1bPkp2/4p1p1/1P6/P4P1K/1B1R4/6r1 b - - 29 47"                  )
                                    (94 . "8/8/p1bPkp2/4p1p1/1P6/P4P1K/1B1R4/5r2 w - - 30 48"                  )))

(setq pygn-mode-test-03-game-start-positions '((1 . 1)
                                               (2 . 982)
                                               (3 . 1963)
                                               (4 . 2944)
                                               (5 . 3925)))

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

(ert-deftest pygn-mode-pgn-at-pos-01 nil
  "Test `pygn-mode-pgn-at-pos' from the first position (a corner case)."
  (pygn-mode-test-with-file "test-01.pgn"
    (should (equal
             "[Event \"?\"]\n"
             (pygn-mode-pgn-at-pos (point-min))))))

(ert-deftest pygn-mode-pgn-at-pos-02 nil
  "Test `pygn-mode-pgn-at-pos' string from every move-start position (test-01.pgn)."
  (pygn-mode-test-with-file "test-01.pgn"
    (dolist (cell pygn-mode-test-01-move-start-positions)
      (let* ((moves          (car cell))
             (move-pos       (cdr cell))
             (move-after-pos (cdr (assoc moves pygn-mode-test-01-move-after-positions))))
        (goto-char (point-min))
        (pygn-mode-next-move moves)
        (should (= (length (pygn-mode-pgn-at-pos (point)))
                   (1- move-after-pos)))))))

(ert-deftest pygn-mode-pgn-at-pos-03 nil
  "Test `pygn-mode-pgn-at-pos' interpreted as a FEN from every move-start position (test-01.pgn)."
  (pygn-mode-test-with-file "test-01.pgn"
    (dolist (cell pygn-mode-test-01-move-start-positions)
      (let* ((moves        (car cell))
             (move-pos     (cdr cell))
             (fen-for-move (cdr (assoc moves pygn-mode-test-01-move-fens))))
        (goto-char (point-min))
        (pygn-mode-next-move moves)
        (should (equal (pygn-mode-pgn-to-fen (pygn-mode-pgn-at-pos (point)))
                       fen-for-move))))))

(ert-deftest pygn-mode-pgn-at-pos-04 nil
  "Test `pygn-mode-pgn-at-pos' exhaustively, from every position in the buffer which follows a move (test-01.pgn)."
  (pygn-mode-test-with-file "test-01.pgn"
    (let ((last-pos (point-max)))

      ;; TODO: this merely hides a bug in pygn-mode-pgn-at-pos
      (goto-char last-pos)
      (skip-syntax-backward "-")
      (forward-char -1)
      (setq last-pos (point))

      (dolist (cell (reverse pygn-mode-test-01-move-start-positions))
        (let* ((moves              (car cell))
               (move-pos           (cdr cell))
               (fen-for-move       (cdr (assoc moves pygn-mode-test-01-move-fens)))
               (trailing-positions (number-sequence last-pos move-pos -1)))
          (dolist (pos trailing-positions)
            (should (stringp (pygn-mode-pgn-at-pos pos)))
            (should (equal (pygn-mode-pgn-to-fen (pygn-mode-pgn-at-pos pos))
                           fen-for-move)))
          (setq last-pos (1- move-pos)))))))

(ert-deftest pygn-mode-pgn-at-pos-05 nil
  "Test `pygn-mode-pgn-at-pos' string from every move-start position (test-02.pgn)."
  (pygn-mode-test-with-file "test-02.pgn"
    (dolist (cell pygn-mode-test-02-move-start-positions)
      (let* ((moves          (car cell))
             (move-pos       (cdr cell))
             (move-after-pos (cdr (assoc moves pygn-mode-test-02-move-after-positions))))
        (goto-char (point-min))
        (pygn-mode-next-move moves)
        (should (= (length (pygn-mode-pgn-at-pos (point)))
                   (1- move-after-pos)))))))

(ert-deftest pygn-mode-pgn-at-pos-06 nil
  "Test `pygn-mode-pgn-at-pos' interpreted as a FEN from every move-start position (test-02.pgn)."
  (pygn-mode-test-with-file "test-02.pgn"
    (dolist (cell pygn-mode-test-02-move-start-positions)
      (let* ((moves        (car cell))
             (move-pos     (cdr cell))
             (fen-for-move (cdr (assoc moves pygn-mode-test-02-move-fens))))
        (goto-char (point-min))
        (pygn-mode-next-move moves)
        (should (equal (pygn-mode-pgn-to-fen (pygn-mode-pgn-at-pos (point)))
                       fen-for-move))))))

(ert-deftest pygn-mode-pgn-at-pos-07 nil
  "Test `pygn-mode-pgn-at-pos' exhaustively, from every position in the buffer which follows a move (test-02.pgn)."
  (pygn-mode-test-with-file "test-02.pgn"
    (let ((last-pos (point-max)))

      ;; TODO: this merely hides a bug in pygn-mode-pgn-at-pos
      (goto-char last-pos)
      (skip-syntax-backward "-")
      (forward-char -1)
      (setq last-pos (point))

      (dolist (cell (reverse pygn-mode-test-02-move-start-positions))
        (let* ((moves              (car cell))
               (move-pos           (cdr cell))
               (fen-for-move       (cdr (assoc moves pygn-mode-test-02-move-fens)))
               (trailing-positions (number-sequence last-pos move-pos -1)))
          (dolist (pos trailing-positions)
            (should (stringp (pygn-mode-pgn-at-pos pos)))
            (should (equal (pygn-mode-pgn-to-fen (pygn-mode-pgn-at-pos pos))
                           fen-for-move)))
          (setq last-pos (1- move-pos)))))))

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

;;; pygn-mode-next-game

(ert-deftest pygn-mode-next-game-01 nil
  "Test `pygn-mode-next-game' from `point-min' to second game."
  (pygn-mode-test-with-file "test-03.pgn"
    (pygn-mode-next-game)
    (should (= (point)
               (cdr (assoc 2 pygn-mode-test-03-game-start-positions))))))

(ert-deftest pygn-mode-next-game-02 nil
  "Test two successive calls to `pygn-mode-next-game'."
  (pygn-mode-test-with-file "test-03.pgn"
    (pygn-mode-next-game)
    (pygn-mode-next-game)
    (should (= (point)
               (cdr (assoc 3 pygn-mode-test-03-game-start-positions))))))

(ert-deftest pygn-mode-next-game-03 nil
  "Test integer ARG to `pygn-mode-next-game'."
  (pygn-mode-test-with-file "test-03.pgn"
    (pygn-mode-next-game 2)
    (should (= (point)
               (cdr (assoc 3 pygn-mode-test-03-game-start-positions))))))

(ert-deftest pygn-mode-next-game-04 nil
  "Test negative integer ARG to `pygn-mode-next-game'."
  (pygn-mode-test-with-file "test-03.pgn"
    (pygn-mode-next-game 2)
    (pygn-mode-next-game -1)
    (should (= (point)
               (cdr (assoc 2 pygn-mode-test-03-game-start-positions))))))

(ert-deftest pygn-mode-next-game-05 nil
  "Test `pygn-mode-next-game' for every ARG which leads to a game (test-03.pgn)."
  (pygn-mode-test-with-file "test-03.pgn"
    (dolist (cell pygn-mode-test-03-game-start-positions)
      (let ((games    (car cell))
            (game-pos (cdr cell)))
        (goto-char (point-min))
        (pygn-mode-next-game (1- games))
        (should (= (point) game-pos))))))

(ert-deftest pygn-mode-next-game-06 nil
  "Test `pygn-mode-next-game' exhaustively, from every position in the buffer which precedes a game (test-03.pgn)."
  (pygn-mode-test-with-file "test-03.pgn"
    (let ((last-pos (point-min)))
      (dolist (cell pygn-mode-test-03-game-start-positions)
        (let* ((games             (car cell))
               (game-pos          (cdr cell))
               (leading-positions (number-sequence last-pos (1- game-pos))))
          (dolist (pos leading-positions)
            (goto-char pos)
            (pygn-mode-next-game)
            (should (= (point) game-pos)))
          (setq last-pos game-pos))))))

(ert-deftest pygn-mode-next-game-07 nil
  "Test that `pygn-mode-next-game' errors from the last game start."
  (pygn-mode-test-with-file "test-03.pgn"
    (let* ((cell     (car (last pygn-mode-test-03-game-start-positions)))
           (games    (car cell))
           (game-pos (cdr cell)))
      (pygn-mode-next-game (1- games))
      (should-error (pygn-mode-next-game)))))

(ert-deftest pygn-mode-next-game-08 nil
  "Test that `pygn-mode-next-game' errors from every position after the last game start."
  (pygn-mode-test-with-file "test-03.pgn"
    (let* ((cell     (car (last pygn-mode-test-03-game-start-positions)))
           (games    (car cell))
           (game-pos (cdr cell))
           (trailing-positions (number-sequence game-pos (point-max))))
      (dolist (pos trailing-positions)
        (goto-char pos)
        (should-error (pygn-mode-next-game))))))

;; TODO pygn-mode-next-game behavior when between games

;;; pygn-mode-previous-game

(ert-deftest pygn-mode-previous-game-01 nil
  "Test `pygn-mode-previous-game' from `point-min' to second game, back to first game."
  (pygn-mode-test-with-file "test-03.pgn"
    (pygn-mode-next-game)
    (pygn-mode-previous-game)
    (should (= (point)
               (cdr (assoc 1 pygn-mode-test-03-game-start-positions))))))

(ert-deftest pygn-mode-previous-game-02 nil
  "Test two successive calls to `pygn-mode-previous-game'."
  (pygn-mode-test-with-file "test-03.pgn"
    (pygn-mode-next-game 3)
    (pygn-mode-previous-game)
    (pygn-mode-previous-game)
    (should (= (point)
               (cdr (assoc 2 pygn-mode-test-03-game-start-positions))))))

(ert-deftest pygn-mode-previous-game-03 nil
  "Test integer ARG to `pygn-mode-previous-game'."
  (pygn-mode-test-with-file "test-03.pgn"
    (pygn-mode-next-game 3)
    (pygn-mode-previous-game 2)
    (should (= (point)
               (cdr (assoc 2 pygn-mode-test-03-game-start-positions))))))

(ert-deftest pygn-mode-previous-game-04 nil
  "Test negative integer ARG to `pygn-mode-previous-game'."
  (pygn-mode-test-with-file "test-03.pgn"
    (pygn-mode-next-game 2)
    (pygn-mode-previous-game -1)
    (should (= (point)
               (cdr (assoc 4 pygn-mode-test-03-game-start-positions))))))

(ert-deftest pygn-mode-previous-game-05 nil
  "Test `pygn-mode-previous-game' for every ARG which leads to a game (test-03.pgn)."
  (pygn-mode-test-with-file "test-03.pgn"
    (dolist (cell (reverse pygn-mode-test-03-game-start-positions))
      (let ((games    (car cell))
            (game-pos (cdr cell))
            (rescaler (length pygn-mode-test-03-game-start-positions)))
        (setq games (- rescaler games))
        (goto-char (cdr (car (last pygn-mode-test-03-game-start-positions))))
        (pygn-mode-previous-game games)
        (should (= (point) game-pos))))))

;; TODO: the save-excursions are needed because the behavior of
;; pygn-mode-previous-game when between games is not well defined.
(ert-deftest pygn-mode-previous-game-06 nil
  "Test `pygn-mode-previous-game' exhaustively, from every position in the buffer which follows a game (test-03.pgn)."
  (pygn-mode-test-with-file "test-03.pgn"
    (let ((last-pos (save-excursion
                      (goto-char (point-max))
                      (skip-syntax-backward "-")
                      (forward-char -1)
                      (point))))
      (dolist (cell (reverse (cdr pygn-mode-test-03-game-start-positions)))
        (let* ((games              (car cell))
               (game-pos           (cdr cell))
               (previous-game      (1- games))
               (previous-game-pos  (cdr (assoc previous-game pygn-mode-test-03-game-start-positions)))
               (trailing-positions (number-sequence last-pos game-pos -1)))
          (dolist (pos trailing-positions)
            (goto-char pos)
            (pygn-mode-previous-game)
            (should (= (point) previous-game-pos)))
          (setq last-pos (save-excursion
                           (goto-char game-pos)
                           (skip-syntax-backward "-")
                           (forward-char -1)
                           (point))))))))

(ert-deftest pygn-mode-previous-game-07 nil
  "Test that `pygn-mode-previous-game' errors from the first game start."
  (pygn-mode-test-with-file "test-03.pgn"
    (let* ((cell     (car pygn-mode-test-03-game-start-positions))
           (games    (car cell))
           (game-pos (cdr cell)))
      (goto-char game-pos)
      (should-error (pygn-mode-previous-game)))))

(ert-deftest pygn-mode-previous-game-08 nil
  "Test that `pygn-mode-previous-game' errors from every position in the first game."
  (pygn-mode-test-with-file "test-03.pgn"
    (let* ((cell     (car pygn-mode-test-03-game-start-positions))
           (games    (car cell))
           (game-pos (cdr cell))
           (second-cell (car (cadr pygn-mode-test-03-game-start-positions)))
           (second-game-pos (cdr cell))
           (positions (number-sequence game-pos (1- second-game-pos))))
      (dolist (pos positions)
        (goto-char pos)
        (should-error (pygn-mode-previous-game))))))

;; TODO pygn-mode-previous-game behavior when between games

;;
;; Emacs
;;
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;

;;; pygn-mode-test.el ends here

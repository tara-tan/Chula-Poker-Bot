;A Poker Texas Hold`Em engine in Common Lisp

;Copyright: Paulo Garcia (paulo.g@chula.ac.th)
;International School of Engineering,
;Faculty of Engineering,
;Chulalongkorn University, Thailand

;Licensed under CC -BY-NC-SA 4.0
;(Creative Commons Attribution-NonCommercial-ShareAlike)

;format for your AI player
;All your code goes into this file

(ql:quickload "cl-csv")
(let
	(
		;"hand" is a list of length 2
		;each item is a card, of format
		;		(list 4 `H) i.e., the 4 of Hearts
		;		first element is from 1-13 (A to K)
		;		second element is suit (`H `D `S `C)
		(hand nil)
		;declare aditional needed persistent variables here
		;	It's your job to store any history you require
		;   between successive game rounds
		;		in these let-defined variables
		;e.g.: (round-counter 0)
	)
	(defun AI-set-hand (cards)
		(setf hand cards)
	)
	(defun readWinChanceMatrix (path) 
		(let ((input-table (cl-csv:read-csv path)))
		(format t "Read ~A rows from ~A~%" (length input-table) path)
		input-table)
	)
	(defun AI-get-hand ()
		hand
	)
	;	This function will be called by the game engine at every
	;		decision point
	; Your main logic goes here
	(defun AI
		(
			;list of cards on the table; may be empty
			cards-on-table
			;game state (our chips and bet, other players' chips and bet); see game engine for explanation of format
			game-state
		)
			;set up global constants for bot logic
		(defparameter *preflopOffSuitmatrix* (readWinChanceMatrix #P"preflopChances/offSuit.csv"))
		(defparameter *preflopOnSuitmatrix* (readWinChanceMatrix #P"preflopChances/onSuit.csv"))
		(let
			(
				;our current state: chips, current bet
				(state 	(find-if
									(lambda (x)
										(if (equal (nth 0 x) `AI)
											t
											nil
										)
									)
									game-state
								)
				)
			)
			;final form in this function is our action
			; must be either:
			;	 "`in" (check or call current max bet)
			;  "`fold" (give up in this round)
			;	 "integer" (any integer value, raising the bet)
			`in
		)
	)
)

(defun last-player-in (game-state)
	(let
		(
			(num-n-folded
				(reduce
					`+
					(mapcar
						(lambda (x)
							(if (equal (nth 3 x) `fold)
								0
								1
							)
						)
						game-state
					)
				)
			)
		)
		(if (equal num-n-folded 1)
			t
			nil
		)
	)
)

;simple player, never raises
(let
	(
		(hand nil)
	)
	(defun noob1-set-hand (cards)
		(setf hand cards)
	)
	(defun noob1-get-hand ()
		hand
	)
	(defun noob1
		(
			cards-on-table
			game-state
		)
		;noob1 never raises, either checks/calls or folds
		(if (last-player-in game-state)
			`in
			(if (> (random 10) 7)
				`fold
				`in
			)
		)
  )
)

;simple player, never folds
(let
	(
		(hand nil)
	)
	(defun noob2-set-hand (cards)
		(setf hand cards)
	)
	(defun noob2-get-hand ()
		hand
	)
	(defun noob2
		(
			cards-on-table
			game-state
		)
		(let
			(
				;our current state: chips, current bet
				(state 	(find-if
									(lambda (x)
										(if (equal (nth 0 x) `noob2)
											t
											nil
										)
									)
									game-state
								)
				)
			)
			;noob2 never folds,
			;either checks/calls or raises big blind
			;if it has enough money
			(if (> (random 10) 7)
				(if (>= (nth 1 state) 40)
					40
					(nth 1 state)
				)
				`in
			)
		)
  )
)

;simple player, never raises
(let
	(
		(hand nil)
	)
	(defun noob3-set-hand (cards)
		(setf hand cards)
	)
	(defun noob3-get-hand ()
		hand
	)
	(defun noob3
		(
			cards-on-table
			game-state
		)
		;noob3 never raises, either checks/calls or folds
		(if (last-player-in game-state)
			`in
			(if (> (random 10) 7)
				`fold
				`in
			)
		)
    )
)

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
(ql:quickload :parse-float)
(ql:quickload 'inferior-shell)
(let
	(
		;"hand" is a list of length 2
		;each item is a card, of format
		;		(list 4 `H) i.e., the 4 of Hearts
		;		first element is from 1-13 (A to K)
		;		second element is suit (`H `D `S `C)
		(updateCount 0)
		(hand nil)
		(round-data ())
		(record-p1 ())
		(record-p2 ())
		(record-p3 ())
		;declare aditional needed persistent variables here
		;	It's your job to store any history you require
		;   between successive game rounds
		;		in these let-defined variables
		;e.g.: (round-counter 0)
	)
	(defun AI-set-hand (cards)
		(setf hand cards)
	)
	(defun AI-get-hand ()
		hand
	)
	;	This function will be called by the game engine at every
	;		decision point
	; Your main logic goes here
	(defun readWinChanceMatrix (path) 
		(let 
			((input-table (cl-csv:read-csv path)))
			
			(format t "Read ~A rows from ~A~%" (length input-table) path)
			(mapcar
				(lambda (row)
					(mapcar
						(lambda (x)
							(parse-float:parse-float x)
						)
						row
					)
				)
				input-table
			)
		)
	)
	(defparameter *preflopOffSuitmatrix* (readWinChanceMatrix #P"preflopChances/offSuit.csv"))
	(defparameter *preflopOnSuitmatrix* (readWinChanceMatrix #P"preflopChances/onSuit.csv"))
	(defun mapranktoindex (rank) ; convert card rank format to ascending by value
		(if (equal rank 1)
			0
		(- 15 (- rank 1))
		)
	)
	(defun get-card-symbol (n)
	(nth (- n 1) (list #\A 2 3 4 5 6 7 8 9 10 #\J #\Q #\K))
	)
	(defun readPotSize (game-state)
		(reduce
			`+
			(mapcar
				(lambda (x)
					(nth 2 x)
				)
				game-state
			)
		)
	)
	(defun lookupPlayerByName (game-state name)
		(find-if
			(lambda (x)
				(if (equal (nth 0 x) name)
					t
					nil
				)
			)
			game-state
		)
	)
	(defun getWinChanceBoard (boardstring)
		(* 100 (parse-float:parse-float 
			(inferior-shell:run/s 
				`(./rankCalc.sh --hand ,"XxXx" ,"--board" ,boardstring))))
	)
	(defun getCurrentRound (cards-on-table)
		(- (length cards-on-table) 2) ; subtract 2 since post-flop starts with 3 cards
	)
	(defstruct player-record
		round
		name
		current-bet
		current-chips
		current-pot
		tableStrength
		currentAction
		previousAction
	)
	(defun actiontonum (action)
		(cond
			((equal action `fold) 1)
			((equal action `CALL-CHECK) 2)
			((equal action `raise) 3)
		)
	)
	(defun update-round-data (game-state cards-on-table)
		(let*
			(
				(current-round (getCurrentRound cards-on-table))
				(players-list
					(mapcar
						(lambda (x)
							(nth 0 x)
						)
						game-state
					)
				)
				(pot-size (readPotSize game-state))
				(boardString 
						(mapcar
							(lambda (x)
								(concatenate 'string 
									(format nil "~A" (get-card-symbol (nth 0 x))) 
									(format nil "~A" (nth 1 x))
								)
							)
							cards-on-table
						
					)
				)
				(boardstring
					(reduce
						(lambda (a b)
							(concatenate 'string a b)
						)
						boardString
					)
				)
				(win-chance (getWinChanceBoard boardstring))
			)
			(dolist (player-name players-list)
			(if (or
				(equal current-round 1)
				(not ( ; not folded
					equal (nth 3 (lookupPlayerByName game-state player-name)) 
						`fold
					)
				)
			)
				(if (and (not (equal player-name `AI)) )
					( let* 
						(
							(player-previous-record 
								(find-if
									(lambda (x)
										(if (and (equal (player-record-round x) (- current-round 1)) 
										(equal (player-record-name x) player-name))
											t
											nil
										)
									)
									round-data
								)
							)
							(player-previous-action
								(if player-previous-record
									(player-record-currentAction player-previous-record)
									nil
								)
							)
							(player-previous-round 
								(if player-previous-record
									(player-record-round player-previous-record)
								)

							)
						)
						; (break)
						(if 
							(if (and (equal current-round 1) (equal player-previous-record nil) )
								t
								(and 
									(not ; not duplicate round
										(equal player-previous-round current-round)
									)
									(not(equal player-previous-record nil)) ; record actually exists (BUG this is a know issue)
									(not ; not duplicate bet size
										(not (equal (player-record-current-bet player-previous-record) 
											(nth 2 (lookupPlayerByName game-state player-name))))
									)
									(not ( ; not folded
										equal (nth 3(lookupPlayerByName game-state player-name)) 
											`fold
										)
									)
								)
							)
							(push 
								(make-player-record 
									:name player-name
									:round current-round
									:current-bet (nth 2 (lookupPlayerByName game-state player-name))
									:current-chips (nth 1 (lookupPlayerByName game-state player-name))
									:current-pot pot-size
									:tableStrength win-chance
									:currentAction (nth 3 (lookupPlayerByName game-state player-name))
									:previousAction player-previous-action
								)
								round-data 
							)
						)
					)
				)
			)
			)
		)
	)
	(defun updateknowledge (round-data game-state)
		(setf updatecount (+ updateCount 1))
		(dolist (record round-data)
			(let ((entry
					(make-player-record 
					:name (player-record-name record)
					:round (player-record-round record)
					:current-bet (min (floor (player-record-current-bet record) 40) 4) 
					:current-chips (min (floor (player-record-current-chips record) 500) 6) 
					:current-pot (min (floor (player-record-current-pot record) 80) 4) 
					:tableStrength (min (floor (player-record-tableStrength record) 15) 5) 
					:currentAction (actiontonum (player-record-currentAction record)) 
					:previousAction (actiontonum (player-record-previousAction record)) 
					)
				))
				(cond 
					((equal (player-record-name record) `noob1) (push entry record-p1))
					((equal (player-record-name record) `noob2) (push entry record-p2))
					((equal (player-record-name record) `noob3) (push entry record-p3))
				)
			)
		)
		( dolist 
			(name 
				(mapcar
					(lambda (x)
						(nth 0 x)
					)
					game-state
				)
			)
			(
				if (not (equal name `AI))
				(let*
					(
						(record (cond 
							((equal name `noob1) record-p1)
							((equal name `noob2) record-p2)
							((equal name `noob3) record-p3)
							)
						)
						(record
							(remove-if (lambda (r)
							(equal nil (player-record-currentAction r)))
						record))	
						(priorFold  (/ (count-if (lambda (x) (equal 1 (player-record-currentAction x)) ) record) (+ (length record) 0.01)))
						(priorCall  (/ (count-if (lambda (x) (equal 2 (player-record-currentAction x)) ) record) (+ (length record) 0.01)))
						(priorRaise (/ (count-if (lambda (x) (equal 3 (player-record-currentAction x)) ) record) (+ (length record) 0.01)))
					)

					(
						format t "calculated prior for ~A fold: ~A ~% call : ~A ~% raise: ~A ~% " name priorFold priorCall priorRaise
					)

				)
				nil
			)
		)

		(if (> updateCount 20)
			(progn 
				(format t "Printing knowledge base... ~%")
				(format t "Player 1 (~A) records: ~A ~%" `noob1 record-p1)
				(format t "Player 2 (~A) records: ~A ~%" `noob2 record-p2)
				(format t "Player 3 (~A) records: ~A ~%" `noob3 record-p3)
				(setf updateCount 0)
				(break)
			)
		)
	)
	(defun calcUtil (game-state expectedwinchance probabilitylist)
		(let*
			(
				(our-state (lookupPlayerByName game-state `AI))
				(our-chips (nth 1 our-state))
				(our-current-bet (nth 2 our-state))
				(pot-size (readPotSize game-state))
				(probFold (nth 0 probabilitylist))
				(probCall (nth 1 probabilitylist))
				(probRaise (nth 2 probabilitylist))
				(util (+ (* probRaise expectedwinchance (+ pot-size 40)) ;find prior
						(* probCall expectedwinchance (/ pot-size 2))
						(* probFold expectedwinchance pot-size)
					)
				)
				;find conditional prob here

				;multinomial naive bayes
			)
			util
			)
		)
		(defun calcCost (game-state expectedwinchance probabilitylist)
		(let*
			(
				(our-state (lookupPlayerByName game-state `AI))
				(our-chips (nth 1 our-state))
				(our-current-bet (nth 2 our-state))
				(pot-size (readPotSize game-state))
				(expected-win (* expectedwinchance pot-size))
				(probFold (nth 0 probabilitylist))
				(probCall (nth 1 probabilitylist))
				(probRaise (nth 2 probabilitylist))
				(util (- expected-win our-current-bet))
			)
			util
			)
		)
	(defun AI
		(
			;list of cards on the table; may be empty
			cards-on-table
			;game state (our chips and bet, other players' chips and bet); see game engine for explanation of format
			game-state
		)
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
			(if (equal (length cards-on-table) 0)
				;this means we're in pre-flop
				( let*
					(
						(card1 (nth 0 hand))
						(card2 (nth 1 hand))
						; check suited
						(suited (equal (nth 1 card1) (nth 1 card2)))
						; check ranks 
						; Provided rank mapping is 1-13 (A to K)
						; we will convert this to 0-12 (A to 2) to sort by value ascending
						(rank1 (mapranktoindex (nth 0 card1)))
						(rank2 (mapranktoindex (nth 0 card2)))
						;win chance from preflop matrix
						(win-chance 
							(if suited
								(nth rank2
										(nth rank1 *preflopOnSuitmatrix*))
								(nth rank2 
										(nth rank1 *preflopOffSuitmatrix*))
							)
						)
					)
					(
						if (not (equal round-data nil))
						(progn
							(format t "Updating knowledge base... ~%")
							(updateknowledge round-data game-state)
						)
						nil
					)
					(setf round-data nil) ; reset round data at start of round
					;simple logic based on win chance (if our hands are above average)
					(format t "Preflop: card1: ~A, card2: ~A, suited: ~A, rank1: ~A, rank2: ~A, win-chance: ~A" 
						card1 card2 suited rank1 rank2 win-chance)
					(cond 
						((> win-chance 30.0) 
							;strong hand, raise big blind (40)
							(if (>= (nth 1 state) 40)
								40
								(nth 1 state)
							)
						)
						((> win-chance 15.0) 
							;average hand, just call
							`in
						)
						((<= win-chance 15.0) 
							; abysmal dogwater (below average hand), fold
							`fold
						)
					)
				)
				;post-flop logic goes here
				;for now, just always in
				(  
					let*
					(
						(card1 (nth 0 hand))
						(card2 (nth 1 hand))
						(boardString 
								(mapcar
									(lambda (x)
										(concatenate 'string 
											(format nil "~A" (get-card-symbol (nth 0 x))) 
											(format nil "~A" (nth 1 x))
										)
									)
									cards-on-table
								
							)
						)
						(cardString (concatenate 'string 
							(format nil "~A" (get-card-symbol (nth 0 card1))) 
							(format nil "~A"  (nth 1 card1)) 
							(format nil "~A"  (get-card-symbol (nth 0 card2))) 
							(format nil "~A" (nth 1 card2))
							)
						)
						(boardstring
							(reduce
								(lambda (a b)
									(concatenate 'string a b)
								)
								boardString
							)
						)
						
						(win-chance (inferior-shell:run/s `(./rankCalc.sh --hand ,cardString ,"--board" ,boardstring)))
						(win-chance (* 100 (parse-float:parse-float win-chance)))

					)
					(format t "recieved data: ~A ~A" game-state cards-on-table)
					(update-round-data game-state cards-on-table)
					(terpri)
					(format t "current round data: ~A ~%" round-data)
					(format t "Postflop: card: ~A, Board: ~A, win-chance: ~A" 
						cardString boardString win-chance)
					(terpri)
					(cond 
						((> win-chance 30.0) 
						(if (>= (nth 1 state) 80)
							;strong hand, raise 2x big blind (80) or more
							(if
								(>= (nth 1 state) 200)
								`in
								(+ 40 (nth 1 state))
							)
							80
							)
						)
						((> win-chance 15.0) 
							;average hand, just call
							`in
						)
						((<= win-chance 15.0) 
							(if (> (nth 1 state) 40)
								; abysmal dogshit (below average hand), fold
								`fold
								`in
							)
							
						)
					)
				)
			)
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

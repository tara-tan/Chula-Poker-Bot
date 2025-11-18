Github : https://github.com/tara-tan/Chula-Poker-Bot.git

## Files:
	"poker.lisp": main game engine. You do not need to modify nor read this file, unless you're looking for more details on the game state format.
	
	"players.lisp": this is where your logic must go: there is a skeleton of a function named "AI" which is the main entry point for your logic.

	"main.lisp": main program entry point. Provides three functions to test a single round, using the default players; a full game, using the default players; and, a full game using the default players + your AI.

## Usage:

	If using sbcl, "sbcl --load main.lisp" will load the entire program into a running Lisp image. Functions in "main" can then be called to test behavior (e.g., "(test-1-round)").
	
	Note 1: sbcl will probably give you 2 style warnings, saying variables in "AI" are not used. These will disappear once you use them.	

	Note 2: program is printing Unicode characters for card suit display. If your terminal does not support Unicode, either upgrade to a terminal that does (it is 2025...) or change the function in "poker.lisp" that prints the suit to just display a letter.

## To run this code:

1. install sbcl and quicklisp  

2. create a conda environment with the package "treys" and modify the conda activation in rankcalc.sh (this code was tested on python 3.12.8)

## Project details:

#### Objective:

starting from deterministic case

model a utility score for current hand state, pot state, flop state, deck state

take into account bluffing:

model other player's behaviour on what actions they would do

make decisions based on other player's behaviour

#### Conditions

standard 52-deck, deck refreshes every round

4 player game

information available is 

- hand (our current hand)

- cards-on-table (flopped cards (if any))

- game-state (list formatted as below)
```
; ... (game-state) for example, for 3 players, it might look like:
;	(list
;		(list `player1 900  50 `in) ;has 900 chips, 50 bet, playing
;		(list `player2 1000 50 `in) ;has 1000 chips, 50 bet, playing
;		(list `player3 200  40 `fold) ;has 200 chips, 40 bet, folded
;	)
```

- I choose to ignore blinds 

- all-ins are ignored

#### pre flop

like any good programmer, I stole the hand rankings from the internet 

link:https://www.reddit.com/r/poker/comments/6phynz/quick_preflop_odds_reference/

since we are ignoring bet size for now, we will only play hands where we have an above average hand in relation to the table (ie. 25% or more in a table for 4 player)

post flop

same as #lways, we need to find an expected value for each of our actions (fold, call, raise) as an objective

we do this by state space exploration (SSE), but the question is of what depth 

ignoring pre-flop,  the max depth is 3 (flop-turn-river). we can do this by a recursive call for SSE

since we cant do a max depth SSE for every call, we'll precalculate all the odds ahead of time using the python treys library and some multiprocessing magic.

But turns out, precalculating everything takes around 168 hrs on my pc, so we'll do monte carlo instead without pre calculation (refer to rankCalc.py)

#### Behaviour tracking

so I'll define the objective function of each round for each action as 

" % chance of me winning X amount I'll get (only accounting for this round's actions including other raises) "

and the cost function will be the inverse of that (how much I expect to lose)

this means we need know how likely other players are  to do each of their actions

we assume that a player's action is markovian ie. their action only depends on their current state

we define state variables to observe (based on the rule of "trust me bro")

- current bet and current chips

- current pot size

- average hand strength (that can be made from current board cards)

- the current round (post flop, turn, river)

- their action in the round before (raise on turn, call on river)

 and the actions availabe as 

- fold

- call

= raise (big blind)

we can model the state variables as either ordinal or continuous

oridinal: we divide the values into buckets (low bet, medium bet,high bet) and treat them as discrete values

doing so we model the system using a naive bayes model 

continuous: we treat the variables as is (as a number), and predict the actions based on that 

for this, we have many options for the model

- clustering (k means) (assuming linearlity)

- support vector machine

- gaussian mixture model

but since we need the posterior distribution for EV calculation, I'll go with multinomial naive bayes with bucketing

we track the stats for each turn of each player (know issue: we can only track players before us ,will fix later) then update the knowledge base every time a full round ends (to avoid weird distribution fluctuations) then use the kb to update a naive bayes model.

then we take the predicted actions to calculate the expected value and objective function (for calculation we assume than we win that round with our current hand) as :

objective = util - cost

util =  p(raise|action)*p(win|action)*(potsize+40)   +  p(call|action)*p(win|action)*(potsize/2) + p(fold|action)*p(win|action)*(potsize)

cost  = p(raise|action)*(1-p(win|action))*(current bet+40)   +  p(call|action)(1-p(win|action))(current bet) + p(fold|action)(1-p(win|action))(current bet)

* note:  p(*|action) cant be reliably found yet, so we just use p( * )

compare expected value of all actions and we get our final answer

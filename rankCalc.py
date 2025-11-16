import time
from treys import Card, Evaluator, Deck
from multiprocessing import cpu_count, Process, Manager
import argparse
numProcesses = 12
deck = Deck()
evaluator = Evaluator()

def sim(evaluator,board,hand,returnDict,procID,rounds=100000,opponents=3):
    count = 0
    score = 0
    errorCount = 0
    lose=0
    roundLost = 0
    original = board.copy() if board else []
    hand = hand.copy()
    while count < rounds:
        deck = Deck()
        deck.shuffle()
        lose = False
        try:
            board = original + deck.draw(5 - len(original))
            count += 1
            score = evaluator.evaluate(board, hand)
            if opponents > 0:
                for _ in range(opponents):
                    oppHand = deck.draw(2)
                    oppScore = evaluator.evaluate(board, oppHand)
                    # if procID == 0:
                    #     print("\nSimulation {}".format(count))
                    #     Card.print_pretty_cards(oppHand)
                    #     Card.print_pretty_cards(hand)
                    #     Card.print_pretty_cards(board)
                    #     print( "oppScore =", oppScore)
                    #     print( "myScore  =", score)
                    #     print( f"lost: {oppScore < score}")
                    if oppScore < score: # we count ties as wins
                        lose = True
                if lose:
                    roundLost += 1 
                    
        except KeyError:
            errorCount += 1
            pass
        except Exception as e:
            print("Process {} encountered error: {}".format(procID,e))
    # print("Process {} finished. simulations completed: {}, errors encountered: {}".format(procID, count, errorCount))
    
    if opponents == 0 :
        returnDict[procID]=score/rounds
    else:
        # print("Process {} finished. simulations completed: {}, errors encountered: {}, losses: {}".format(procID, count, errorCount, roundLost ))
        returnDict[procID]=((rounds - roundLost)/rounds)


if __name__ == "__main__":
    argparser = argparse.ArgumentParser()
    argparser.add_argument('--hand', type=str, required=True, help='Hand cards in format AcAd')
    argparser.add_argument('--board', type=str, required=False, help='Board cards in format KcQsJh')
    args = argparser.parse_args()
    handCards = [Card.new(args.hand[i:i+2].capitalize()) for i in range(0, len(args.hand), 2)]
    boardCards = []
    if args.board:
        boardCards = [Card.new(args.board[i:i+2]) for i in range(0, len(args.board), 2)]

    # Card.print_pretty_cards(boardCards)
    # Card.print_pretty_cards(handCards)

    manager = Manager()
    return_dict = manager.dict()
    start = time.time()
    processes = []
    for i in range(numProcesses):
        processes.append(Process(target=sim, args=(Evaluator(),boardCards,handCards ,return_dict,i,100000//numProcesses))) 
        # take around 0.4 seconds on 12 cores
        # with 3 opponents, takes around 0.7 seconds on 12 cores
        processes[i].start()

    for i in range(numProcesses):
        processes[i].join()

    # print("Main process finished")
    end = time.time()
    # print("Time taken: {}".format(end - start))
    # print(f"results: {return_dict}")
    print((sum(return_dict.values())/numProcesses)) # average score form mc out of 7402


# example call: python rankCalc.py --hand AcAs

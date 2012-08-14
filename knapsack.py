# Basic backtracking example---a depth-first search where we 
# prune fruitless branches.

goal  = 10
possibleValues = [ [1, 5, 7],
                   [1, 3, 2],
                   [1, 2, 8] ]

def flatten(lstOfLists):
    acc = []
    for sublist in lstOfLists:
        acc += sublist
    return acc

def canTake(bindings, newVal):
    return sum(bindings) + newVal <= goal

def solves(bindings):
    return len(bindings) == 3 and sum(bindings) == goal

def knapsack(bindings):
    if solves(bindings):
        return [bindings]
    else:
        depth = len(bindings)
        if depth == 3:
            return []
        else:
            return flatten([ knapsack(bindings + (v,))
                             for v in possibleValues[depth]
                             if canTake(bindings, v) ])

print knapsack(())



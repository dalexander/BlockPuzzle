
type Bound   = Int
type Unbound = [Int]

goal = 10

possibleSettings :: [Unbound]
possibleSettings  = [ [1, 5, 7],
                      [1, 3, 2],
                      [1, 2, 8] ]

solvesKnapsack :: [Bound] -> Bool
solvesKnapsack bindings = (sum bindings == 10)

knapsack :: [Unbound] -> [[Bound]]
knapsack possibilities = knapsack' possibilities []
    where knapsack' :: [Unbound] -> [Bound] -> [[Bound]]
          knapsack' [] bindings
              | (sum bindings == 10) = [bindings]
              | otherwise            = []
          knapsack' (u:us) bindings = 
              do 
                v <- u
                True <- return (sum(v:bindings) <= 10)
                knapsack' us (v:bindings)
              



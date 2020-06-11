{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import qualified Data.Set as S

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = EmptyNode{depth :: Int} | Node {state :: s, previousAction :: a, parentNode :: Node s a, depth :: Int, children :: [(a,s)]} deriving (Eq,Show)

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}

nodeState :: Node s a -> s
nodeState = state

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor 
    Primește starea inițială și creează nodul corespunzător acestei stări, 
    având drept copii nodurile succesorilor stării curente.
-}

createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace cs = Node cs (fst $ head $ successors cs) (EmptyNode (-1)) 0 (successors cs)

{-
    *** TODO PENTRU BONUS ***

    Ordonează întreg spațiul stărilor după euristica din ProblemState. 
    Puteți folosi `sortBy` din Data.List.
-}

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace = undefined


{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.
-}

limitedDfs :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
           

limitedDfs startNode maxDepth = reverse $ fst $ (limitedDfsHelper maxDepth ([],S.empty) startNode)

limitedDfsHelper :: (ProblemState s a, Ord s) => Int -> ([Node s a],S.Set s) -> Node s a -> ([Node s a],S.Set s)
limitedDfsHelper maxDepth (visited,set) v 
    | (depth v) > maxDepth = (visited,set)
    | S.member (nodeState v) set =  (visited,set) -- already seen v, so skip it
    | otherwise = foldl (limitedDfsHelper maxDepth) (v:visited,S.insert (nodeState v) set) (sucs v)
    where
        sucs node = [Node (snd x) (fst x) node ((depth node) + 1) (successors (snd x)) | x <- (children node)]

{- --Somehow Working Variant, Let's Call It V1
limitedDfs node maxDepth = node : limitedDfsHelper node maxDepth ([],S.empty)

limitedDfsHelper :: (ProblemState s a, Ord s) => Node s a -> Int -> ([Node s a],S.Set s) -> [Node s a]
limitedDfsHelper node maxDepth (acc,set)
    | ((depth node) >= maxDepth) = acc
    | (null (children node)) = acc
    | isGoal (nodeState node) = acc ++ [node]
    | otherwise = concat $ map (\x-> if S.member (snd x) set then [] else limitedDfsHelper (newNode x) maxDepth ((acc ++ [newNode x]),(S.insert (nodeState $ newNode x) set))) (children node)
    where newNode x = Node (snd x) (fst x) node ((depth node) + 1) (successors (snd x))
--}

   
{-
    *** TODO ***
2
    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}

iterativeDeepening :: (ProblemState s a, Ord s)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening n = iterativeDeepeningHelper n 0 0

iterativeDeepeningHelper :: (ProblemState s a, Ord s) => Node s a -> Int -> Int -> (Node s a, Int)
iterativeDeepeningHelper n dep howMany
    | (depth element) == (-1) = iterativeDeepeningHelper n (dep + 1) (howMany + (length $ list))
    | otherwise = (element, (howMany + index))
    where 
        list = limitedDfs n dep
        (element,index) = filterGetFirst (\x -> isGoal (nodeState x)) list 0
    
filterGetFirst :: (Node s a->Bool) -> [Node s a] -> Int -> (Node s a,Int)

filterGetFirst _ [] _ = (EmptyNode (-1),0)
filterGetFirst p (x:xs) start
    | p x = (x,start)
    | null xs = (EmptyNode (-1),0)
    | otherwise = filterGetFirst p xs (start+1)
{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

extractPath :: Node s a -> [(a, s)]
extractPath n 
    | (depth n) <= 0 = []
    | otherwise =  (extractPath (parentNode n)) ++ [(previousAction n, nodeState n)]

{-
    *** TODO ***

    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru 
    a găsi prima stare finală și reface calea către nodul inițial folosind 
    extractPath. 
  
    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește 
      -> Bool       -- Dacă să folosească sau nu euristica dată 
      -> [(a, s)]   -- Lista perechilor
solve s b = extractPath (fst (iterativeDeepening (createStateSpace s)))
--solve = undefined
{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))


-- TODO MAKE LIMITEDDFS NOT RETURN REAPEATED STATES
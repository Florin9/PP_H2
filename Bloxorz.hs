{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A
import Data.List
import Data.Maybe

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East | Never
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}
--type Switch = (Int,Int) --id, nr de campuri
data Cell =  EmptySpace | Hard | Soft | Switch | Block | WinningTile
    deriving (Eq,Ord)

instance Show Cell where
    show c 
        | c == Hard = hardTile:[]
        | c == Soft = softTile:[]
        | c == Switch = switch:[]
        | c == EmptySpace = emptySpace:[]
        | c == WinningTile = winningTile:[]
        | c == Block = block:[]
        | otherwise = []

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}

--orientation: 0-vertical 1 - culcat pe x, 2- culcat pe y
data Level = Level{gameMap :: (A.Array Position Cell), switchTiles :: [Position], corner :: Position, blockPos :: [Position], switches :: [(Position,Int)], orientation :: Int, wonGame :: Bool, lostGame :: Bool} 
    deriving (Eq, Ord)

{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    *** TODO ***

    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

instance Show Level where
    show level
        |(wonGame level) = '\n' : unlines [concat [show (newMap A.! (x,y)) | y <- [0..a]] | x <- [0..b]] ++ "Congrats! You won!\n"
        |(lostGame level) = '\n' : unlines [concat [show (newMap A.! (x,y)) | y <- [0..a]] | x <- [0..b]] ++ "Game Over\n"
        | otherwise =  '\n' : unlines [concat [show (newMap A.! (x,y)) | y <- [0..a]] | x <- [0..b]]
        where
            a = (snd (corner level))
            b = (fst (corner level))
            newMap = (gameMap level)A.//[(p,Block) | p <- (blockPos level)]

{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel p1 p2 = Level (A.array ((0,0),p1) [((x,y),EmptySpace) | x <- [0..(fst p1)], y<-[0..(snd p1)]]) [] p1 (p2:[]) [] 0 False False

{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}
charToCell :: Char -> Cell
charToCell c
    | c == 'H' = Hard
    | c == 'S' = Soft
    | c == 'W' = WinningTile
    | otherwise = EmptySpace
    
addTile :: Char -> Position -> Level -> Level
addTile c p l = Level ((gameMap l)A.//((p,(charToCell c)):[])) (switchTiles l) (corner l) (blockPos l) (switches l) (orientation l) (wonGame l) (lostGame l)

{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch p pList l = Level ((gameMap l)A.//((p,Switch):[])) ((switchTiles l) ++ pList) (corner l) (blockPos l) ((switches l)++[(p,(length pList))]) (orientation l) (wonGame l) (lostGame l)

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}
anyCellOnEmpty :: Level -> Bool
anyCellOnEmpty l
    | (length p) == 1 = (gameMap l)A.! firstPos == EmptySpace
    | otherwise = (gameMap l)A.! firstPos == EmptySpace || (gameMap l)A.! secondPos == EmptySpace 
    where
        p = blockPos l
        firstPos = head $ blockPos l
        secondPos = head $ tail $ blockPos l
        
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

getDelay :: Position -> Int -> Level -> Int
getDelay _ 0 _ = 0
getDelay p index l = (snd $ (switches l)!!(index - 1)) + getDelay p (index-1) l


activate :: Level -> Level
activate l
-- este activat, il dezactivez
    | ((orientation l) == 0 )&&( ((gameMap l)A.!firstPos) == Switch )&&( ((gameMap l)A.!((switchTiles l)!!(delay))) == Hard) = Level ((gameMap l)A.//[(x,EmptySpace) | x <- tileList]) (switchTiles l) (corner l) (blockPos l) (switches l) (orientation l) (wonGame l) (lostGame l)
-- este dezactivat, il activez
    | ((orientation l) == 0 )&&( ((gameMap l)A.!firstPos) == Switch )=  Level ((gameMap l)A.//[(x,Hard) | x <- tileList]) (switchTiles l) (corner l) (blockPos l) (switches l) (orientation l) (wonGame l) (lostGame l)
    | (length (blockPos l) == 2 )&&( ((gameMap l)A.!firstPos) == Switch )&&( ((gameMap l)A.!((switchTiles l)!!(delay))) == Hard) =  Level ((gameMap l)A.//[(x,EmptySpace) | x <- tileList]) (switchTiles l) (corner l) (blockPos l) (switches l) (orientation l) (wonGame l) (lostGame l)
    | (length (blockPos l) == 2 )&&( ((gameMap l)A.!firstPos) == Switch )= Level ((gameMap l)A.//[(x,Hard) | x <- tileList]) (switchTiles l) (corner l) (blockPos l) (switches l) (orientation l) (wonGame l) (lostGame l)
    --inca 2 cazuri, pentru secondPos
    | (length (blockPos l) == 2 )&&( ((gameMap l)A.!secondPos) == Switch )&&( (((gameMap l)A.!((switchTiles l)!!(delay2))) == Hard) )= Level ((gameMap l)A.//[(x,EmptySpace) | x <- tileList2]) (switchTiles l) (corner l) (blockPos l) (switches l) (orientation l) (wonGame l) (lostGame l)
    | (length (blockPos l) == 2 )&&( ((gameMap l)A.!secondPos) == Switch )=  Level ((gameMap l)A.//[(x,Hard) | x <- tileList2]) (switchTiles l) (corner l) (blockPos l) (switches l) (orientation l) (wonGame l) (lostGame l)
    | ((gameMap l)A.!firstPos == Soft )&& ((orientation l) == 0 )= Level (gameMap l) (switchTiles l)(corner l) (blockPos l) (switches l) (orientation l) (wonGame l) True  --TODO, va face jocul pierdut
    | ((gameMap l)A.!firstPos == WinningTile )&& ((orientation l) == 0 )= Level (gameMap l) (switchTiles l)(corner l) (blockPos l) (switches l) (orientation l) True (lostGame l)  --TODO, va face jocul castigat
    | anyCellOnEmpty l = Level (gameMap l) (switchTiles l)(corner l) (blockPos l) (switches l) (orientation l) (wonGame l) True --TODO, joc pierdut
    | otherwise = l
        where
            tileList = slice delay ((fromJust $ lookup firstPos (switches l)) + delay) (switchTiles l)
            delay
                |(lookup firstPos (switches l)) == Nothing = 0
                |otherwise = getDelay firstPos (fromJust $ elemIndex (firstPos,fromJust $ lookup firstPos (switches l)) (switches l)) l
            tileList2 = slice delay ((fromJust $ lookup secondPos (switches l)) + delay2) (switchTiles l)
            delay2 = getDelay secondPos (fromJust $ elemIndex (secondPos,(fromJust $ lookup secondPos (switches l))) (switches l)) l
            firstPos = head $ blockPos l
            secondPos = head $ tail $ blockPos l
{- gasesc elementul din lista switches l, ma intereseaza index-} 
{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}
--orientation: 0-vertical 1 - culcat pe y, 2- culcat pe x
-- mode: 0 = minim, 1 = maxim
getX :: Int -> [Position] -> Int
getX mode iPos
    | length (iPos) == 1 = (fst (head iPos))
    | mode == 0 = if (fst (head iPos)) <= (fst (head $ tail iPos))
                            then (fst (head iPos))
                            else (fst (head $ tail iPos))
    | mode == 1 = if (fst (head iPos)) >= (fst (head $ tail iPos))
                            then (fst (head iPos))
                            else (fst (head $ tail iPos))
getX _ _ = 0
    
getY :: Int -> [Position] -> Int
getY mode iPos
    | length (iPos) == 1 = (snd (head iPos))
    | mode == 0 = if (snd (head iPos)) <= (snd (head $ tail iPos))
                            then (snd (head iPos))
                            else (snd (head $ tail iPos))
    | mode == 1 = if (snd (head iPos)) >= (snd (head $ tail iPos))
                            then (snd (head iPos))
                            else (snd (head $ tail iPos))
                            
getY _ _ = 0
                            
updateLevel :: [Position] -> Int -> Level -> Level
updateLevel p o l = Level (gameMap l) (switchTiles l) (corner l) p (switches l) o (wonGame l) (lostGame l)

move :: Directions -> Level -> Level
move d l
    | (lostGame l) == True || (wonGame l) == True = l
    | d == North && orn == 0 = activate $ updateLevel ((getX 0 iPos - 1,getY 0 iPos):(getX 0 iPos - 2,getY 0 iPos):[]) 1 l
    | d == North && orn == 1 = activate $ updateLevel ((getX 0 iPos - 1,(getY 0 iPos)):[]) 0 l
    | d == North && orn == 2 = activate $ updateLevel ((getX 0 iPos - 1,getY 0 iPos):(getX 0 iPos - 1,getY 1 iPos):[]) 2 l
    | d == South && orn == 0 = activate $ updateLevel ((getX 1 iPos + 1,getY 0 iPos):(getX 1 iPos + 2,getY 1 iPos):[]) 1 l
    | d == South && orn == 1 = activate $ updateLevel ((getX 1 iPos + 1,getY 0 iPos):[]) 0 l
    | d == South && orn == 2 = activate $ updateLevel ((getX 1 iPos + 1,getY 0 iPos):(getX 1 iPos + 1,getY 1 iPos):[]) 2 l
    | d == East  && orn == 0 = activate $ updateLevel ((getX 0 iPos,getY 0 iPos + 1):(getX 1 iPos,getY 0 iPos + 2):[]) 2 l
    | d == East  && orn == 1 = activate $ updateLevel ((getX 0 iPos,getY 0 iPos + 1):(getX 1 iPos,getY 0 iPos + 1):[]) 1 l
    | d == East  && orn == 2 = activate $ updateLevel ((getX 0 iPos,getY 1 iPos + 1):[]) 0 l
    | d == West  && orn == 0 = activate $ updateLevel ((getX 0 iPos,getY 0 iPos - 1):(getX 0 iPos,getY 0 iPos - 2):[]) 2 l
    | d == West  && orn == 1 = activate $ updateLevel ((getX 0 iPos,getY 0 iPos - 1):(getX 1 iPos,getY 0 iPos - 1):[]) 1 l
    | d == West  && orn == 2 = activate $ updateLevel ((getX 0 iPos,getY 0 iPos - 1):[]) 0 l
    | otherwise = l
    where
        iPos = (blockPos l)
        orn = (orientation l)

{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame l = (wonGame l) == False && (lostGame l) == False

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors l = [(dir,move dir l) | dir <- [North,South,East,West], (lostGame (move dir l))==False || (wonGame (move dir l)) == True ]

    isGoal = wonGame

    -- Doar petru BONUS
    -- heuristic = undefined

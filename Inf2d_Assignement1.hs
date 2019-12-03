-- Inf2d Assignment 1 2017-2018
-- Matriculation number: s1601847
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import ConnectFour

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6

{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the 3pm Tuesday 13th March 2018.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]


-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.
next::Branch-> [Branch]
next branch = [(x,y):branch | (x,y)<- poscont, not((x,y) `elem` branch) && (x<7 && x>0) && (y<7 && y>0)]
              where poscont = [(a+1,b),(a-1,b),(a,b+1),(a,b-1)]
                    (a, b) = (fst(head(branch)), snd(head(branch)))

-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
checkArrival::Node-> Node-> Bool
checkArrival destination curNode = destination == curNode

-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.
breadthFirstSearch::Node-> (Branch-> [Branch])-> [Branch]->[Node]-> Maybe Branch
breadthFirstSearch destination next branches exploredList
  | length branches == 0 = Nothing
  | y /= [] = Just (head(y)) --if y isn't empty, AKA if branch y reaches the destination, then return the head of branch y
  | otherwise = breadthFirstSearch destination next z expList -- otherwise do breadth first search again with z as the new branches
    where y = [ x | x <- branches, checkArrival destination $ head x] -- y equals a list of branches where the head of each branch is the destination
          a = foldr1 (++) [next i | i<- branches] -- a equals a list of branches made out of the next possible moves
          z = [ b| b <- a, not((head b) `elem` expList) ] -- z is equaled to a except we filtered out the branches that have already been explored
          expList= head(head(branches)):exploredList --adds the current position to the explored list




-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.

depthFirstSearch::Node-> (Branch-> [Branch])-> [Branch]-> [Node]-> Maybe Branch
depthFirstSearch destination next  branches exploredList
  | length branches == 0 = Nothing
  | y /= [] = Just (head(y)) --if y isn't empty, AKA if branch y reaches the destination, then return the head of branch y
  | otherwise = depthFirstSearch destination next nbranches expList -- otherwise do depth first search again with z as the new branches
  where
    y = if checkArrival destination (head(head branches)) then branches else []
    expList = head(head(branches)) : exploredList
    p = next (head(branches))
    fbranches = [ x | x <- p, not(head(x) `elem` expList) ]
    nbranches = fbranches ++ tail(branches)



-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree..

depthLimitedSearch::Node-> (Branch-> [Branch])-> [Branch]-> Int-> [Node]-> Maybe Branch
depthLimitedSearch destination next branches d exploredList
  | length branches == 0 = Nothing
  | d == 0 = Nothing
  | y /= [] = Just (head(y))
  | otherwise = depthLimitedSearch destination next nbranches d expList
    where y = if checkArrival destination (head(head branches)) then branches else []
          expList = head(head(branches)) : exploredList
          p = next (head(branches))
          fbranches = [ x | x <- p, not(head(x) `elem` expList) , length(head(branches))< d] -- this makes sure that it only runs if the length of the head of branches is smaller than the pregiven depth
          nbranches = fbranches ++ tail(branches)





-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
iterDeepSearch:: Node-> (Branch-> [Branch])-> Node-> Int-> Maybe Branch
iterDeepSearch destination next initialNode d
  | y /= Nothing = y
  | otherwise =  iterDeepSearch destination next initialNode (d+1)
    where y = depthLimitedSearch destination next [[initialNode]] d []
-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the current position and the destination position.
manhattan::Node-> Node-> Int
manhattan position destination = abs(fst(destination) -fst(position)) + abs(snd(destination) -snd(position))

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.
bestFirstSearch:: Node-> (Branch-> [Branch])-> (Node->Int)-> [Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic branches exploredList
  | length branches == 0 = Nothing
  | y /= [] = Just (head(y))
  | otherwise = bestFirstSearch destination next heuristic nbranches expList
    where y = if checkArrival destination (head(head branches)) then branches else []
          expList = head(head(branches)) : exploredList
          p = next (head(branches))
          bbranches = sortBy (\x y ->  heuristic (head(x)) `compare` heuristic (head(y))) [ x | x <- p, not(head(x) `elem` expList)] -- this compares the head of all the next possible branches with each other using the heuristic
          nbranches = bbranches ++ tail(branches)

-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch:: Node-> (Branch-> [Branch])-> (Node->Int)-> (Branch-> Int)-> [Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost branches exploredList
  | length branches == 0 = Nothing
  | y /= [] = Just (head(y))
  | otherwise = aStarSearch destination next heuristic cost nbranches expList
    where y = if checkArrival destination (head(head branches)) then branches else []
          expList = head(head(branches)) : exploredList
          p = next (head(branches))
          bbranches = sortBy (\x z -> f (head x ) `compare`  f (head z )) [ x | x <- p, not(head(x) `elem` expList)]
          nbranches = bbranches ++ tail(branches)
          f x = (heuristic(x)) + cost(head(branches)) -- similar to best first search except you add in the cost of the branches along with the heuristic when comparing


-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch-> Int
cost branch = length(branch)-1


-- In this section, the function determines the score of a terminal state, assigning it a value of +1, -1 or 0:
eval :: Game-> Int
eval game = if checkWin game compPlayer == True then -1 else if checkWin game humanPlayer == True then 1 else  0


-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Role-> Game-> Int
minimax player game
    |player == 1 = if terminal game then (eval game) else maximum ([minimax (switch player) mov | mov <- moves game player]) -- recursively goes through the possible moves until it reaches a terminal state which is optimal according to the player
    |player == 0 = if terminal game then (eval game) else minimum ([minimax (switch player) mov | mov <- moves game player])
    |otherwise = 0

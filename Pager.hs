module Pager where

import Data.Maybe (maybeToList)

data Pager a = Pager {
    curPage :: Maybe a,
    prev :: [a],
    next :: [a]
} deriving Show

instance Eq a => Eq (Pager a) where
    (Pager c p n) == (Pager c' p' n') = (c == c' && p == p' && n == n')

setCurPage :: a -> Pager a -> Pager a
setCurPage x pager = pager { curPage = Just x, prev = maybeToList (curPage pager) ++ prev pager }

prevPage :: Pager a -> Pager a
prevPage pager
    | not (null (prev pager)) =
        pager { curPage = Just (head (prev pager)), prev = drop 1 (prev pager), next = maybeToList (curPage pager) ++ next pager }
    | otherwise = pager

nextPage :: Pager a -> Pager a
nextPage pager
    | not (null (next pager)) =
        pager { curPage = Just (head (next pager)), next = drop 1 (next pager), prev = maybeToList (curPage pager) ++ prev pager }
    | otherwise = pager

-- reset cur page to last next page
lastPage :: Pager a -> Pager a
lastPage pager
    | not (null (next pager)) =
        let newCur = last (next pager)
            fromNext = reverse (next pager)
            newPrev = drop 1 fromNext ++ maybeToList (curPage pager) ++ prev pager
        in  pager { curPage = Just newCur, prev = newPrev, next = [] }
    | otherwise = pager

-- reset cur page to first prev page
firstPage :: Pager a -> Pager a
firstPage pager
    | not (null (prev pager)) =
        let newCur = last (prev pager)
            fromPrev = reverse (prev pager)
            newNext = drop 1 fromPrev ++ maybeToList (curPage pager) ++ next pager
        in  pager { curPage = Just newCur, prev = [], next = newNext }
    | otherwise = pager

inPager :: Eq a => a -> Pager a -> Bool
inPager a pager = maybe False (a ==) (curPage pager) || a `elem` next pager || a `elem` prev pager

-- tests if elem is curPage or head of next
headOfPager :: Eq a => a -> Pager a -> Bool
headOfPager a pager = maybe False (a ==) (curPage pager) || a `elem` (take 1 (next pager))

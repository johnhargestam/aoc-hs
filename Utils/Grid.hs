module Utils.Grid where

-- Grid sel left right top bottom
data Grid a = Grid a [a] [a] [[a]] [[a]] deriving (Show)

value :: Grid a -> a
value (Grid x _ _ _ _) = x

west :: Grid a -> Maybe (Grid a)
west (Grid _ [] _ _ _) = Nothing
west (Grid sel (l:ls) rs ts bs) = Just $ Grid l ls (sel:rs) ts bs

east :: Grid a -> Maybe (Grid a)
east (Grid _ _ [] _ _) = Nothing
east (Grid sel ls (r:rs) ts bs) = Just $ Grid r (sel:ls) rs ts bs

north :: Grid a -> Maybe (Grid a)
north (Grid _ _ _ [] _) = Nothing
north (Grid sel ls rs (t:ts) bs) = Just $ Grid sel' ls' rs' ts (b:bs)
  where
    (ls',sel':rs') = splitAt (length ls) t
    b = ls ++ (sel:rs)

south :: Grid a -> Maybe (Grid a)
south (Grid _ _ _ _ []) = Nothing
south (Grid sel ls rs ts (b:bs)) = Just $ Grid sel' ls' rs' (t:ts) bs
  where
    (ls',sel':rs') = splitAt (length ls) b
    t = ls ++ (sel:rs)

gridToList :: Grid a -> [[a]]
gridToList (Grid sel ls rs ts bs) = reverse ts ++ [ls ++ (sel:rs)] ++ bs

listToGrid :: [[a]] -> Grid a
listToGrid [] = error "cannot make empty Grid"
listToGrid ([]:_) = error "cannot make empty Grid"
listToGrid ((t:tr):ts) = Grid t [] tr [] ts

walking :: (Grid a -> Maybe (Grid a)) -> Grid a -> [Grid a]
walking f t1 = go (f t1)
  where go (Just t2) = t1 : walking f t2
        go Nothing   = [t1]

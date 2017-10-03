----------------
-- bubble sort
----------------
bubblesort :: (Ord a) => [a] -> [a]
bubblesort [] = []
bubblesort xs = y : bubblesort ys
  where
    -- 小さい要素を右端へ寄せる
    swp [x] = [x]
    swp (x:y:zs)
      | x < y     = y : swp (x:zs)
      | otherwise = x : swp (y:zs)

    -- 右端へ寄せてひっくり返し先頭とそれ以外を束縛
    (y:ys) = reverse $ swp xs

module Main where

spiralHelper ::
     (Int -> (Int, Int) -> a) -> Int -> Int -> Int -> Int -> Int -> [a]
spiralHelper func n x y dx dy =
  let (dx', dy') =
        if x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y)
          then (-dy, dx)
          else (dx, dy)
  in func n (x, y) : spiralHelper func (n + 1) (x + dx') (y + dy') dx' dy'

spiral :: (Int -> (Int, Int) -> a) -> [a]
spiral func = spiralHelper func 1 0 0 0 (-1)

partA :: Int -> Int
partA i = spiral (\_ (x, y) -> abs x + abs y) !! i

partB :: Int -> Int
partB i =
  let helper 1 p = (1, p)
      helper n (x, y) =
        let near (a, b) = (a - x) * (a - x) + (b - y) * (b - y) < 4
        in (sum $ map fst $ filter (near . snd) $ take (n - 1) res, (x, y))
      res = spiral helper
  in head $ filter (> i) $ map fst res

main :: IO ()
main = do
  let i = 265149
  putStrLn "Day 03"
  putStr "\tPart A: "
  print $ partA i
  putStr "\tPart B: "
  print $ partB i

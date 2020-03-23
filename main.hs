fizz :: Int -> String
fizz n | n `mod` 15 == 0  = "FizzBuzz"
       | n `mod` 3  == 0  = "Fizz"
       | n `mod` 5  == 0  = "Buzz"
       | otherwise = show n

quick_sort        :: (Ord a) => [a] -> [a]
quick_sort []     = []
quick_sort (x:xs) = quick_sort ls ++ [x] ++ quick_sort rs
  where
    ls            = [l | l <- xs, l <= x]
    rs            = [r | r <- xs, r > x]

main :: IO()
main = do
  print "quick_sort:"
  print (quick_sort [3, 1, 9, 8, 10])

  print "fizz:"
  mapM_ putStrLn $ map fizz [1..20]

import Data.Char

{-
Attempt at a state monad
-}

type State = Int -- state represented as Int


-- type ST holds a 'state function'
-- this function takes a state and generates a value and an updated state
newtype ST a = ST { stfn :: State -> (a, State) }


instance Functor ST where
  -- change ST a -> ST b
  -- calculate state (a, State) then just apply f
  fmap f st = ST $ \t -> let (a,s) = stfn st t in (f a, s)


instance Applicative ST where
  pure a = ST $ \st -> (a, st)
  (ST f) <*> (ST g) = ST $ \st -> let (fn, s1) = f st
                                      (a, s2) = g s1
                                   in (fn a, s2)


instance Monad ST where
  return a = ST $ \st -> (a, st)
  (ST stf) >>= f = ST $ \t -> let (a, s) = stf t in stfn (f a) s

next :: String -> ST String
next str = ST $ \st -> ((chr st):str, st+1)

jump :: State -> a -> ST a
jump n str = ST $ \st -> (str, st + n)

test1 :: ST String
test1 = return "!"
         >>= next     -- add 'a'
         >>= next     -- add 'b'
         >>= jump 2   -- skip c, d
         >>= next     -- add 'e'

test2 :: ST String
test2 = let ops = [next, next, jump 2, next, next]
        in foldr (=<<) (return "!") ops

main = do
  putStrLn $ show $ stfn test1 97
  putStrLn $ show $ stfn test2 97

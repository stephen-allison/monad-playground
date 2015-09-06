import Data.Char

{-
Attempt at a state monad
-}

type State = Int -- state represented as Int


-- type ST holds a 'state function'
-- this function takes a state and generates a value and an updated state
newtype ST s a = ST { stfn :: s -> (a, s) }


instance Functor (ST s) where
  -- change ST a -> ST b
  -- calculate state (a, State) then just apply f
  fmap f st = ST $ \t -> let (a,s) = stfn st t in (f a, s)


instance Applicative (ST s) where
  pure a = ST $ \st -> (a, st)

  {-
  (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  this means the value yielded by the state function 'f'
  in (ST f) is a function a -> b
  -}
  (ST f) <*> (ST g) = ST $ \st -> let (fn, s1) = f st
                                      (a, s2) = g s1
                                   in (fn a, s2)


instance Monad (ST s) where
  return a = ST $ \st -> (a, st)
  (ST stf) >>= f = ST $ \t -> let (a, s) = stf t in stfn (f a) s

next :: String -> ST State String
next str = ST $ \st -> ((chr st):str, st+1)

jump :: State -> a -> ST Int a
jump n str = ST $ \st -> (str, st + n)


testFmap :: ST State Int
testFmap = fmap ord $ return 'a'

testApply :: ST Int String
testApply = let s1 = return "hello "
                s2 = return "world"
                in (++) <$> s1 <*> s2

testBind :: ST State String
testBind = return "!"
           >>= next     -- add 'a'
           >>= next     -- add 'b'
           >>= jump 2   -- skip c, d
           >>= next     -- add 'e'

testBindViaFold :: ST State String
testBindViaFold = let ops = [next, next, jump 2, next, next]
                  in foldr (=<<) (return "!") ops



main = do
  putStrLn $ showResult (stfn testFmap 0) (97,0)
  putStrLn $ showResult (stfn testApply 0) ("hello world", 0)
  putStrLn $ showResult (stfn testBind 97) ("eba!", 102)
  putStrLn $ showResult (stfn testBindViaFold 97) ("feba!", 103)

showResult result expected =
  let (msg,sign) = res (result == expected)
  in
    msg ++ "\t\t" ++ (show result) ++ " " ++ sign ++ " " ++ (show expected)

res True = ("OK","==")
res False = ("Fail","/=")

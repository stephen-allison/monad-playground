data Box a = Box a deriving Show

instance Functor Box where
  fmap f (Box a) = Box $ f a

instance Applicative Box where
  pure = Box
  Box f <*> box = fmap f box

instance Monad Box where
  return = Box
  (Box a) >>= f = f a  --f::a -> Box a

main = do
  let b = Box "Hi"
  putStrLn $ peek b
  return ()

peek (Box a) = a

boxy :: Box Int -> Box Int
boxy box = do
  n1 <- box
  let box' = Box (n1 + 2)
  n2 <- box'
  return (n1 + n2)

boxy2 :: Box Int -> Box Int
boxy2 box = box >>= \a -> return (a + 2)

-- boxy3 is desugared equivalent to boxy
boxy3 :: Box Int -> Box Int
boxy3 box = box >>= \n1 -> Box (n1 + 2)
                >>= \n2 -> Box (n1 + n2)

exclaimBox :: (Show a) => a -> Box String
exclaimBox num =  return (show num ++ "!")

addBox :: Int -> Box Int
addBox num = Box (num + 1)

allBoxy :: Int -> Box String
allBoxy num = Box num >>= addBox >>= addBox >>= exclaimBox

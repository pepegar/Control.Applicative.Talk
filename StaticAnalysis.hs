{-# LANGUAGE DeriveFunctor #-}

module StaticAnalysis where

import Control.Applicative.Free

data ExpF x = Val Int
            | Add (ExpF x) (ExpF x)
            deriving (Functor, Show)

type Exp x = Ap ExpF x

instance Monoid Int where
    mempty  = 0
    mappend = (+)

analyze :: ExpF a -> Int
analyze (Val i) = 1
analyze (Add p q) = analyze p + analyze q

eval :: ExpF a -> Int
eval (Val i) = i
eval (Add p q) = eval p + eval q

expression1 :: Exp x
expression1 = liftAp $ Add (Add (Add (Val 1) (Val 2)) (Val 3)) (Val 4)

main :: IO ()
main = do
    putStrLn "expression 1"
    print $ runAp_ eval expression1
    print $ runAp_ analyze expression1

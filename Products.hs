{-# LANGUAGE TypeOperators, GADTs #-}
module Products where

import Control.Applicative

data Product m n a = Product {
    first :: m a,
    second :: n a
}

instance (Functor m, Functor n) => Functor (Product m n) where
    fmap f fa = Product (f <$> first fa) (f <$> second fa)

instance (Applicative m, Applicative n) => Applicative (Product m n) where
    pure x = Product (pure x) (pure x)
    mf <*> mx = Product (first mf <*> first mx) (second mf <*> second mx)


main :: IO ()
main = print "asdf"

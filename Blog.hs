{-# LANGUAGE GADTs #-}
module Blog where

import Control.Applicative.Free
import Control.Applicative

type Author = String
type Post = String
type Id = Int

-- A page of the blog, that will consist of a post and its author
data Page = Page {
    post :: Post,
    author :: Author
} deriving Show

-- Our ADT
data BlogF a where
    GetPost :: Id -> BlogF Post
    GetAuthor :: Id -> BlogF Author

-- Our Free Applicative
type Blog a = Ap BlogF a

-- Convenience functions
getPost :: Id -> Blog Post
getPost id = liftAp $ GetPost id

getAuthor :: Id -> Blog Author
getAuthor id = liftAp $ GetAuthor id

-- as you can imagine, a Page does not need for the post and author to be
-- fetched in certain order, it's applicative!
renderPage :: Id -> Id -> Blog Page
renderPage postId authorId = Page <$> getPost postId
                                  <*> getAuthor authorId

-- The interpreter for our Free Applicative.  Theoretically is a Natural
-- transformation with the shape (BlogF ~> IO)
interpIO :: BlogF a -> IO a
interpIO (GetPost id) = putStrLn ("getting post " ++ show id ++ " from DB") *> pure "this is the post"
interpIO (GetAuthor id) = putStrLn ("getting author " ++ show id ++ " from DB") *> pure "Pepe García"

main :: IO ()
main = do 
    page <- runAp interpIO $ renderPage 1 1
    print page

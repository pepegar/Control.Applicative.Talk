{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module StaticAnalysis where

import Control.Applicative.Free
import Control.Applicative

type Author = String
type Comment = String
type Post = String
type Id = Int

-- Our ADT
data BlogF a where
    GetPost :: Id -> BlogF Post
    GetAuthor :: Id -> BlogF Author
    GetComments :: Id -> BlogF [(Comment, Author)]

-- A page of the blog, that will consist of a post and its author
data Page = Page {
    post :: Post,
    author :: Author,
    comments :: [(Comment, Author)]
} deriving Show

-- Our Free Applicative
type Blog a = Ap BlogF a

instance Monoid Int where
    mempty = 0
    mappend = (+)

getPost :: Id -> Blog Post
getPost id = liftAp $ GetPost id

getAuthor :: Id -> Blog Author
getAuthor id = liftAp $ GetAuthor id

getComments :: Id -> Blog [(Comment, Author)]
getComments id = liftAp $ GetComments id

renderPage :: Id -> Id -> Blog Page
renderPage post author = Page <$> getPost post
                              <*> getAuthor author
                              <*> getComments post


interpIO :: BlogF a -> IO a
interpIO (GetPost id) = putStrLn ("getting post " ++ show id ++ " from DB") *> pure "this is the post"
interpIO (GetAuthor id) = putStrLn ("getting author " ++ show id ++ " from DB") *> pure "@pepe"
interpIO (GetComments id) = putStrLn ("getting comments for post " ++ show id ++ " from DB")
    *> pure [
        ("this post rocks", "@anler"),
        ("you're right, @anler", "@lorenzo"),
        ("Oh boy, I love haskell so bad!", "@dani"),
        ("Indeed, Haskell is better than Erlang!", "@joseluis")
    ]

countInstructions :: BlogF a -> Int
countInstructions (GetPost _) = 1
countInstructions (GetAuthor _) = 1
countInstructions (GetComments _) = 1


main :: IO ()
main = do
    putStrLn "NUMBER OF REQUESTS TO THE DB:"
    print instructions
    putStrLn ""
    putStrLn "PAGE RENDERING:"
    page <- runAp interpIO page
    print page
        where instructions = runAp_ countInstructions page
              page = renderPage 1 1

module Tree where

data Tree = Node | Nil
            deriving (Show)

data Node = Node {
    left  :: Maybe Node,
    right :: Maybe Node
}
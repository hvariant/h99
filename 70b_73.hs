import Text.ParserCombinators.Parsec

data Tree a = Node a [Tree a]
        deriving (Eq, Show)

tree1 = Node 'a' []
 
tree2 = Node 'a' [Node 'b' []]
 
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
 
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
 
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

nnodes :: Tree a -> Int
nnodes (Node _ l) = 1 + (sum $ map nnodes l)


treeToString :: Tree Char -> String
treeToString (Node c sub) = c:(concat $ map (\t -> treeToString t) sub) ++ "^"


stringToTreeRules :: Parser (Tree Char)
stringToTreeRules = do
                     tag <- oneOf ['a'..'z']
                     sub <- ss
                     char '^'

                     return (Node tag sub)
    where ss = do
                mt <- optionMaybe (oneOf ['a'..'z'])
                case mt of
                    Nothing -> return []
                    Just tag -> do
                               sub' <- ss
                               char '^'
                               let t = Node tag sub'

                               sub <- ss
                               return (t:sub)

stringToTree :: String -> Tree Char
stringToTree s = case parse stringToTreeRules "parse error" s of
                    Left _ -> error "parse error"
                    Right t -> t



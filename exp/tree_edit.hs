import HCL

type Forest a = [Node a]
data Node a   = Node a (Forest a)
myForest     :: Forest String
myForest      = [Node "1"
                   [Node "1.1"
                      [Node "1.1.1" []],
                    Node "1.2" []],
                 Node "2" []]
forestElems  :: Forest a -> [a]
forestElems   = concat . map nodeElems
 where nodeElems (Node x cs) = x : forestElems cs
depthMap     :: (Int -> a -> b) -> Int -> Forest a -> Forest b
depthMap f d  = map depthNode
 where depthNode (Node x cs)
          = Node (f d x) (depthMap f (d+1) cs)
showForest   :: Forest String -> String
showForest    = unlines . forestElems . depthMap indent 1
 where indent d x = replicate (2*d) '\SP' ++ x
data Position a = Pos {left :: [Node a],
                       up   :: [Level a],
                       right:: [Node a]}
type Level a    = ([Node a], a, [Node a])
rootPosition   :: Forest a -> Position a
rootPosition f  = Pos [] [] f

reconstruct               :: Position a -> Forest a
reconstruct (Pos ls us rs) = foldl recon (reverse ls ++ rs) us
 where recon fs (ls,x,rs) = reverse ls ++ [Node x fs] ++ rs
rightValue                         :: Position a -> Maybe a
rightValue (Pos _ _ (Node x _ : _)) = Just x
rightValue _                        = Nothing

moveUp, moveDown, moveLeft, moveRight
  :: Position a -> Maybe (Position a)

moveLeft  (Pos ls us rs)
               = repos ls (\n ns -> Pos ns us (n:rs))

moveRight (Pos ls us rs)
               = repos rs (\n ns -> Pos (n:ls) us ns)

moveDown  (Pos ls us rs)
               = repos rs (\(Node x cs) ns -> Pos [] ((ls,x,ns):us) cs)

moveUp    (Pos ls us rs)
               = repos us (\(as,x,bs) vs -> Pos as vs (make x : bs))
                 where make x = Node x (reverse ls ++ rs)

repos         :: [b] -> (b -> [b] -> Position a) -> Maybe (Position a)
repos []     f = Nothing
repos (x:xs) f = Just (f x xs)
insertNode    :: a -> Position a -> Position a
insertNode x (Pos ls us rs) = Pos ls us (Node x [] : rs)

deleteNode    :: Position a -> Maybe (Position a)
deleteNode (Pos ls us rs) = repos rs (\_ ns -> Pos ls us ns)
reflect       :: Position a -> Position a
reflect (Pos ls us rs) = Pos rs us ls
loop  :: Pos -> IO ()
loop p = do ch <- getChar
            putChar '\n'
            case ch of
             -- whitepace
             '\n' -> loop p
             '\t' -> loop p
             ' '  -> loop p

             -- basic movement
             'c'  -> tryTo p moveDown  noNode loop
             'n'  -> tryTo p moveRight noNode loop
             'b'  -> tryTo p moveLeft  noPrev loop
             'p'  -> tryTo p moveUp    noPar  loop

             -- delete and insert
             'd'  -> tryTo p deleteNode noNode loop

             'i'  -> do putStrLn "Enter new key: "
                        key <- getLine
                        loop (insertNode key p)

             -- display commands
             'k'  -> tryTo p rightValue noNode $ \x ->
                     putStrLn x >> loop p

             's'  -> do putStr
                         (showForest
                            (reconstruct
                               (insertNode "<*>" p)))
                        loop p


             -- a reflection
             'r'  -> loop (reflect p)

             -- quit command
             'q'  -> return ()

             _    -> do putStrLn "Error: bad command"
                        loop p

tryTo         :: Pos -> (Pos -> Maybe a) -> String -> (a -> IO ()) -> IO ()
tryTo p f e c  = case f p of
                   Just x  -> c x
                   Nothing -> do putStrLn e
                                 loop p
type Pos = Position String
noNode = "Error: not at node"
noPrev = "Error: no previous sibling"
noPar  = "Error: node has no parent"
main = loop (rootPosition myForest)

main2 = runRequest $ reqIterate treeMenu (rootPosition myForest)

treeMenu tree = reqMenu choices Nothing
  where
    choices = 
      [("Move Down", makeMove moveDown),
      ("Move Up", makeMove moveUp),
      ("Move Left", makeMove moveLeft),
      ("Move Right", makeMove moveRight),
      ("Insert Node", addNode),
      ("Remove Node", removeNode),
      ("Print Curr Value", do { reqIO $ putStrLn (currValue tree); return tree }),
      ("Print Entire Tree", do { reqIO $ putStr (showForest (reconstruct (insertNode "<*>" tree))); return tree }),
      ("Reflect Tree", return (reflect tree)),
      ("Quit", reqFail)]
    makeMove direction = treeOp "Can't make move" direction
    removeNode = treeOp "No node to delete" deleteNode
    treeOp errMsg op =
      maybe (reqIO (putStrLn errMsg) >> return tree)
            return
            (op tree)
    addNode =
      do
        val <- prompt "Enter a new key: " reqResp
        return (insertNode val tree)
    currValue tree = maybe "" (\val -> "Current Node: " ++ val) (rightValue tree)
      

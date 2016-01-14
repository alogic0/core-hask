module Language where 
--import Utils 
data Expr a 
  = EVar Name             -- Variables 
  | ENum Int              -- Numbers 
  | EConstr Int Int       -- Constructor tag arity 
  | EAp (Expr a) (Expr a) -- Applications 
  | ELet                  -- Let(rec) expressions 
      IsRec               -- boolean with True = recursive, 
      [(a, Expr a)]       -- Definitions 
      (Expr a)            -- Body of let(rec) 
  | ECase                 -- Case expression 
      (Expr a)            -- Expression to scrutinise 
      [Alter a]           -- Alternatives
  | ELam [a] (Expr a)     -- Lambda abstractions 
      deriving (Show)

type CoreExpr = Expr Name

type Name = String

type IsRec = Bool 

recursive, nonRecursive :: IsRec 
recursive = True 
nonRecursive = False

bindersOf :: [(a,b)] -> [a] 
bindersOf defns = [name | (name, rhs) <- defns]

rhssOf :: [(a,b)] -> [b] 
rhssOf defns = [rhs| (name, rhs) <- defns]

type Alter a = (Int, [a], Expr a) 
type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool 
isAtomicExpr (EVar v) = True 
isAtomicExpr (ENum n) = True 
isAtomicExpr e = False

type Program a = [ScDefn a] 
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a) 
type CoreScDefn = ScDefn Name

preludeDefs :: CoreProgram 
preludeDefs 
  = [ ("I", ["x"], EVar "x")
    , ("K", ["x","y"], EVar "x")
    , ("K1",["x","y"], EVar "y")
    , ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x")) 
                               (EAp (EVar "g") (EVar "x")))
    , ("compose", ["f","g","x"], EAp (EVar "f") 
                                     (EAp (EVar "g") (EVar "x")))
    , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr 
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s) 
  where 
  e2s = e2 : e2s

data Iseq 
  = INil 
  | IStr String 
  | IAppend Iseq Iseq

iNil :: Iseq              -- The empty iseq 
iNil = INil 

iStr :: String -> Iseq      -- Turn a string into an iseq 
iStr str = IStr str

iAppend :: Iseq -> Iseq -> Iseq    -- Append two iseqs 
iAppend seq1 seq2 = IAppend seq1 seq2 

iNewline :: Iseq      -- New line with indentation 
iNewline = IStr "\n"

iIndent :: Iseq -> Iseq      -- Indent an iseq 
iIndent seq = seq 

flatten :: [Iseq] -> String 
flatten [] = ""
flatten (INil : seqs) = flatten seqs
flatten (IStr s : seqs) = s ++ (flatten seqs)
flatten (IAppend seq1 seq2 : seqs) = flatten (seq1 : seq2 : seqs)

iDisplay :: Iseq -> String    -- Turn an iseq into a string
iDisplay seq = flatten [seq]

iConcat:: [Iseq] -> Iseq 
iConcat xs = foldr iAppend iNil xs

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave s xs =
  iConcat (iInterleave' s xs)
  where
  iInterleave' _ [] = []
  iInterleave' _ [x] = [x]
  iInterleave' s (x:xs) = x : s : iInterleave' s xs

pprAExpr :: CoreExpr -> Iseq 
pprAExpr e | isAtomicExpr e = pprExpr e 
pprAExpr e | otherwise = (iStr "(") `iAppend` pprExpr e `iAppend` (iStr ")")

pprExpr :: CoreExpr -> Iseq 
pprExpr (ENum n) = iStr $ show n 
pprExpr (EVar v) = iStr v 
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr) 
  = iConcat [ iStr keyword, iNewline 
            , iStr "",iIndent (pprDefns defns),iNewline 
            , iStr "in ",pprExpr expr ] 
  where 
  keyword | not isrec = "let" 
          | isrec = "letrec" 

pprDefns :: [(Name,CoreExpr)] -> Iseq 
pprDefns defns = iInterleave sep (map pprDefn defns) 
  where 
  sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseq 
pprDefn (name, expr) 
  = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]


--pprint :: CoreProgram -> String
--pprint prog = iDisplay (pprProgram prog)



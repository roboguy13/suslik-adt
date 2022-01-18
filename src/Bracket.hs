module Bracket
  (Bracketed
  ,topLevelString
  ,topLevel
  ,bLine
  ,bracketed
  ,bUnlines
  ,getBracketedItem
  ,getBracketed
  )
  where

import           Data.List

data Bracketed
  = TopLevel [Bracketed]
  | Bracketed BracketedItem

data BracketedItem
  = StringItem     [String]
  | BracketedItem  [BracketedItem]

instance Semigroup Bracketed where
  TopLevel xs <> TopLevel ys = TopLevel (xs <> ys)
  x <> TopLevel ys = TopLevel (x : ys)
  TopLevel xs <> y = TopLevel (xs ++ [y])
  Bracketed x <> Bracketed y = TopLevel [Bracketed x, Bracketed y]

indentLevel :: Int
indentLevel = 2

indentSpaces :: String
indentSpaces = replicate indentLevel ' '

indent :: String -> String
indent = (indentSpaces ++)

topLevelString :: String -> Bracketed
topLevelString = Bracketed . StringItem . (:[])

topLevel :: BracketedItem -> Bracketed
topLevel = Bracketed

leftBracket, rightBracket :: String
leftBracket = "{"
rightBracket = "}"

bUnlines :: [String] -> BracketedItem
bUnlines = StringItem

bracketed :: [BracketedItem] -> BracketedItem
bracketed = BracketedItem

bLine :: String -> BracketedItem
bLine = StringItem . (:[])

getBracketedStringList :: BracketedItem -> [String]
getBracketedStringList (StringItem xs) = xs
getBracketedStringList (BracketedItem xs) = --map (indent . getBracketedItem) xs
  let r = concatMap (map indent . getBracketedStringList) xs
  in
  leftBracket : r ++ [rightBracket]

unlinesMap :: (a -> String) -> [a] -> String
unlinesMap f = unlines . map f

getBracketedItem :: BracketedItem -> String
getBracketedItem (StringItem xs) = unlines xs
getBracketedItem (BracketedItem xs) =
  let r = unlines $ concatMap getBracketedStringList xs
  in
  r

getBracketed :: Bracketed -> String
getBracketed (TopLevel xs) = intercalate "\n" $ map getBracketed xs
getBracketed (Bracketed x) = getBracketedItem x

test :: Bracketed
test =
  topLevel $
    bracketed
    [ bLine "test"
    , bracketed
      [ bLine "innerTest"
      , bracketed
        [ bLine "innerInnerTest"
        ]
      ]
    ]


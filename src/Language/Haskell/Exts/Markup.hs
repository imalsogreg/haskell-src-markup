{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Exts.Markup where

import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Exts.Annotated.Syntax
import           Data.Generics.Uniplate
import           Data.Typeable
import qualified Data.Text.Lazy as T
import           GHC.Int
import           GHC.Generics

-- TODO - escape characters in writeAttr, writeTag
-- TODO - escape characters in pre block, adjusting insertion index

data TagData = TagData { className :: T.Text
                       , spanId    :: T.Text
                       , spanAttrs :: [(T.Text,T.Text)]
                       }

------------------------------------------------------------------------------
testModule :: IO (Module SrcSpanInfo, [Comment])
testModule = do
  (ParseOk (m,comms)) <- parseFileWithComments defaultParseMode
                 "/home/greghale/Programming/throwaway/fib/src/Fib.hs"
  return (m,comms)

writeAttr :: (T.Text, T.Text) -> T.Text
writeAttr (k,v) = T.concat [k,"=","\"",v,"\""]

writeTag :: TagData -> T.Text
writeTag TagData{..} =
  T.concat ["<span class=\""
           ,className
           ,"\" "
           ,T.intercalate " " (map writeAttr spanAttrs)
           ,">"]

type TagOps = [(SrcSpanInfo,TagData)]

type PreLine  = T.Text
type PreBlock = [PreLine] -- Lines of text

type TargetFun = Int64 -> Int64

------------------------------------------------------------------------------
addTagToLine :: (TargetFun, PreLine) -> (SrcSpanInfo,TagData)
             -> (TargetFun, PreLine)
addTagToLine (getInsPt, pLine) (SrcSpanInfo pos _,t) =
  let startTag     = writeTag t
      endTag       = "</span>"
      startPos     = getInsPt . fI. snd $ srcSpanStart pos
      endPos       = getInsPt . fI. snd $ srcSpanEnd   pos
      (pA,pB,pC)   = let (ab, c) = T.splitAt endPos   pLine
                         (a,  b) = T.splitAt startPos ab
                     in  (a,b,c)
      getInsPt' x
        | x < startPos = x
        | x < endPos   = x + T.length startTag
        | otherwise    = x + T.length startTag + T.length endTag
  in  (getInsPt' . getInsPt, T.concat [pA, startTag, pB, endTag, pC])

addTagsToLine :: PreLine -> [(SrcSpanInfo,TagData)] -> PreLine
addTagsToLine pLine xs = snd $ foldr (flip addTagToLine) (id,pLine) xs

fI = fromIntegral

testCode :: PreLine
testCode = "123456789abcdef"

testTag :: (SrcSpanInfo,TagData)
testTag = let srcSpan = SrcSpan "whoknows" 1 0 1 4
              td = TagData "Func" "" [("font","huge")]
          in  (SrcSpanInfo srcSpan [],td)

testTag2 :: (SrcSpanInfo,TagData)
testTag2 = let srcSpan = SrcSpan "whoknows" 1 6 1 7
               td = TagData "Part" "" [("font","medium")]
           in  (SrcSpanInfo srcSpan [],td)

testTag3 :: (SrcSpanInfo,TagData)
testTag3 = let srcSpan = SrcSpan "whoknows" 1 0 1 13
               td = TagData "Part" "" [("font","medium")]
           in  (SrcSpanInfo srcSpan [],td)

testPos = SrcSpanInfo (SrcSpan "" 1 1 1 10) []

instance Generic SrcSpanInfo where

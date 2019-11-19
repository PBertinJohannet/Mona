{-# LANGUAGE LambdaCase #-}
import Test.HUnit
import Mona
import qualified TestParser as P
import Control.Arrow
import Text.Parsec
import System.Directory
import qualified Data.Text.Lazy as L
import System.Path.Glob

asTest :: P.Test -> [Test]
asTest = fmap shouldGive . asTest'
  where
    asTest' = \case
      P.BaseCode name code tests -> withBase name code <$> (tests >>= asTest')
      P.End expected pos pos' -> return ("", "", expected)

type Name = String
type Code = String
type Expected = String
type TestVs = (Name, Code, Expected)

withBase :: String -> String -> TestVs -> TestVs
withBase name code (name', code', expected) = (name ++ "." ++ name', code ++ "\n" ++ code', expected)

shouldGive :: TestVs -> Test
shouldGive (name, code, expected) = name ~: expected ~=? compile code


{-
singleSection = "(Others . unique = participation in something interesting.)"
  `shouldGive` [Multiple "Others " [Sub ("unique ", [Normal "participation in something interesting."])]]
-}


runTestFile :: String -> IO ()
runTestFile name = do
  f <- L.pack <$> readFile name
  case P.parseModule name f of
    Left e -> print e
    Right t -> do
      runTestTT $ TestLabel name $ TestList $ asTest t
      return ()

main :: IO ()
main = do
  files <- glob "test/*.md"
  traverse runTestFile files
  return ()

{-# LANGUAGE LambdaCase #-}
import Test.HUnit
import Mona
import qualified TestParser as P
import Control.Arrow
import Text.Parsec
import qualified Data.Text.Lazy as L

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
      c <- runTestTT $ TestLabel name $ TestList (t >>= asTest)
      print c

main :: IO ()
main = do
  runTestFile "test/safeList.md"
  return ()

{-# LANGUAGE LambdaCase #-}
import Test.HUnit
import Mona
import Env
import qualified TestParser as P
import Control.Arrow
import Text.Parsec
import System.Directory
import qualified Data.Text.Lazy as L
import System.FilePath.Posix
import Data.Maybe


asTest :: P.Test -> [Test]
asTest = fmap shouldGive . asTest'
  where
    asTest' = \case
      P.BaseCode name code tests -> withBase name code <$> (tests >>= asTest')
      P.End expected schemes pos pos' -> return ("", "", expected, schemes)

type SchemeConstraint = (String, String)
type Name = String
type Code = String
type Expected = String
type Schemes = [SchemeConstraint]
type TestVs = (Name, Code, Expected, Schemes)

withBase :: String -> String -> TestVs -> TestVs
withBase name code (name', code', expected, schemes) = (name ++ "." ++ name', code ++ "\n" ++ code', expected, schemes)

shouldGive :: TestVs -> Test
shouldGive (name, code, expected, schemes) = name ~: expected ~=? check schemes (compile code)

check :: [SchemeConstraint] -> Either String (Env.Envs)-> String
check _ (Left s) = s
check schemes (Right envs) = fromMaybe "compiled successfully" $ checkSchemes (varEnv envs) schemes

checkSchemes :: Env.Env -> [SchemeConstraint] -> Maybe String
checkSchemes _ [] = Nothing
checkSchemes types ((name, expectedScheme):ss) = case Env.lookup name types of
  Nothing -> Just $ "error not found " ++ name
  Just scheme -> 
    let schemeStr = pretty scheme in
      if schemeStr == expectedScheme 
        then checkSchemes types ss 
        else Just ("expected " ++ name ++ " of type : " ++ expectedScheme ++ " but got : " ++ schemeStr)


runTestFile :: String -> IO ()
runTestFile name = do
  f <- L.pack <$> readFile name
  case P.parseModule name f of
    Left e -> print e
    Right t -> do
      runTestTT $ TestLabel name $ TestList $ asTest t
      return ()

runDir :: String -> IO ()
runDir dir = do
  files <- filter (".md" `isExtensionOf`) <$> getDirectoryContents dir
  let fullPaths = (dir ++) <$> files
  traverse runTestFile fullPaths
  return ()

main :: IO ()
main = do
  runDir "test/"
  runDir "test/errors/"
  runDir "test/others/"
  runDir "test/gadts/"
  runDir "test/examples/"

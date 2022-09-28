module Main where
import Mona
import System.Environment
(<&>) = flip fmap


main = (getArgs <&> head >>= readFile) >>= run >>= putStrLn

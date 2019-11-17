import Test.HUnit
import Parser
import Data.Text.Lazy (pack)
import Control.Arrow
import Text.Parsec

shouldGive :: String -> [ExprT String] -> Test
shouldGive source result = source ~: Right result ~=? (removePos <$> parseConf "source" (pack source))

singleSection = "(Others . unique = participation in something interesting.)"
  `shouldGive` [Multiple "Others " [Sub ("unique ", [Normal "participation in something interesting."])]]

singleSectionAlt = "(Others = ( unique = participation in something interesting.))"
  `shouldGive` [Multiple "Others " [Sub ("unique ", [Normal "participation in something interesting."])]]

twoSections = "(Others = ( first = participation in something interesting.)\n(second = some cool project))"
  `shouldGive` [Multiple "Others " [Sub ("first ", [Normal "participation in something interesting."]),
                                  Sub ("second ", [Normal "some cool project"])]]

noSubSection = "(Others = participation in something interesting.)"
  `shouldGive` [Single ("Others ", [Normal "participation in something interesting."])]

block = "(Others = (( first = participation in something interesting.)\n(second = some cool project)))"
  `shouldGive` [Multiple "Others " [Block [("first ", [Normal "participation in something interesting."]),
                                          ("second ", [Normal "some cool project"])]]]

escapeChar = "(Others = participation in \\(something interesting.)"
  `shouldGive` [Single ("Others ", [Normal "participation in ", Literal "(", Normal "something interesting."])]

italicInside = "(Others = ( unique = participation in /something/ interesting.))"
  `shouldGive` [Multiple "Others " [Sub ("unique ", [Normal "participation in ", Italics "something", Normal " interesting."])]]

boldInside = "(Others = ( unique = participation in *something* interesting.))"
  `shouldGive` [Multiple "Others " [Sub ("unique ", [Normal "participation in ", Bold "something", Normal " interesting."])]]

linkInside = "(Others = ( unique = participation in [something#https://chrisdone.com/posts/monads-are-burritos/] interesting.))"
  `shouldGive` [Multiple "Others " [Sub ("unique ", [Normal "participation in ", Link "something" "https://chrisdone.com/posts/monads-are-burritos/", Normal " interesting."])]]

italicThenBoldThenLinkThenNormal = "(Others = ( unique =/ participation/* in *[something#https://chrisdone.com/posts/monads-are-burritos/] interesting.))"
  `shouldGive` [Multiple "Others " [Sub ("unique ", [Italics " participation", Bold " in ", Link "something" "https://chrisdone.com/posts/monads-are-burritos/", Normal " interesting."])]]

completeExample = ("(Personal informations =\n" ++
  "(Name/Surname = Pierre Bertin-Johannet)\n" ++
  "(Mail = pierre.bertin-johannet@orange.fr)\n" ++
  "(italics = /italics/)\n" ++
  "(adress = begin \\n second)\n" ++
  "(github = [PBertinJohannet#https://github.com/PBertinJohannet]))\n" ++
  "(Work experience =\n" ++
  "  (bold = *bold*)\n" ++
  "  (\n" ++
  "   (block1 = first)\n" ++
  "   (block2 = second)\n" ++
  "  )\n" ++
  "  (paren = \\(inparen\\)))\n" ++
  "(Languages = some interesting languages)\n" ++
  "(Others . unique = participation in something interesting.)")
  `shouldGive`
  [
    Multiple "Personal informations "
      [
        Sub ("Name/Surname ", [Normal "Pierre Bertin-Johannet"]),
        Sub ("Mail ", [Normal "pierre.bertin-johannet@orange.fr"]),
        Sub ("italics ", [Italics "italics"]),
        Sub ("adress ", [Normal "begin ", Literal "n", Normal " second"]),
        Sub ("github ", [Link "PBertinJohannet" "https://github.com/PBertinJohannet"])
      ],
    Multiple "Work experience "
      [
        Sub ("bold ", [Bold "bold"]),
        Block
          [
            ("block1 ", [Normal "first"]),
            ("block2 ", [Normal "second"])
          ],
        Sub ("paren ", [Literal "(", Normal "inparen", Literal ")"])
      ],
    Single ("Languages ", [Normal "some interesting languages"]),
    Multiple "Others " [Sub ("unique ", [Normal "participation in something interesting."])]
  ]

main :: IO ()
main = do
  c <- runTestTT $ TestList [singleSection, singleSectionAlt, twoSections, noSubSection, block, escapeChar, italicInside, boldInside, linkInside, italicThenBoldThenLinkThenNormal, completeExample]
  print c
  return ()

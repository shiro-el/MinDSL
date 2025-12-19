{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import MinDSL.AST
import MinDSL.Lexer
import MinDSL.Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lexer" $ do
    lexerSpec

  describe "Expression Parser" $ do
    expressionSpec

  describe "Scale Parser" $ do
    scaleSpec

-- =============================================================================
-- Lexer Tests
-- =============================================================================

lexerSpec :: Spec
lexerSpec = do
  describe "integer" $ do
    it "parses positive integers" $ do
      parse integer "" "42" `shouldParse` 42

    it "parses zero" $ do
      parse integer "" "0" `shouldParse` 0

  describe "stringLiteral" $ do
    it "parses double-quoted strings" $ do
      parse stringLiteral "" "\"hello\"" `shouldParse` "hello"

    it "parses single-quoted strings" $ do
      parse stringLiteral "" "'world'" `shouldParse` "world"

    it "handles escape sequences" $ do
      parse stringLiteral "" "\"hello\\nworld\"" `shouldParse` "hello\nworld"

  describe "identifier" $ do
    it "parses simple identifiers" $ do
      parse identifier "" "foo" `shouldParse` "foo"

    it "parses identifiers with underscores" $ do
      parse identifier "" "foo_bar" `shouldParse` "foo_bar"

    it "parses identifiers with numbers" $ do
      parse identifier "" "q1" `shouldParse` "q1"

    it "rejects reserved words" $ do
      parse identifier "" `shouldFailOn` "scale"

-- =============================================================================
-- Expression Tests
-- =============================================================================

expressionSpec :: Spec
expressionSpec = do
  describe "literals" $ do
    it "parses integers" $ do
      parse expressionP "" "42" `shouldParse` LiteralInt 42

    it "parses floats" $ do
      parse expressionP "" "3.14" `shouldParse` LiteralFloat 3.14

    it "parses strings" $ do
      parse expressionP "" "\"hello\"" `shouldParse` LiteralString "hello"

    it "parses booleans" $ do
      parse expressionP "" "true" `shouldParse` LiteralBool True
      parse expressionP "" "false" `shouldParse` LiteralBool False

    it "parses null" $ do
      parse expressionP "" "null" `shouldParse` LiteralNull

  describe "identifiers" $ do
    it "parses simple identifiers" $ do
      parse expressionP "" "foo" `shouldParse` Identifier "foo"

  describe "range expressions" $ do
    it "parses item ranges" $ do
      parse expressionP "" "q1..q9" `shouldParse` Range "q1" "q9"

  describe "function calls" $ do
    it "parses no-arg calls" $ do
      parse expressionP "" "foo()" `shouldParse` Call "foo" []

    it "parses single-arg calls" $ do
      parse expressionP "" "sum(q1)" `shouldParse` Call "sum" [Identifier "q1"]

    it "parses multi-arg calls" $ do
      parse expressionP "" "max(a, b)" `shouldParse`
        Call "max" [Identifier "a", Identifier "b"]

    it "parses nested calls" $ do
      parse expressionP "" "round(sum(q1..q9))" `shouldParse`
        Call "round" [Call "sum" [Range "q1" "q9"]]

  describe "arrays" $ do
    it "parses empty arrays" $ do
      parse expressionP "" "[]" `shouldParse` Array []

    it "parses arrays with elements" $ do
      parse expressionP "" "[1, 2, 3]" `shouldParse`
        Array [LiteralInt 1, LiteralInt 2, LiteralInt 3]

  describe "binary operations" $ do
    it "parses addition" $ do
      parse expressionP "" "1 + 2" `shouldParse`
        Binary Add (LiteralInt 1) (LiteralInt 2)

    it "parses subtraction" $ do
      parse expressionP "" "5 - 3" `shouldParse`
        Binary Sub (LiteralInt 5) (LiteralInt 3)

    it "respects precedence" $ do
      parse expressionP "" "1 + 2 * 3" `shouldParse`
        Binary Add (LiteralInt 1) (Binary Mul (LiteralInt 2) (LiteralInt 3))

    it "parses comparisons" $ do
      parse expressionP "" "x >= 10" `shouldParse`
        Binary Gte (Identifier "x") (LiteralInt 10)

    it "parses logical operators" $ do
      parse expressionP "" "a and b or c" `shouldParse`
        Binary Or (Binary And (Identifier "a") (Identifier "b")) (Identifier "c")

  describe "unary operations" $ do
    it "parses negation" $ do
      parse expressionP "" "-5" `shouldParse` Unary Neg (LiteralInt 5)

    it "parses not" $ do
      parse expressionP "" "not true" `shouldParse` Unary Not (LiteralBool True)

  describe "ternary operations" $ do
    it "parses simple ternary" $ do
      parse expressionP "" "true ? 1 : 2" `shouldParse`
        Conditional (LiteralBool True) (LiteralInt 1) (LiteralInt 2)

    it "parses ternary with comparison" $ do
      parse expressionP "" "x > 5 ? \"high\" : \"low\"" `shouldParse`
        Conditional
          (Binary Gt (Identifier "x") (LiteralInt 5))
          (LiteralString "high")
          (LiteralString "low")

    it "parses nested ternary (right-associative)" $ do
      parse expressionP "" "a ? 1 : b ? 2 : 3" `shouldParse`
        Conditional (Identifier "a") (LiteralInt 1)
          (Conditional (Identifier "b") (LiteralInt 2) (LiteralInt 3))

    it "parses ternary with function calls" $ do
      parse expressionP "" "count(all) > 5 ? sum(q1..q9) : 0" `shouldParse`
        Conditional
          (Binary Gt (Call "count" [Identifier "all"]) (LiteralInt 5))
          (Call "sum" [Range "q1" "q9"])
          (LiteralInt 0)

-- =============================================================================
-- Scale Parser Tests
-- =============================================================================

scaleSpec :: Spec
scaleSpec = do
  describe "minimal scale" $ do
    it "parses a minimal scale definition" $ do
      let input = T.unlines
            [ "scale: TestScale"
            , "version: \"1.0.0\""
            , "meta:"
            , "  name: { ko: \"테스트\", en: \"Test\" }"
            , "response_type: likert(1-5)"
            , "items:"
            , "  - { id: q1, text: { ko: \"질문 1\", en: \"Question 1\" } }"
            , "scoring:"
            , "  total: sum(q1)"
            ]
      case parseScale input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right scale -> do
          scaleName scale `shouldBe` "TestScale"
          scaleVersion scale `shouldBe` "1.0.0"
          length (scaleItems scale) `shouldBe` 1

  describe "PHQ-9 style scale" $ do
    it "parses a PHQ-9 style scale" $ do
      let input = T.unlines
            [ "scale: PHQ9"
            , "version: \"1.0.0\""
            , "meta:"
            , "  name: { ko: \"환자 건강 설문지-9\", en: \"Patient Health Questionnaire-9\" }"
            , "  category: depression"
            , "response_type: likert(0-3)"
            , "items:"
            , "  - { id: q1, text: { ko: \"일을 하는 것에 흥미나 재미가 거의 없음\", en: \"Little interest or pleasure\" } }"
            , "  - { id: q2, text: { ko: \"기분이 가라앉거나 우울하거나 희망이 없음\", en: \"Feeling down, depressed\" } }"
            , "scoring:"
            , "  total: sum(q1..q2)"
            , "  severity: cut(total, [5, 10, 15, 20], [minimal, mild, moderate, moderately_severe, severe])"
            ]
      case parseScale input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right scale -> do
          scaleName scale `shouldBe` "PHQ9"
          length (scaleItems scale) `shouldBe` 2
          let (ruleName, _) = head $ scoringRules $ scaleScoring scale
          ruleName `shouldBe` "total"

  describe "extended features" $ do
    it "parses time_frame in meta" $ do
      let input = T.unlines
            [ "scale: TestScale"
            , "version: \"1.0.0\""
            , "meta:"
            , "  name: { ko: \"테스트\", en: \"Test\" }"
            , "  time_frame: { ko: \"지난 2주\", en: \"Past 2 weeks\" }"
            , "response_type: likert(1-5)"
            , "items:"
            , "  - { id: q1, text: { ko: \"질문 1\", en: \"Question 1\" } }"
            , "scoring:"
            , "  total: sum(q1)"
            ]
      case parseScale input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right scale -> do
          case metaTimeFrame (scaleMeta scale) of
            Nothing -> expectationFailure "time_frame should be present"
            Just tf -> do
              textKo tf `shouldBe` Just "지난 2주"
              textEn tf `shouldBe` Just "Past 2 weeks"

    it "parses reverse field in items" $ do
      let input = T.unlines
            [ "scale: TestScale"
            , "version: \"1.0.0\""
            , "meta:"
            , "  name: { ko: \"테스트\", en: \"Test\" }"
            , "response_type: likert(1-5)"
            , "items:"
            , "  - { id: q1, text: { ko: \"질문 1\", en: \"Question 1\" } }"
            , "  - { id: q2, text: { ko: \"질문 2\", en: \"Question 2\" }, reverse: true }"
            , "scoring:"
            , "  total: sum(q1..q2)"
            ]
      case parseScale input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right scale -> do
          let items = scaleItems scale
          itemReverse (items !! 0) `shouldBe` False
          itemReverse (items !! 1) `shouldBe` True

    it "parses subscale field in items" $ do
      let input = T.unlines
            [ "scale: TestScale"
            , "version: \"1.0.0\""
            , "meta:"
            , "  name: { ko: \"테스트\", en: \"Test\" }"
            , "response_type: likert(1-5)"
            , "items:"
            , "  - { id: q1, text: { ko: \"질문 1\", en: \"Question 1\" }, subscale: cognitive }"
            , "  - { id: q2, text: { ko: \"질문 2\", en: \"Question 2\" }, subscale: somatic }"
            , "scoring:"
            , "  total: sum(q1..q2)"
            ]
      case parseScale input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right scale -> do
          let items = scaleItems scale
          itemSubscale (items !! 0) `shouldBe` Just "cognitive"
          itemSubscale (items !! 1) `shouldBe` Just "somatic"

    it "parses subscales section" $ do
      let input = T.unlines
            [ "scale: TestScale"
            , "version: \"1.0.0\""
            , "meta:"
            , "  name: { ko: \"테스트\", en: \"Test\" }"
            , "response_type: likert(1-5)"
            , "items:"
            , "  - { id: q1, text: { ko: \"질문 1\", en: \"Question 1\" } }"
            , "  - { id: q2, text: { ko: \"질문 2\", en: \"Question 2\" } }"
            , "subscales:"
            , "  cognitive:"
            , "    label: { ko: \"인지적\", en: \"Cognitive\" }"
            , "    items: [q1, q2]"
            , "scoring:"
            , "  total: sum(q1..q2)"
            ]
      case parseScale input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right scale -> do
          length (scaleSubscales scale) `shouldBe` 1
          let sub = head (scaleSubscales scale)
          subscaleName sub `shouldBe` "cognitive"
          subscaleItems sub `shouldBe` ["q1", "q2"]

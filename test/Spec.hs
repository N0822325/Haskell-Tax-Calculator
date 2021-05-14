module Main where

import Test.HUnit
import Test.QuickCheck
import Lib

main :: IO ()
main = do
  quickCheck prop_check_negative
  quickCheck prop_check_taxDeduct

  results <- runTestTT allTests
  print results

prop_check_negative :: Float -> Bool
prop_check_negative income = tax income >= 0

prop_check_taxDeduct :: Float -> Bool
prop_check_taxDeduct income = result
  where
    result
      | income < 0        = True
      | income <= 12500   = (tax income) == income
      | otherwise         = (tax income) < income


test1 :: Test -- Min Case (untaxable)
test1 = TestCase (assertEqual "10,000" 10000 (tax 10000))

test2 :: Test -- First Tax Bracket (22,500 taxable)
test2 = TestCase (assertEqual "35,000" 30500 (tax 35000))

test3 :: Test -- Second Tax Bridge (37,499 taxable)
test3 = TestCase (assertEqual "49,999" 42499.2 (tax 49999))

test4 :: Test -- Second Tax Bridge (37,500 taxable)
test4 = TestCase (assertEqual "50,000" 42500 (tax 50000))

test5 :: Test -- Second Tax Bracket (60,000 taxable)
test5 = TestCase (assertEqual "72,500" 56000 (tax 72500))

test6 :: Test -- Tax Deductable Threashold (max) [87,500 taxable]
test6 = TestCase (assertEqual "100,000" 72500 (tax 100000))

test7 :: Test -- Tax Deductable Threashold (min) [125,000 taxable]
test7 = TestCase (assertEqual "125,000" 82500 (tax 125000))

test8 :: Test -- Third Tax Bridge (37,500 - 112499 split)
test8 = TestCase (assertEqual "149,999" 97499.4 (tax 149999))

test9 :: Test -- Third Tax Bridge (37,500 - 112500 split)
test9 = TestCase (assertEqual "150,000" 97500 (tax 150000))

test10 :: Test -- Third Tax Bracket (37,500 - 112500 -  225000 split)
test10 = TestCase (assertEqual "375,000" 221250 (tax 375000))

allTests :: Test
allTests = TestList [test1, test2, test3, test4, test5,
                      test6, test7, test8, test9, test10]

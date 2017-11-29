{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
-- import Test.Framework.Providers.QuickCheck2 (testProperty)

import Ex02

main = defaultMain tests -- runs the tests

tests :: [TF.Test]
tests = [ testGroup "\n\nExercise 02 Tests (120 marks)\n"
            [ insertTests
            , lookupTests
            , statsTests
            ]
        ]


-- tests for Part 1 (insert) -----------------------------------------------------


insertTests :: TF.Test
insertTests
 = testGroup "\nPart 1 - insert (40 marks)\n"
    [ testCase "1 -> 100 into C {5 marks}"
      ( ins 1 100 C @?= B 1 100 )
    , testCase "3->300 into B 5 500 {5 marks}"
      ( ins 3 300 (B 5 500) @?= A (B 3 300) C 5 500 )
    , testCase "5->5000 into B 5 500 {5 marks}"
      ( ins 5 5000 (B 5 500) @?= B 5 5000 )
    , testCase "7->700 into B 5 500 {5 marks}"
      ( ins 7 700 (B 5 500) @?= A C (B 7 700) 5 500 )
    , testCase "3->300 into A C (B 7 700) 5 500 {4 marks}"
      ( ins 3 300 (A C (B 7 700) 5 500)
       @?= A (B 3 300) (B 7 700) 5 500 )
    , testCase "7->700 into A (B 3 300) C 5 500 {4 marks}"
      ( ins 7 700 (A (B 3 300) C 5 500)
        @?= A (B 3 300) (B 7 700) 5 500 )
    , testCase "4->400 into A (B 3 300) (B 7 700) 5 500 {4 marks}"
       ( ins 4 400 (A (B 3 300) (B 7 700) 5 500) @?=
           A (A C (B 4 400) 3 300) (B 7 700) 5 500 )
    , testCase "6->600 into A (B 3 300) (B 7 700) 5 500 {4 marks}"
       ( ins 6 600 (A (B 3 300) (B 7 700) 5 500) @?=
           A (B 3 300) (A (B 6 600) C 7 700) 5 500 )
    , testCase "7->49 into A (B 3 300) (B 7 700) 5 500 {4 marks}"
       ( ins 7 49 (A (B 3 300) (B 7 700) 5 500) @?=
           A (B 3 300) (B 7 49) 5 500 )
    ]


-- Tests for Part 2 (lookup) -----------------------------------------------------

c :: IntFun
c = C

lookupTests :: TF.Test
lookupTests
 = testGroup "\nPart 2 - lookup (40 marks)\n"
    [ testCase "3 in C ? (Maybe){3 marks}"
        (lkp c 3 @?= Nothing )
    , testCase "3 in C ? ([]){3 marks}"
        (lkp c 3 @?= [] )
    , testCase "3 in B 5 500 (Maybe)){3 marks}"
        (lkp (B 5 500) 3 @?= Nothing )
    , testCase "3 in B 5 500 ([])){3 marks}"
        (lkp (B 5 500) 3 @?= [] )
    , testCase "3 in B 3 300 (Maybe)){3 marks}"
        (lkp (B 3 300) 3 @?= Just 300 )
    , testCase "3 in B 3 300 ([])){3 marks}"
        (lkp (B 3 300) 3 @?= [300] )
    , testCase "3 in A (B 3 300) (B 7 700) 5 500 ([])){4 marks}"
        (lkp (A (B 3 300) (B 7 700) 5 500) 3 @?= [300] )
    , testCase "7 in A (B 3 300) (B 7 700) 5 500 (Maybe)){3 marks}"
        (lkp (A (B 3 300) (B 7 700) 5 500) 7 @?= Just 700 )
    , testCase "5 in A (B 3 300) (B 7 700) 5 500 ([])){3 marks}"
        (lkp (A (B 3 300) (B 7 700) 5 500) 5 @?= [500] )
    , testCase "1 in A (B 3 300) (B 7 700) 5 500 (Maybe)){3 marks}"
        (lkp (A (B 3 300) (B 7 700) 5 500) 1 @?= Nothing )
    , testCase "4 in A (B 3 300) (B 7 700) 5 500 ([])){3 marks}"
        (lkp (A (B 3 300) (B 7 700) 5 500) 4 @?= [] )
    , testCase "6 in A (B 3 300) (B 7 700) 5 500 (Maybe)){3 marks}"
        (lkp (A (B 3 300) (B 7 700) 5 500) 6 @?= Nothing )
    , testCase "8 in A (B 3 300) (B 7 700) 5 500 ([])){3 marks}"
        (lkp (A (B 3 300) (B 7 700) 5 500) 8 @?= [] )
    ]


-- Tests for Part 3 (stats) -----------------------------------------------------

statsTests :: TF.Test
statsTests
 = testGroup "\nPart 32 - stats (40 marks)\n"
    [ testCase "[] {4 marks}"
        ( getsumsandlength 0 0 0 [] @?= (0,0,0) )
    , testCase "[0]  {3 marks}"
        ( getsumsandlength 0 0 0 [0] @?= (0,0,1) )
    , testCase "[1] {3 marks}"
        ( getsumsandlength 0 0 0 [1] @?= (1,1,1) )
    , testCase "[1,1] {3 marks}"
        ( getsumsandlength 0 0 0 [1,1] @?= (2,2,2) )
    , testCase "[1,1,2] 0 {3 marks}"
        ( getsumsandlength 0 0 0 [1,1,2] @?= (4,6,3) )
    , testCase "[0..10] 0 {3 marks}"
        ( getsumsandlength 0 0 0 [0..10] @?= (55,385,11) )
    , testCase "[-2,2,-2,2,-2,2,-2,2] 0 {3 marks}"
       (getsumsandlength 0 0 0 [-2,2,-2,2,-2,2,-2,2] @?= (0,32,8) )
    , testCase "[] (OFFSET) 0 {3 marks}"
       (getsumsandlength 1000 1000 1000 [] @?= (1000,1000,1000) )
    , testCase "[0] (OFFSET) 0 {3 marks}"
       (getsumsandlength 1000 1000 1000 [0] @?= (1000,1000,1001))
    , testCase "[1] (OFFSET){3 marks}"
        (getsumsandlength 1000 1000 1000 [1] @?= (1001,1001,1001))
    , testCase "[1,1,2] (OFFSET){3 marks}"
       (getsumsandlength 1000 1000 1000 [1,1,2] @?= (1004,1006,1003))
    , testCase "[0..10] (OFFSET){3 marks}"
       (getsumsandlength 1000 1000 1000 [0..10] @?= (1055,1385,1011))
    , testCase "[-2,2,-2,2,-2,2,-2,] (OFFSET){3 marks}"
        (getsumsandlength 1000 1000 1000 [-2,2,-2,2,-2,2,-2,2]
               @?= (1000,1032,1008))
    ]

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test (tests) where

import Distribution.TestSuite.QuickCheck
import Test.QuickCheck

import System.Console.ANSI.Code

instance Arbitrary Code where arbitrary = oneof [fmap CharCode arbitrary]
                              shrink = const []

tests :: IO [Test]
tests = do return [ testProperty "Success" True
                  , testProperty "Round-Trip" (\s -> encode (decode s) === s)
                  , testProperty "Round-Trip-Show" (\s -> encode (read $ show $ decode s) === s)
                  , testProperty "Round-Trip-Code" (\c -> (fst $ decode1 $ encode1 (c :: Code)) === c)
                  , testProperty "DirectionEnc" (encode1 (CS "" "" 'D') === "\ESC[D")
                  , testProperty "DirectionDec" (decode1 "\ESC[D" === (CS "" "" 'D', ""))
                  ]

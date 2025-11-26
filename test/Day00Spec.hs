{-# LANGUAGE OverloadedStrings #-}

module Day00Spec where

import qualified AoC
import qualified Day00
import Test.Hspec

spec :: Spec
spec = do
  describe "The Universe" $
    it "should be 42 for starters" $ do
      let solution = Day00.solve "foo"
      AoC.one solution `shouldBe` "42"

module Control.CaseSpec where

import Control.Case

import Test.Hspec

spec :: Spec
spec = do
  describe "gfold" $ do
    it "Bool" $ do
      gfold True "false" "true" `shouldBe` "true"

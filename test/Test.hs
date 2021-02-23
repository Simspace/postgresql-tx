{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Main where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Tx.HEnv (HEnv)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Hspec (HasCallStack, describe, expectationFailure, hspec, it, shouldBe)
import qualified Data.List as List
import qualified Database.PostgreSQL.Tx.HEnv as HEnv

main :: IO ()
main = hspec do
  describe "HEnv" do
    it "works for unique elements" do
      let h :: HEnv '[Int, Char, Bool]
          h = HEnv.fromGeneric (1, 'a', True)
      HEnv.select h `shouldBe` (1 :: Int)
      HEnv.select h `shouldBe` 'a'
      HEnv.select h `shouldBe` True

    it "fails to compile for missing elements" do
      shouldNotCompile "MissingHEnv"

    it "fails to compile for duplicate elements" do
      shouldNotCompile "DuplicateHEnv"

-- | Test support for asserting that expressions should not compile.
--
-- Attempts to compile the supplied @moduleName@ in @testdata/${moduleName}.hs@ and
-- asserts that it both fails to compile and reports error messages that match
-- the contents of @testdata/${moduleName}.txt@.
--
-- You may be tempted to use @-fdefer-type-errors@ and/or the
-- @should-not-typecheck@ package; however, this doesn't work for custom type errors.
-- See: https://gitlab.haskell.org/ghc/ghc/-/issues/18310
shouldNotCompile :: (HasCallStack) => String -> IO ()
shouldNotCompile moduleName = do
  stackExe <- fromMaybe "stack" <$> lookupEnv "STACK_EXE"
  (exitCode, _out, err) <- readProcessWithExitCode
    stackExe
    [ "ghc"
    , "--"
    , "testdata/" <> moduleName <> ".hs"
    , "-fno-code"
    ]
    ""
  when (exitCode == ExitSuccess) do
    expectationFailure $ "Unexpected compilation success for module " <> moduleName
  let actual = CompileError $ unlines $ dropWhile skipLine $ lines err
  expected <- fmap CompileError $ readFile $ "testdata/" <> moduleName <> ".txt"
  actual `shouldBe` expected
  where
  skipLine line =
    "Stack has not been tested with" `List.isPrefixOf` line
      || null line

newtype CompileError = CompileError String
  deriving newtype (Eq)

instance Show CompileError where
  show (CompileError s) = s

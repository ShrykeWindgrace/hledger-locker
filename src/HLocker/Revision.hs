{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
module HLocker.Revision (gitVersion, appVersion) where

import           Data.Version         (showVersion)
import           GitHash
import           Paths_hledger_locker (version)
import           System.Info          (compilerName)
import Language.Haskell.TH
import System.Environment
import Language.Haskell.TH.Syntax

gitVersion :: String
gitVersion = case $$tGitInfoCwdTry of
    Left _ -> unwords [
        "Version:", appVersion,
        "Compiler:", compilerName, TOOL_VERSION_ghc,
        mkGitRevision
        ]
    Right g -> unwords [
        "Version:", appVersion,
        "GitRevision:", giHash g,
        "(" ++ show (giCommitCount g), "commits)",
        "GitBranch:", giBranch g,
        "Compiler:", compilerName, TOOL_VERSION_ghc
        ]

appVersion :: String
appVersion = showVersion version

mkGitRevision :: String
mkGitRevision = case $( do
    v <- runIO $ lookupEnv "HLOCKER_BUILD_TIME_GIT_REV"
    lift v
    ) of
    Nothing -> "(no git info available)"
    Just rev -> "GitRevision: " <> rev

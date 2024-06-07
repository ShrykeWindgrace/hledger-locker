{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
module HLocker.Revision (gitVersion, appVersion) where

import           Data.Version         (showVersion)
import           GitHash
import           Paths_hledger_locker (version)
import           System.Info          (compilerName)

gitVersion :: String
gitVersion = case $$tGitInfoCwdTry of
    Left _ -> unwords [
        "Version:", appVersion,
        "Compiler:", compilerName, TOOL_VERSION_ghc,
        "(no git info available)"
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

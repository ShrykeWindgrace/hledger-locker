{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
module Revision (gitVersion) where

import           Data.Version         (showVersion)
import           Development.GitRev   (gitBranch, gitCommitCount, gitHash)
import           Paths_hledger_locker (version)
import           System.Info          (compilerName)

gitVersion :: String
gitVersion = unwords [
    "Version:", showVersion version,
    "GitRevision:", $(gitHash),
    "(" ++ $(gitCommitCount), "commits)",
    "GitBranch:", $(gitBranch),
    "Compiler:", compilerName, TOOL_VERSION_ghc
    ]




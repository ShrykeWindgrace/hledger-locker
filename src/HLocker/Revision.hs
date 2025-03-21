{-# LANGUAGE CPP             #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module HLocker.Revision (gitVersion, appVersion) where

import           Data.Version         (showVersion)
import GitHash ( giBranch, giCommitCount, giHash, tGitInfoCwdTry )
import           PackageInfo_hledger_locker (version)
import           System.Info          (compilerName)
import Language.Haskell.TH ( runIO )
import System.Environment ( lookupEnv )
import Language.Haskell.TH.Syntax ( Lift(lift) )

gitVersion :: String
gitVersion = either
    do const $ unwords [
        "Version:", appVersion,
        "Compiler:", compilerName, TOOL_VERSION_ghc,
        mkGitRevision
        ]
    do (\g -> unwords [
        "Version:", appVersion,
        "GitRevision:", giHash g,
        "(" ++ show (giCommitCount g), "commits)",
        "GitBranch:", giBranch g,
        "Compiler:", compilerName, TOOL_VERSION_ghc
        ])
    do $$tGitInfoCwdTry

appVersion :: String
appVersion = showVersion version

mkGitRevision :: String
mkGitRevision = maybe "(no git info available)" ("GitRevision: " <>)  $( do
    v <- runIO $ lookupEnv "HLOCKER_BUILD_TIME_GIT_REV"
    lift v
    )

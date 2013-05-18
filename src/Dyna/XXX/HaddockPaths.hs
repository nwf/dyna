-- | This is a little program which integrates with Cabal to extract the
-- @--read-interface@ options that we should be passing to Haddock, based on
-- our cabal configuration.
--
-- It expects to be called with one argument, the HTML path template for
-- imported objects.  See the Makefile for an example.
--
-- Much of this file is lifted in whole or in part from Cabal's internals,
-- especially Distribution.Simple.Haddock.  As such, it is placed under
-- the same license as Cabal.

{-# LANGUAGE ScopedTypeVariables #-}
module Dyna.XXX.HaddockPaths where

import           Control.Monad
import           Data.Either
import           Data.IORef
import           Data.List
import           Data.Maybe
import qualified Data.Set as S
import           Distribution.InstalledPackageInfo
import           Distribution.Package
import           Distribution.Simple.Configure
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PackageIndex
import           Distribution.Text
import           System.Environment
import           System.Directory
import           System.IO

main :: IO ()
main = do
  [hpt] <- getArgs
  lbi <- getPersistBuildConfig "dist"
  sr <- newIORef (S.empty)
  withComponentsLBI (localPkgDescr lbi) lbi (cb (toPathTemplate hpt) lbi sr)
  readIORef sr >>= mapM_ (putStrLn . showif) . S.toList
 where
  cb hpt lbi sr (_ :: Component) clbi = do
    let directDeps = map fst (componentPackageDeps clbi)
    let Left transitiveDeps = dependencyClosure (installedPkgs lbi) directDeps
    interfaces <- sequence
      [ case interfaceAndHtmlPath lbi hpt ipkg of
          Nothing -> return (Left (packageId ipkg))
          Just (interface, html) -> do
            exists <- doesFileExist interface
            if exists
              then return (Right (interface, html))
              else return (Left (packageId ipkg))
      | ipkg <- allPackages transitiveDeps
      ]
    let (missing,found) = partitionEithers interfaces

    when (not $ null missing) $ do
      hPutStrLn stderr $ "MISSING: [" ++ intercalate "," (map display missing) ++ "]"

    mapM_ (modifyIORef sr . S.insert) found

  showif (i,mh) = "--read-interface=" ++ mh ++"," ++ i

  interfaceAndHtmlPath :: LocalBuildInfo 
                       -> PathTemplate
                       -> InstalledPackageInfo
                       -> Maybe (FilePath, FilePath)
  interfaceAndHtmlPath lbi hpt pkg = do
    interface <- listToMaybe (haddockInterfaces pkg)
    let html = substPathTemplate (packageId pkg) lbi hpt
    return (interface, html)

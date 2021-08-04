{-# LANGUAGE CPP #-}

module GhcNameCatcher (plugin) where

import Control.Monad (filterM)
import Control.Monad.Trans.Writer.CPS

import Data.Generics (everywhereM, mkM)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

import System.Directory
  (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>), (<.>))

#if __GLASGOW_HASKELL__ < 900
import qualified GhcPlugins as P
import TcRnTypes (TcGblEnv, TcM, tcg_binds, tcg_mod)
import TcRnMonad (failWithM)
import TyCoRep (Type(TyConApp, ForAllTy))
import Fingerprint (fingerprintFingerprints, getFileHash)
#else
import qualified GHC.Plugins as P
import GHC.Tc.Types (TcGblEnv, TcM, tcg_binds, tcg_mod)
import GHC.Data.IOEnv (failWithM)
import GHC.Core.TyCo.Rep (Type(TyConApp, ForAllTy))
import GHC.Fingerprint (fingerprintFingerprints, getFileHash)
#endif

--------------------------------------------------------------------------------
-- Main definitions
--------------------------------------------------------------------------------

plugin :: P.Plugin
plugin = P.defaultPlugin
  { P.typeCheckResultAction = \opts _summary -> process opts
  , P.pluginRecompile = recompile }

recompile :: [P.CommandLineOption] -> IO P.PluginRecompile
recompile (outDir:_) = do
  exists <- doesDirectoryExist outDir
  if not exists then pure P.ForceRecompile
  else do
    contents <- map (outDir </>) <$> listDirectory outDir
    files <- filterM doesFileExist contents
    let csvFiles = filter ((".csv" ==) . takeExtension) files
    fingerprints <- traverse getFileHash csvFiles
    pure . P.MaybeRecompile $ fingerprintFingerprints fingerprints
recompile [] = pure P.ForceRecompile

process :: [P.CommandLineOption] -> TcGblEnv -> TcM TcGblEnv
process (outDir:_) env = (env <$) . P.liftIO $ do
  let ids = execWriter . everywhereM (mkM logVar) $ tcg_binds env
      modName = P.moduleNameString . P.moduleName $ tcg_mod env
  createDirectoryIfMissing True outDir
  writeIds (outDir </> modName <.> "csv") ids
process [] _ = failWithM
  $  "Please specify an output directory for ghc-name-catcher with "
  <> "-fplugin-opt=GhcNameCatcher:path/to/output/dir"

logVar :: P.Located P.Id -> Writer IdBag (P.Located P.Id)
logVar ident
  | Just tc <- getTyCon . P.varType $ P.unLoc ident
  , P.RealSrcSpan {} <- P.getLoc ident
  = ident <$ tell (ident +:: tc)
  | otherwise = pure ident

writeIds :: FilePath -> IdBag -> IO ()
writeIds file
  = writeFile file
  . unlines
  . map (intercalate "," . uncurry (:) . fmap S.toList)
  . M.toList
  . getIds

--------------------------------------------------------------------------------
-- IdBag Monoid
--------------------------------------------------------------------------------

newtype IdBag = IdBag { getIds :: M.Map String (S.Set String) }

(+::) :: (P.NamedThing var, P.NamedThing typ) => var -> typ -> IdBag
var +:: typ = IdBag
  . M.singleton (P.getOccString typ)
  $ S.singleton (P.getOccString var)

instance Semigroup IdBag where
  IdBag m <> IdBag n = IdBag $ M.unionWith S.union m n

instance Monoid IdBag where
  mempty = IdBag mempty

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

getTyCon :: Type -> Maybe P.TyCon
getTyCon (TyConApp tc _) = Just tc
getTyCon (ForAllTy _ t) = getTyCon t
getTyCon _ = Nothing

module GhcNameCatcher (plugin) where

import Control.Monad.Trans.Writer.CPS

import Data.Generics (everywhereM, mkM)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))

import qualified GhcPlugins as P
import TcRnTypes (TcGblEnv, TcM, tcg_binds, tcg_mod)
import TcRnMonad (failWithM)
import TyCoRep (Type(TyConApp))

--------------------------------------------------------------------------------
-- Main definitions
--------------------------------------------------------------------------------

plugin :: P.Plugin
plugin = P.defaultPlugin
  { P.typeCheckResultAction = \opts _summary -> process opts
  , P.pluginRecompile = P.purePlugin }

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
  | TyConApp tc _ <- P.varType (P.unLoc ident)
  , P.RealSrcSpan _ <- P.getLoc ident
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

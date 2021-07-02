import CmmBuildInfoTables
import CmmInfo
import CmmPipeline
import CorePrep
import CoreToStg
import Data.Foldable
import Data.Functor
import GHC
import GHC.Paths
import GhcPlugins
import HscMain
import SimplStg
import StgCmm
import StgFVs
import qualified Stream
import TidyPgm

main :: IO ()
main = do
  let fn = "MyMap.hs"
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $
      do
        do
          dflags' <- getSessionDynFlags
          let dflags = updOptLevel 2 $ dflags' {verbosity = 2}
          void $ setSessionDynFlags dflags
        target <- guessTarget fn Nothing
        addTarget target
        mod_graph <- depanal [] False
        mod_summary <-
          case find ((== fn) . msHsFilePath) (mgModSummaries mod_graph) of
            Just mod_summary -> pure mod_summary
            _ -> error "Not found."
        let this_mod = ms_mod mod_summary
        parsed_module <- parseModule mod_summary
        typechecked_module <- typecheckModule parsed_module
        desugared_module <- desugarModule typechecked_module
        let mod_guts = dm_core_module desugared_module
        hsc_env <- getSession
        let dflags = hsc_dflags hsc_env
        _ <- liftIO $ do
          simpl_guts <- hscSimplify hsc_env [] mod_guts
          (cg_guts, _) <- tidyProgram hsc_env simpl_guts
          let data_tycons = filter isDataTyCon (cg_tycons cg_guts)
          (prepd_binds, _) <-
            corePrepPgm
              hsc_env
              this_mod
              (ms_location mod_summary)
              (cg_binds cg_guts)
              data_tycons
          let (stg_binds, _) = coreToStg dflags this_mod prepd_binds
          simpl_stg_binds <- stg2stg dflags this_mod stg_binds
          let cg_stg_binds = annTopBindingsFreeVars simpl_stg_binds
              cmm_stream =
                codeGen
                  dflags
                  this_mod
                  data_tycons
                  ([], [])
                  cg_stg_binds
                  (cg_hpc_info cg_guts)
              cmms =
                void $
                  Stream.mapAccumL
                    (cmmPipeline hsc_env)
                    (emptySRT this_mod)
                    cmm_stream
          raw_cmms <- cmmToRawCmm dflags cmms
          pure ()
        liftIO $ putStrLn "Done."

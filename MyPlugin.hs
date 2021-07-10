module MyPlugin
  ( plugin,
  )
where

import CmmBuildInfoTables
import CmmInfo
import CmmPipeline
import CorePrep
import CoreToStg
import Data.Functor
import GhcPlugins
import SimplStg
import StgCmm
import StgFVs
import qualified Stream
import TidyPgm

plugin :: Plugin
plugin = defaultPlugin {installCoreToDos = myCorePlugin}

myCorePlugin :: CorePlugin
myCorePlugin _ todos =
  pure $ todos ++ [CoreDoPluginPass "MyCorePluginPass" myCorePluginPass]

myCorePluginPass :: CorePluginPass
myCorePluginPass simpl_guts = do
  hsc_env <- getHscEnv
  let dflags = hsc_dflags hsc_env
      this_mod = mg_module simpl_guts
      Just mod_summary =
        mgLookupModule (hsc_mod_graph hsc_env) (mg_module simpl_guts)
  _ <- liftIO $ do
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
    putStrLn "Done."
  pure simpl_guts

{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import DriverPhases
import DriverPipeline
import GHC
import GHC.Paths
import GhcPlugins
import Hooks
import HscMain
import MkIface
import Panic
import System.Environment.Blank

myRunPhaseHook ::
  PhasePlus -> FilePath -> DynFlags -> CompPipeline (PhasePlus, FilePath)
myRunPhaseHook (HscOut _ _ HscRecomp {..}) _ _ = do
  output_fn <- phaseOutputFilename StopLn
  PipeState {..} <- getPipeState
  final_iface <-
    liftIO
      (mkFullIface hsc_env {hsc_dflags = hscs_iface_dflags} hscs_partial_iface)
  P $ \_env state -> pure (state {iface = Just final_iface}, ())
  liftIO $
    hscMaybeWriteIface
      hscs_iface_dflags
      final_iface
      hscs_old_iface_hash
      hscs_mod_location
  liftIO $
    writeFile output_fn $
      showSDoc hscs_iface_dflags $
        ppr
          (cg_binds hscs_guts)
  pure (RealPhase StopLn, output_fn)
myRunPhaseHook pp input dflags = runPhase pp input dflags

main :: IO ()
main = do
  args <- getArgs
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $
      do
        srcs <- do
          dflags0 <- getSessionDynFlags
          (dflags1, fileish_args, _) <-
            parseDynamicFlags dflags0 $
              map noLoc args
          let dflags2 =
                dflags1
                  { hooks = emptyHooks {runPhaseHook = Just myRunPhaseHook}
                  }
          void $ setSessionDynFlags dflags2
          pure $ map unLoc fileish_args
        targets <- traverse (`guessTarget` Nothing) srcs
        setTargets targets
        ok_flag <- load LoadAllTargets
        when (failed ok_flag) $ throwGhcException $ Panic "Failed."
        liftIO $ putStrLn "Done."

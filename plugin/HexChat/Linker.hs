{-# OPTIONS_GHC -fno-warn-tabs #-}

module HexChat.Linker where

import Prelude hiding (print)

import Control.DeepSeq
import Control.Exception
import Data.Function
import Data.IORef
import Data.List
import Data.Maybe
import Data.Version
import Foreign
import Foreign.C.String
import GHC.Types
import System.IO.Unsafe
import Unsafe.Coerce

import GHC.Paths (libdir)

import ErrUtils
import Exception
import Config
import DynFlags
import GHC
import GHCi.RemoteTypes
import GhcMonad
import HscMain
import HscTypes
import Linker
import Name
import Outputable

import HexChat
import qualified HexChat.Internal as I
import Paths_hexchat

infoSymbol = "info"

data Script = Script
	{
		scriptFile :: String,
		scriptHandle :: I.Plugin,
		scriptInfo :: ModInfo Any,
		scriptNonce :: Any,
		scriptEnv :: HscEnv
	}

{-# NOINLINE scripts #-}
scripts :: IORef [Script]
scripts = unsafePerformIO $ newIORef []

reportException :: ExceptionMonad m => m a -> m a -> m a
reportException def f = gcatch f $ \e -> do
	liftIO $ print $ show (e :: SomeException)
	def

suppress :: ExceptionMonad m => Bool -> m a -> (a -> m b) -> m (Maybe b)
suppress True m f = gcatch (Just <$> m) (\e -> return Nothing `const` (e :: SomeException)) >>= traverse f
suppress False m f = Just <$> (m >>= f)

logAction :: LogAction
logAction dflags reason severity srcSpan style msg = print $ renderWithStyle dflags (mkLocMessageAnn Nothing severity srcSpan msg) style

loadScript :: Bool -> String -> Ghc Bool
loadScript sup file = reportException (return True) $ do
	dflags <- getSessionDynFlags
	liftIO (newHscEnv dflags) >>= setSession
	mv <- suppress sup (guessTarget file Nothing) $ \target -> do
		setTargets [target]
		load LoadAllTargets

		graph <- depanal [] True
		let mod = ms_mod $ fromMaybe (error "Module not loaded") $ find (\m -> ml_hs_file (ms_location m) == Just file) graph
		modinfo <- fromMaybe (error "Module not loaded") <$> getModuleInfo mod
		let name = fromMaybe (error $ "No symbol '" ++ infoSymbol ++ "'") $ find (\n -> getOccString n == infoSymbol) $ modInfoExports modinfo

		hsc <- getSession
		info <- liftIO $ do
			fref <- getHValue hsc name
			withForeignRef fref $ \ref -> do
				value <- localRef ref
				return (unsafeCoerce value :: ModInfo Any)
		return (info, hsc)
	case mv of
		Nothing -> return False
		Just (info, hsc) -> liftIO $ do
			evaluate $ force (modName info, modVersion info, modAuthor info, modDescription info)
			I.joinStaticData (modStaticData info)
			plugin <- I.getPlugin
			handle <- I.pluginguiAdd plugin file (modName info) (modDescription info) (modVersion info)
			nonce <- reportException (return undefined) $ I.withHandle handle $ modInit info
			modifyIORef scripts $ (Script file handle info nonce hsc :)
			return True

unloadScript :: Bool -> String -> Ghc Bool
unloadScript sup file = reportException (return True) $ do
	mf <- liftIO $ find (\s -> scriptFile s == file) <$> readIORef scripts
	case mf of
		Nothing -> if sup then return False else liftIO $ evaluate $ error "No such script"
		Just s -> do
			liftIO $ modifyIORef scripts (deleteBy ((==) `on` scriptHandle) s)
			liftIO $ I.withHandle (scriptHandle s) $ deinitScript s
			return True

deinitScript :: Script -> IO ()
deinitScript s = do
	plugin <- I.getPlugin
	I.pluginguiRemove plugin $ scriptHandle s
	finally (modDeinit (scriptInfo s) (scriptNonce s)) $ I.unhookHandle $ scriptHandle s

commandLoad, commandUnload, commandReload :: [String] -> [String] -> Ghc Eat
commandLoad w we = do
	let (_:file:_) = w
	b <- loadScript True file
	return (if b then EatAll else EatNone)

commandUnload w we = do
	let (_:file:_) = w
	b <- unloadScript True file
	return (if b then EatAll else EatNone)

commandReload w we = do
	let (_:file:_) = w
	b <- unloadScript True file
	if not b then return EatNone
		else do
			loadScript False file
			return EatAll

foreign export ccall plugin_init :: I.Plugin_Init
foreign export ccall plugin_deinit :: I.Plugin_Deinit

{-# NOINLINE borrowedStrings #-}
borrowedStrings :: IORef [CString]
borrowedStrings = unsafePerformIO $ newIORef []

{-# NOINLINE session #-}
session :: IORef Session
session = unsafePerformIO $ newIORef (error "uninitialized Session")

plugin_init plugin pname pdesc pver arg = reportException (return 0) $ do
	I.initStaticData plugin

	name <- newCString "Haskell"
	desc <- newCString "Haskell scripting plugin"
	ver <- newCString $ showVersion version ++ "/" ++ cProjectVersion
	modifyIORef borrowedStrings ([name, desc, ver] ++)

	poke pname name
	poke pdesc desc
	poke pver ver

	runGhc (Just libdir) $ do
		dflags <- getSessionDynFlags
		(dflags', _, _) <- parseDynamicFlagsCmdLine dflags [noLoc "-package ghc"]
		setSessionDynFlags $ updateWays $ addWay' WayDyn $ dflags' { ghcLink = LinkInMemory, log_action = logAction }
		reifyGhc $ \s -> do
			writeIORef session s
			hookCommand "load" pri_NORM "" $ \w we -> reflectGhc (commandLoad w we) s
			hookCommand "unload" pri_NORM "" $ \w we -> reflectGhc (commandUnload w we) s
			hookCommand "reload" pri_NORM "" $ \w we -> reflectGhc (commandReload w we) s

		return 1

plugin_deinit plugin = reportException (return 0) $ do
	s <- readIORef session
	flip reflectGhc s $ do
		liftIO $ traverse deinitScript =<< readIORef scripts
		liftIO $ readIORef borrowedStrings >>= mapM free
		liftIO $ writeIORef borrowedStrings []
		return 1

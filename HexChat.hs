{-# OPTIONS_GHC -fno-warn-tabs #-}

module HexChat
	(
		ModInfo(..), modInfo,

		I.pri_HIGHEST, I.pri_HIGH, I.pri_NORM, I.pri_LOW, I.pri_LOWEST,
		Hook, Eat(..), EventAttrs(..), Context, List,

		command,
		print,
		emitPrint,
		emitPrintAttrs,
		sendModes,
		nickCmp,
		strip,
		getPrefs,
		getPrefString,
		getPrefInt,
		getPrefBool,
		listGet,
		listFields,
		listNext,
		listStr,
		listInt,
		listTime,
		hookCommand,
		hookPrint,
		hookPrintAttrs,
		hookServer,
		hookServerAttrs,
		unhook,
		findContext,
		getContext,
		setContext,
		withContext
	)
	where

import Prelude hiding (print)

import Control.Exception
import Control.Monad
import Data.IORef
import Foreign.C.Types

import HexChat.Internal (StaticData, staticData, lHooks, getPlugin, getHandle, withHandle, Hook, Eat(..), EventAttrs(..), Context, List)
import qualified HexChat.Internal as I

data ModInfo a = ModInfo
	{
		modName :: String,
		modVersion :: String,
		modAuthor :: String,
		modDescription :: String,
		modInit :: IO a,
		modDeinit :: a -> IO (),
		modStaticData :: StaticData
	}

modInfo :: ModInfo a
modInfo = ModInfo
	{
		modName = "module",
		modVersion = "",
		modAuthor = "",
		modDescription = "No description provided",
		modInit = return $ error "modInit",
		modDeinit = \_ -> return (),
		modStaticData = staticData
	}

command :: String -> IO ()
command str = do
	plugin <- getPlugin
	I.command plugin str

print :: String -> IO ()
print str = do
	plugin <- getPlugin
	I.print plugin str

emitPrint :: String -> [String] -> IO Bool
emitPrint event vs = do
	plugin <- getPlugin
	I.emitPrint plugin event vs

emitPrintAttrs :: EventAttrs -> String -> [String] -> IO Bool
emitPrintAttrs attrs event vs = do
	plugin <- getPlugin
	I.emitPrintAttrs plugin attrs event vs

sendModes :: [String] -> Int -> Char -> Char -> IO ()
sendModes targets modes sign mode = do
	plugin <- getPlugin
	I.sendModes plugin targets modes sign mode

nickCmp :: String -> String -> IO Ordering
nickCmp s1 s2 = do
	plugin <- getPlugin
	I.nickCmp plugin s1 s2

strip :: Bool -> Bool -> String -> IO String
strip sc sf str = do
	plugin <- getPlugin
	I.strip plugin sc sf str

getPrefs :: String -> (String -> IO a) -> (Int -> IO a) -> (Bool -> IO a) -> IO (Maybe a)
getPrefs key cstr cint cbool = do
	plugin <- getPlugin
	I.getPrefs plugin key cstr cint cbool

getPrefString :: String -> IO (Maybe String)
getPrefString key = join <$> getPrefs key (return . Just) (const $ return Nothing) (const $ return Nothing)

getPrefInt :: String -> IO (Maybe Int)
getPrefInt key = join <$> getPrefs key (const $ return Nothing) (return . Just) (const $ return Nothing)

getPrefBool :: String -> IO (Maybe Bool)
getPrefBool key = join <$> getPrefs key (const $ return Nothing) (const $ return Nothing) (return . Just)

listGet :: String -> IO (Maybe List)
listGet key = do
	plugin <- getPlugin
	I.listGet plugin key

listFields :: String -> IO [String]
listFields key = do
	plugin <- getPlugin
	I.listFields plugin key

listNext :: List -> IO Bool
listNext list = do
	plugin <- getPlugin
	I.listNext plugin list

listStr :: List -> String -> IO String
listStr list key = do
	plugin <- getPlugin
	I.listStr plugin list key

listInt :: List -> String -> IO Int
listInt list key = do
	plugin <- getPlugin
	I.listInt plugin list key

listTime :: List -> String -> IO CTime
listTime list key = do
	plugin <- getPlugin
	I.listTime plugin list key

hookCommand :: String -> CInt -> String -> ([String] -> [String] -> IO Eat) -> IO Hook
hookCommand cmd pri desc f = do
	plugin <- getPlugin
	handle <- getHandle
	hook <- I.hookCommand plugin cmd pri desc $ \w we -> withHandle handle $ f w we
	readIORef lHooks >>= flip modifyIORef ((handle, hook) :)
	return hook

hookPrint :: String -> CInt -> ([String] -> IO Eat) -> IO Hook
hookPrint cmd pri f = do
	plugin <- getPlugin
	handle <- getHandle
	hook <- I.hookPrint plugin cmd pri $ \w -> withHandle handle $ f w
	readIORef lHooks >>= flip modifyIORef ((handle, hook) :)
	return hook

hookPrintAttrs :: String -> CInt -> ([String] -> EventAttrs -> IO Eat) -> IO Hook
hookPrintAttrs cmd pri f = do
	plugin <- getPlugin
	handle <- getHandle
	hook <- I.hookPrintAttrs plugin cmd pri $ \w attrs -> withHandle handle $ f w attrs
	readIORef lHooks >>= flip modifyIORef ((handle, hook) :)
	return hook

hookServer :: String -> CInt -> ([String] -> [String] -> IO Eat) -> IO Hook
hookServer cmd pri f = do
	plugin <- getPlugin
	handle <- getHandle
	hook <- I.hookServer plugin cmd pri $ \w we -> withHandle handle $ f w we
	readIORef lHooks >>= flip modifyIORef ((handle, hook) :)
	return hook

hookServerAttrs :: String -> CInt -> ([String] -> [String] -> EventAttrs -> IO Eat) -> IO Hook
hookServerAttrs cmd pri f = do
	plugin <- getPlugin
	handle <- getHandle
	hook <- I.hookServerAttrs plugin cmd pri $ \w we attrs -> withHandle handle $ f w we attrs
	readIORef lHooks >>= flip modifyIORef ((handle, hook) :)
	return hook

unhook :: Hook -> IO ()
unhook hook = do
	plugin <- getPlugin
	readIORef lHooks >>= flip modifyIORef (filter ((/= hook) . snd))
	I.unhook plugin hook

findContext :: Maybe String -> Maybe String -> IO (Maybe Context)
findContext server channel = do
	plugin <- getPlugin
	I.findContext plugin server channel

getContext :: IO Context
getContext = do
	plugin <- getPlugin
	I.getContext plugin

setContext :: Context -> IO Bool
setContext context = do
	plugin <- getPlugin
	I.setContext plugin context

withContext :: Context -> IO a -> IO a
withContext context f = bracket (swap context) swap (const f)
	where swap context = do
		old <- getContext
		b <- setContext context
		unless b $ error "Could not set context"
		return old

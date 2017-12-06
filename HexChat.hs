{-# OPTIONS_GHC -fno-warn-tabs #-}
{-|
Module      : HexChat
Description : HexChat scripting interface
Copyright   : (C) 2017 mniip
License     : MIT
Maintainer  : mniip@mniip.com
Stability   : none
Portability : none

A HexChat script is a @.hs@ file that defines a global identifier @info@ with type 'ModInfo'. This structure contains the necessary metadata about a script as well its initialization and deinitialization functions. From your initialization function you may want to hook some events. Any remaining hooks are automatically unhooked after the deinitializer so you don't have to worry about that.
-}
module HexChat
	(
		-- * Module Info
		ModInfo(..), modInfo,

		-- * Interfacing HexChat
		command,
		print,
		emitPrint,
		EventAttrs(..),
		emitPrintAttrs,
		sendModes,
		nickCmp,
		strip,
		getPrefs,
		getPrefString,
		getPrefInt,
		getPrefBool,
		List,
		listGet,
		listFields,
		listNext,
		listStr,
		listInt,
		listTime,

		-- * Hooks
		-- | All hooks have a priority that determines the order in which HexChat invokes them. Each hook can also affect the propagation of the event to other hooks.
		I.pri_HIGHEST, I.pri_HIGH, I.pri_NORM, I.pri_LOW, I.pri_LOWEST,
		Eat(..),
		Hook,
		hookCommand,
		hookPrint,
		hookPrintAttrs,
		hookServer,
		hookServerAttrs,
		unhook,

		-- * Contexts
		-- | Some HexChat interface functions are specific to a given 'Context', which corresponds to a tab or window. By default whenever a hook is invoked it is running in the event's relevant context. If you need to output text or commands to a different tab you should change the context.
		Context,
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

-- | This datatype contains metadata about the script. Every script should define an global binding named @info@ of this type. This structure contains internal fields necessary for scripting to work, so when constructing an object please use the default value 'modInfo' with record modification notation, like this:
--
-- @
-- info = 'modInfo' { 'modName' = "a module", 'modAuthor' = "somepony", ... }
-- @
--
-- The type parameter signifies the return type of the initializer and has no special meaning otherwise, you can specialize it to anything you want.
--
-- If you need to pass a lot of data to the deinitializer you could simply let @a ~ 'IO' ()@ and @'modDeinit' = 'id'@, such as in the following:
--
-- @
-- info = 'modInfo'
--     { 'modInit' = do
--           x <- createX
--           'return' $ do
--               destroyX x
--     , 'modDeinit' = 'id'
--     }
-- @
data ModInfo a = ModInfo
	{
		modName :: String, -- ^ Name of the script.
		modVersion :: String, -- ^ Version of the script.
		modAuthor :: String, -- ^ Author of the script.
		modDescription :: String, -- ^ A short description of the script that can fit in the "Plugins and Scripts" list window.
		modInit :: IO a, -- ^ This is the entry point of the script. It will be executed immediately after the script is compiled and loaded. The returned value will be opaquely passed to 'modDeinit'.
		modDeinit :: a -> IO (), -- ^ This function will be executed shortly before the script is unloaded. You can place any cleanup routines here. As argument it receives whatever 'modInit' returned.
		modStaticData :: StaticData -- ^ An internal field that lets the different (to the linker) instances of the HexChat library identify eachother. Please inherit the value of this field from 'modInfo'.
	}

-- | A default value for 'ModInfo'. This has some sensible defaults and also provides the values of internal fields necessary for the operation of the interface.
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

-- | Invoke a HexChat command as if the user has typed it in the inputbox. Do not include the leading @/@.
command :: String -> IO ()
command str = do
	plugin <- getPlugin
	I.command plugin str

-- | Print some text to the buffer.
print :: String -> IO ()
print str = do
	plugin <- getPlugin
	I.print plugin str

-- | Output a Text Event to the buffer. First argument is the Text Event identifier (see @Settings -> Text Events@), second is the list of event's parameters.
emitPrint :: String -> [String] -> IO Bool
emitPrint event vs = do
	plugin <- getPlugin
	I.emitPrint plugin event vs

-- | Same as 'emitPrint' but also lets you specify Event Attributes for the event.
emitPrintAttrs :: EventAttrs -> String -> [String] -> IO Bool
emitPrintAttrs attrs event vs = do
	plugin <- getPlugin
	I.emitPrintAttrs plugin attrs event vs

-- | @'sendModes' targets n sign mode@ where @sign@ is @+@ or @-@ and @mode@ is a channel mode character will set the specified channel mode on each of the specified targets, sending @n@ modes per line, or the server's advertised maximum if @n@ is zero.
sendModes :: [String] -> Int -> Char -> Char -> IO ()
sendModes targets modes sign mode = do
	plugin <- getPlugin
	I.sendModes plugin targets modes sign mode

-- this should really be 'IO (String -> String -> Ordering)' but the underlying C API wouldn't let you easily do that...
-- | Compare two nicknames according to the rules of the server.
nickCmp :: String -> String -> IO Ordering
nickCmp s1 s2 = do
	plugin <- getPlugin
	I.nickCmp plugin s1 s2

-- | @'strip' colors format@ will strip colors if @colors@ is 'True', and miscellaneous formatting if @formatting@ is 'True', from the provided string.
strip :: Bool -> Bool -> String -> IO String
strip sc sf str = do
	plugin <- getPlugin
	I.strip plugin sc sf str

-- | Get a HexChat preference value (see @/set@). You should pass 3 continuations one of which will be invoked depending on the type of the actual preference. Returns 'Nothing' if no preference with that name exists.
getPrefs :: String -> (String -> IO a) -> (Int -> IO a) -> (Bool -> IO a) -> IO (Maybe a)
getPrefs key cstr cint cbool = do
	plugin <- getPlugin
	I.getPrefs plugin key cstr cint cbool

-- | Return the value of a preference that is supposedly a String. Returns 'Nothing' if no such preference exists or it is of the wrong type.
getPrefString :: String -> IO (Maybe String)
getPrefString key = join <$> getPrefs key (return . Just) (const $ return Nothing) (const $ return Nothing)

-- | Return the value of a preference that is supposedly an Int. Returns 'Nothing' if no such preference exists or it is of the wrong type.
getPrefInt :: String -> IO (Maybe Int)
getPrefInt key = join <$> getPrefs key (const $ return Nothing) (return . Just) (const $ return Nothing)

-- | Return the value of a preference that is supposedly a Bool. Returns 'Nothing' if no such preference exists or it is of the wrong type.
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

-- | @'hookCommand' cmd priority description f@ registers a command named @cmd@ (with description @description@). The given @f@ will be passed a list of command's arguments (proper words) and a list of "leftovers" for every position in the word list (with the exact original whitespace).
--
-- If @cmd@ is @""@ then instead the hook will be invoked whenever the user types anything not beginning with @/@.
hookCommand :: String -> CInt -> String -> ([String] -> [String] -> IO Eat) -> IO Hook
hookCommand cmd pri desc f = do
	plugin <- getPlugin
	handle <- getHandle
	hook <- I.hookCommand plugin cmd pri desc $ \w we -> withHandle handle $ f w we
	readIORef lHooks >>= flip modifyIORef ((handle, hook) :)
	return hook

-- | @'hookPrint' event priority f@ hooks @f@ to be invoked whenever a Text Event @event@ is to be displayed on screen. The given @f@ will be passed a list of event's parameters.
hookPrint :: String -> CInt -> ([String] -> IO Eat) -> IO Hook
hookPrint cmd pri f = do
	plugin <- getPlugin
	handle <- getHandle
	hook <- I.hookPrint plugin cmd pri $ \w -> withHandle handle $ f w
	readIORef lHooks >>= flip modifyIORef ((handle, hook) :)
	return hook

-- | @'hookPrintAttrs' event priority f@ hooks @f@ to be invoked whenever a Text Event @event@ is to be displayed on screen. The given @f@ will be passed a list of event's parameters, and the attributes.
hookPrintAttrs :: String -> CInt -> ([String] -> EventAttrs -> IO Eat) -> IO Hook
hookPrintAttrs cmd pri f = do
	plugin <- getPlugin
	handle <- getHandle
	hook <- I.hookPrintAttrs plugin cmd pri $ \w attrs -> withHandle handle $ f w attrs
	readIORef lHooks >>= flip modifyIORef ((handle, hook) :)
	return hook

-- | @'hookServer' word priority f@ hooks @f@ to be invoked whenever a @word@ command arrives from the IRC server. The given @f@ will be passed a list of command's arguments (proper words) and a list of "leftovers" for every position in the word list (with the exact original whitespace). Processing of @:@ long arguments is *NOT* done.
--
-- If @cmd@ is @"RAW LINE"@ then the hook will be invoked for all commands received from the server.
hookServer :: String -> CInt -> ([String] -> [String] -> IO Eat) -> IO Hook
hookServer cmd pri f = do
	plugin <- getPlugin
	handle <- getHandle
	hook <- I.hookServer plugin cmd pri $ \w we -> withHandle handle $ f w we
	readIORef lHooks >>= flip modifyIORef ((handle, hook) :)
	return hook

-- | @'hookServer' word priority f@ hooks @f@ to be invoked whenever a @word@ command arrives from the IRC server. The given @f@ will be passed a list of command's arguments (proper words), a list of "leftovers" for every position in the word list (with the exact original whitespace), and the event attributes. Processing of @:@ long arguments is *NOT* done.
--
-- If @cmd@ is @"RAW LINE"@ then the hook will be invoked for all commands received from the server.
hookServerAttrs :: String -> CInt -> ([String] -> [String] -> EventAttrs -> IO Eat) -> IO Hook
hookServerAttrs cmd pri f = do
	plugin <- getPlugin
	handle <- getHandle
	hook <- I.hookServerAttrs plugin cmd pri $ \w we attrs -> withHandle handle $ f w we attrs
	readIORef lHooks >>= flip modifyIORef ((handle, hook) :)
	return hook

-- | Remove the given hook. All hooks are automatically removed when the script is unloaded.
unhook :: Hook -> IO ()
unhook hook = do
	plugin <- getPlugin
	readIORef lHooks >>= flip modifyIORef (filter ((/= hook) . snd))
	I.unhook plugin hook

-- | @'findContext' mserver mtabname@ finds the context corresponding to the given tab name (or to the front tab if 'Nothing') in the given server (or in any of the servers if 'Nothing').
findContext :: Maybe String -> Maybe String -> IO (Maybe Context)
findContext server channel = do
	plugin <- getPlugin
	I.findContext plugin server channel

-- | Obtains the current context.
getContext :: IO Context
getContext = do
	plugin <- getPlugin
	I.getContext plugin

-- | Sets the current context. The scope of this function is limited to the currently executing hook (or the initializer/deinitializer).
setContext :: Context -> IO Bool
setContext context = do
	plugin <- getPlugin
	I.setContext plugin context

-- | Execute a given IO action in a given context a-la 'bracket'.
withContext :: Context -> IO a -> IO a
withContext context f = bracket (swap context) swap (const f)
	where swap context = do
		old <- getContext
		b <- setContext context
		unless b $ error "Could not set context"
		return old

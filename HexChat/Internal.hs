{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-|
Module      : HexChat.Internal
Description : HexChat scripting interface
Copyright   : (C) 2017 mniip
License     : MIT
Maintainer  : mniip@mniip.com
Stability   : none
Portability : none

This module contains the "raw" functions that leak the fact that all Haskell scripts are actually executed under the same @hexchat_plugin@ structure. You should unhook everything you have hooked.
-}

module HexChat.Internal
	(
		pri_HIGHEST, pri_HIGH, pri_NORM, pri_LOW, pri_LOWEST,
		Hook, Eat(..), EventAttrs(..), Context, List,

		command,
		print,
		emitPrint,
		emitPrintAttrs,
		sendModes,
		nickCmp,
		strip,
		getPrefs,
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
		pluginguiAdd,
		pluginguiRemove,

		StaticData,
		staticData,
		lPlugin, lHandle, lHooks,
		initStaticData,
		joinStaticData,
		getPlugin,
		getHandle,
		withHandle,
		unhookHandle,

		module Foreign.C.Types,
		Plugin(..),
		Plugin_Init(..),
		Plugin_Deinit(..)
	)
	where

import Prelude hiding (print)

import Control.Monad
import Control.Exception
import Data.Coerce
import Data.IORef
import Foreign
import Foreign.C.Types
import Foreign.C.String
import GHC.StaticPtr
import System.IO.Unsafe

data HexChat_Plugin
newtype Plugin = Plugin (Ptr HexChat_Plugin) deriving (Show, Eq, Ord)
data HexChat_Hook
-- | An opaque type referencing a particular hook. Can be passed to 'HexChat.unhook'.
newtype Hook = Hook (Ptr HexChat_Hook) deriving (Show, Eq, Ord)
data HexChat_Context
-- | An opaque type referencing a context (tab or window).
newtype Context = Context (Ptr HexChat_Context) deriving (Show, Eq, Ord)
data HexChat_List
data List = List (IORef Bool) (ForeignPtr HexChat_List) deriving (Eq)
data HexChat_EventAttrs

foreign import capi "hexchat-plugin.h value HEXCHAT_PRI_HIGHEST" pri_HIGHEST :: CInt
foreign import capi "hexchat-plugin.h value HEXCHAT_PRI_HIGH" pri_HIGH :: CInt
foreign import capi "hexchat-plugin.h value HEXCHAT_PRI_NORM" pri_NORM :: CInt
foreign import capi "hexchat-plugin.h value HEXCHAT_PRI_LOW" pri_LOW :: CInt
foreign import capi "hexchat-plugin.h value HEXCHAT_PRI_LOWEST" pri_LOWEST :: CInt
foreign import capi "hexchat-plugin.h value HEXCHAT_EAT_NONE" eat_NONE :: CInt
foreign import capi "hexchat-plugin.h value HEXCHAT_EAT_HEXCHAT" eat_HEXCHAT :: CInt
foreign import capi "hexchat-plugin.h value HEXCHAT_EAT_PLUGIN" eat_PLUGIN :: CInt
foreign import capi "hexchat-plugin.h value HEXCHAT_EAT_ALL" eat_ALL :: CInt

foreign import capi "hexchat-plugin.h hexchat_command" hexchat_command :: Plugin -> CString -> IO ()
foreign import capi "hexchat-plugin.h hexchat_print" hexchat_print :: Plugin -> CString -> IO ()
foreign import capi "hexchat-plugin.h hexchat_emit_print" hexchat_emit_print :: Plugin -> CString -> CString -> CString -> CString -> CString -> CString -> IO Bool
foreign import capi "hexchat-plugin.h hexchat_emit_print_attrs" hexchat_emit_print_attrs :: Plugin -> Ptr HexChat_EventAttrs -> CString -> CString -> CString -> CString -> CString -> CString -> IO Bool
foreign import capi "hexchat-plugin.h hexchat_send_modes" hexchat_send_modes :: Plugin -> Ptr CString -> CInt -> CInt -> CChar -> CChar -> IO ()
foreign import capi "hexchat-plugin.h hexchat_nickcmp" hexchat_nickcmp :: Plugin -> CString -> CString -> IO CInt
foreign import capi "hexchat-plugin.h hexchat_strip" hexchat_strip :: Plugin -> CString -> CInt -> CInt -> IO CString
foreign import capi "hexchat-plugin.h hexchat_free" hexchat_free :: Plugin -> Ptr a -> IO ()
foreign import capi "hexchat-plugin.h hexchat_event_attrs_create" hexchat_event_attrs_create :: Plugin -> IO (Ptr HexChat_EventAttrs)
foreign import capi "hexchat-plugin.h hexchat_event_attrs_free" hexchat_event_attrs_free :: Plugin -> Ptr HexChat_EventAttrs -> IO ()
foreign import capi "hexchat-plugin.h hexchat_get_info" hexchat_get_info :: Plugin -> CString -> IO CString
foreign import capi "hexchat-plugin.h hexchat_get_prefs" hexchat_get_prefs :: Plugin -> CString -> Ptr CString -> Ptr CInt -> IO CInt
foreign import capi "hexchat-plugin.h hexchat_list_get" hexchat_list_get :: Plugin -> CString -> IO (Ptr HexChat_List)
foreign import capi "hexchat-plugin.h hexchat_list_fields" hexchat_list_fields :: Plugin -> CString -> IO (Ptr CString)
foreign import capi "hexchat-plugin.h hexchat_list_next" hexchat_list_next :: Plugin -> Ptr HexChat_List -> IO Bool
foreign import capi "hexchat-plugin.h hexchat_list_str" hexchat_list_str :: Plugin -> Ptr HexChat_List -> CString -> IO CString
foreign import capi "hexchat-plugin.h hexchat_list_int" hexchat_list_int :: Plugin -> Ptr HexChat_List -> CString -> IO CInt
foreign import capi "hexchat-plugin.h hexchat_list_time" hexchat_list_time :: Plugin -> Ptr HexChat_List -> CString -> IO CTime
foreign import capi "hexchat-plugin.h hexchat_list_free" hexchat_list_free :: Plugin -> Ptr HexChat_List -> IO ()
foreign import capi "hexchat-plugin.h hexchat_hook_command" hexchat_hook_command :: Plugin -> CString -> CInt -> FunPtr (Ptr CString -> Ptr CString -> StablePtr a -> IO CInt) -> CString -> StablePtr a -> IO Hook
foreign import capi "hexchat-plugin.h hexchat_hook_print" hexchat_hook_print :: Plugin -> CString -> CInt -> FunPtr (Ptr CString -> StablePtr a -> IO CInt) -> StablePtr a -> IO Hook
foreign import capi "hexchat-plugin.h hexchat_hook_print_attrs" hexchat_hook_print_attrs :: Plugin -> CString -> CInt -> FunPtr (Ptr CString -> Ptr HexChat_EventAttrs -> StablePtr a -> IO CInt) -> StablePtr a -> IO Hook
foreign import capi "hexchat-plugin.h hexchat_hook_server" hexchat_hook_server :: Plugin -> CString -> CInt -> FunPtr (Ptr CString -> Ptr CString -> StablePtr a -> IO CInt) -> StablePtr a -> IO Hook
foreign import capi "hexchat-plugin.h hexchat_hook_server_attrs" hexchat_hook_server_attrs :: Plugin -> CString -> CInt -> FunPtr (Ptr CString -> Ptr CString -> Ptr HexChat_EventAttrs -> StablePtr a -> IO CInt) -> StablePtr a -> IO Hook
foreign import capi "hexchat-plugin.h hexchat_hook_timer" hexchat_hook_timer :: Plugin -> CInt -> FunPtr (StablePtr a -> IO CInt) -> StablePtr a -> IO Hook
foreign import capi "hexchat-plugin.h hexchat_unhook" hexchat_unhook :: Plugin -> Hook -> IO (StablePtr a)
foreign import capi "hexchat-plugin.h hexchat_find_context" hexchat_find_context :: Plugin -> CString -> CString -> IO Context
foreign import capi "hexchat-plugin.h hexchat_get_context" hexchat_get_context :: Plugin -> IO Context
foreign import capi "hexchat-plugin.h hexchat_set_context" hexchat_set_context :: Plugin -> Context -> IO Bool
foreign import capi "hexchat-plugin.h hexchat_plugingui_add" hexchat_plugingui_add :: Plugin -> CString -> CString -> CString -> CString -> CString -> IO Plugin
foreign import capi "hexchat-plugin.h hexchat_plugingui_remove" hexchat_plugingui_remove :: Plugin -> Plugin -> IO ()

foreign import ccall "&call_haskell_command" ptr_call_haskell_command :: FunPtr (Ptr CString -> Ptr CString -> StablePtr ([String] -> [String] -> IO Eat) -> IO CInt)
foreign export ccall call_haskell_command :: Ptr CString -> Ptr CString -> StablePtr ([String] -> [String] -> IO Eat) -> IO CInt
call_haskell_command pw pwe sf = reportException (return eat_NONE) $ do
	w <- peekStringArray pw 1 32
	we <- peekStringArray pwe 1 32
	f <- deRefStablePtr sf
	fromEat <$> f w we

foreign import ccall "&call_haskell_print" ptr_call_haskell_print :: FunPtr (Ptr CString -> StablePtr ([String] -> IO Eat) -> IO CInt)
foreign export ccall call_haskell_print :: Ptr CString -> StablePtr ([String] -> IO Eat) -> IO CInt
call_haskell_print pw sf = reportException (return eat_NONE) $ do
	ws <- peekStringArray pw 0 4
	f <- deRefStablePtr sf
	fromEat <$> f ws

foreign import ccall "&call_haskell_print_attrs" ptr_call_haskell_print_attrs :: FunPtr (Ptr CString -> Ptr HexChat_EventAttrs -> StablePtr ([String] -> EventAttrs -> IO Eat) -> IO CInt)
foreign export ccall call_haskell_print_attrs :: Ptr CString -> Ptr HexChat_EventAttrs -> StablePtr ([String] -> EventAttrs -> IO Eat) -> IO CInt
call_haskell_print_attrs pw pa sf = reportException (return eat_NONE) $ do
	ws <- peekStringArray pw 0 4
	attrs <- toAttrs pa
	f <- deRefStablePtr sf
	fromEat <$> f ws attrs

foreign import ccall "&call_haskell_server" ptr_call_haskell_server :: FunPtr (Ptr CString -> Ptr CString -> StablePtr ([String] -> [String] -> IO Eat) -> IO CInt)
foreign export ccall call_haskell_server :: Ptr CString -> Ptr CString -> StablePtr ([String] -> [String] -> IO Eat) -> IO CInt
call_haskell_server pw pwe sf = reportException (return eat_NONE) $ do
	w <- peekStringArray pw 1 32
	we <- peekStringArray pwe 1 32
	f <- deRefStablePtr sf
	fromEat <$> f w we

foreign import ccall "&call_haskell_server_attrs" ptr_call_haskell_server_attrs :: FunPtr (Ptr CString -> Ptr CString -> Ptr HexChat_EventAttrs -> StablePtr ([String] -> [String] -> EventAttrs -> IO Eat) -> IO CInt)
foreign export ccall call_haskell_server_attrs :: Ptr CString -> Ptr CString -> Ptr HexChat_EventAttrs -> StablePtr ([String] -> [String] -> EventAttrs -> IO Eat) -> IO CInt
call_haskell_server_attrs pw pwe pa sf = reportException (return eat_NONE) $ do
	w <- peekStringArray pw 1 32
	we <- peekStringArray pwe 1 32
	attrs <- toAttrs pa
	f <- deRefStablePtr sf
	fromEat <$> f w we attrs

foreign import capi "&hexchat_event_attrs_free" ptr_finalize_attrs :: FunPtr (Plugin -> Ptr HexChat_EventAttrs -> IO ())
foreign import capi "&hexchat_list_free" ptr_finalize_list :: FunPtr (Plugin -> Ptr HexChat_List -> IO ())


-- | This type defines whether the current hook "consumes" the event or lets other hooks know about it.
data Eat = EatNone -- ^ Pass the event to everything else.
	| EatHexChat -- ^ Pass the event to all other scripts but not HexChat.
	| EatPlugin -- ^ Pass the event to HexChat but not any other scripts.
	| EatAll -- ^ Completely consume the event.
	deriving (Show, Read, Eq)

fromEat :: Eat -> CInt
fromEat EatNone = eat_NONE
fromEat EatHexChat = eat_HEXCHAT
fromEat EatPlugin = eat_PLUGIN
fromEat EatAll = eat_ALL

-- | Event attributes.
data EventAttrs = EventAttrs { server_time_utc :: CTime } deriving (Show, Read, Eq)

fromAttrs :: Plugin -> EventAttrs -> IO (ForeignPtr HexChat_EventAttrs)
fromAttrs plugin attrs = do
	ptr <- hexchat_event_attrs_create plugin
	poke (castPtr ptr) (server_time_utc attrs)
	newForeignPtrEnv (coerce ptr_finalize_attrs) (coerce plugin) ptr

withAttrs :: Plugin -> EventAttrs -> (Ptr HexChat_EventAttrs -> IO a) -> IO a
withAttrs plugin attrs f = do
	fp <- fromAttrs plugin attrs
	ret <- withForeignPtr fp f
	finalizeForeignPtr fp
	return ret

toAttrs :: Ptr HexChat_EventAttrs -> IO EventAttrs
toAttrs ptr = EventAttrs <$> peek (castPtr ptr)

peekStringArray :: Ptr CString -> Int -> Int -> IO [String]
peekStringArray ps i j | i >= j = return []
peekStringArray ps i j = do
	p <- peekElemOff ps i
	if p == nullPtr then return []
		else do
			s <- peekCString p
			if null s then return []
				else (s :) <$> peekStringArray ps (i + 1) j

with2CString :: String -> String -> (CString -> CString -> IO a) -> IO a
with2CString a b f = withCString a $ \a -> withCString b $ \b -> f a b

with3CString :: String -> String -> String -> (CString -> CString -> CString -> IO a) -> IO a
with3CString a b c f = withCString a $ \a -> withCString b $ \b -> withCString c $ \c -> f a b c

with4CString :: String -> String -> String -> String -> (CString -> CString -> CString -> CString -> IO a) -> IO a
with4CString a b c d f = withCString a $ \a -> withCString b $ \b -> withCString c $ \c -> withCString d $ \d -> f a b c d

with5CString :: String -> String -> String -> String -> String -> (CString -> CString -> CString -> CString -> CString -> IO a) -> IO a
with5CString a b c d e f = withCString a $ \a -> withCString b $ \b -> withCString c $ \c -> withCString d $ \d -> withCString e $ \e -> f a b c d e

reportException :: IO a -> IO a -> IO a
reportException def f = catch f $ \e -> do
	plugin <- getPlugin
	print plugin $ show (e :: SomeException)
	def


command :: Plugin -> String -> IO ()
command plugin str = withCString str $ hexchat_command plugin

print :: Plugin -> String -> IO ()
print plugin str = withCString str $ hexchat_print plugin

emitPrint :: Plugin -> String -> [String] -> IO Bool
emitPrint plugin event (v1:v2:v3:v4:_) = with5CString event v1 v2 v3 v4 $ \event v1 v2 v3 v4 -> hexchat_emit_print plugin event v1 v2 v3 v4 nullPtr
emitPrint plugin event [v1, v2, v3] = with4CString event v1 v2 v3 $ \event v1 v2 v3 -> hexchat_emit_print plugin event v1 v2 v3 nullPtr nullPtr
emitPrint plugin event [v1, v2] = with3CString event v1 v2 $ \event v1 v2 -> hexchat_emit_print plugin event v1 v2 nullPtr nullPtr nullPtr
emitPrint plugin event [v1] = with2CString event v1 $ \event v1 -> hexchat_emit_print plugin event v1 nullPtr nullPtr nullPtr nullPtr
emitPrint plugin event [] = withCString event $ \event -> hexchat_emit_print plugin event nullPtr nullPtr nullPtr nullPtr nullPtr

emitPrintAttrs :: Plugin -> EventAttrs -> String -> [String] -> IO Bool
emitPrintAttrs plugin attrs event (v1:v2:v3:v4:_) = withAttrs plugin attrs $ \attrs -> with5CString event v1 v2 v3 v4 $ \event v1 v2 v3 v4 -> hexchat_emit_print_attrs plugin attrs event v1 v2 v3 v4 nullPtr
emitPrintAttrs plugin attrs event [v1, v2, v3] = withAttrs plugin attrs $ \attrs -> with4CString event v1 v2 v3 $ \event v1 v2 v3 -> hexchat_emit_print_attrs plugin attrs event v1 v2 v3 nullPtr nullPtr
emitPrintAttrs plugin attrs event [v1, v2] = withAttrs plugin attrs $ \attrs -> with3CString event v1 v2 $ \event v1 v2 -> hexchat_emit_print_attrs plugin attrs event v1 v2 nullPtr nullPtr nullPtr
emitPrintAttrs plugin attrs event [v1] = withAttrs plugin attrs $ \attrs -> with2CString event v1 $ \event v1 -> hexchat_emit_print_attrs plugin attrs event v1 nullPtr nullPtr nullPtr nullPtr
emitPrintAttrs plugin attrs event [] = withAttrs plugin attrs $ \attrs -> withCString event $ \event -> hexchat_emit_print_attrs plugin attrs event nullPtr nullPtr nullPtr nullPtr nullPtr

sendModes :: Plugin -> [String] -> Int -> Char -> Char -> IO ()
sendModes plugin targets modes sign mode = let len = length targets
	in allocaBytes (sizeOf (undefined :: CString) * length targets) $ \arr -> do
		bracket (mapM newCString targets) (mapM free) $ \strs -> do
			sequence_ $ zipWith (pokeElemOff arr) [0..] strs
			hexchat_send_modes plugin arr (fromIntegral len) (fromIntegral modes) (castCharToCChar sign) (castCharToCChar mode)

nickCmp :: Plugin -> String -> String -> IO Ordering
nickCmp plugin s1 s2 = with2CString s1 s2 $ \s1 s2 -> do
	ret <- hexchat_nickcmp plugin s1 s2
	return $ if ret > 0 then GT else if ret < 0 then LT else EQ

strip :: Plugin -> Bool -> Bool -> String -> IO String
strip plugin sc sf str = withCString str $ \str -> do
	res <- hexchat_strip plugin str (-1) flags
	r <- peekCString res
	hexchat_free plugin res
	return r
	where flags = (if sc then 1 else 0) + (if sf then 2 else 0)

getInfo :: Plugin -> String -> IO (Maybe String)
getInfo plugin key
	| key `elem` ["gtkwin_ptr", "win_ptr"] = return Nothing
	| otherwise = withCString key $ \key -> do
		ptr <- hexchat_get_info plugin key
		if ptr == nullPtr then return Nothing
			else Just <$> peekCString ptr

getPrefs :: Plugin -> String -> (String -> IO a) -> (Int -> IO a) -> (Bool -> IO a) -> IO (Maybe a)
getPrefs plugin key cstr cint cbool = withCString key $ \key -> alloca $ \ps -> alloca $ \pi -> do
	ret <- hexchat_get_prefs plugin key ps pi
	case ret of
		0 -> return Nothing
		1 -> Just <$> (peek ps >>= peekCString >>= cstr)
		2 -> Just <$> (peek pi >>= cint . fromIntegral)
		3 -> Just <$> (peek pi >>= cbool . (== 0))

listGet :: Plugin -> String -> IO (Maybe List)
listGet plugin key = withCString key $ \key -> do
	ptr <- hexchat_list_get plugin key
	if coerce ptr == nullPtr then return Nothing
		else do
			u <- newIORef False
			Just <$> List u <$> newForeignPtrEnv (coerce ptr_finalize_list) (coerce plugin) ptr

listFields :: Plugin -> String -> IO [String]
listFields plugin key = withCString key $ \key -> do
	ret <- hexchat_list_fields plugin key
	if ret == nullPtr then return []
		else walkList ret 0
	where walkList ptr n = do
		str <- peekElemOff ptr n
		if str == nullPtr then return []
			else (:) <$> peekCString str <*> walkList ptr (n + 1)

listNext :: Plugin -> List -> IO Bool
listNext plugin (List u ptr) = do
	ret <- withForeignPtr ptr $ hexchat_list_next plugin
	writeIORef u ret
	return ret

listStr :: Plugin -> List -> String -> IO String
listStr _ _ "context" = error "Cannot listStr context"
listStr plugin (List u ptr) key = do
	b <- readIORef u
	unless b $ error "Attempted to use uninitialized List"
	withCString key $ \key -> withForeignPtr ptr $ \ptr -> do
		str <- hexchat_list_str plugin ptr key
		if str == nullPtr then putStrLn "null" >> return ""
			else peekCString str

listInt :: Plugin -> List -> String -> IO Int
listInt plugin (List u ptr) key = do
	b <- readIORef u
	unless b $ error "Attempted to use uninitialized List"
	withCString key $ \key -> withForeignPtr ptr $ \ptr -> do
		fromIntegral <$> hexchat_list_int plugin ptr key

listTime :: Plugin -> List -> String -> IO CTime
listTime plugin (List u ptr) key = do
	b <- readIORef u
	unless b $ error "Attempted to use uninitialized List"
	withCString key $ \key -> withForeignPtr ptr $ \ptr -> do
		hexchat_list_time plugin ptr key

hookCommand :: Plugin -> String -> CInt -> String -> ([String] -> [String] -> IO Eat) -> IO Hook
hookCommand plugin cmd pri desc f = do
	sf <- newStablePtr f
	hook <- with2CString cmd desc $ \cmd desc -> hexchat_hook_command plugin cmd pri ptr_call_haskell_command desc sf
	return hook

hookPrint :: Plugin -> String -> CInt -> ([String] -> IO Eat) -> IO Hook
hookPrint plugin cmd pri f = do
	sf <- newStablePtr f
	hook <- withCString cmd $ \cmd -> hexchat_hook_print plugin cmd pri ptr_call_haskell_print sf
	return hook

hookPrintAttrs :: Plugin -> String -> CInt -> ([String] -> EventAttrs -> IO Eat) -> IO Hook
hookPrintAttrs plugin cmd pri f = do
	sf <- newStablePtr f
	hook <- withCString cmd $ \cmd -> hexchat_hook_print_attrs plugin cmd pri ptr_call_haskell_print_attrs sf
	return hook

hookServer :: Plugin -> String -> CInt -> ([String] -> [String] -> IO Eat) -> IO Hook
hookServer plugin cmd pri f = do
	sf <- newStablePtr f
	hook <- withCString cmd $ \cmd -> hexchat_hook_server plugin cmd pri ptr_call_haskell_server sf
	return hook

hookServerAttrs :: Plugin -> String -> CInt -> ([String] -> [String] -> EventAttrs -> IO Eat) -> IO Hook
hookServerAttrs plugin cmd pri f = do
	sf <- newStablePtr f
	hook <- withCString cmd $ \cmd -> hexchat_hook_server_attrs plugin cmd pri ptr_call_haskell_server_attrs sf
	return hook

unhook :: Plugin -> Hook -> IO ()
unhook plugin hook = do
	sf <- hexchat_unhook plugin hook
	freeStablePtr sf

findContext :: Plugin -> Maybe String -> Maybe String -> IO (Maybe Context)
findContext plugin server channel = do
	ptr <- case (server, channel) of
		(Just server, Just channel) -> with2CString server channel $ \server channel -> hexchat_find_context plugin server channel
		(Nothing, Just channel) -> withCString channel $ \channel -> hexchat_find_context plugin nullPtr channel
		(Just server, Nothing) -> withCString server $ \server -> hexchat_find_context plugin server nullPtr
	if coerce ptr == nullPtr then return Nothing
		else return (Just ptr)

getContext :: Plugin -> IO Context
getContext plugin = hexchat_get_context plugin

setContext :: Plugin -> Context -> IO Bool
setContext plugin context = hexchat_set_context plugin context

pluginguiAdd :: Plugin -> String -> String -> String -> String -> IO Plugin
pluginguiAdd plugin file name desc ver = with4CString file name desc ver $ \file name desc ver -> hexchat_plugingui_add plugin file name desc ver nullPtr

pluginguiRemove :: Plugin -> Plugin -> IO ()
pluginguiRemove = hexchat_plugingui_remove

type Plugin_Init = Plugin -> Ptr CString -> Ptr CString -> Ptr CString -> CString -> IO CInt
type Plugin_Deinit = Plugin -> IO CInt


data StaticData = StaticData
	{
		s_plugin :: IORef Plugin,
		s_handle :: IORef (IORef Plugin),
		s_hooks :: IORef (IORef [(Plugin, Hook)])
	}

lPlugin = s_plugin staticData
lHandle = s_handle staticData
lHooks = s_hooks staticData

{-# NOINLINE staticData #-}
staticData = unsafePerformIO $ StaticData
	<$> (newIORef $ error "unlinked plugin in StaticData")
	<*> (newIORef $ error "unlinked handle in StaticData")
	<*> (newIORef $ error "unlinked hooks in StaticData")

getPlugin :: IO Plugin
getPlugin = readIORef lPlugin

getHandle :: IO Plugin
getHandle = readIORef lHandle >>= readIORef

withHandle :: Plugin -> IO a -> IO a
withHandle handle f = bracket (swap handle) swap (const f)
	where swap handle = do
		loc <- readIORef lHandle
		atomicModifyIORef loc (\old -> (handle, old))

initStaticData :: Plugin -> IO ()
initStaticData plugin = do
	writeIORef lPlugin plugin
	writeIORef lHandle =<< newIORef plugin
	writeIORef lHooks =<< newIORef []

joinStaticData :: StaticData -> IO ()
joinStaticData s = do
	readIORef lPlugin >>= writeIORef (s_plugin s)
	readIORef lHandle >>= writeIORef (s_handle s)
	readIORef lHooks >>= writeIORef (s_hooks s)

unhookHandle :: Plugin -> IO ()
unhookHandle handle = do
	m <- readIORef lHooks >>= readIORef
	plugin <- getPlugin
	traverse (unhook plugin) $ map snd $ filter ((== handle) . fst) m
	readIORef lHooks >>= flip modifyIORef (filter ((/= handle) . fst))

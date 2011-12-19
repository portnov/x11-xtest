{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.X11.XTest
  (queryXTestSupport,
   fakeMotion,
   fakeButtonPress,
   movePointer,
   withGrabbedControl,
   sendKey)
  where

import Control.Monad
import Graphics.X11.Xlib
import Foreign
import Foreign.C.Types

-- FFI imports

-- XTestFakeButtonEvent(display, button, is_press, delay)
foreign import ccall unsafe "X11/extensions/XTest.h XTestFakeButtonEvent"
    xFakeButtonEvent :: Display -> Button -> Bool -> Time -> IO Status

foreign import ccall unsafe "X11/extensions/XTest.h XTestFakeMotionEvent"
    xFakeMotionEvent :: Display -> CInt -> CInt -> CInt -> Time -> IO Status

foreign import ccall unsafe "X11/extensions/XTest.h XTestGrabControl"
    xGrabControl :: Display -> Bool -> IO Status

foreign import ccall unsafe "X11/extensions/XTest.h XTestFakeKeyEvent"
    xFakeKeyEvent :: Display -> KeyCode -> Bool -> CULong -> IO Status

foreign import ccall unsafe "X11/extensions/XTest.h XTestQueryExtension"
    xQueryExtension :: Display -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO Bool

-- | Ask the X server if XTest extension is supported.
-- Returns Nothing, if extension is not supported.
-- Otherwise, it returns:
--
--  * Event number for the first event for this extension (undefined for current version of XTest).
--
--  * Error number for the first error for this extension (undefined for current version of XTest).
--
--  * Major and
--
--  * minor versions of the extension.
--
queryXTestSupport :: Display -> IO (Maybe (Int, Int, Int, Int))
queryXTestSupport dpy = do
  alloca $ \pevent ->
    alloca $ \perror ->
      alloca $ \pmajor ->
        alloca $ \pminor -> do
          b <- xQueryExtension dpy pevent perror pmajor pminor
          if b
            then do
                 event <- peek pevent
                 error <- peek perror
                 major <- peek pmajor
                 minor <- peek pminor
                 return $ Just (fromIntegral event, fromIntegral error,
                                fromIntegral major, fromIntegral minor)
            else return Nothing

-- | Perform some IO actions while control grabbed by XTest
withGrabbedControl :: Display -> IO a -> IO a
withGrabbedControl dpy action = do
    st <- xGrabControl dpy True
    if st /= 0 -- Grabbed successfully
      then do
           result <- action
           xGrabControl dpy False
           return result
      else fail $ "XTest cannot grab control"

-- | Send fake key press
sendKey :: Display
        -> [KeySym] -- ^ Modifier keys (say, xK_Control_L). Set to [] if modifier is not needed.
        -> KeySym   -- ^ Key to press (say, xK_n).
        -> IO ()
sendKey dpy mods keysym = do
  keycode <- keysymToKeycode dpy keysym
  when (keycode /= 0) $ withGrabbedControl dpy $ do
      -- Press mods
      forM_ mods $ \modsym -> do
          code <- keysymToKeycode dpy modsym
          xFakeKeyEvent dpy code True 0

      -- Press and release key
      xFakeKeyEvent dpy keycode True  0
      xFakeKeyEvent dpy keycode False 0

      -- Release mods
      forM_ (reverse mods) $ \modsym -> do
          code <- keysymToKeycode dpy modsym
          xFakeKeyEvent dpy code False 0

      sync dpy False
      return ()

-- | Create fake pointer motion event.
fakeMotion :: Display      -- 
           -> ScreenNumber --
           -> Int          -- ^ X
           -> Int          -- ^ Y
           -> IO ()
fakeMotion dpy sid x y = do
  xFakeMotionEvent dpy (fromIntegral sid) (fromIntegral x) (fromIntegral y) 0
  return ()

-- | Create fake mouse button click event.
fakeButtonPress :: Display
                -> Button  -- ^ Mouse button number
                -> IO ()
fakeButtonPress dpy button = do
  xFakeButtonEvent dpy button True 0
  xFakeButtonEvent dpy button False 0
  return ()

-- | Move mouse pointer.
movePointer :: Display
            -> ScreenNumber
            -> XID          -- ^ Root window XID
            -> Int          -- ^ delta X
            -> Int          -- ^ delta Y
            -> IO ()
movePointer dpy sid root dx dy = do
 (_,_,_,x,y,_,_,_) <- queryPointer dpy root
 fakeMotion dpy sid (fromIntegral x + dx) (fromIntegral y + dy)


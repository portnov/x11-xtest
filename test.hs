
import Control.Concurrent
import Graphics.X11
import Graphics.X11.XTest

withDisplay :: String -> (Display -> IO a) -> IO ()
withDisplay str action = do
  dpy <- openDisplay str
  action dpy
  closeDisplay dpy

main = do
  withDisplay ":0" $ \dpy -> do
      Just (a,b,c,d) <- queryXTestSupport dpy
      print (a,b,c,d)
      sendKey dpy [] xK_m
      threadDelay 1000000
      sendKey dpy [xK_Shift_L] xK_n



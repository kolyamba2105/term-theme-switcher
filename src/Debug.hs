module Debug (debugLog) where

debugLog :: Show a => IO a -> IO a
debugLog a = (a >>= print) *> a

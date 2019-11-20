-- | Upload
{-# language ScopedTypeVariables #-}

import Data.Aeson
import Control.Monad
import Control.Concurrent (threadDelay)

import Paths

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = do
    static <- getStaticDir
    startGUI defaultConfig { jsStatic = Just static, jsWindowReloadOnDisconnect = False } setup

setup :: Window -> UI ()
setup w = void $ do
    return w # set title "Upload"
    UI.addStyleSheet w "upload.css"

    dropArea :: Element <-
      UI.div #. "drop-area"
             # set (attr "ondragover") "event.preventDefault()"
             # set (attr "ondrop"    ) "event.preventDefault()"
             #+ [UI.div #+ [string "drop files here"]]

    runFunction $ ffi "console.log(`hello world %%o`, %1)" dropArea

    on UI.dragEnter dropArea $ \_ -> do
      element dropArea #. "drop-area highlight"
      liftIO $ print "dragEnter"

    incoming <- ffiExport $ \(x :: Value) -> do
      print ("incoming file", x)

    runFunction $ ffi "%1.addEventListener('drop', e => [...e.dataTransfer.files].forEach(f => f.arrayBuffer().then(b => %2([f.name, f.type, f.lastModified, String.fromCharCode.apply(null, new Uint8Array(b))]))), true)" dropArea incoming

    on UI.dragLeave dropArea $ \_ -> do
      runFunction $ ffi "$(%1).removeClass(%2)" dropArea "highlight"
      liftIO $ print "dragLeave"

    getBody w #+ [pure dropArea]

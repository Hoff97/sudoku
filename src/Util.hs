module Util where

import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (liftIO)
import           Data.List                       (intercalate)
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Backend.Gtk
import           Diagrams.Prelude                hiding (set)
import           Diagrams.TwoD.Size
import           Graphics.UI.Gtk

diagramWindowed :: Diagram Cairo -> IO ()
diagramWindowed x = do
    initGUI
    w <- windowNew
    da <- drawingAreaNew
    w `containerAdd` da

    set w [windowDefaultWidth := 380, windowDefaultHeight := 380, containerBorderWidth := 10]

    let scaledX = toGtkCoords x

    da `on` exposeEvent $ liftIO $ do
        dw <- widgetGetDrawWindow da
        renderToGtk dw scaledX
        return True

    da `widgetAddEvents` [PointerMotionMask]
    widgetShowAll w
    onDestroy w mainQuit
    mainGUI

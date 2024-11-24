{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Consultation

main :: IO ()
main = do
    startGUI defaultConfig { jsStatic = Just "static" } setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Sistema Médico"

    menuTitle <- UI.h1 # set text "Sistema de Gestión Médica"
    menuInfo <- UI.p # set text "Seleccione una opción:"
    
    btnConsultations <- UI.button # set text "Hojas de Consulta"
    btnExit <- UI.button # set text "Salir"

    layout <- column
        [ element menuTitle
        , element menuInfo
        , row [element btnConsultations, element btnExit]
        ]

    getBody window #+ [element layout]

    -- Botón para abrir las consultas
    on UI.click btnConsultations $ \_ -> runConsultation window []

    -- Botón para salir
    on UI.click btnExit $ \_ -> runFunction $ ffi "window.close()"

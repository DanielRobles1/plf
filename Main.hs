{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Monad (void)

import Consultation

main :: IO ()
main = do
    startGUI defaultConfig { jsStatic = Just "static" } setup

setup :: Window -> UI ()
setup window = do
    -- Configurar el título de la ventana
    return window # set title "Sistema Médico"

    -- Crear el encabezado con un diseño moderno
    header <- UI.div # set style [("background-color", "#3498db"),
                                 ("color", "white"),
                                 ("text-align", "center"),
                                 ("padding", "20px 0"),
                                 ("box-shadow", "0px 4px 8px rgba(0,0,0,0.1)")]
                     #+ [UI.h1 # set text "Bienvenido al Sistema Médico" 
                               # set style [("font-size", "28px"),
                                           ("font-family", "Arial, sans-serif"),
                                           ("margin", "0")]] 

    -- Crear la sección de información
    menuTitle <- UI.h1 # set text "Sistema de Gestión Médica" 
                       # set style [("font-size", "32px"), 
                                   ("color", "#2c3e50"), 
                                   ("text-align", "center"),
                                   ("margin-top", "50px"),
                                   ("font-family", "Arial, sans-serif")] 

    menuInfo <- UI.p # set text "Seleccione una opción:" 
                     # set style [("font-size", "18px"),
                                 ("color", "#34495e"),
                                 ("text-align", "center"),
                                 ("margin-top", "10px"),
                                 ("font-family", "Arial, sans-serif")]

    -- Crear los botones estilizados
    btnConsultations <- UI.button # set text "Hojas de Consulta" 
                                 # set style [("background-color", "#2ecc71"),
                                             ("color", "white"),
                                             ("padding", "15px 30px"),
                                             ("border-radius", "8px"),
                                             ("border", "none"),
                                             ("font-size", "16px"),
                                             ("cursor", "pointer"),
                                             ("margin", "10px")]
    btnExit <- UI.button # set text "Salir" 
                        # set style [("background-color", "#e74c3c"),
                                    ("color", "white"),
                                    ("padding", "15px 30px"),
                                    ("border-radius", "8px"),
                                    ("border", "none"),
                                    ("font-size", "16px"),
                                    ("cursor", "pointer"),
                                    ("margin", "10px")]

    -- Crear el pie de página
    footer <- UI.div # set style [("background-color", "#2c3e50"),
                                ("color", "white"),
                                ("text-align", "center"),
                                ("padding", "10px 0"),
                                ("position", "absolute"),
                                ("bottom", "0"),
                                ("width", "100%"),
                                ("font-family", "Arial, sans-serif"),
                                ("box-shadow", "0px -4px 8px rgba(0,0,0,0.1)")]
                     #+ [UI.p # set text "© 2024 Sistema Médico. Todos los derechos reservados."]

    -- Definir la disposición inicial
    layout <- column
        [ element menuTitle
        , element menuInfo
        , row [element btnConsultations, element btnExit]
        ] # set style [("display", "flex"), 
                      ("flex-direction", "column"), 
                      ("align-items", "center"), 
                      ("justify-content", "center"),
                      ("height", "calc(100vh - 120px)"), -- Restar espacio para header y footer
                      ("margin", "0 auto")]

    -- Aplicar estilo general al cuerpo
    getBody window # set style [("background-color", "#f4f6f6"),
                                ("font-family", "Arial, sans-serif"),
                                ("margin", "0"),
                                ("padding", "0")]

    -- Agregar elementos iniciales al cuerpo
    getBody window #+ [element header, element layout, element footer]

    -- Acción del botón "Hojas de Consulta"
    on UI.click btnConsultations $ \_ -> do
        limpiarPantalla window
        runConsultation window []

    -- Acción del botón "Salir"
    on UI.click btnExit $ \_ -> do
        limpiarPantalla window
        runFunction $ ffi "alert('Saliendo del sistema médico...'); window.close()"

-- Función para limpiar la pantalla antes de cargar nuevo contenido
limpiarPantalla :: Window -> UI ()
limpiarPantalla window = do
    void $ getBody window # set children [] -- Usa void para ignorar el resultado

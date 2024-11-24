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
    -- Configurar el título de la ventana
    return window # set title "Sistema Médico"

    -- Crear el encabezado con un título principal
    header <- UI.div # set style [("background-color", "#2c3e50"),
                                 ("color", "white"),
                                 ("text-align", "center"),
                                 ("padding", "20px 0")]
                     #+ [UI.h1 # set text "Bienvenido al Sistema Médico"]

    -- Crear la sección de información
    menuTitle <- UI.h1 # set text "Sistema de Gestión Médica" 
                       # set style [("font-size", "32px"), 
                                   ("color", "#2c3e50"), 
                                   ("text-align", "center"),
                                   ("margin-top", "50px")]

    menuInfo <- UI.p # set text "Seleccione una opción:" 
                     # set style [("font-size", "18px"),
                                 ("color", "#34495e"),
                                 ("text-align", "center"),
                                 ("margin-top", "10px")]

    -- Crear los botones con un tema médico
    btnConsultations <- UI.button # set text "Hojas de Consulta" 
                                 # set style [("background-color", "#00bfae"),
                                             ("color", "white"),
                                             ("padding", "12px 25px"),
                                             ("border-radius", "8px"),
                                             ("border", "none"),
                                             ("font-size", "16px")]
    btnExit <- UI.button # set text "Salir" 
                        # set style [("background-color", "#e74c3c"),
                                    ("color", "white"),
                                    ("padding", "12px 25px"),
                                    ("border-radius", "8px"),
                                    ("border", "none"),
                                    ("font-size", "16px")]

    -- Crear el pie de página
    footer <- UI.div # set style [("background-color", "#2c3e50"),
                                ("color", "white"),
                                ("text-align", "center"),
                                ("padding", "10px 0"),
                                ("position", "absolute"),
                                ("bottom", "0"),
                                ("width", "100%")]
                     #+ [UI.p # set text "© 2024 Sistema Médico. Todos los derechos reservados."]

    -- Definir la disposición con espaciado y alineación centrada
    layout <- column
        [ element menuTitle
        , element menuInfo
        , UI.hr # set style [("border", "1px solid #ccc"), 
                            ("width", "80%"), 
                            ("margin", "20px auto")]
        , row [element btnConsultations, element btnExit]
        ] # set style [("display", "flex"), 
                      ("flex-direction", "column"), 
                      ("align-items", "center"), 
                      ("justify-content", "center"),
                      ("height", "100vh"),
                      ("margin-bottom", "60px")]  -- Espacio para el pie de página

    -- Aplicar el estilo al cuerpo de la página (fondo y fuente)
    getBody window # set style [("background-color", "#f4f6f6"), 
                                ("font-family", "Arial, sans-serif")]

    -- Agregar el encabezado, la disposición y el pie de página al cuerpo de la ventana
    getBody window #+ [element header, element layout, element footer]

    -- Botón para abrir las consultas
    on UI.click btnConsultations $ \_ -> runConsultation window []

    -- Botón para salir de la aplicación
    on UI.click btnExit $ \_ -> runFunction $ ffi "window.close()"

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import PatientManagement
import Consultation
import SearchSystem
import AppointmentSystem
import Reports

main :: IO ()
main = do
    startGUI defaultConfig { jsStatic = Just "static" } setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Sistema de Gestión Médica"

    menuTitle <- UI.h1 # set text "Sistema de Gestión Médica"
    menuInfo <- UI.p # set text "Seleccione una opción:"
    
    btnPatients <- UI.button # set text "Gestión de Expedientes"
    btnConsultations <- UI.button # set text "Hojas de Consulta"
    btnSearch <- UI.button # set text "Búsqueda Avanzada"
    btnAppointments <- UI.button # set text "Control de Citas"
    btnReports <- UI.button # set text "Generación de Reportes"
    btnExit <- UI.button # set text "Salir"

    layout <- column
        [ element menuTitle
        , element menuInfo
        , row [element btnPatients, element btnConsultations]
        , row [element btnSearch, element btnAppointments]
        , element btnReports
        , element btnExit
        ]

    getBody window #+ [element layout]

    -- Botones con liftIO
    on UI.click btnPatients $ \_ -> do
        liftIO runPatientManagement
        runFunction $ ffi "alert('Gestión de Expedientes ejecutada')"
    
    on UI.click btnConsultations $ \_ -> do
        liftIO $ runConsultation []
        runFunction $ ffi "alert('Hojas de Consulta abiertas')"

    on UI.click btnSearch $ \_ -> do
        liftIO runSearchSystem
        runFunction $ ffi "alert('Búsqueda Avanzada ejecutada')"

    on UI.click btnAppointments $ \_ -> do
        liftIO runAppointmentSystem
        runFunction $ ffi "alert('Control de Citas ejecutado')"

    on UI.click btnReports $ \_ -> do
        liftIO runReports
        runFunction $ ffi "alert('Reportes generados')"

    on UI.click btnExit $ \_ -> do
        runFunction $ ffi "window.close()"

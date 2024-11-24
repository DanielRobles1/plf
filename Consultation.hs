{-# LANGUAGE OverloadedStrings #-}
module Consultation where
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core



import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

type Consultation = (String, String, String, String, String) -- (Fecha, Doctor, Diagnóstico, Tratamiento, Observaciones)

runConsultation :: Window -> [Consultation] -> UI ()
runConsultation window consultations = do
    -- Títulos y botones
    menuTitle <- UI.h1 # set text "Hojas de Consulta"
    menuInfo <- UI.p # set text "Seleccione una opción:"
    
    btnNew <- UI.button # set text "Crear Nueva Consulta"
    btnView <- UI.button # set text "Ver Consultas"
    btnBack <- UI.button # set text "Volver"

    layout <- column
        [ element menuTitle
        , element menuInfo
        , row [element btnNew, element btnView, element btnBack]
        ]

    getBody window #+ [element layout]

    -- Acciones de los botones
    on UI.click btnNew $ \_ -> createConsultation window consultations
    on UI.click btnView $ \_ -> viewConsultations window consultations
    on UI.click btnBack $ \_ -> do
        getBody window # set children [] -- Limpia la ventana
        runFunction $ ffi "alert('Volviendo al menú principal')"

createConsultation :: Window -> [Consultation] -> UI ()
createConsultation window consultations = do
    getBody window # set children [] -- Limpia la ventana

    -- Entradas de texto
    title <- UI.h1 # set text "Crear Nueva Consulta"
    inputDate <- UI.input # set (UI.attr "placeholder") "Fecha (dd/mm/aaaa)"
    inputDoctor <- UI.input # set (UI.attr "placeholder") "Nombre del Doctor"
    inputDiagnosis <- UI.input # set (UI.attr "placeholder") "Diagnóstico"
    inputTreatment <- UI.input # set (UI.attr "placeholder") "Tratamiento"
    inputNotes <- UI.textarea # set (UI.attr "placeholder") "Observaciones"
    btnSave <- UI.button # set text "Guardar"
    btnCancel <- UI.button # set text "Cancelar"

    layout <- column
        [ element title
        , element inputDate
        , element inputDoctor
        , element inputDiagnosis
        , element inputTreatment
        , element inputNotes
        , row [element btnSave, element btnCancel]
        ]

    getBody window #+ [element layout]

    -- Acciones de los botones
    on UI.click btnSave $ \_ -> do
        date <- get value inputDate
        doctor <- get value inputDoctor
        diagnosis <- get value inputDiagnosis
        treatment <- get value inputTreatment
        notes <- get value inputNotes
        let newConsultation = (date, doctor, diagnosis, treatment, notes)
        liftIO $ putStrLn $ "Consulta registrada: " ++ show newConsultation
        runFunction $ ffi "alert('Consulta registrada exitosamente')"
        runConsultation window (consultations ++ [newConsultation])
    
    on UI.click btnCancel $ \_ -> runConsultation window consultations

    viewConsultations :: Window -> [Consultation] -> UI ()
    viewConsultations window consultations = do
        -- Limpia el contenido actual de la ventana
        getBody window # set children []
    
        -- Título de la página
        title <- UI.h1 # set text "Consultas Registradas"
    
        -- Si no hay consultas, muestra un mensaje
        consultationsLayout <- if null consultations
            then UI.p # set text "No hay consultas registradas."
            else UI.div #+ map (UI.p # set text . formatConsultation) consultations
    
        -- Botón para regresar
        btnBack <- UI.button # set text "Volver"
    
        -- Estructura de la página
        layout <- column
            [ element title
            , element consultationsLayout
            , element btnBack
            ]
    
        -- Agrega la estructura al cuerpo de la ventana
        getBody window #+ [element layout]
    
        -- Acción del botón "Volver"
        on UI.click btnBack $ \_ -> runConsultation window consultations
    
    
formatConsultation :: Consultation -> String
formatConsultation (date, doctor, diagnosis, treatment, notes) =
    "Fecha: " ++ date ++
    ", Doctor: " ++ doctor ++
    ", Diagnóstico: " ++ diagnosis ++
    ", Tratamiento: " ++ treatment ++
    ", Observaciones: " ++ notes

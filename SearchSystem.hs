{-# LANGUAGE OverloadedStrings #-}

module SearchSystem where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import System.IO
import Data.List (isInfixOf)
import Control.Exception (try, IOException)

type Patient = (String, String, String) -- (Nombre, FechaNacimiento, Diagnóstico)
type Consultation = (String, String, String, String, String) -- (Fecha, Doctor, Diagnóstico, Tratamiento, Notas)

-- Archivos CSV
patientsFile :: FilePath
patientsFile = "patients.csv"

consultationsFile :: FilePath
consultationsFile = "consultations.csv"

-- Configuración de la interfaz gráfica
setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Sistema de Búsqueda Médica"

    -- Crear elementos de la interfaz
    header <- UI.div #. "header" #+
        [ UI.h1 #+ [UI.string "Sistema de Búsqueda Médica"]
            # set style [("font-family", "'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"), 
                         ("color", "#2c3e50"), 
                         ("font-size", "36px"), 
                         ("margin", "0"), 
                         ("padding", "20px 0")]
        , UI.p #. "subtitle" #+ [UI.string "Encuentra rápidamente pacientes y consultas."]
            # set style [("font-family", "'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"), 
                         ("color", "#7f8c8d"), 
                         ("font-size", "18px"), 
                         ("margin", "0"), 
                         ("padding", "0 0 20px 0")]
        ]

    searchInput <- UI.input # set (UI.attr "placeholder") "Ingrese término de búsqueda"
        # set style [("padding", "10px"), ("border", "1px solid #ccc"), ("border-radius", "4px"), ("font-size", "16px"), ("width", "80%")]
        #. "search-input"

    searchButton <- UI.button #+ [UI.string "Buscar"]
        #. "search-button"
        # set style [("background-color", "#4CAF50"), ("color", "white"), ("padding", "12px 25px"), ("border-radius", "8px"), ("border", "none"), ("font-size", "16px"), ("cursor", "pointer"), ("margin-top", "20px")]

    backButton <- UI.button #+ [UI.string "Volver al Menú Principal"]
        #. "back-button"
        # set style [("background-color", "#E74C3C"), ("color", "white"), ("padding", "12px 25px"), ("border-radius", "8px"), ("border", "none"), ("font-size", "16px"), ("cursor", "pointer"), ("display", "none"), ("margin-top", "20px")]

    resultsPatients <- UI.div #. "results-patients"
    resultsConsultations <- UI.div #. "results-consultations"

    -- Contenedor principal
    getBody window #+ 
        [ element header
        , UI.div #. "search-bar" #+ [element searchInput, element searchButton]
        , UI.div #. "menu-main" #+ [element backButton]
        , UI.hr
        , UI.div #. "results-section" #+ 
            [ UI.h2 #+ [UI.string "Resultados en Pacientes:"]
                # set style [("font-size", "20px"), ("color", "#2980b9"), ("font-weight", "bold"), ("margin-bottom", "10px")]
            , element resultsPatients
            ]
        , UI.div #. "results-section" #+ 
            [ UI.h2 #+ [UI.string "Resultados en Consultas:"]
                # set style [("font-size", "20px"), ("color", "#2980b9"), ("font-weight", "bold"), ("margin-bottom", "10px")]
            , element resultsConsultations
            ]
        ]

    -- Lógica del botón de búsqueda
    on UI.click searchButton $ \_ -> do
        term <- get value searchInput
        patients <- liftIO loadPatients
        consultations <- liftIO loadConsultations

        let matchingPatients = filter (matchesPatient term) patients
        let matchingConsultations = filter (matchesConsultation term) consultations

        -- Mostrar resultados en pacientes
        element resultsPatients # set children []
        if null matchingPatients
            then element resultsPatients #+ [UI.p #. "no-results" #+ [UI.string "No se encontraron pacientes que coincidan."]]
            else do
                patientElements <- mapM displayPatient matchingPatients
                element resultsPatients # set children patientElements

        -- Mostrar resultados en consultas
        element resultsConsultations # set children []
        if null matchingConsultations
            then element resultsConsultations #+ [UI.p #. "no-results" #+ [UI.string "No se encontraron consultas que coincidan."]]
            else do
                consultationElements <- mapM displayConsultation matchingConsultations
                element resultsConsultations # set children consultationElements

        -- Mostrar botón de volver al menú
        element backButton # set UI.style [("display", "block")]

    -- Lógica del botón de volver al menú principal
    on UI.click backButton $ \_ -> do
        -- Limpiar los resultados y esconder el botón de volver
        element resultsPatients # set children []
        element resultsConsultations # set children []
        element backButton # set UI.style [("display", "none")]

        -- Volver a mostrar la barra de búsqueda y el botón de búsqueda
        element searchInput # set UI.style [("display", "inline-block")]
        element searchButton # set UI.style [("display", "inline-block")]
        element resultsPatients # set UI.style [("display", "none")]
        element resultsConsultations # set UI.style [("display", "none")]

        -- Opcional: Limpiar el campo de búsqueda
        element searchInput # set UI.value ""

-- Cargar pacientes desde el archivo CSV
loadPatients :: IO [Patient]
loadPatients = do
    result <- try (readFile patientsFile) :: IO (Either IOException String)
    case result of
        Left _ -> return []
        Right content -> return (map parsePatient (lines content))

-- Cargar consultas desde el archivo CSV
loadConsultations :: IO [Consultation]
loadConsultations = do
    result <- try (readFile consultationsFile) :: IO (Either IOException String)
    case result of
        Left _ -> return []
        Right content -> return (map parseConsultation (lines content))

-- Analizar un paciente desde una línea CSV
parsePatient :: String -> Patient
parsePatient line =
    let (name, rest) = break (== ',') line
        rest' = drop 1 rest
        (dob, diagnosis) = break (== ',') rest'
    in (name, takeWhile (/= ',') dob, drop 1 diagnosis)

-- Analizar una consulta desde una línea CSV
parseConsultation :: String -> Consultation
parseConsultation line =
    let (date, rest1) = break (== ',') line
        rest1' = drop 1 rest1
        (doctor, rest2) = break (== ',') rest1'
        rest2' = drop 1 rest2
        (diagnosis, rest3) = break (== ',') rest2'
        rest3' = drop 1 rest3
        (treatment, notes) = break (== ',') rest3'
    in (date, doctor, diagnosis, treatment, drop 1 notes)

-- Verificar si un paciente coincide con el término de búsqueda
matchesPatient :: String -> Patient -> Bool
matchesPatient term (name, dob, diagnosis) =
    any (isInfixOf term) [name, dob, diagnosis]

-- Verificar si una consulta coincide con el término de búsqueda
matchesConsultation :: String -> Consultation -> Bool
matchesConsultation term (date, doctor, diagnosis, treatment, notes) =
    any (isInfixOf term) [date, doctor, diagnosis, treatment, notes]

-- Mostrar información de un paciente en la interfaz gráfica
displayPatient :: Patient -> UI Element
displayPatient (name, dob, diagnosis) = do
    UI.div #. "patient-card" #+ 
        [ UI.p #. "patient-name" #+ [UI.string $ "Nombre: " ++ name]
        , UI.p #. "patient-dob" #+ [UI.string $ "Fecha de Nacimiento: " ++ dob]
        , UI.p #. "patient-diagnosis" #+ [UI.string $ "Diagnóstico: " ++ diagnosis]
        ] # set style [("background-color", "#f0f9ff"), ("padding", "15px"), ("margin", "10px 0"), ("border-radius", "8px"), ("box-shadow", "0 2px 4px rgba(0,0,0,0.1)"), ("border-left", "4px solid #2980b9")]

-- Mostrar información de una consulta en la interfaz gráfica
displayConsultation :: Consultation -> UI Element
displayConsultation (date, doctor, diagnosis, treatment, notes) = do
    UI.div #. "consultation-card" #+ 
        [ UI.p #. "consultation-date" #+ [UI.string $ "Fecha: " ++ date]
        , UI.p #. "consultation-doctor" #+ [UI.string $ "Doctor: " ++ doctor]
        , UI.p #. "consultation-diagnosis" #+ [UI.string $ "Diagnóstico: " ++ diagnosis]
        , UI.p #. "consultation-treatment" #+ [UI.string $ "Tratamiento: " ++ treatment]
        , UI.p #. "consultation-notes" #+ [UI.string $ "Notas: " ++ notes]
        ] # set style [("background-color", "#f9f9f9"), ("padding", "15px"), ("margin", "10px 0"), ("border-radius", "8px"), ("box-shadow", "0 2px 4px rgba(0,0,0,0.1)"), ("border-left", "4px solid #2980b9")]

-- Función main para ejecutar la aplicación
main :: IO ()
main = startGUI defaultConfig setup

module Consultation where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.IO
import Control.Exception (try, IOException)
import Data.List (intercalate)

type Consultation = (String, String, String, String, String) -- (Fecha, Doctor, Diagnóstico, Tratamiento, Observaciones)

csvFile :: FilePath
csvFile = "consultations.csv"

-- Guardar consultas en un archivo CSV
saveConsultations :: [Consultation] -> IO ()
saveConsultations consultations = do
    let csvData = unlines $ map formatConsultationCSV consultations
    putStrLn "Guardando las siguientes consultas en el archivo CSV:"  -- Depuración
    putStrLn csvData  -- Muestra las consultas que se van a guardar
    writeFile csvFile csvData  -- Sobrescribe todo el archivo con las consultas nuevas

-- Leer consultas desde un archivo CSV
loadConsultations :: IO [Consultation]
loadConsultations = do
    content <- readFileSafe csvFile
    let linesContent = lines content
    putStrLn "Cargando las siguientes consultas desde el archivo CSV:"  -- Depuración
    putStrLn content  -- Muestra el contenido del archivo cargado
    return $ map parseConsultationCSV linesContent

-- Leer archivo de manera segura
readFileSafe :: FilePath -> IO String
readFileSafe path = do
    handle <- try (readFile path) :: IO (Either IOException String)
    case handle of
        Left _  -> return ""  -- Si no existe o ocurre un error, devuelve un string vacío
        Right c -> return c

-- Formatear una consulta como línea CSV
formatConsultationCSV :: Consultation -> String
formatConsultationCSV (date, doctor, diagnosis, treatment, notes) =
    intercalate "," [date, doctor, diagnosis, treatment, notes]

-- Parsear una línea CSV a una consulta
parseConsultationCSV :: String -> Consultation
parseConsultationCSV line =
    let parts = splitOnChar ',' line
    in case parts of
        [date, doctor, diagnosis, treatment, notes] -> (date, doctor, diagnosis, treatment, notes)
        _ -> ("", "", "", "", "") -- Manejo de errores en caso de que la línea esté incompleta

-- Dividir una cadena en partes según un delimitador (reemplaza a `splitOn`)
splitOnChar :: Char -> String -> [String]
splitOnChar _ [] = [""]
splitOnChar delimiter str =
    foldr (\c acc -> if c == delimiter then "":acc else (c : head acc) : tail acc) [""] str

-- Lógica de la interfaz
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
        
        -- Guardar la nueva consulta
        liftIO $ do
            existingConsultations <- loadConsultations
            let updatedConsultations = existingConsultations ++ [newConsultation]
            saveConsultations updatedConsultations  -- Guardar en el archivo CSV
            putStrLn $ "Consulta registrada: " ++ show newConsultation
        
        -- Recargar las consultas después de guardar
        updatedConsultations <- liftIO loadConsultations

        -- Mostrar mensaje de éxito
        runFunction $ ffi "alert('Consulta registrada exitosamente')"
        
        -- Volver a mostrar las consultas
        runConsultation window updatedConsultations
    
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
        else UI.div #+ map createConsultationElement consultations

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

-- Función auxiliar para convertir una consulta en un elemento de UI
createConsultationElement :: Consultation -> UI Element
createConsultationElement consultation = do
    UI.p # set text (formatConsultation consultation)

formatConsultation :: Consultation -> String
formatConsultation (date, doctor, diagnosis, treatment, notes) =
    "Fecha: " ++ date ++
    ", Doctor: " ++ doctor ++
    ", Diagnóstico: " ++ diagnosis ++
    ", Tratamiento: " ++ treatment ++
    ", Observaciones: " ++ notes

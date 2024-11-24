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
    -- Títulos y botones con estilo
    menuTitle <- UI.h1 # set text "Hojas de Consulta"
                        # set style [("font-size", "36px"),
                                    ("color", "#2c3e50"),
                                    ("text-align", "center"),
                                    ("font-family", "Arial, sans-serif")]
    menuInfo <- UI.p # set text "Seleccione una opción:"
                     # set style [("font-size", "18px"),
                                 ("color", "#34495e"),
                                 ("text-align", "center"),
                                 ("font-family", "Arial, sans-serif")]

    btnNew <- UI.button # set text "Crear Nueva Consulta"
                        # set style [("background-color", "#00bfae"),
                                    ("color", "white"),
                                    ("padding", "12px 25px"),
                                    ("border-radius", "8px"),
                                    ("border", "none"),
                                    ("font-size", "16px"),
                                    ("cursor", "pointer")]
    btnView <- UI.button # set text "Ver Consultas"
                         # set style [("background-color", "#3498db"),
                                     ("color", "white"),
                                     ("padding", "12px 25px"),
                                     ("border-radius", "8px"),
                                     ("border", "none"),
                                     ("font-size", "16px"),
                                     ("cursor", "pointer")]
    btnBack <- UI.button # set text "Volver"
                         # set style [("background-color", "#e74c3c"),
                                     ("color", "white"),
                                     ("padding", "12px 25px"),
                                     ("border-radius", "8px"),
                                     ("border", "none"),
                                     ("font-size", "16px"),
                                     ("cursor", "pointer")]

    layout <- column
        [ element menuTitle
        , element menuInfo
        , row [element btnNew, element btnView, element btnBack]
        ] # set style [("text-align", "center"),
                      ("margin-top", "30px"),
                      ("padding", "20px"),
                      ("font-family", "Arial, sans-serif")]

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

    -- Entradas de texto con diseño
    title <- UI.h1 # set text "Crear Nueva Consulta"
                   # set style [("font-size", "36px"),
                               ("color", "#2c3e50"),
                               ("text-align", "center"),
                               ("font-family", "Arial, sans-serif")]
    inputDate <- UI.input # set (UI.attr "placeholder") "Fecha (dd/mm/aaaa)"
                          # set style [("padding", "10px"),
                                      ("border", "1px solid #ccc"),
                                      ("border-radius", "4px"),
                                      ("font-size", "16px"),
                                      ("width", "80%")]
    inputDoctor <- UI.input # set (UI.attr "placeholder") "Nombre del Doctor"
                            # set style [("padding", "10px"),
                                        ("border", "1px solid #ccc"),
                                        ("border-radius", "4px"),
                                        ("font-size", "16px"),
                                        ("width", "80%")]
    inputDiagnosis <- UI.input # set (UI.attr "placeholder") "Diagnóstico"
                              # set style [("padding", "10px"),
                                          ("border", "1px solid #ccc"),
                                          ("border-radius", "4px"),
                                          ("font-size", "16px"),
                                          ("width", "80%")]
    inputTreatment <- UI.input # set (UI.attr "placeholder") "Tratamiento"
                              # set style [("padding", "10px"),
                                          ("border", "1px solid #ccc"),
                                          ("border-radius", "4px"),
                                          ("font-size", "16px"),
                                          ("width", "80%")]
    inputNotes <- UI.textarea # set (UI.attr "placeholder") "Observaciones"
                              # set style [("padding", "10px"),
                                          ("border", "1px solid #ccc"),
                                          ("border-radius", "4px"),
                                          ("font-size", "16px"),
                                          ("width", "80%")]
    btnSave <- UI.button # set text "Guardar"
                        # set style [("background-color", "#00bfae"),
                                    ("color", "white"),
                                    ("padding", "12px 25px"),
                                    ("border-radius", "8px"),
                                    ("border", "none"),
                                    ("font-size", "16px"),
                                    ("cursor", "pointer")]
    btnCancel <- UI.button # set text "Cancelar"
                          # set style [("background-color", "#e74c3c"),
                                      ("color", "white"),
                                      ("padding", "12px 25px"),
                                      ("border-radius", "8px"),
                                      ("border", "none"),
                                      ("font-size", "16px"),
                                      ("cursor", "pointer")]

    layout <- column
        [ element title
        , element inputDate
        , element inputDoctor
        , element inputDiagnosis
        , element inputTreatment
        , element inputNotes
        , row [element btnSave, element btnCancel]
        ] # set style [("text-align", "center"),
                      ("padding", "20px"),
                      ("font-family", "Arial, sans-serif")]

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

    -- Título de la página con estilo
    title <- UI.h1 # set text "Consultas Registradas"
                   # set style [("font-size", "36px"),
                               ("color", "#2c3e50"),
                               ("text-align", "center"),
                               ("font-family", "Arial, sans-serif")]

    -- Si no hay consultas, muestra un mensaje
    consultationsLayout <- if null consultations
        then UI.p # set text "No hay consultas registradas."
                  # set style [("font-size", "18px"),
                              ("color", "#34495e"),
                              ("text-align", "center"),
                              ("font-family", "Arial, sans-serif")]
        else UI.div #+ map createConsultationElement consultations

    -- Botón para regresar con estilo
    btnBack <- UI.button # set text "Volver"
                         # set style [("background-color", "#e74c3c"),
                                     ("color", "white"),
                                     ("padding", "12px 25px"),
                                     ("border-radius", "8px"),
                                     ("border", "none"),
                                     ("font-size", "16px"),
                                     ("cursor", "pointer")]

    -- Estructura de la página
    layout <- column
        [ element title
        , element consultationsLayout
        , element btnBack
        ] # set style [("text-align", "center"),
                      ("padding", "20px"),
                      ("font-family", "Arial, sans-serif")]

    -- Agrega la estructura al cuerpo de la ventana
    getBody window #+ [element layout]

    -- Acción del botón "Volver"
    on UI.click btnBack $ \_ -> runConsultation window consultations

-- Función auxiliar para convertir una consulta en un elemento de UI
createConsultationElement :: Consultation -> UI Element
createConsultationElement consultation = do
    UI.p # set text (formatConsultation consultation)
         # set style [("font-size", "18px"),
                      ("color", "#2c3e50"),
                      ("font-family", "Arial, sans-serif"),
                      ("margin-bottom", "15px")]

formatConsultation :: Consultation -> String
formatConsultation (date, doctor, diagnosis, treatment, notes) =
    "Fecha: " ++ date ++
    ", Doctor: " ++ doctor ++
    ", Diagnóstico: " ++ diagnosis ++
    ", Tratamiento: " ++ treatment ++
    ", Observaciones: " ++ notes

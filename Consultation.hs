module Consultation where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Monad (void)
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
    result <- try (readFile csvFile) :: IO (Either IOException String)
    case result of
        Left _ -> return []  -- Si no existe el archivo, retornar una lista vacía
        Right content -> return (map parseConsultationCSV (lines content))

  
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


    layout <- column
        [ element menuTitle
        , element menuInfo
        , row [element btnNew, element btnView, element btnBack]
        ] # set style [("display", "flex"), 
        ("flex-direction", "column"), 
       ("align-items", "center"), 
       ("justify-content", "center"),
       ("height", "calc(100vh - 120px)"),
         ("margin", "0 auto")]
    getBody window # set style [("background-color", "#f4f6f6"),
         ("font-family", "Arial, sans-serif"),
         ("margin", "0"),
         ("padding", "0")]
    getBody window #+ [element header, element layout, element footer]

    -- Acciones de los botones
    on UI.click btnNew $ \_ -> createConsultation window consultations
    on UI.click btnView $ \_ ->viewConsultations window consultations
    on UI.click btnBack $ \_ -> do
        getBody window # set children [] -- Limpia la ventana
        runFunction $ ffi "alert('Volviendo al menú principal')"

createConsultation :: Window -> [Consultation] -> UI ()
createConsultation window consultations = do
    getBody window # set children [] -- Limpia la ventana

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

    layout <- column
        [ element title
        , element inputDate
        , element inputDoctor
        , element inputDiagnosis
        , element inputTreatment
        , element inputNotes
        , row [element btnSave, element btnCancel]
        ] # set style [("display", "flex"), 
        ("flex-direction", "column"), 
       ("align-items", "center"), 
       ("justify-content", "center"),
       ("height", "calc(100vh - 120px)"),
         ("margin", "0 auto")]

    getBody window # set style [("background-color", "#f4f6f6"),
                      ("font-family", "Arial, sans-serif"),
                      ("margin", "0"),
                      ("padding", "0")]
    getBody window #+ [element header, element layout, element footer]

    -- Acciones de los botones
    on UI.click btnSave $ \_ -> do
        getBody window # set children [] -- Limpia la ventana
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
    
    on UI.click btnCancel $ \_ -> do
         getBody window # set children [] -- Limpia la ventana
         runConsultation window consultations


-- Función auxiliar para convertir una consulta en un elemento estilizado de UI
createConsultationElement :: Consultation -> UI Element
createConsultationElement (date, doctor, diagnosis, treatment, notes) = do

    -- Estructura tipo "tarjeta"
    UI.div #+ 
        [ UI.h3 # set text ("Fecha: " ++ date)
                # set style [("font-size", "20px"),
                             ("color", "#3498db"),
                             ("font-weight", "bold"),
                             ("margin-bottom", "5px")]
        , UI.p # set text ("Doctor: " ++ doctor)
               # set style [("margin", "5px 0"),
                            ("color", "#2c3e50")]
        , UI.p # set text ("Diagnóstico: " ++ diagnosis)
               # set style [("margin", "5px 0"),
                            ("color", "#2c3e50")]
        , UI.p # set text ("Tratamiento: " ++ treatment)
               # set style [("margin", "5px 0"),
                            ("color", "#2c3e50")]
        , UI.p # set text ("Observaciones: " ++ notes)
               # set style [("margin", "5px 0"),
                            ("color", "#2c3e50"),
                            ("font-style", "italic")]
        ] # set style [("border", "1px solid #ccc"),
                       ("border-radius", "8px"),
                       ("padding", "15px"),
                       ("margin", "10px 0"),
                       ("background-color", "#f9f9f9"),
                       ("box-shadow", "0 2px 4px rgba(0, 0, 0, 0.1)"),
                       ("width", "80%"),
                       ("margin-left", "auto"),
                       ("margin-right", "auto")]

-- Función principal para mostrar las consultas con diseño actualizado
viewConsultations :: Window -> [Consultation] -> UI ()
viewConsultations window consultations = do
    getBody window # set children [] -- Limpia la ventana
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
    title <- UI.h1 # set text "Consultas Registradas"
                   # set style [("font-size", "36px"),
                               ("color", "#2c3e50"),
                               ("text-align", "center"),
                               ("margin-bottom", "20px"),
                               ("font-family", "Arial, sans-serif")]

    consultationsLayout <- if null consultations
        then UI.p # set text "No hay consultas registradas."
                  # set style [("font-size", "18px"),
                              ("color", "#34495e"),
                              ("text-align", "center"),
                              ("font-family", "Arial, sans-serif")]
        else UI.div #+ map createConsultationElement consultations

    btnBack <- UI.button # set text "Volver"
                         # set style [("background-color", "#e74c3c"),
                                     ("color", "white"),
                                     ("padding", "12px 25px"),
                                     ("border-radius", "8px"),
                                     ("border", "none"),
                                     ("font-size", "16px"),
                                     ("cursor", "pointer"),
                                     ("margin-top", "20px")]
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


    layout <- column
        [ element title
        , element consultationsLayout
        , element btnBack
        ] # set style [("display", "flex"), 
        ("flex-direction", "column"), 
       ("align-items", "center"), 
      
       ("height", "calc(100vh - 120px)"),
         ("margin", "0 auto")]

    getBody window #+ [element header,element layout]

    on UI.click btnBack $ \_ -> do
         getBody window # set children [] -- Limpia la ventana
         runConsultation window consultations
    
    
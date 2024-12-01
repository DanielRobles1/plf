module Reports where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.List (find, intercalate)
import Control.Monad (void)
import Graphics.UI.Threepenny.Core (split)

import System.IO
import Control.Exception (try, IOException)

type Patient = (String, String, String) -- (Medicamento, Caducidad, Cantidad)
csvFile :: FilePath
csvFile = "Medicamentos.csv"


-- Función principal para ejecutar el sistema de gestión de pacientes
runReports :: Window -> UI ()
runReports window = do
    -- Títulos y botones con estilo
    menuTitle <- UI.h1 # set text "Medicamentos"
                        # set style [("font-size", "36px"),
                                    ("color", "#2c3e50"),
                                    ("text-align", "center"),
                                    ("font-family", "Arial, sans-serif")]
    menuInfo <- UI.p # set text "Seleccione una opción:"
                     # set style [("font-size", "18px"),
                                 ("color", "#34495e"),
                                 ("text-align", "center"),
                                 ("font-family", "Arial, sans-serif")]

    btnRegister <- UI.button # set text "Registrar nuevo medicamento"
                             # set style [("background-color", "#00bfae"),
                                         ("color", "white"),
                                         ("padding", "12px 25px"),
                                         ("border-radius", "8px"),
                                         ("border", "none"),
                                         ("font-size", "16px"),
                                         ("cursor", "pointer")]
    btnView <- UI.button # set text "Ver Medicamentos"
                         # set style [("background-color", "#3498db"),
                                     ("color", "white"),
                                     ("padding", "12px 25px"),
                                     ("border-radius", "8px"),
                                     ("border", "none"),
                                     ("font-size", "16px"),
                                     ("cursor", "pointer")]
    btndespa <- UI.button # set text "Despachar"
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
        , row [element btnRegister, element btnView, element btnBack]
        ] # set style [("text-align", "center"),
                      ("margin-top", "30px"),
                      ("padding", "20px"),
                      ("font-family", "Arial, sans-serif")]

    getBody window #+ [element layout]

    -- Acciones de los botones
    on UI.click btnRegister $ \_ -> createPatientForm window
    on UI.click btnView $ \_ -> viewPatients window
    on UI.click btndespa $ \_ -> dispatchMedicationForm  window
    on UI.click btnBack $ \_ -> do
        getBody window # set children [] -- Limpia la ventana
        runFunction $ ffi "alert('Volviendo al menú principal')"

-- Crear formulario de registro de paciente
createPatientForm :: Window -> UI ()
createPatientForm window = do
    getBody window # set children [] -- Limpia la ventana

    title <- UI.h1 # set text "Registrar Nuevo Medicamento"
                   # set style [("font-size", "36px"),
                               ("color", "#2c3e50"),
                               ("text-align", "center"),
                               ("font-family", "Arial, sans-serif")]
    inputName <- UI.input # set (UI.attr "placeholder") "Nombre del medicamento"
                          # set style [("padding", "10px"),
                                      ("border", "1px solid #ccc"),
                                      ("border-radius", "4px"),
                                      ("font-size", "16px"),
                                      ("width", "80%")]
    inputDob <- UI.input # set (UI.attr "placeholder") "Fecha de caducidad (dd/mm/aaaa)"
                         # set style [("padding", "10px"),
                                     ("border", "1px solid #ccc"),
                                     ("border-radius", "4px"),
                                     ("font-size", "16px"),
                                     ("width", "80%")]
    inputDiagnosis <- UI.input # set (UI.attr "placeholder") "Cantidad"
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
        , element inputName
        , element inputDob
        , element inputDiagnosis
        , row [element btnSave, element btnCancel]
        ] # set style [("text-align", "center"),
                      ("padding", "20px"),
                      ("font-family", "Arial, sans-serif")]

    getBody window #+ [element layout]

    -- Acciones de los botones
    on UI.click btnSave $ \_ -> do
        name <- get value inputName
        dob <- get value inputDob
        diagnosis <- get value inputDiagnosis
        let newPatient = (name, dob, diagnosis)

        -- Guardar el nuevo paciente en el archivo CSV
        liftIO $ appendFile csvFile (formatPatientCSV newPatient ++ "\n")
        liftIO $ putStrLn $ "Paciente registrado: " ++ show newPatient

        -- Volver al menú principal
        runReports window

    on UI.click btnCancel $ \_ -> do
        -- Volver al menú principal
        runReports window

-- Ver pacientes registrados
viewPatients :: Window -> UI ()
viewPatients window = do
    getBody window # set children [] -- Limpia la ventana

    title <- UI.h1 # set text "Medicamentos Registrados"
                   # set style [("font-size", "36px"),
                               ("color", "#2c3e50"),
                               ("text-align", "center"),
                               ("margin-bottom", "30px"),
                               ("font-family", "Arial, sans-serif")]

    patientsList <- liftIO loadPatients
    patientsLayout <- if null patientsList
        then UI.p # set text "No hay Medicamentos registrados."
                  # set style [("font-size", "18px"),
                              ("color", "#34495e"),
                              ("text-align", "center"),
                              ("font-family", "Arial, sans-serif")]
        else UI.div #+ map createPatientElement patientsList

    btnBack <- UI.button # set text "Volver"
                         # set style [("background-color", "#e74c3c"),
                                     ("color", "white"),
                                     ("padding", "12px 25px"),
                                     ("border-radius", "8px"),
                                     ("border", "none"),
                                     ("font-size", "16px"),
                                     ("cursor", "pointer"),
                                     ("margin-top", "20px")]

    layout <- column
        [ element title
        , element patientsLayout
        , element btnBack
        ] # set style [("text-align", "center"),
                      ("padding", "20px"),
                      ("font-family", "Arial, sans-serif")]

    getBody window #+ [element layout]

    on UI.click btnBack $ \_ -> runReports window

-- Crear una representación visual de un paciente
createPatientElement :: Patient -> UI Element
createPatientElement (name, dob, diagnosis) = do
    UI.div #+ 
        [ UI.h3 # set text ("Nombre: " ++ name)
                # set style [("font-size", "20px"),
                             ("color", "#3498db"),
                             ("font-weight", "bold"),
                             ("margin-bottom", "5px")]
        , UI.p # set text ("Fecha de caducidad: " ++ dob)
               # set style [("margin", "5px 0"),
                            ("color", "#2c3e50")]
        , UI.p # set text ("Existencia: " ++ diagnosis)
               # set style [("margin", "5px 0"),
                            ("color", "#2c3e50")]
        ] # set style [("border", "1px solid #ccc"),
                       ("border-radius", "8px"),
                       ("padding", "15px"),
                       ("margin", "10px 0"),
                       ("background-color", "#f9f9f9"),
                       ("box-shadow", "0 2px 4px rgba(0, 0, 0, 0.1)")]

-- Función para cargar los pacientes desde el archivo CSV
loadPatients :: IO [Patient]
loadPatients = do
    result <- try (readFile csvFile) :: IO (Either IOException String)
    case result of
        Left _ -> return []  -- Si no existe el archivo, retornar una lista vacía
        Right content -> return (map parsePatient (lines content))

-- Función para analizar la información de un paciente desde el CSV
parsePatient :: String -> Patient
parsePatient line =
    let (name, rest) = break (== ',') line
        rest' = dropWhile (== ',') rest  -- Eliminar la coma inicial
        (dob, diagnosis) = break (== ',') rest'
    in (name, dropWhile (== ',') dob, dropWhile (== ',') diagnosis)


-- Formatear un paciente a una línea CSV
formatPatientCSV :: Patient -> String
formatPatientCSV (name, dob, diagnosis) = name ++ "," ++ dob ++ "," ++ diagnosis

-- Función principal para iniciar la aplicación
main :: IO ()
main = startGUI defaultConfig { jsStatic = Just "static" } runReports



dispatchCsvFile :: FilePath
dispatchCsvFile = "Despachos.csv"

-- Crear formulario para despachar medicamentos
dispatchMedicationForm :: Window -> UI ()
dispatchMedicationForm window = do
    getBody window # set children [] -- Limpia la ventana

    title <- UI.h1 # set text "Despachar Medicamento"
                   # set style [("font-size", "36px"),
                               ("color", "#2c3e50"),
                               ("text-align", "center"),
                               ("font-family", "Arial, sans-serif")]
    inputPatient <- UI.input # set (UI.attr "placeholder") "Nombre del paciente"
                             # set style [("padding", "10px"),
                                         ("border", "1px solid #ccc"),
                                         ("border-radius", "4px"),
                                         ("font-size", "16px"),
                                         ("width", "80%")]

    inputMedication <- UI.input # set (UI.attr "placeholder") "Nombre del medicamento"
                                # set style [("padding", "10px"),
                                            ("border", "1px solid #ccc"),
                                            ("border-radius", "4px"),
                                            ("font-size", "16px"),
                                            ("width", "80%")]

    inputQuantity <- UI.input # set (UI.attr "placeholder") "Cantidad a despachar"
                              # set style [("padding", "10px"),
                                          ("border", "1px solid #ccc"),
                                          ("border-radius", "4px"),
                                          ("font-size", "16px"),
                                          ("width", "80%")]

    btnDispatch <- UI.button # set text "Despachar"
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
        , element inputPatient
        , element inputMedication
        , element inputQuantity
        , row [element btnDispatch, element btnCancel]
        ] # set style [("text-align", "center"),
                      ("padding", "30px"),
                      ("font-family", "Arial, sans-serif")]

    getBody window #+ [element layout]

    -- Acciones de los botones
    on UI.click btnDispatch $ \_ -> do
        patient <- get value inputPatient
        medication <- get value inputMedication
        quantityStr <- get value inputQuantity
        let quantity = read quantityStr :: Int
        liftIO $ putStrLn $ "Paciente: " ++ patient ++ ", Medicamento: " ++ medication ++ ", Cantidad: " ++ quantityStr

 

        -- Realizar el despacho
        result <- liftIO $ dispatchMedication (patient, medication, quantity)
        case result of
            Left errorMsg -> runFunction $ ffi "alert(%1)" errorMsg
            Right _ -> runFunction $ ffi "alert('Despacho realizado con éxito')"

        -- Volver al menú principal
        runReports window

    on UI.click btnCancel $ \_ -> runReports window

-- Función para despachar un medicamento
dispatchMedication :: (String, String, Int) -> IO (Either String ())
dispatchMedication (patient, medication, quantity) = do
    patients <- loadPatients
    case find (\(name, _, _) -> name == medication) patients of
        Nothing -> return $ Left "Medicamento no encontrado."
        Just (name, expiry, stockStr) -> do
            let stock = read stockStr :: Int
            if quantity > stock
                then return $ Left "Cantidad insuficiente en inventario."
                else do
                    -- Actualizar inventario
                    let updatedPatients = map (\p@(n, e, s) ->
                            if n == name
                            then (n, e, show (stock - quantity))
                            else p) patients
                    writeFile csvFile (unlines $ map formatPatientCSV updatedPatients)

                    -- Registrar el despacho en Despachos.csv
                    let dispatchRecord = intercalate "," [patient, name, show quantity]
                    appendFile dispatchCsvFile (dispatchRecord ++ "\n")

                    return $ Right ()

-- Función para cargar los pacientes desde el archivo CSV
loaPatients :: IO [Patient]
loaPatients = do
    result <- try (readFile csvFile) :: IO (Either IOException String)
    case result of
        Left _ -> return []  -- Si no existe el archivo, retornar una lista vacía
        Right content ->
            let linesContent = lines content
            in return $ map parsePatient linesContent


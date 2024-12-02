module MedicationManagement where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.List (find, intercalate)
import Control.Monad (void)
import Graphics.UI.Threepenny.Core (split)

import System.IO
import Control.Exception (try, IOException)

type Patient = (String, String, String) -- (Nombre, FechaNacimiento, Diagnóstico)
csvFile :: FilePath
csvFile = "Des.csv"

-- Función principal para ejecutar el sistema de gestión de pacientes
runMedicationManagement :: Window -> UI ()
runMedicationManagement window = do
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
    menuTitle <- UI.h1 # set text "Despacho de medicamentos"
                        # set style [("font-size", "36px"),
                                    ("color", "#2c3e50"),
                                    ("text-align", "center"),
                                    ("font-family", "Arial, sans-serif")]
    menuInfo <- UI.p # set text "Seleccione una opción:"
                     # set style [("font-size", "18px"),
                                 ("color", "#34495e"),
                                 ("text-align", "center"),
                                 ("font-family", "Arial, sans-serif")]

    btnRegister <- UI.button # set text "Despachar"
                             # set style [("background-color", "#00bfae"),
                                         ("color", "white"),
                                         ("padding", "12px 25px"),
                                         ("border-radius", "8px"),
                                         ("border", "none"),
                                         ("font-size", "16px"),
                                         ("cursor", "pointer")]
    btnView <- UI.button # set text "Ver medicamentos despachados"
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
        , row [element btnRegister, element btnView, element btnBack]
        ] # set style [("display", "flex"), 
        ("flex-direction", "column"), 
        ("align-items", "center"), 
        ("justify-content", "center"),
        ("height", "calc(100vh - 120px)"),
        ("margin", "0 auto")]
-- Aplicar estilo general al cuerpo
    getBody window # set style [("background-color", "#f4f6f6"),
                ("font-family", "Arial, sans-serif"),
                   ("margin", "0"),
                 ("padding", "0")]
      -- Agregar elementos iniciales al cuerpo
    getBody window #+ [element header, element layout, element footer]

    -- Acciones de los botones
    on UI.click btnRegister $ \_ -> createPatientForm window
    on UI.click btnView $ \_ -> viewPatients window
    on UI.click btnBack $ \_ -> do
        getBody window # set children [] -- Limpia la ventana
        runFunction $ ffi "alert('Volviendo al menú principal')"

-- Crear formulario de registro de paciente
createPatientForm :: Window -> UI ()
createPatientForm window = do
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


    title <- UI.h1 # set text "Registrar despacho de medicamento"
                   # set style [("font-size", "36px"),
                               ("color", "#2c3e50"),
                               ("text-align", "center"),
                               ("font-family", "Arial, sans-serif")]
    inputName <- UI.input # set (UI.attr "placeholder") "Nombre del paciente"
                          # set style [("padding", "10px"),
                                      ("border", "1px solid #ccc"),
                                      ("border-radius", "4px"),
                                      ("font-size", "16px"),
                                      ("width", "80%")]
    inputDob <- UI.input # set (UI.attr "placeholder") "Medicamento"
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
        , element inputName
        , element inputDob
        , element inputDiagnosis
        , row [element btnSave, element btnCancel]
        ] # set style [("display", "flex"), 
        ("flex-direction", "column"), 
        ("align-items", "center"), 
        ("justify-content", "center"),
        ("height", "calc(100vh - 120px)"),
        ("margin", "0 auto")]

    getBody window #+ [element header,element layout,element footer]

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
        runMedicationManagement window

    on UI.click btnCancel $ \_ -> do
        getBody window # set children [] -- Limpia la ventana 
        -- Volver al menú principal
        runMedicationManagement window

-- Ver pacientes registrados
viewPatients :: Window -> UI ()
viewPatients window = do
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
    

    title <- UI.h1 # set text "Despachos de medicamentos Registrados"
                   # set style [("font-size", "36px"),
                               ("color", "#2c3e50"),
                               ("text-align", "center"),
                               ("margin-bottom", "20px"),
                               ("font-family", "Arial, sans-serif")]

    patientsList <- liftIO loadPatients
    patientsLayout <- if null patientsList
        then UI.p # set text "No hay pacientes registrados."
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
        , element patientsLayout
        , element btnBack
        ] # set style [("display", "flex"), 
        ("flex-direction", "column"), 
       ("align-items", "center"), 
      
       ("height", "calc(100vh - 120px)"),
         ("margin", "0 auto")]

    getBody window #+ [element header,element layout,element footer]

    on UI.click btnBack $ \_ -> do
      getBody window # set children [] -- Limpia la ventana    
    runMedicationManagement window

-- Crear una representación visual de un paciente
createPatientElement :: Patient -> UI Element
createPatientElement (name, dob, diagnosis) = do
    UI.div #+ 
        [ UI.h3 # set text ("Nombre: " ++ name)
                # set style [("font-size", "20px"),
                             ("color", "#3498db"),
                             ("font-weight", "bold"),
                             ("margin-bottom", "5px")]
        , UI.p # set text ("Medicamento: " ++ dob)
               # set style [("margin", "5px 0"),
                            ("color", "#2c3e50")]
        , UI.p # set text ("Cantidad: " ++ diagnosis)
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
main = startGUI defaultConfig { jsStatic = Just "static" } runMedicationManagement

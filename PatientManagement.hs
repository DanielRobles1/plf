module PatientManagement where

import Data.List (find)
import Data.IORef  -- Necesario para trabajar con IORef
import System.IO.Unsafe (unsafePerformIO)  -- Para crear variables globales

-- Tipo para representar a un paciente
type Patient = (String, String, String) -- (Nombre, FechaNacimiento, Diagnóstico)

-- Lista mutable de pacientes (se actualizará en cada operación)
patients :: IORef [Patient]
patients = unsafePerformIO (newIORef [])

-- Función principal para ejecutar el sistema de gestión de pacientes
runPatientManagement :: IO ()
runPatientManagement = do
    putStrLn "Gestión de Expedientes"
    putStrLn "1. Registrar nuevo paciente"
    putStrLn "2. Ver expediente"
    putStrLn "0. Volver"
    option <- getLine
    case option of
        "1" -> registerPatient
        "2" -> viewPatient
        "0" -> return ()
        _   -> putStrLn "Opción no válida" >> runPatientManagement

-- Función para registrar un nuevo paciente
registerPatient :: IO ()
registerPatient = do
    putStrLn "Ingrese el nombre del paciente:"
    name <- getLine
    putStrLn "Ingrese la fecha de nacimiento (dd/mm/aaaa):"
    dob <- getLine
    putStrLn "Ingrese el diagnóstico inicial:"
    diagnosis <- getLine
    let newPatient = (name, dob, diagnosis)
    -- Actualizar la lista de pacientes
    modifyIORef patients (\p -> newPatient : p)
    putStrLn $ "Paciente registrado: " ++ show newPatient
    runPatientManagement

-- Función para ver el expediente de un paciente
viewPatient :: IO ()
viewPatient = do
    putStrLn "Ingrese el nombre del paciente a buscar:"
    name <- getLine
    patientList <- readIORef patients
    case find (\(n, _, _) -> n == name) patientList of
        Just patient -> putStrLn $ "Expediente: " ++ show patient
        Nothing      -> putStrLn "Paciente no encontrado."
    runPatientManagement

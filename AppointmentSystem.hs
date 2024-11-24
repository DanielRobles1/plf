module AppointmentSystem where

-- Define a type to represent an Appointment
data Appointment = Appointment {
    name :: String,
    date :: String,
    time :: String
} deriving Show

-- A list to hold the appointments (In memory for now)
type AppointmentList = [Appointment]

-- Function to add an appointment
addAppointment :: Appointment -> AppointmentList -> AppointmentList
addAppointment appt appts = appt : appts

-- Function to display all appointments
listAppointments :: AppointmentList -> IO ()
listAppointments [] = putStrLn "No appointments found."
listAppointments (x:xs) = do
    putStrLn ("Appointment: " ++ show x)
    listAppointments xs

-- Main function to run the appointment system
runAppointmentSystem :: IO ()
runAppointmentSystem = do

    -- Simulate appointment management
    let appt1 = Appointment "John Doe" "2024-11-20" "10:00 AM"
    let appt2 = Appointment "Jane Smith" "2024-11-21" "2:00 PM"
    let appts = addAppointment appt1 []  -- Start with an empty list of appointments
    let appts' = addAppointment appt2 appts
    putStrLn "Appointments:"
    listAppointments appts'
    return ()

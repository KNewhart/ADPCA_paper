# library(xts)

# Rolling window sizes to test
# rollingWindowDays <- c(3,5,7,9,11)

# dataLocation <- "C:\\Users\\Kate Newhart\\Dropbox\\Data\\MP_SBMBR_data\\"
# dataLocation <- "C:\\Users\\Kate Newhart\\Dropbox\\Newhart ADPCA Paper\\R\\clean\\data\\"

stateVarsBR <- c("BIO_1\\CURRENT_PHASE",
                 "BIO_2\\CURRENT_PHASE",
                 "BIO_BLOWER_1\\RUNNING",
                 "BIO_BLOWER_2\\RUNNING")
stateVarsMT <- c("MBR_1\\CURRENT_MODE",
                 "MBR_2\\CURRENT_MODE",
                 "MBR\\CURRENT_FLUX_MODE",
                 "MBR_1\\CURRENT_STATE",
                 "MBR_2\\CURRENT_STATE",
                 "MBR_1_AIR_SCOUR_VALVE\\COMMAND_1",
                 "MBR_2_AIR_SCOUR_VALVE\\COMMAND_STAT")
# Process variables from raw data that will be used for the BR and MT. 
varsBR <- c("BIO_1\\CURRENT_PHASE",
            "BIO_1\\DO\\PROCESS_VALUE",
            "BIO_2\\CURRENT_PHASE",
            "BIO_2\\DO\\PROCESS_VALUE",
            "BIO_BLOWER_1\\FLOW\\PROCESS_VALUE",
            "BIO_BLOWER_1\\RUNNING",
            "BIO_BLOWER_2\\FLOW\\PROCESS_VALUE",
            "BIO_BLOWER_2\\RUNNING",
            "SEWAGE\\FLOW\\PROCESS_VALUE",
            "SEWAGE\\LEVEL\\PROCESS_VALUE",
            "AMBIENT_TEMP\\PROCESS_VALUE",
            "BIO_1\\LEVEL\\PROCESS_VALUE",
            "BIO_1\\TEMPERATURE\\PROCESS_VALUE",
            "BIO_2\\LEVEL\\PROCESS_VALUE",
            "BIO_2\\TEMPERATURE\\PROCESS_VALUE",
            "RAS_TROUGH\\PH\\PROCESS_VALUE",
            "RAS_TROUGH\\TSS\\PROCESS_VALUE",
            "RAS_TROUGH\\TEMPERATURE\\PROCESS_VALUE")
varsMT <- c("MBR_1\\CURRENT_MODE",
            "MBR_1\\PERM_FLOW\\PROCESS_VALUE",
            "MBR_1\\PERM_PRESS\\PROCESS_VALUE",
            "MBR_2\\CURRENT_MODE",
            "MBR_2\\PERM_FLOW\\PROCESS_VALUE",
            "MBR_2\\PERM_PRESS\\PROCESS_VALUE",
            "MBR\\AIR_SCOUR_FLOW\\PROCESS_VALUE_TANK_1",
            "MBR\\AIR_SCOUR_FLOW\\PROCESS_VALUE_TANK_2",
            "MBR\\CURRENT_FLUX_MODE",
            "MBR_1\\CURRENT_STATE",
            "MBR_2\\CURRENT_STATE",
            "MBR_1\\TRANS_PRESS\\PROCESS_VALUE",
            "MBR_2\\TRANS_PRESS\\PROCESS_VALUE",
            "RAS_TROUGH\\TEMPERATURE\\PROCESS_VALUE",
            "MBR\\AIR_SCOUR_PRESSURE\\PROCESS_VALUE_1",
            "MBR\\AIR_SCOUR_PRESSURE\\PROCESS_VALUE_2",
            "MBR_1_AIR_SCOUR_VALVE\\COMMAND_1",
            "MBR_2_AIR_SCOUR_VALVE\\COMMAND_STAT",
            "MBR\\AIR_SCOUR_PRESSURE\\PROCESS_VALUE",
            "RAS_TROUGH\\DO\\PROCESS_VALUE",
            "RAS_TROUGH\\PH\\PROCESS_VALUE",
            "RAS_TROUGH\\TSS\\PROCESS_VALUE",
            "PERMEATE_TANK\\CONDUCTIVITY\\PROCESS_VALUE",
            "PERMEATE_TANK\\LEVEL\\PROCESS_VALUE",
            "BIO_2\\TSS\\PROCESS_VALUE",
            "BIO_1\\TSS\\PROCESS_VALUE",
            "MBR_1\\INF_FLOW\\PROCESS_VALUE",
            "MBR_1\\LEVEL\\PROCESS_VALUE",
            "MBR_2\\INF_FLOW\\PROCESS_VALUE",
            "MBR_2\\LEVEL\\PROCESS_VALUE",
            "PERMEATE_TANK\\TURBIDITY\\PROCESS_VALUE")
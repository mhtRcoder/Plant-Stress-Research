library(SPEI)

# Set working directory to the folder containing input files
setwd("C:\\Users\\Kamruzzaman Milon\\Desktop\\Sabista\\Data\\Data\\historical\\SPEI_calculation\\Data")

# Create a new directory path for the output files
output_folder <- "C:\\Users\\Kamruzzaman Milon\\Desktop\\Sabista\\Data\\Data\\historical\\SPEI_calculation\\Result\\"

# Create the output folder if it doesn't exist
if (!file.exists(output_folder)) {
  dir.create(output_folder)
}

# List files matching the pattern "ID"
fl <- list.files(pattern = "ID")

# Iterate over each file
for (i in 1:length(fl)) {
  # Read the current file
  df <- read.csv(fl[i])
  
  # Calculate Penman method
  pen <- penman(df$tmin, df$tmax, df$wspd, Rs = df$rsds, RH = df$rhum,
                lat = df$Lat[1], z = df$Elevation[1], na.rm = TRUE)
  
  # Calculate the water balance
  bal <- df$prcp - pen
  
  # Calculate SPEI
  sp1 <- spei(bal, 3, na.rm = TRUE)
  
  # Generate output file path
  output_file <- paste(output_folder, "SPEI3-", fl[i], sep = "")
  
  # Write SPEI results to CSV file in the output folder
  write.csv(sp1$fitted, output_file)
}

###########################
library(SPEI)

# Set working directory to the folder containing input files
setwd("C:\\Users\\Kamruzzaman Milon\\Desktop\\Sabista\\Data\\Data\\historical\\SPEI_calculation\\Data")

# Create a new directory path for the output files
output_folder <- "C:\\Users\\Kamruzzaman Milon\\Desktop\\Sabista\\Data\\Data\\historical\\SPEI_calculation\\Result\\"

# Create the output folder if it doesn't exist
if (!file.exists(output_folder)) {
  dir.create(output_folder)
}

# List files matching the pattern "ID"
fl <- list.files(pattern = "ID")

# Iterate over each file
for (i in 1:length(fl)) {
  # Read the current file
  df <- read.csv(fl[i])
  
  # Calculate Penman method
  pen <- penman(df$tmin, df$tmax, df$wspd, Rs = df$rsds, RH = df$rhum,
                lat = df$Lat[1], z = df$Elevation[1], na.rm = TRUE)
  
  # Calculate the water balance
  bal <- df$prcp - pen
  
  # Calculate SPEI
  sp1 <- spei(bal, 6, na.rm = TRUE)
  
  # Generate output file path
  output_file <- paste(output_folder, "SPEI6-", fl[i], sep = "")
  
  # Write SPEI results to CSV file in the output folder
  write.csv(sp1$fitted, output_file)
}



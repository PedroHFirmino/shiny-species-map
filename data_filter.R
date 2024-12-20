
#Filter for Poland
install.packages("data.table")
library(data.table)

#Important - You must have change the directory
input_file <- "data/occurence.csv"
output_file <- "data/poland_data.csv"

#Read
cat("Reading the file...\n")
data <- fread(input_file, colClasses = "character")

#Verify the first few lines
cat("First few lines of the file:\n")
print(head(data))

#Filter by 'country' column exists
if (!"country" %in% colnames(data)) {
  cat("The 'country' column was not found. The current columns are:", colnames(data), "\n")
  stop("Process interrupted..")
}

#Filter the data for 'Poland'
cat("Filtering data for 'Poland'\n")
poland_data <- data[country == "Poland"]

#Quantity of records were found
cat("Total records for 'Poland':", nrow(poland_data), "\n")

#Check if there is data to save
if (nrow(poland_data) > 0) {
  cat("Saving the filtered data...\n")
  fwrite(poland_data, output_file)
} else {
  cat("No data for 'Poland' found.\n")
}

cat("Processing completed saved in:", output_file, "\n")
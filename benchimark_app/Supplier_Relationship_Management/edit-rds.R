# Assuming you have a note.rds file containing a data frame
loaded_data <- readRDS("note.rds")


#Rename the variable names
library(dplyr)
colnames(loaded_data)
lookup <- c("Company" = "Description", "Cost" = "Names", "Quality" = "Request", "Delivery" = "Completed", "Technology" = "Comments")
loaded_data = rename(loaded_data, all_of(lookup))

#Delete all rows by slice() function
note = note %>% slice(0)

loaded_data = loaded_data %>% slice(0)

getwd()

# Save the modified data back to the RDS file
saveRDS(note, "note.rds")

saveRDS(loaded_data, "note.rds")


#Make a data frame from scratch.
Date = c()
Company = c()
Cost = c()
Quality = c()
Delivery = c()
Technology = c()
Communication = c()

df = data.frame(Date, Company,Cost ,Quality ,Delivery ,Technology)
saveRDS(df, "note.rds")

library(tidyverse)
library(readxl)
library(writexl)
library(dplyr)


# writexl -----------------------------------------------------------------

file_path <- file.path("data", "Sensory Profile.xlsx")

product_info <- read_excel(path  = file_path,
                           sheet = "Product Info",
                           range = "A1:D12",
                           col_names = TRUE)

  # Selecting Products with High Protein
high_prot <- product_info %>% 
  filter(Protein %in% "High") %>% 
  pull(Product)

  # Filter Data to only keep Products with High Protein
high_prot_data <- read_xlsx(path = file_path,
                            sheet = "Data") %>% 
  filter(Product %in% high_prot)

  # Exporting Table to Excel
write_xlsx(data, 
           path = file.path("output", "High Protein Products only.xlsx"),
           col_names = TRUE)


# openxlsx ----------------------------------------------------------------

library(openxlsx)

  # Create workbook object
wb <- openxlsx::createWorkbook()

  # Add a new worksheet
addWorksheet(wb, sheetName = "Mean", gridLines = FALSE)


  # Creating the Sensory Profiles with some Product Information
p_info <- read_xlsx(file_path, sheet = "Product Info") %>% 
  dplyr::select(-Type)

sensory <- read_xlsx(file_path, sheet="Data") %>% 
  inner_join(p_info, by="Product") %>% 
  relocate(Protein:Fiber, .after=Product)

senso_mean <- sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Attribute", values_to="Score") %>% 
  dplyr::select(-Judge) %>% 
  pivot_wider(names_from=Attribute, values_from=Score, values_fn=mean)


  # Exporting the Results to Excel
writeDataTable(wb,
               sheet = "Mean",
               x = senso_mean, 
               startCol = 1,
               startRow = 1,
               colNames = TRUE, rowNames = TRUE, 
               tableStyle = "TableStyleLight9")


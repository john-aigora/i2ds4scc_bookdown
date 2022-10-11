library(tidyverse)
library(here)
library(readxl)

# Excel -------------------------------------------------------------------

file_path <- file.path("data", "biscuits_sensory_profile.xlsx")

product_info <- read_excel(path=file_path, sheet="Product Info", range="A1:D12", col_names=TRUE)

  ## {writexl}
library(writexl)

    # Selecting Products with High Protein
high_prot <- product_info %>% 
  filter(Protein %in% "High") %>% 
  pull(Product)

    # Filter Data to only keep Products with High Protein
high_prot_data <- read_xlsx(path=file_path, sheet="Data") %>% 
  filter(Product %in% high_prot)

    # Exporting Table to Excel
write_xlsx(high_prot_data, path="output/export.xlsx", col_names=TRUE)

  ## {openxlsx}
library(openxlsx)

    # Create workbook object
wb <- openxlsx::createWorkbook()

    # Add a new worksheet
addWorksheet(wb, sheetName = "Mean", gridLines = FALSE)

    # Compute the mean table
p_info <- read_xlsx(file_path, sheet = "Product Info") %>% 
  dplyr::select(-Type)

sensory <- readxl::read_xlsx(file_path, sheet="Data") %>% 
  inner_join(p_info, by="Product") %>% 
  relocate(Protein:Fiber, .after=Product)

senso_mean <- sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Attribute", values_to="Score") %>% 
  dplyr::select(-Judge) %>% 
  pivot_wider(names_from=Attribute, values_from=Score, values_fn=mean)

    # Exporting the Results to Excel
writeDataTable(wb, sheet="Mean", x=senso_mean, startCol=1, startRow=1, colNames=TRUE, rowNames=FALSE, tableStyle="TableStyleLight9")

    # Visualize table
openXL(wb)

    # Manually control the design
options("openxlsx.borderColour" = "#4F80BD")
options("openxlsx.borderStyle" = "thin")
options("openxlsx.numFmt" = "0.0")
modifyBaseFont(wb,fontName = "Calibri", fontSize = 10)
headSty <- createStyle(fgFill="#DCE6F1", border="TopBottom", halign="center", textDecoration="bold")

addWorksheet(wb, sheetName = "Mean (manual formatting)", gridLines = FALSE)
freezePane(wb, sheet=2, firstRow=TRUE, firstCol=TRUE)
writeData(wb, sheet=2, x=senso_mean, startCol=1, startRow=1, colNames=TRUE, rowNames=FALSE, headerStyle=headSty)

    # Conditional formatting
pos_style <- createStyle(fontColour = "firebrick3", bgFill = "mistyrose1")
neg_style <- createStyle(fontColour = "navy", bgFill = "lightsteelblue")

overall_mean <- senso_mean %>% 
  summarize(across(where(is.numeric), mean))

addWorksheet(wb, sheetName = "Conditional Formatting", gridLines=FALSE)
writeDataTable(wb, sheet=3, x=senso_mean, startCol=1, startRow=1, colNames=TRUE, rowNames=FALSE)

for (v in 1:ncol(overall_mean)){
  conditionalFormatting(wb, sheet=3, cols=v+3, rows=1+1:nrow(senso_mean), rule=paste0(">", overall_mean[1,v]), style=pos_style)
  conditionalFormatting(wb, sheet=3, cols=v+3, rows=1+1:nrow(senso_mean), rule=paste0("<", overall_mean[1,v]), style=neg_style)
}

saveWorkbook(wb, file="output/export2.xlsx")

# PowerPoint --------------------------------------------------------------



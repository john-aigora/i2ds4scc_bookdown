library(tidyverse)
library(readxl)


# Excel -------------------------------------------------------------------

  ## writexl 

library(writexl)
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
  ## openxlsx

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


  # Exporting the Results to Excel (automated formatting)
writeDataTable(wb,
               sheet = "Mean",
               x = senso_mean, 
               startCol = 1,
               startRow = 1,
               colNames = TRUE, rowNames = TRUE, 
               tableStyle = "TableStyleLight9")

openXL(wb)

  # Exporting the Results to Excel (manual formatting)

    # Pre-define options to control the borders 
options("openxlsx.borderColour" = "#4F80BD")
options("openxlsx.borderStyle" = "thin")

    # Automatically set Number formats to 3 values after the decimal
options("openxlsx.numFmt" = "0.0")

    # Change the font to Calibri size 10
modifyBaseFont(wb,fontName = "Calibri", fontSize = 10)

    # Header Style (blue background, top/bottom borders, text centered/bold)
headSty <- createStyle(fgFill = "#DCE6F1",
                       border = "TopBottom",
                       halign = "center",
                       textDecoration = "bold")

    # Add new worksheet
addWorksheet(wb, sheetName = "Mean (manual formatting)", gridLines = FALSE)

    # Freeze Panel
freezePane(wb, sheet=2, firstRow=TRUE, firstCol=TRUE)

writeData(wb,
          sheet =2,
          x = senso_mean, 
          startCol = 1,
          startRow = 1,
          colNames = TRUE, rowNames = FALSE, 
          headerStyle = headSty)

openXL(wb)

  # Conditional Formatting

    # Styles for conditional formatting
pos_style <- createStyle(fontColour = "firebrick3", bgFill = "mistyrose1")
neg_style <- createStyle(fontColour = "navy", bgFill = "lightsteelblue")

    # Compute the overall mean
overall_mean <- senso_mean %>% 
  summarize(across(where(is.numeric), mean))

    # Create the worksheet and write the Data
addWorksheet(wb, sheetName = "Conditional Formatting", gridLines=FALSE)
writeDataTable(wb, sheet = 3, x = senso_mean, 
               startCol = 1, startRow = 1,
               colNames = TRUE, rowNames = FALSE)

for (v in 1:ncol(overall_mean)){
  
  conditionalFormatting(wb,
                        sheet = 3,
                        cols  = v + 3,
                        rows  = 1 + 1:nrow(senso_mean), 
                        rule  = paste0(">", overall_mean[1,v]),
                        style = pos_style)
  
  conditionalFormatting(wb,
                        sheet = 3,
                        cols  = v + 3,
                        rows  = 1 + 1:nrow(senso_mean), 
                        rule  = paste0("<", overall_mean[1,v]),
                        style = neg_style)
  
}

openXL(wb)

# saveWorbook(wb, file="temp/excel export.xlsx")

# PowerPoint --------------------------------------------------------------

library(officer)

  ## Creating template
pptx_obj <- read_pptx()

pptx_obj2 <- read_pptx(file.path("data", "templates", "intergral.pptx"))

pptx_obj %>%
  layout_summary()

pptx_obj2 %>%
  layout_summary()

pptx_obj %>% 
  layout_properties() %>% 
  filter(name == "Title and Content")

master = "Office Theme"
pptx_obj <- pptx_obj %>% 
  add_slide(layout = 'Title and Content', master = master)

my_data <- c("My functions are:", "ph_with", "ph_location_type")
pptx_obj <- pptx_obj %>%
  ph_with(value = "My first title", location = ph_location_type(type = "title")) %>% 
  ph_with(value = my_data, location = ph_location_type(type = 'body'))

my_data <- "My new text positioned using ph_location()"
pptx_obj <- pptx_obj %>%
  add_slide(layout = "Title and Content", master = master) %>% 
  ph_with(value = my_data, location = ph_location(left = 2, top = 2, width = 3, height = 1))

print(pptx_obj, "temp/my powerpoint export.pptx")

  ## Formatting Text
my_prop <- fp_text(color = "red", font.size = 14) # Formatting option
my_text <- ftext("First Line in Red", prop = my_prop) # First line of text, formatted

my_par <- fpar(my_text) # text into a paragraph
blank_line <- fpar("") # other empty paragraph to introduce an empty line

my_par2 <- fpar("Second Line") # second line of text, unformatted
my_list <- block_list(my_par, blank_line, my_par2) # Final block with the two lines of text separated by the empty line

pptx_obj <- pptx_obj %>%
  add_slide(layout = "Title and Content", master = master) %>% 
  ph_with(value = my_list, location = ph_location_type(type = "body") )

print(pptx_obj, target = "temp/my powerpoint export.pptx")

text1 <- fpar("FIRST SENTENCE")
text2 <- fpar("second sentence")
text3 <- fpar("THIRD SENTENCE")
my_data <- block_list(text1, text2, text3)

pptx_obj <- pptx_obj %>%
  add_slide(layout = "Title and Content", master = master) %>% 
  ph_with(value = my_data, level_list = c(1,2,1), location = ph_location_type(type = 'body'))

print(pptx_obj, target = "temp/my powerpoint export.pptx")

  ## Exporting Tables

ft_data <- senso_mean %>%
  dplyr::select(Product, Salty, Sweet, Sour, Bitter) %>% 
  mutate(across(where(is.numeric), round, 2)) 

pptx_obj <- pptx_obj %>%
  add_slide(layout = "Title and Content", master = master) %>%
  ph_with(value = ft_data, location = ph_location_type(type = "body")) %>%
  print(target = "temp/my powerpoint export.pptx")

  ## {flextable}
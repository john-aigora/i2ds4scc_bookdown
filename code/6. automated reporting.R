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

library(officer)
pptx_obj <- read_pptx() 
pptx_obj_custom <- read_pptx(file.path("data", "example.pptx"))

pptx_obj %>%
  layout_summary()

pptx_obj %>% 
  layout_properties() %>% 
  filter(name == "Title and Content")

  ## Adding slides
master <- "Office Theme"
pptx_obj <- pptx_obj %>% 
  add_slide(layout = 'Title and Content', master = master)

  ## Playing with slides
pptx_obj <- pptx_obj %>% 
  add_slide("Two Content", master=master)

pptx_obj <- pptx_obj %>% 
  move_slide(index=2, to=1)

pptx_obj <- pptx_obj %>% 
  remove_slide(index=1)

  ## Exporting content
my_data <- c("My functions are:", "ph_with", "ph_location_type")
pptx_obj <- pptx_obj %>%
  ph_with(value = "My first title", location = ph_location_type(type = "title")) %>% 
  ph_with(value = my_data, location = ph_location_type(type = 'body'))

my_data2 <- "My new text positioned using ph_location()"
pptx_obj <- pptx_obj %>%
  add_slide(layout = "Title and Content", master = master) %>% 
  ph_with(value = my_data2, location = ph_location(left = 2, top = 2, width = 3, height = 1))

print(pptx_obj, "output/my export.pptx")

  ## Formatting Text
my_prop <- fp_text(color = "red", font.size = 14)
my_text <- ftext("First Line in Red", prop = my_prop)
my_par <- fpar(my_text)
blank_line <- fpar("")
my_par2 <- fpar("Second Line")
my_list <- block_list(my_par, blank_line, my_par2)

pptx_obj <- pptx_obj %>%
  add_slide(layout = "Title and Content", master = master) %>% 
  ph_with(value = my_list, location = ph_location_type(type = "body") )

text1 <- fpar("FIRST SENTENCE")
text2 <- fpar("second sentence")
text3 <- fpar("THIRD SENTENCE")
my_data <- block_list(text1, text2, text3)

pptx_obj <- pptx_obj %>%
  add_slide(layout = "Title and Content", master = master) %>% 
  ph_with(value = my_data, level_list = c(1,2,1), location = ph_location_type(type = 'body'))

  ## Exporting Tables
ft_data <- senso_mean %>%
  dplyr::select(Product, Salty, Sweet, Sour, Bitter) %>% 
  mutate(across(where(is.numeric), round, 2)) 

pptx_obj <- read_pptx() %>%
  add_slide(layout = "Title and Content", master = master) %>%
  ph_with(value = ft_data, location = ph_location_type(type = "body"))

library(flextable)
ft_table <- ft_data %>% 
  arrange(Product) %>% 
  flextable()

ft_table <- ft_table %>% 
  fontsize(size = 11) %>%
  font(fontname = "Roboto", part = "header") %>%
  color(color = "white", part = "header") %>%
  bold(part = "header") %>%
  align(align = "center", part = "header") %>%
  bg(bg = "#324C63", part = "header") %>%
  font(fontname = "Calibri", part = "body") %>% 
  bg(i = 1:nrow(ft_data), bg = "#EDEDED") %>% 
  bold(i = nrow(ft_data), j = 1:ncol(ft_data)) %>% 
  italic(i = nrow(ft_data), j = ~Product + Salty + Sweet + Sour + Bitter) %>%
  color(i =  nrow(ft_data), j = ~Sour, color = "red") %>%
  color(i =  nrow(ft_data), j = ~Sweet, color = "orange") %>% 
  autofit()

my_border <- fp_border(color = "black", style = "solid", width = 1)
ft_table <- ft_table %>%
  border_outer(part = "all", border = my_border) %>%
  border_inner(part = "body", border = fp_border(style = "dashed")) %>% 
  width(j = 1, width = 1.2)

pptx_obj <- pptx_obj %>%
  add_slide(layout = "Title and Content", master = master) %>%
  ph_with(value = ft_table, ph_location(left = 2, top = 2, width = 4))

  ## Exporting Figures
chart_to_export <- senso_mean %>%
  dplyr::select(Product, Salty, Sweet, Sour, Bitter) %>% 
  arrange(Product) %>% 
  pivot_longer(Salty:Bitter, names_to = 'Attribute', values_to = 'Value') %>% 
  ggplot(aes(x = Product, y = Value, fill = Attribute)) + 
  geom_col(position = 'dodge')+
  xlab("")+
  theme_bw()

library(rvg)
pptx_obj <- pptx_obj %>%
  add_slide(layout = "Title and Content", master = master) %>%
  ph_with(value = dml(ggobj = chart_to_export), location = ph_location_type(type = 'body'))

library(mschart)
mydata <- senso_mean %>%
  dplyr::select(Product, Salty, Sweet, Sour, Bitter) %>% 
  arrange(Product) %>% 
  pivot_longer(Salty:Bitter, names_to = 'Attribute', values_to = 'Value')

my_barchart <- ms_barchart(data=mydata, x="Product", y="Value", group="Attribute")

pptx_obj <- pptx_obj %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = my_barchart, location = ph_location_type(type = "body"))

print(pptx_obj, target = "output/my export.pptx")

# Word --------------------------------------------------------------------

docx_obj <- read_docx() %>% 
  body_add_par(value = "My Text", style = "Normal") %>%
  body_add_par(value = "Other Text", style = "Normal") %>% 
  body_add_par(value = "Conclusion", style = "Normal")

print(docx_obj, target = "output/my export.docx")

  ## Formatting text
my_format <- fp_text(font.family = 'Calibri', font.size = 14, bold = TRUE, color = 'blue')
my_text <- ftext('Here is another example of text', my_format)
my_par <- fpar(my_text)

docx_obj <- read_docx() %>% 
  body_add_par(value = "Document Title", style = "heading 1") %>% 
  body_add_par(value = "", style = "Normal") %>% 
  body_add_fpar(my_par, style = "Normal")

  ## Adding tables and figures
table_num <- run_autonum(seq_id = "tab", pre_label = "Table ", bkm = "tables")
figure_num <- run_autonum(seq_id = "fig", pre_label = "Figure ", bkm = "figures")

docx_obj <- docx_obj %>% 
  body_add_par(value = "Exporting Tables", style = "heading 2") %>% 
  body_add_par(value = "", style = "Normal") %>% 
  body_add_par(value = "Here is my first table:", style = "Normal") %>% 
  body_add_par(value = "", style = "Normal") %>% 
  body_add_table(value = head(mtcars)[,1:4], style = "table_template") %>% 
  body_add_caption(block_caption("My first table.", style="centered", autonum=table_num)) %>% 
  body_add_par(value = "Exporting Figures", style = "heading 2") %>% 
  body_add_par(value = "", style = "Normal") %>% 
  body_add_par(value = "Here is my first figure:", style = "Normal") %>% 
  body_add_par(value = "", style = "Normal") %>% 
  body_add_gg(value = chart_to_export) %>% 
  body_add_caption(block_caption("My first figure.", style="centered", autonum=figure_num))

  ## Formating and Table of Content
docx_obj <- docx_obj %>% 
  body_add_break() %>% 
  body_add_par(value = "Conclusion", style = "heading 1") %>% 
  body_add_break() %>%
  body_add_par("Table of Contents", style = "heading 1") %>% 
  body_add_toc(level = 2)

print(docx_obj, target = "output/my export.docx")
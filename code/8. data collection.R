library(tidyverse)

# Experimental Design -----------------------------------------------------

  ## Power analysis
library(pwr)
pwr.t.test(n=NULL, sig.level=0.05, type="paired", alternative="two.sided", power=0.8, d=0.3)

  ## Latin Square design
library(SensoMineR)
wdes_5P10J <- WilliamsDesign(5)

wdes_5P20J <- do.call(rbind, replicate(2, wdes_5P10J, simplify=FALSE))
rownames(wdes_5P20J) <- paste("judge", 1:20, sep="")

library(AlgDesign)
nbJ=13
nbP=5
nbR=nbP

wdes_5P10J <- WilliamsDesign(nbP)
tab <- cbind(prod=as.vector(t(wdes_5P10J)), judge=rep(1:nbJ,each=nbR), rank=rep(1:nbR,nbJ))
optdes_5P13J <- optFederov(~prod+judge+rank, data=tab, augment=TRUE, nTrials=nbJ*nbP, rows=1:(nbJ*nbP), nRepeats = 100)
xtabs(optdes_5P13J$design)

nbJ=13
nbP=5
nbR=nbP

optdes_5P13J <- optimaldesign(nbP, nbP, nbR)$design
tab <- cbind(prod=as.vector(t(optdes_5P13J)),judge=rep(1:nbJ,each=nbR),rank=rep(1:nbR,nbJ))
add <- cbind(prod=rep(1:nbP,nbR),judge=rep(nbJ+1,nbP*nbR),rank=rep(1:nbR,each=nbP))
optdes_5P14J <- optFederov(~prod+judge+rank,data=rbind(tab,add), augment=TRUE, nTrials=(nbJ+1)*nbP,
                           rows=1:(nbJ*nbP), nRepeats = 100)

  ## Balanced Incomplete Block Design
incompDesign1 <- optimaldesign(nbPanelist=10, nbProd=5, nbProdByPanelist=3)
incompDesign1$design

library(crossdes)
incompDesign2 <- find.BIB(trt=5, b=10, k=3)
isGYD(incompDesign2)
williams.BIB(incompDesign2)

  ## Sensory Informed Designs
# write the code?

  ## Mixture Design
library(mixexp)
mdes <- SLD(fac=3, lev=3)
DesignPoints(mdes)

mdes2 <- Xvert(nfac=3, uc=c(.30, .40, .50), lc=c(.15, .25, .30), ndm = 1, plot = FALSE) %>% 
  mutate(across(where(is.numeric), round, digits = 3))

MixBiscuits <- optFederov(~ -1+x1+x2+x3+x1:x2+x1:x3+x2:x3+x1:x2:x3, mdes2, nTrials=9)
DesignPoints(MixBiscuits$design, axislabs = c("Butter","Sugar","Flour"), pseudo = TRUE)

Bmixt <- MixBiscuits$design %>%
  as_tibble() %>%
  select(-4) %>%
  mutate(Product = c("A", "B", "C", "D", "E", "F", "G", "H", "I"), .before=x1) %>%
  mutate(scores = c(7.5, 5.4, 5.5, 7.0, 6.0, 8.0, 5.8, 6.8, 7.9)) %>%
  rename("Butter"=x1, "Sugar"=x2, "Flour"=x3, "Liking"=scores)

invisible(
  capture.output(
    res <- MixModel(Bmixt, response="Liking", mixcomps=c("Butter","Sugar","Flour"), model=4)
  )
)

ModelPlot(model = res,
          dimensions = list(x1="Butter", x2="Sugar", x3="Flour"),
          lims = c(0.15, 0.30, 0.25, 0.40, 0.30, 0.50), constraints = TRUE,
          contour = TRUE, cuts = 12, fill = TRUE, pseudo = TRUE,
          axislabs = c("Butter", "Sugar", "Flour"))

  ## Screening Designs
library(FrF2)
FrF2(nruns=16, nfactors=5, randomize=FALSE)
FrF2(nruns=8, nfactors=5, randomize=FALSE)

# Data Importation --------------------------------------------------------

  ## Structured Excel File
library(here)
file_path <- here("data","biscuits_sensory_profile.xlsx") 

library(readxl)
sensory <- read_xlsx(file_path, sheet="Data")

  ## Unstructured Excel File
file_path <- here("data","biscuits_traits.xlsx") 
var_names <- read_xlsx(file_path, sheet="Variables")
var_labels <- read_xlsx(file_path, sheet="Levels") %>% 
  inner_join(dplyr::select(var_names, Code, Name), by=c(Question="Code"))
biscuits_traits_data <- read_xlsx(file_path, sheet="Data", col_names=var_names$Name, skip=1)

  ## Multiple Sheets
path <- file.path("data", "excel_scrap.xlsx")
files <- path %>% 
  excel_sheets() %>% 
  set_names(.) %>% 
  map(~read_xlsx(path, sheet = .))

files %>% 
  enframe(name = "Session", value = "data") %>% 
  unnest(cols = c(data))

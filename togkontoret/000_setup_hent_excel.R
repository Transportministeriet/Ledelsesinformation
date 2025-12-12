



# Load required packages
library(pdftools)
library(tidyverse)
library(tibble)
library(janitor)
library(data.table)
library(readxl)
library(tesseract)

sti_togkontor <- "S:/Togkontoret - Databank/Afleveret data - BDK DSB/Data fra DSB/"


filer <- dir(sti_togkontor) %>% 
  str_subset("^\\d+") %>% 
  map_dfr(~dir(str_c(sti_togkontor,.x)) %>% enframe(name=NULL, value = "file")) %>%
  mutate(
    # Extract month and year from the filename
    month = str_extract(file, "\\d{2}"),
    year = str_extract(file, "\\d{4}")
  ) 


pdf_md_aar <- dir("S:/TRM Databank/012 Togkontoret/Tilsendt DSB/") %>% 
  word(start = -1, sep = "\\/") %>% 
  str_extract("[a-zæøå]+\\s\\d{4}") 
  

datoer <- dmy(paste("1", pdf_md_aar), locale = "da_DK.UTF-8") 

# Find the latest date
pdf_md_aar_max <- pdf_filer[which.max(datoer)]

pdf_aar <- pdf_md_aar_max %>% 
  parse_number()

pdf_md <- pdf_md_aar_max %>% 
  str_sub(end = 3)

pdf_fil <- str_c("S:/TRM Databank/012 Togkontoret/Tilsendt DSB/Punktlighed m.m for ", pdf_md_aar_max, ".pdf" )

md_lvls <-  c("jan", "feb", "mar", "apr", "maj", "jun",
              "jul", "aug", "sep", "okt", "nov", "dec", 
              "aar_til_dato")







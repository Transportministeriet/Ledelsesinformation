
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

library(tidyverse)
library(readxl)
library(sf)
library(TRMvisual)
library(extrafont)
library(statbank)
library(highcharter)
library(knitr)
library(extrafont)
library(ggpp)
library(officer)
library(lubridate)
library(plotly)
library(scales)
library(downloadthis)
library(htmltools)
library(rvg)
library(grid)
 
lang <- getOption("highcharter.lang")
lang$decimalPoint <- ","
lang$thousandsSep <- "."
lang$numericSymbols <- highcharter::JS("null") # optional: remove the SI prefixes
options(highcharter.lang = lang)

options(scipen = 100, digits = 2, OutDec = ",")
Sys.setenv(Lang = "da")

knit_hooks$set(inline = function(x) {
  if(is.numeric(x)){
    return(prettyNum(x, big.mark=".", digits = 2))
  } else{
    return(x)
  }
})

if(month(Sys.Date())==1){
  stjerne <- ""
  stjerne_tekst <- ""
} else{
  stjerne <- "*"
  stjerne_tekst <- "*År til dato"
}

sti_kf <- "S:/TRM Databank/003 Klimafremskrivning"
sti <- "S:/TRM Databank/004 Bilstatistik/Personbil"


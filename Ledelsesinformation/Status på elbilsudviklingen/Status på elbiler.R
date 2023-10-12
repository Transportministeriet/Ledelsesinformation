





library(tidyverse)
library(readxl)
library(sf)
library(TRMvisual)
library(extrafont)
library(ggrepel)
library(statbank)
library(highcharter)

options(pillar.sigfig = 7)
options(digits = 6)
Sys.setenv(Lang = "da")

tbl_dst("BIL54", "da")$BILTYPE  

data_bil <- tbl_dst("BIL54", "da") |> 
  filter(BILTYPE %in% c("4000101002", "4000100001"), OMRÅDE == "000", DRIV %in% c("20205", "20210", "20225", "20232"),
         BRUG %in% c("1000")) |> 
  use_labels() |> 
  collect() |> 
  select(-c("OMRÅDE", "BRUG"))

data_bil_1 <- data_bil |> 
  mutate(AAR = round(as.numeric(str_sub(TID, end=4 )) + (as.numeric(str_sub(TID, start=-2 ))-1)/12, 3),
         DATE = as.Date(str_c(str_sub(TID, end=4 ), "-", str_sub(TID, start=-2 ), "-01")),
         DATE_1 = DATE %m+% months(1),
         UDV_BIL = INDHOLD-lag(INDHOLD),
         .by = c("BILTYPE", "DRIV")) 
  

dat_fig_1 <- tibble("AAR"=round(sort(rep(2022:2026, 12)) + rep((c(1:12)-1)/12, 5), 3)) |> 
  mutate(KF_UDV = 2467*exp(0.1713*(AAR-2022)) ) |> 
  inner_join(data_bil_1 |> 
              filter(BILTYPE=="Personbiler i alt", DRIV=="El") |> 
               mutate(vaetg_gnst = round((lag(UDV_BIL, 2)+lag(UDV_BIL)+UDV_BIL)/3, 0)) |>  
              select(-c("BILTYPE","BRUG","DRIV","TID","INDHOLD")),
            by="AAR"
              ) |> 
  filter(AAR>=2023)


dat_fig_1 |> 
  pivot_longer(c("KF_UDV", "UDV_BIL", "vaetg_gnst"), names_to = "VAR", values_to = "ANTAL_BILER") |> 
  (\(y) hchart(filter(y, VAR!="vaetg_gnst"),
    'line', hcaes(x = DATE, y = ANTAL_BILER, group  = VAR), marker = FALSE
  ) |> 
  hc_colors(c(trm_colors(c("blaa", "gul")), trm_colors("gul"))) |> 
  hc_add_series(filter(y, VAR=="vaetg_gnst"), type = "line", 
                hcaes(x = DATE, y = ANTAL_BILER, group  = VAR), marker = FALSE, dashStyle = "dash") |>
    hc_yAxis(title = list(text = "Antal biler"),
                          labels = list(
                            formatter = JS("function() {
      return Highcharts.numberFormat(this.value, 0, '.', '.');
    }"))) |> 
    hc_xAxis(title = list(text = "Måned")))()



DST_tal <- data_bil_1 |>
  filter(BILTYPE=="Personbiler i alt", DRIV=="El", 
         DATE_1 %in% c(max(DATE_1), max(DATE_1) |> floor_date("years"))) |> 
  mutate(x_axis = str_c(format(DATE_1, "%d. %B %Y"), " (DST)"),
         INDHOLD,
         .keep = "none",
         .after = 1)
  
KF_tal <- tibble(x_axis = c("01. January 2024 (KF23)", "01. January 2025 (KF23)"),
       INDHOLD = c(147238, 187507)) 

bind_rows(DST_tal, KF_tal) |> 
  hchart('column', hcaes(x = x_axis, y = INDHOLD), color = trm_colors("blaa")) |>
  hc_yAxis(title = list(text = "Antal biler (Tusind)"),
           labels = list(
             formatter = JS("function() {
      return this.value >= 1000 ? (this.value / 1000).toFixed(0) : this.value;
    }"))) |> 
  hc_xAxis(title = list(text = ""))

library(readxl)

dat_fig_3 <- read_excel("Data/kf23_dataark_-_transport.xlsx", sheet = "Bestand", skip = 4, .name_repair = ~str_c("y", .x)) |> 
  rename(TYPE=1, DRIV=2) |> 
  filter(!is.na(y2019)) |> 
  mutate(DRIV = coalesce(DRIV,"Hele bestanden")) |> 
  fill(TYPE, .direction = "down") |> 
  pivot_longer(cols = starts_with("y"), names_to = "AAR", values_to = "ANTAL_BILER") |> 
  mutate(AAR=parse_number(AAR),
         UDV_ANTAL_BILER=round(ANTAL_BILER-lag(ANTAL_BILER)), 
         MD_UDV_ANTAL_BILER=round(UDV_ANTAL_BILER/12),
         .by = c("TYPE", "DRIV"))
  
dat_fig_3 |> 
  filter(TYPE=="Personbiler", DRIV=="BEV") |> 
  hchart('line', hcaes(x = AAR , y = UDV_ANTAL_BILER), marker = FALSE
  ) |> 
  hc_colors(c(trm_colors(c("blaa", "gul")), trm_colors("gul"))) |> 
  hc_yAxis(title = list(text = "Antal biler (Tusind)"),
           labels = list(
             formatter = JS("function() {
      return this.value >= 1000 ? (this.value / 1000).toFixed(0) : this.value;
    }"))) |> 
  hc_xAxis(title = list(text = "År"))


data_fig_4 <- data_bil_1 |> 
  filter(year(DATE_1)>2019, month(DATE_1) %in% c(month(floor_date(max(DATE_1), "years")), month(max(DATE_1))),
         BILTYPE=="Personbiler i alt") |> 
  mutate(DRIV_1 = ifelse(DRIV %in% c("Benzin", "Diesel"), "Fossil biler", DRIV) |> 
           factor(levels = c("El", "Pluginhybrid", "Fossil biler")),
         AAR = year(DATE_1)) |> 
  summarise(INDHOLD = sum(INDHOLD),
            .by = c("BILTYPE", "DRIV_1", "AAR", "DATE_1") ) |> 
  mutate(UDV_ANTAL_BIL = INDHOLD-lag(INDHOLD),
         .by = c("DRIV_1", "AAR"))

data_fig_4 |> 
  filter(!is.na(UDV_ANTAL_BIL)) |> 
  hchart('column', hcaes(x = AAR, y = UDV_ANTAL_BIL, group = DRIV_1 )) |> 
  hc_colors(c(trm_colors(c("blaa", "orange", "graa")))) |> 
  hc_yAxis(title = list(text = "Antal biler (Tusind)"),
           labels = list(
             formatter = JS("function() {
      return this.value ? (this.value / 1000).toFixed(0) : this.value;
    }"))) |> 
  hc_xAxis(title = list(text = "År"))
  

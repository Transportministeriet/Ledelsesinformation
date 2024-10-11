
library(tidyverse)
library(knitr)
library(rmarkdown)
library(lubridate)
library(statbank)
library(readxl)

# tribble(~aarmd, ~type, ~koersels_dato,
#         "2024M05", "Personbil", format(Sys.time(), "%Y-%m-%d %X"),
#         "2024M05", "Lastbil", format(Sys.time(), "%Y-%m-%d %X"),
#         "2024M05", "Bus", format(Sys.time(), "%Y-%m-%d %X"),
#         "2024M05", "Ladestander", format(Sys.time(), "%Y-%m-%d %X")) %>%
#   write.csv2(row.names = F, "Opdaterings log.csv")

log <- read.csv2("Opdaterings log.csv") 

log_seneste_peri <- log %>% 
  # filter(type =="Personbiler") %>%
  group_by(type) %>% 
  slice_tail(n=1) %>% 
  # tail(1) %>% 
  select(type, aarmd) %>% 
  deframe() 

dst_pers_peri <- tbl_dst("BIL54", "da")$TID %>% 
  tail(1) %>% 
  mutate(type = "Personbil") %>% 
  pull(id, name = type)


bilstat_peri <- map(c("Lastbil", "Bus"), 
    ~dir(str_c("S:/CKA/Databank/006 Bilstatistik/", .x)) %>% 
      enframe() %>% 
      mutate(dato = coalesce(parse_date_time2(value, orders = "dmY") %>% as.Date(),
                             map_chr(value, ~strsplit(.x, ', ', fixed = TRUE)[[1]][2]) %>% 
                               map_chr(~strsplit(.x, '- ', fixed = TRUE)[[1]][2]) %>% 
                               parse_date_time2(orders = "dmY") %>% as.Date()
      ),
      aarmd = str_c(year(dato), "M", str_pad(month(dato), 
                                             width = 2,
                                             side = "left",
                                             pad = "0")
      ),
      type = .x
      ) %>% 
      filter(str_detect(value, "Bestand")) %>% 
      filter(dato == max(dato)) %>% 
      pull(aarmd, name = type)) %>% unlist()

ladest_peri <- read_excel( "S:/CKA/Databank/002 Ladeinfrastruktur/kommuner_ladeeffekt.xlsx",
                      skip = 2, .name_repair = ~str_replace_all(.x, " |-", "_") %>% 
                        str_to_upper()) %>% 
  bind_rows(read_excel( "S:/CKA/Databank/002 Ladeinfrastruktur/kommuner_ladeeffekt_prew.xlsx",
                        skip = 2, .name_repair = ~str_replace_all(.x, " |-", "_") %>% 
                          str_to_upper())) %>% 
  mutate(dato = as.Date(YEARMONTHSHORT),
         aarmd = str_c(year(dato), "M", str_pad(month(dato), 
                                                width = 2,
                                                side = "left",
                                                pad = "0")),
         type = "Ladestander") %>% 
  filter(dato == max(dato)) %>% 
  distinct(type, aarmd) %>% deframe() 
  
seneste_peri <- c(dst_pers_peri, bilstat_peri, ladest_peri)


output_sti = "S:/CKA/Databank/011 Output Vidensbank"


render_fkt <- function(rapport, senest_peri_nu, output_path){
  
  render(input = str_c(getwd(), "/", rapport, ".rmd"), 
         output_file = str_c(output_path, "/", rapport, ".html"))
  
  
  log <- read.csv2("Opdaterings log.csv") 
  
  log_ny <- log %>% 
    add_row(aarmd = senest_peri_nu, 
            type = rapport,
            koersels_dato = format(Sys.time(), "%Y-%m-%d %X")) %>% 
    arrange(type, aarmd) %>% 
    distinct(aarmd, type, .keep_all = T)
  
  write.csv2(log_ny, row.names = F, "Opdaterings log.csv")
}

rapporter_navn <- log$type %>% unique()

map(rapporter_navn, function(x){
  if(log_seneste_peri[x]!=seneste_peri[x]){
    tryCatch(render_fkt(x, seneste_peri[x], output_sti),
             error = print("fejl"))
  } else{str_c("Der er ikke ny månede for ", x)}})


map(rapporter_navn, function(x){
  tryCatch(render_fkt(x, seneste_peri[x], output_sti))})




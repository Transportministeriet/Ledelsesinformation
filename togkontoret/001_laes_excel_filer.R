

# Load required packages
library(pdftools)
library(dplyr)
library(stringr)
library(tibble)
library(janitor)
library(data.table)

sti_togkontor <- "S:/Togkontoret - Databank/Afleveret data - BDK DSB/Data fra DSB/"

str_subset(dir(sti_togkontor), "^\\d+")

filer <- dir(sti_togkontor) %>% 
  str_subset("^\\d+") %>% 
  map_dfr(~dir(str_c(sti_togkontor,.x)) %>% enframe(name=NULL, value = "file")) %>%
  mutate(
    # Extract month and year from the filename
    month = str_extract(file, "\\d{2}"),
    year = str_extract(file, "\\d{4}")
  ) 


filer_til_out <- filer %>% 
  filter(month==max(month),
         .by = "year") %>% 
  arrange(year) %>% 
  filter(year>2016)

map2(filer_til_out$file[[1]], filer_til_out$year[[1]],
     ~excel_sheets(str_c(sti_togkontor, .y,"/", .x  )))


map2(filer_til_out$file[[8]], filer_til_out$year[[8]],
         ~read_excel(str_c(sti_togkontor, .y,"/", .x), sheet = "Kundepunktlighed") %>% 
       filter(!if_all(.cols = everything(), ~is.na(.x)))) %>% view()


laes_data_17_23 <- function(fil, aar_inp){
  
  tabeller <- map_dfr(fil,
                      ~read_excel(str_c(sti_togkontor, aar_inp,"/", .x), sheet = "Kundepunktlighed") %>% 
                        filter(!if_all(.cols = everything(), ~is.na(.x))) %>% 
                        mutate(tabel = str_extract(Kundepunktlighed, "(?<=Tabel )\\d+")) %>% 
                        filter(Kundepunktlighed!="Bemærkninger" | is.na(Kundepunktlighed)) %>% 
                        fill(tabel)) %>% 
    group_split(tabel) %>% 
    map(~.x %>% filter(row_number()>1) %>% 
          set_names(.[2,]) %>% 
          rename(type = 1) %>% 
          rename_with(~str_replace_all(.x, c("Å"="aa", " "="_"))) %>% 
          select(-last_col()) %>% 
          mutate(aar = aar_inp))
  
  
  kol_navne <- tabeller %>% map(~.x %>% pull(type)) %>% map(~.x[[1]]) %>% unlist() %>% str_replace_all(c("-"="_", " "="_", "__"="_")) %>% 
    str_to_lower()
  
  tabeller %>% 
    set_names(kol_navne) %>% 
    map2_dfr(names(.), ~filter(.x, row_number()>2) %>% 
               pivot_longer(cols = -c("type", "aar"), names_to = "maaned", values_to = "pct") %>% 
               mutate(tog_type=.y,
                      pct = as.numeric(pct) %>% round(1))) 
}

kundepunkt_17_23 <- map_dfr(c(1:7), 
    ~laes_data_17_23(filer_til_out$file[[.x]], filer_til_out$year[[.x]]))

laes_data_24_ <- function(fil, aar_inp){
  
  tabeller <- read_excel(str_c(sti_togkontor, aar_inp,"/", fil), sheet = "Kundepunktlighed") %>% 
    filter(!if_all(.cols = everything(), ~is.na(.x))) %>% 
    mutate(tabel = str_extract(Kundepunktlighed, "(?<=Tabel )\\d+")) %>% 
    filter(row_number() < which(Kundepunktlighed == "Beregningsprincipper generelt:") | is.na(Kundepunktlighed) ) %>% 
    fill(tabel) %>% filter(!is.na(tabel)) %>%  
  group_split(tabel) %>% 
    map(~.x %>% filter(row_number()>1) %>% 
          set_names(.[2,]) %>% 
          rename(type = 1) %>% 
          rename_with(~str_replace_all(.x, c("Å"="aa", " "="_"))) %>% 
          select(-last_col()) %>% 
          mutate(aar = aar_inp))
  
  
  kol_navne <- tabeller %>% map(~.x %>% pull(type)) %>% map(~.x[[1]]) %>% unlist() %>% str_replace_all(c("-"="_", " "="_", "__"="_")) %>% 
    str_to_lower()
  
  tabeller %>% 
    set_names(kol_navne) %>% 
    map2_dfr(names(.), ~filter(.x, row_number()>2) %>% 
               pivot_longer(cols = -c("type", "aar"), names_to = "maaned", values_to = "pct") %>% 
               mutate(tog_type=.y,
                      pct = as.numeric(pct) %>% round(1))) 
}

kundepunkt_24_ <- map_dfr(c(8:length(filer_til_out$file)), 
                            ~laes_data_24_(filer_til_out$file[[.x]], filer_til_out$year[[.x]])) %>% 
  filter(!is.na(pct))



data_til_fig <- kundepunkt_24_ %>% 
  dplyr::filter(type =="Kundepunktlighed (2:59)" | 
                  str_detect(type, "Kontraktkrav")) %>% 
  dplyr::mutate(type = str_remove_all(type, "\\d+|[:punct:]") %>% 
           str_squish()) %>% 
  rbind(kundepunkt_17_23 %>% 
              mutate(type = str_remove_all(type, "\\d+") %>% 
                       str_squish())) %>% 
  mutate(maaned = factor(maaned, levels = c("jan", "feb", "mar", "apr", "maj", "jun",
                                            "jul", "aug", "sep", "okt", "nov", "dec", 
                                            "aar_til_dato")),
         tog_type = str_remove_all(tog_type, "(?<=tog)[:alpha:]+"))

fwrite(data_til_fig, "S:/TRM Databank/012 Togkontoret/Kundepunktlighed.csv", sep = ";")


lvls <- data_til_fig %>% 
  filter(maaned!="aar_til_dato", tog_type=="fjern_og_regionaltog", type=="Kundepunktlighed" ) %>%
  arrange(aar, maaned) %>% 
  distinct(aar, maaned) %>%
  mutate(aar_md = str_c(maaned, " ", aar)) %>%
  pull(aar_md) %>% 
  tail(12)

fig_kunde <- data_til_fig %>%
  mutate(aar_md = str_c(maaned, " ", aar)) %>% 
  filter(maaned!="aar_til_dato", str_detect(tog_type,"fjern_og_regionaltog"),
         aar_md %in% lvls)

highchart() %>% 
  hc_add_series(fig_kunde %>% filter(type=="Kundepunktlighed"), 'line', 
                hcaes(x = aar_md, y =pct), marker = FALSE, color = trm_colors("blaa"), 
                name = "Kundepunktlighed") %>% 
  hc_add_series(fig_kunde %>% filter(type=="Kontraktkrav"), 'line', 
                hcaes(x = aar_md, y =pct), marker = FALSE, color = "black", dashStyle = "Dash",
                name = "Kontraktkrav") %>% 
  hc_xAxis(categories = lvls,
           title = list(text = NULL)) %>% 
  hc_yAxis(title = list(text = "Andel (pct.)")) %>% 
  trm_hc_format("<b>Figur 1</b> Seneste tolv måneders kundepunktlighed",
                note = str_glue("Kilde: DSB"))
  


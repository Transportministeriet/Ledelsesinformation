

source("togkontoret/000_setup_hent_excel.R")


filer_til_out <- filer %>% 
  filter(month==max(month),
         .by = "year") %>% 
  mutate(sheet_name = ifelse(year<2017, "Kunderettidighed", "Kundepunktlighed") ) %>% 
  arrange(year)

map2(filer_til_out$file[[1]], filer_til_out$year[[1]],
     ~excel_sheets(str_c(sti_togkontor, .y,"/", .x  )))

#Mellem 2015 og 2016 hedder det Kunderettidighed.

laes_data_17_23 <- function(fil, aar_inp, sheet){
  
  tabeller <- map_dfr(fil,
                      ~read_excel(str_c(sti_togkontor, aar_inp,"/", .x), sheet = sheet) %>% 
                        filter(!if_all(.cols = everything(), ~is.na(.x))) %>% 
                        mutate(tabel = str_extract(get(sheet), "(?<=Tabel )\\d+")) %>% 
                        filter(get(sheet)!="Bemærkninger" | is.na(get(sheet))) %>% 
                        fill(tabel)) %>% 
    group_split(tabel) %>% 
    map(~.x %>% filter(row_number()>1) %>% 
          set_names(.[2,]) %>% 
          rename(type = 1) %>% 
          rename_with(~str_replace_all(.x, c("Å"="aa", " "="_"))) %>% 
          select(-last_col()) %>% 
          mutate(aar = aar_inp))
  
  
  kol_navne <- tabeller %>% map(~.x %>% pull(type)) %>% map(~.x[[1]]) %>% unlist() %>% 
    str_replace_all(c("-"="_", " "="_", "__"="_")) %>% 
    str_to_lower()
  
  tabeller %>% 
    set_names(kol_navne) %>% 
    map2_dfr(names(.), ~filter(.x, row_number()>2) %>% 
               pivot_longer(cols = -c("type", "aar"), names_to = "maaned", values_to = "pct") %>% 
               mutate(tog_type=.y,
                      pct = as.numeric(pct) %>% round(1))) 
}

kundepunkt_15_23 <- map_dfr(c(1:9), 
    ~laes_data_17_23(filer_til_out$file[[.x]], filer_til_out$year[[.x]], filer_til_out$sheet_name[[.x]])) %>% 
  mutate(type = ifelse(type=="Kunderettidighed","Kundepunktlighed", type ))

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

kundepunkt_24_ <- map_dfr(c(10:length(filer_til_out$file)), 
                            ~laes_data_24_(filer_til_out$file[[.x]], filer_til_out$year[[.x]])) %>% 
  filter(!is.na(pct))



data_til_fig <- kundepunkt_24_ %>% 
  dplyr::filter(type =="Kundepunktlighed (2:59)" | 
                  str_detect(type, "Kontraktkrav")) %>% 
  dplyr::mutate(type = str_remove_all(type, "\\d+|[:punct:]") %>% 
           str_squish()) %>% 
  rbind(kundepunkt_15_23 %>% 
              mutate(type = str_remove_all(type, "\\d+") %>% 
                       str_squish())) %>% 
  mutate(maaned = factor(maaned, levels = md_lvls),
         aar = as.numeric(aar),
         tog_type = str_remove_all(tog_type, "(?<=tog)[:alpha:]+"))


# Udtræk side 8
pdf_convert(pdf_fil, pages = 1, 
            filenames = "side1.png", dpi = 300)

ocr_engine <- tesseract("dan")
tekst <- ocr("side1.png", engine = ocr_engine)


# Del op i linjer
linjer <- str_split(tekst , "\n")[[1]] %>% 
  str_replace_all(c(" %"="", ","=".")) %>% 
  str_subset("KUNDEPUNKTLIGHED") %>% 
  word(sep = " CVR")



# Saml til én tabel
materiel_df <- read_table2(
  paste(linjer, collapse = "\n"),
  col_names = c("tog_type", "type_1", "Kundepunktlighed", "ÅTM", "Kontraktkrav")
) %>% 
  mutate(tog_type = ifelse(tog_type=="F&R", "fjern_og_regionaltog", "s_tog"),
         aar = pdf_aar,
         maaned = pdf_md) %>% 
  select(-type_1, -ÅTM) %>% 
  pivot_longer(cols = c("Kundepunktlighed", "Kontraktkrav"),
               names_to = "type", values_to = "pct") %>% 
  filter(type!="Kontraktkrav")
  
fwrite(data_til_fig %>% 
         bind_rows(materiel_df) %>% 
         mutate(maaned = factor(maaned, levels = md_lvls)) %>% 
         arrange(tog_type, type, aar, maaned), 
       "S:/TRM Databank/012 Togkontoret/Kundepunktlighed.csv", sep = ";")








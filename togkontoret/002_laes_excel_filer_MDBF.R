

source("togkontoret/000_setup_hent_excel.R")


filer_til_out <- filer %>% 
  filter(month==max(month),
         .by = "year") %>% 
  mutate(sheet_name = "Km mellem hændelser") %>% 
  arrange(year)

#Mellem 2015 og 2016 hedder det Kunderettidighed.

laes_data_15_22 <- function(fil, aar_inp, sheet){
 
  tabeller <- map_dfr(fil,
                      ~read_excel(str_c(sti_togkontor, aar_inp,"/", fil), sheet = sheet) %>% 
                        rename_with(.cols = 2:14,
                                    ~str_c("kol", str_extract_all( .x, "\\d+", simplify = T))) %>% 
                        rename(togsaetkm = 1) %>% 
                        filter(!if_all(.cols = everything(), ~is.na(.x))) %>% 
                        mutate(tabel = str_extract(togsaetkm, "(?<=Tabel )\\d+")) %>% 
                        filter(togsaetkm!="Bemærkninger" | is.na(togsaetkm)) %>% 
                        fill(tabel)) %>% 
    group_split(tabel) %>% 
    map(~.x %>% filter(row_number()>1) %>% 
          set_names(.[2,]) %>% 
          rename(type = 1) %>% 
          rename_with(~str_replace_all(.x, c("Å"="aa", " "="_"))) %>% 
          select(-last_col()) %>% 
          mutate(aar = aar_inp))
  
  
  kol_navne <- tabeller %>% map(~.x %>% pull(type)) %>% 
    map(~.x[[2]]) %>% unlist() %>% 
    word(start =2, sep = "\n") %>% 
    str_replace_all(c("-|\\,|&| |\\."="_", "__"="_")) %>% 
    str_to_lower() %>% 
    str_c("t_", .)
  
  tabeller %>% 
    set_names(kol_navne) %>% 
    map2_dfr(names(.), ~filter(.x, row_number()>2) %>% 
               pivot_longer(cols = -c("type", "aar"), 
                            names_to = "maaned", values_to = "km") %>% 
               mutate(tog_type=.y,
                      km = as.numeric(km) %>% round(0)))
}


kundepunkt_15_22 <- map_dfr(c(1:9), 
                            ~laes_data_15_22(filer_til_out$file[[.x]], filer_til_out$year[[.x]], filer_til_out$sheet_name[[.x]])) 

laes_data_24_ <- function(fil, aar_inp){
  
  tabeller <- read_excel(str_c(sti_togkontor, aar_inp,"/", fil), sheet = "Km mellem hændelser") %>% 
    filter(!if_all(.cols = everything(), ~is.na(.x))) %>% 
    rename(type = 1) %>% 
    mutate(tabel = str_extract(type, "(?<=Tabel )\\d+")) %>% 
    filter(row_number() < which(type == "Bemærkninger") | 
             is.na(type) ) %>% 
    fill(tabel) %>% filter(!is.na(tabel)) %>% 
          set_names(.[3,]) %>% 
    filter(row_number()>3) %>% 
          rename(type = 1) %>% 
          rename_with(~str_replace_all(.x, c("Å"="aa", " "="_"))) %>% 
          select(-last_col()) %>% 
          mutate(aar = aar_inp)
  
  
  
  tabeller %>%  
    pivot_longer(cols = -c("type", "aar"), 
                 names_to = "maaned", values_to = "km") %>% 
    mutate(km = as.numeric(km) %>% round(0))
}

kundepunkt_24_ <- map_dfr(c(10:length(filer_til_out$file)), 
                          ~laes_data_24_(filer_til_out$file[[.x]], filer_til_out$year[[.x]])) %>% 
  filter(!is.na(km))


data_til_fig <- kundepunkt_24_ %>% 
  bind_rows(kundepunkt_15_22) %>% 
  mutate(maaned = factor(maaned, levels = md_lvls),
         aar = as.numeric(aar)) %>% 
  arrange(aar, maaned)


pdf_text <- pdf_text(pdf_fil)

# Udtræk side 8
side8 <- pdf_text[8] %>% 
  str_replace_all("-\n", "-")

# Del op i linjer
linjer <- str_split(side8, "\n")[[1]]

# Behold kun linjer med materieltal
tabel_linjer <- linjer[which(str_detect(linjer, "^[A-Z]|^DD|^IC1"))]

# Saml til én tabel
materiel_df <- read_table2(
  paste(linjer, collapse = "\n"),
  col_names = c("type", "Seneste_12_mdr", "km", "Maal", "Kørte_km_i_mio")) %>% 
  filter(between(row_number(), 5, 12) & !is.na(Seneste_12_mdr)) %>%
  mutate(type = case_when(type=="DD-"~"Dobbeltdækkere",
                          type=="IC1-"~"IC1-vogne", 
                          type=="S-tog"~"SA/SE", 
                          T~type),
         aar = pdf_aar,
         maaned = pdf_md,
         km = str_remove_all(km, "\\.") %>%  as.numeric()) %>% 
  select(-Seneste_12_mdr,-Maal, -Kørte_km_i_mio )
  

fwrite(data_til_fig %>% 
         bind_rows(materiel_df) %>% 
         mutate(maaned = factor(maaned, levels = md_lvls)) %>% 
         arrange(type, aar, maaned), "S:/TRM Databank/012 Togkontoret/MDBF.csv", 
       sep = ";")






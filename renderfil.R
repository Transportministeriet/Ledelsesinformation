
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
  mutate(dato = as.Date(YEARMONTHSHORT),
         aarmd = str_c(year(dato), "M", str_pad(month(dato), 
                                                width = 2,
                                                side = "left",
                                                pad = "0")),
         type = "Ladestander") %>% 
  filter(dato == max(dato)) %>% 
  distinct(type, aarmd) %>% deframe() 
  
seneste_peri <- c(dst_pers_peri, bilstat_peri, ladest_peri)


Destination = "S:/CKA/Databank/011 Output Vidensbank"

render_fkt <- function(rapport, senest_peri_nu, output_path) {
  # Construct the full path to the R Markdown file
  rmd_file <- str_c(getwd(), "/", rapport, ".rmd")
  
  # Ensure the output directory exists
  dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  
  # Render the report and specify the output destination
  render(input = rmd_file, output_file = str_c(output_path, "/", rapport, ".html"))
  
  # Read the existing log file
  log <- read.csv2("Opdaterings log.csv") 
  
  # Add a new row to the log with the report generation details
  log_ny <- log %>% 
    add_row(aarmd = senest_peri_nu, 
            type = rapport,
            koersels_dato = format(Sys.time(), "%Y-%m-%d %X")) %>% 
    arrange(type, aarmd) %>% 
    distinct(aarmd, type, .keep_all = TRUE)
  
  # Write the updated log back to the CSV file
  write.csv2(log_ny, row.names = FALSE, "Opdaterings log.csv")
}


rapporter_navn <- log$type %>% unique()

map(rapporter_navn, function(x) {
  tryCatch(
    {
      # Attempt to render the report
      render_fkt(x, seneste_peri[x], Destination)},
    error = function(e) {
      # Print the error message, but continue
      message("Error encountered while processing report: ", x)
      message("Error message: ", e$message)
    }
  )
})


# map(rapporter_navn, function(x) {
#   if (log_seneste_peri[x] != seneste_peri[x]) {
#     tryCatch(
#       {
#         # Attempt to render the report
#         render_fkt(x, seneste_peri[x])
#       },
#       error = function(e) {
#         # Print the error message, but continue
#         message("Error encountered while processing report: ", x)
#         message("Error message: ", e$message)
#       }
#     )
#   } else {
#     # If there is no new month for the report, return a message
#     str_c("Der er ikke ny månede for ", x)
#   }
# })
# 
# 

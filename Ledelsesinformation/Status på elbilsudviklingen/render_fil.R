


rmarkdown::render("Status på elbilsudvikling.rmd", 
                  output_file = str_glue("Status på elbilsudvikling ({format(Sys.time() %m-% months(1), '%B %Y')}).html"))
                  
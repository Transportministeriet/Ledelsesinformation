

lang <- getOption("highcharter.lang")
lang$decimalPoint <- ","
lang$thousandsSep <- "."
lang$numericSymbols <- highcharter::JS("null") # optional: remove the SI prefixes
options(highcharter.lang = lang)


options(scipen = 100, digits = 2, OutDec = ",")
Sys.setenv(Lang = "da")
Sys.setlocale("LC_TIME", "da_DK.UTF-8")

knit_hooks$set(inline = function(x) {
  if(is.numeric(x) | is.data.frame(x)){
    return(prettyNum(x, big.mark=".", digits = 2))
  } else{
    return(x)
  }
})

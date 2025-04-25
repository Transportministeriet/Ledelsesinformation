

# Load required packages
library(rvest)
library(stringr)
library(pdftools)

# URL of the page
url <- "https://www.dsb.dk/om-dsb/virksomheden/rapporter-og-regnskab/rapportering-til-Transportministeriet/"

# Read the HTML content
page <- read_html(url)

# Extract all links from the page
pdf_links <- page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  na.omit() %>%
  str_subset("\\.pdf$") %>%
  unique()

# Prepend the domain to relative links
pdf_links <- ifelse(str_starts(pdf_links, "http"), pdf_links, paste0("https://www.dsb.dk", pdf_links))

# Create a folder to store downloaded PDFs
dir.create("DSB_PDFs", showWarnings = FALSE)

# Download PDFs
for (link in pdf_links) {
  filename <- basename(link)
  download.file(link, destfile = file.path("DSB_PDFs", filename), mode = "wb")
  message("Downloaded: ", filename)
}

# Example: Read text from the first downloaded PDF
first_pdf <- list.files("DSB_PDFs", full.names = TRUE)[1]
pdf_text(first_pdf)[1]  # Show text from the first page

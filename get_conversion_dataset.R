url <- "https://data.bis.org/static/bulk/WS_XRU_csv_flat.zip"
file_name <- "WS_XRU_csv_flat.zip"

download.file(url, paste(file_name, sep = ""), mode = "wb")

unzip(file_name)

file.rename("WS_XRU_csv_flat.csv","historical_exchange_rates.csv")
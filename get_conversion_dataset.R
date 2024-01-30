url <- "https://data.bis.org/static/bulk/WS_XRU_csv_flat.zip"
file_name <- "WS_XRU_csv_flat.zip"
file_path <- "/workspaces/esports_WLeffects/"

download.file(url, paste(file_path, file_name, sep = ""), mode = "wb")

unzip(file_name, exdir=file_path)
library(ComtradeDatabaseDownloader)

# Comtrade trade data
get_comtrade(
  "annual", startyear = 2019, endyear = 2021, 
  token = "8K+Ux0yKc/xsg/fbofljZijFtTTVVK64UZDHgh+jXGlp5q27ZPNQFpqq8xJG8s8SEHr2Ui0rR06ZwyEcUbGdPCU2O3H3Vtn+i3qLTAXrA8otLpf9Af/MqsuB7oVoH+e9B9qAOi5HqfzfazeqjzGBB7B1AqkeHSckOisESJ7XESM=", 
  savedir = "D:/Google Drive/2.Study/ECON4091 (Master's Thesis)/ShinyApp/data",
  int64 = FALSE
)

# OxCGRT
url_oxcgrt <- "https://github.com/OxCGRT/covid-policy-tracker/blob/master/data/timeseries/stringency_index.csv?raw=true"
download.file(url = url_oxcgrt, "data/npi.csv")

# Map data
f <- "data/map"
dir.create(f)
url_map <- "http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip"
download.file(url = url_map , destfile = file.path(f, "world.zip"))
unzip(file.path(f, "world.zip"), exdir = f)
unlink(file.path(f, "world.zip"))

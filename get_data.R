#### DEPENDENCIES

## Check if required packages are available

if (!("ComtradeDatabaseDownloader" %in% rownames(installed.packages()))) {
  if (!("devtools" %in% rownames(installed.packages()))) {
    install.packages("devtools")
  }
  library(devtools)
  install_github("andreas-andersen/ComtradeDatabaseDownloader")
}

library(ComtradeDatabaseDownloader)
library(dotenv)

# Load local "api.env" if available store the Comtrade token here
# Input "?get_comtrade" for details on Comtrade tokens

if (file.exists("api.env")) {
  load_dot_env("api.env")
}


#### CREATE DIRECTORIES

f <- "data/map"
dir.create("data", showWarnings = FALSE)
dir.create(f, showWarnings = FALSE)


#### GET FILES

## Get Comtrade trade data

get_comtrade(
  "annual", startyear = 2019, endyear = 2021, 
  token = Sys.getenv("COMTRADE_TOKEN"), 
  savedir = "data",
  int64 = FALSE
)

## Get OxCGRT data

url_oxcgrt <- {
  paste0("https://github.com/OxCGRT/covid-policy-tracker/blob/master/data/",
  "timeseries/stringency_index.csv?raw=true")
}
download.file(url = url_oxcgrt, "data/npi.csv")

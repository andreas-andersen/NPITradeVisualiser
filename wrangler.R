library(data.table)
library(tidyverse)
library(rgdal)

## WRANGLE TRADE DATA ##

# Import trade data downloaded with "ComtradeDatabaseDownloader"
df <- read.csv("data/gravity.csv", encoding = "UTF-8")

# Generate reporter/partner pair
df[,"rp"] <- paste(df$repcode, df$parcode, sep = "-")
rp <- unique(df$rp)

# Generate first-difference change in percentage
diff_percentage <- function(df, rp)
{
  temp <- df[df$rp == rp,]
  imp <- temp[temp$flow == "Import",]
  exp <- temp[temp$flow == "Export",]
  
  if (nrow(imp) >= 2) {
    imp_res <- c(NA, (imp[1,"value"] - imp[2,"value"]) / imp[1,"value"])  
    if (nrow(imp) == 3) {
      imp_res <- c(imp_res, (imp[1,"value"] - imp[3,"value"]) / imp[1,"value"])
    }
    imp[, "diff"] <- imp_res
  } else if (nrow(imp) == 1) {
    imp[, "diff"] <- NA
  }
  
  if (nrow(exp) >= 2) {
    exp_res <- c(NA, (exp[1,"value"] - exp[2,"value"]) / exp[1,"value"])  
    if (nrow(exp) == 3) {
      exp_res <- c(exp_res, (exp[1,"value"] - exp[3,"value"]) / exp[1,"value"])
    }
    exp[, "diff"] <- exp_res
  } else if (nrow(exp) == 1) {
    exp[, "diff"] <- NA
  }
  
  res <- rbind(imp, exp)
  return(res)
}

c_df <-lapply(rp, function(x) diff_percentage(df, x))
c_df <- bind_rows(c_df)

# Clean data
c_df <- c_df[c_df$year %in% c(2020, 2021),]
c_df <- c_df[order(c_df$repcode, c_df$parcode, c_df$flow, c_df$year),]
rownames(c_df) <- NULL

# Create variables for map
c_df_i_2020 <- c_df[
  (c_df$flow == "Import" & c_df$year == 2020),
  c("year", "repcode", "parcode", "diff")
]
c_df_i_2020 <- spread(c_df_i_2020, repcode, diff, sep = "_i_2020_")
c_df_i_2021 <- c_df[
  (c_df$flow == "Import" & c_df$year == 2021),
  c("year", "repcode", "parcode", "diff")
]
c_df_i_2021 <- spread(c_df_i_2021, repcode, diff, sep = "_i_2021_")

c_df_e_2020 <- c_df[
  (c_df$flow == "Export" & c_df$year == 2020),
  c("year", "repcode", "parcode", "diff")
]
c_df_e_2020 <- spread(c_df_e_2020, repcode, diff, sep = "_e_2020_")

c_df_e_2021 <- c_df[
  (c_df$flow == "Import" & c_df$year == 2021),
  c("year", "repcode", "parcode", "diff")
]
c_df_e_2021 <- spread(c_df_e_2021, repcode, diff, sep = "_e_2021_")

c_df_map <- cbind(
  c_df_i_2020,
  c_df_i_2021[,3:length(c_df_i_2021)],
  c_df_e_2020[,3:length(c_df_e_2020)],
  c_df_e_2021[,3:length(c_df_e_2021)]
)


## WRANGLE SI DATA ##

# Import trade data downloaded with "ComtradeDatabaseDownloader"
npi <- read.csv("data/npi.csv", encoding = "UTF-8")
c_npi <- npi[,2:734]
c_npi <- gather(c_npi, date, si, X01Jan2020:X31Dec2021)

# Create date variables
c_npi[,"date"] <- as.Date(c_npi$date, "X%d%b%Y")
c_npi[,"year"] <- year(c_npi$date)
c_npi <- c_npi[order(c_npi$country_code, c_npi$date),]

# Generate World average
w_npi <- c_npi %>% 
  group_by(date, year) %>% 
  summarise(si = mean(si, na.rm = TRUE))
w_npi[,"country_code"] <- "WLD"
w_npi[,"country_name"] <- "World"

# Clean data
c_npi <- rbind(c_npi, w_npi)
rownames(c_npi) <- NULL

# Summarise mean and peak SI value by country and year
sum_npi <- c_npi %>% 
  group_by(country_code, year) %>% 
  summarise(mean_si = mean(si), peak_si = max(si))

sum_npi_2020 <- sum_npi[sum_npi$year == 2020,]
colnames(sum_npi_2020) <- c(
  "country_code", "year", "mean_si_2020", "peak_si_2020") 
sum_npi_2021 <- sum_npi[sum_npi$year == 2021,]
colnames(sum_npi_2021) <- c(
  "country_code", "year", "mean_si_2021", "peak_si_2021") 
sum_npi <- cbind(sum_npi_2020, sum_npi_2021[,c("mean_si_2021", "peak_si_2021")])
sum_npi <- sum_npi[,c(1, 3:length(sum_npi))]


## IMPORT MAP DATA ##

trade_map <- main_map <- readOGR(
  dsn = "data/map",
  layer = "TM_WORLD_BORDERS_SIMPL-0.3",
  verbose = FALSE
)


## JOIN DATA AND EXPORT ##

main_map@data <- left_join(map@data, sum_npi,
                 by = c("ISO3" = "country_code"))
saveRDS(main_map, "app/main_map.rds")

trade_map@data <- left_join(map@data, sum_npi,
                           by = c("ISO3" = "country_code"))
saveRDS(trade_map, "app/trade_map.rds")

saveRDS(c_npi, "app/npi.rds")

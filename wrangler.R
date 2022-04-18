#### DEPENDENCIES

library(data.table)
library(tidyverse)
library(sf)



#### WRANGLING

### TRADE DATA

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
    imp_res <- c(NA, (imp[2,"value"] - imp[1,"value"]) / imp[1,"value"])  
    if (nrow(imp) == 3) {
      imp_res <- c(imp_res, (imp[3,"value"] - imp[1,"value"]) / imp[1,"value"])
    }
    imp[, "diff"] <- imp_res
  } else if (nrow(imp) == 1) {
    imp[, "diff"] <- NA
  }
  
  if (nrow(exp) >= 2) {
    exp_res <- c(NA, (exp[2,"value"] - exp[1,"value"]) / exp[1,"value"])  
    if (nrow(exp) == 3) {
      exp_res <- c(exp_res, (exp[3,"value"] - exp[1,"value"]) / exp[1,"value"])
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
  c("repcode", "parcode", "diff")
]
c_df_i_2020 <- spread(c_df_i_2020, repcode, diff, sep = "_i_2020_")
c_df_i_2021 <- c_df[
  (c_df$flow == "Import" & c_df$year == 2021),
  c("repcode", "parcode", "diff")
]
c_df_i_2021 <- spread(c_df_i_2021, repcode, diff, sep = "_i_2021_")

c_df_e_2020 <- c_df[
  (c_df$flow == "Export" & c_df$year == 2020),
  c("repcode", "parcode", "diff")
]
c_df_e_2020 <- spread(c_df_e_2020, repcode, diff, sep = "_e_2020_")

c_df_e_2021 <- c_df[
  (c_df$flow == "Import" & c_df$year == 2021),
  c("repcode", "parcode", "diff")
]
c_df_e_2021 <- spread(c_df_e_2021, repcode, diff, sep = "_e_2021_")

c_df_map <- cbind(
  c_df_i_2020, 
  c_df_i_2021[,2:length(c_df_i_2021)],
  c_df_e_2020[,2:length(c_df_e_2020)],
  c_df_e_2021[,2:length(c_df_e_2021)]
)
c_df_map["data"] <- c_df_i_2020[, "repcode_i_2020_USA"]


### NPI DATA

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


### MAP DATA

trade_map <- main_map <- st_read("data/map")



#### EXPORTING

### MAP DATA

main_map <- left_join(
  main_map, sum_npi, by = c("ISO3" = "country_code"))
saveRDS(test, "app/main_map.rds")

trade_map <- left_join(
  trade_map, c_df_map, by = c("ISO3" = "parcode"))
saveRDS(trade_map, "app/trade_map.rds")


### DATA FRAMES

saveRDS(c_npi, "app/npi.rds")

saveRDS(c_df, "app/trade.rds")


### COUNTRY NAME DICTIONARY

country_names <- main_map$NAME
names(country_names) <- main_map$ISO3
saveRDS(country_names, "app/country_names.RDS")


### MAP LABELS

## Main map labels

sprintf(
  '<div class="title">%s</div> 
  <table>
    <tr class="top-row">
      <th></th>
      <th>2020</th>
      <th>2021</th>
    </tr>
    <tr>
      <td>Mean Stringency Index</td>
      <td class="center">%.00f</td>
      <td class="center">%.00f</td>
    </tr>
    <tr class="row">
      <td>Peak Stringency Index</td>
      <td class="center">%.00f</td>
      <td class="center">%.00f</td>
    </tr>
  </table>',
  main_map$NAME, 
  main_map$mean_si_2020, main_map$mean_si_2021,
  main_map$peak_si_2020, main_map$peak_si_2021
) %>% lapply(htmltools::HTML) %>% saveRDS("app/labels.rds")

## Trade map labels

trade_cols <- colnames(trade_map)[14:length(trade_map)-2]
trade_labels <- lapply(
  trade_cols, 
  function(x) {
    flow <- if (str_split(x, "_", simplify = TRUE)[2] == "i") {
      "Imports from" } else { "Exports to" }
    year <- str_split(x, "_", simplify = TRUE)[3]
    rep <- str_split(x, "_", simplify = TRUE)[4]
    selected <- c(
      if (flow == "Imports" & year == "2020") "selected" else "",
      if (flow == "Imports" & year == "2021") "selected" else "",
      if (flow == "Exports" & year == "2020") "selected" else "",
      if (flow == "Exports" & year == "2021") "selected" else ""
    )
    sprintf(
      '<div class="trade-title">%s %s (%s)</div>
      %.00f%%',
      flow, trade_map$NAME, year,
      trade_map[[x]] * 100
    ) %>% lapply(htmltools::HTML)
  }
)
names(trade_labels) <- trade_cols
saveRDS(trade_labels, "app/trade_labels.RDS")

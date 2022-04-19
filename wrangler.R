#### DEPENDENCIES

library(data.table)
library(tidyverse)
library(sf)



#### WRANGLING

### TRADE DATA

# Import trade data downloaded with "ComtradeDatabaseDownloader"
df <- read.csv("data/gravity.csv", encoding = "UTF-8")

# Generate reporter and reporter/partner pair vectors
df[,"rp"] <- paste(df$repcode, df$parcode, sep = "-")
rp <- unique(df$rp)
reps <- unique(df$repcode)

## Trade data for map

# Generate first-difference change in percentage
diff_percentage <- function(df, rp) {
  
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

## Trade variables for map

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

## Trade data for graph

# Reorder values in descending order in trade value for 2019 for each flow.
order_flows <- function(df, repcode) {
  
  temp_i <- df[(df$repcode == repcode & df$flow == "Import"),]
  temp_e <- df[(df$repcode == repcode & df$flow == "Export"),]
  
  temp_i_2019 <- temp_i[temp_i$year == 2019,]
  temp_e_2019 <- temp_e[temp_e$year == 2019,]
  
  temp_i_2019 <- temp_i_2019[
    order(temp_i_2019$value, decreasing = TRUE), c("repcode", "parcode")
  ]
  temp_e_2019 <- temp_e_2019[
    order(temp_e_2019$value, decreasing = TRUE), c("repcode", "parcode")
  ]
  
  res <- rbind(
    left_join(temp_i_2019, temp_i, by = c("repcode", "parcode")),
    left_join(temp_e_2019, temp_e, by = c("repcode", "parcode"))
  )
  return(res)
  
}

d_df <- lapply(reps, function(x) order_flows(c_df, x))
d_df <- bind_rows(d_df)

rownames(d_df) <- NULL


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
saveRDS(main_map, "app/main_map.rds")

trade_map <- left_join(
  trade_map, c_df_map, by = c("ISO3" = "parcode"))
saveRDS(trade_map, "app/trade_map.rds")


### DATA FRAMES

saveRDS(c_npi, "app/npi.rds")

saveRDS(d_df, "app/trade.rds")


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

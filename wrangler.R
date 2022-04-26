#### DEPENDENCIES

library(data.table)
library(rmapshaper)
library(tidyverse)
library(sf)



#### IMPORTING DATA

trade <- read.csv("data/gravity.csv", encoding = "UTF-8")
npi <- read.csv("data/npi.csv", encoding = "UTF-8")



#### SUPPORTING FUNCTIONS

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



#### WRANGLING

### TRADE DATA

# Generate reporter and reporter/partner pair vectors
trade[,"rp"] <- paste(trade$repcode, trade$parcode, sep = "-")
rp <- unique(trade$rp)
reps <- unique(trade$repcode)

## Trade data for map

c_trade <-lapply(rp, function(x) diff_percentage(trade, x))
c_trade <- bind_rows(c_trade)

## Trade variables for map

c_trade_i_2020 <- c_trade[
  (c_trade$flow == "Import" & c_trade$year == 2020),
  c("repcode", "parcode", "diff")
] %>% pivot_wider(
  names_from = repcode, values_from = diff, names_prefix = "i_2020_")

c_trade_i_2021 <- c_trade[
  (c_trade$flow == "Import" & c_trade$year == 2021),
  c("repcode", "parcode", "diff")
] %>% pivot_wider(
  names_from = repcode, values_from = diff, names_prefix = "i_2021_")

c_trade_e_2020 <- c_trade[
  (c_trade$flow == "Export" & c_trade$year == 2020),
  c("repcode", "parcode", "diff")
] %>% pivot_wider(
  names_from = repcode, values_from = diff, names_prefix = "e_2020_")

c_trade_e_2021 <- c_trade[
  (c_trade$flow == "Import" & c_trade$year == 2021),
  c("repcode", "parcode", "diff")
] %>% pivot_wider(
  names_from = repcode, values_from = diff, names_prefix = "e_2021_")

c_trade_map <- cbind(
  c_trade_i_2020, 
  c_trade_i_2021[,2:length(c_trade_i_2021)],
  c_trade_e_2020[,2:length(c_trade_e_2020)],
  c_trade_e_2021[,2:length(c_trade_e_2021)]
)
c_trade_map["trade"] <- c_trade_i_2020[, "i_2020_USA"]

## Trade data for graph

d_trade <- lapply(reps, function(x) order_flows(c_trade, x))
d_trade <- bind_rows(d_trade)

rownames(d_trade) <- NULL


### NPI DATA

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
sum_npi["npi"] <- sum_npi[,"mean_si_2021"]


### MAP DATA

map <- st_read("data/map")
map <- na.omit(map)
map[map$iso3 == "COD", "name"] <- "DR Congo"
map <- ms_simplify(map, keep = 0.04, keep_shapes = TRUE)

npi_map <- trade_map <- map



#### EXPORTING

### MAP DATA

npi_map <- inner_join(
  npi_map, sum_npi, by = c("iso3" = "country_code"))
saveRDS(npi_map, "app/npi_map.RDS")

trade_map <- left_join(
  trade_map, c_trade_map, by = c("iso3" = "parcode"))
saveRDS(trade_map, "app/trade_map.RDS")


### DATA FRAMES

saveRDS(c_npi, "app/npi.RDS")

saveRDS(d_trade, "app/trade.RDS")


### COUNTRY NAME DICTIONARY

country_names <- npi_map$name
names(country_names) <- npi_map$iso3
saveRDS(country_names, "app/country_names.RDS")


### MAP LABELS

## Main map labels

sprintf(
  '<div class="label-title">%s</div> 
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
  npi_map$name, 
  npi_map$mean_si_2020, npi_map$mean_si_2021,
  npi_map$peak_si_2020, npi_map$peak_si_2021
) %>% lapply(htmltools::HTML) %>% saveRDS("app/labels.RDS")

## Trade map labels

trade_cols <- colnames(trade_map)[11:length(trade_map)-2]
trade_labels <- lapply(
  trade_cols, 
  function(x) {
    flow <- if (str_split(x, "_", simplify = TRUE)[1] == "i") {
      "Imports from" } else { "Exports to" }
    year <- str_split(x, "_", simplify = TRUE)[2]
    rep <- str_split(x, "_", simplify = TRUE)[3]
    sprintf(
      '<div class="trade-title">%s %s (%s)</div>
      %.00f%%',
      flow, trade_map$name, year,
      trade_map[[x]] * 100
    ) %>% lapply(htmltools::HTML)
  }
)
names(trade_labels) <- trade_cols

# Replace "NA%" with "-"
trade_labels <- lapply(
  trade_labels, function(x) lapply(x, function(y) gsub("NA%", "-", y)))

saveRDS(trade_labels, "app/trade_labels.RDS")

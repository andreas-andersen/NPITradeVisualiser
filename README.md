# NPITradeVisualiser

A shiny dashboard for visualising COVID-19 NPI stringency and changes in 
global trade flows. Includes scripts for downloading, wrangling and deploying.

![Screenshot](img/illustration_screenshot.png?raw=true "Screenshot")

### Prerequisites:

- Downloading data from the Comtrade database requires the 
[`ComtradeDatabaseDownloader`](https://github.com/andreas-andersen/ComtradeDatabaseDownloader)
package and a premium site license subscription.

## Quick setup guide:

All wrangled data used in the dashboard is included as `.RDS` files in the 
`app` folder.

If you wish to complete the data wrangling yourself, run the following scripts
in order:

1. `get_data.R` to download data (see script comments for details)
2. `wrangler.R` to wrangle and export data
3. Press the "Run App" button within RStudio or deploy app to server.

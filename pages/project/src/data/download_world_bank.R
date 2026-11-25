# download_world_bank.R
# ------------------------------------------------------------------------------
# Purpose : Download a small set of World Development Indicators (WDI) from the
#           World Bank API and save the UNMODIFIED result locally.
# Lives in: pages/project/src/data/download_world_bank.R
# Writes  : pages/project/data/world_bank_raw.Rds
# Run in  : RStudio Console, once:
#             source(here::here("pages/project/src/data/download_world_bank.R"))
#
# The website never calls the API when it renders. It only reads the saved
# file, so the project stays reproducible if the API changes or goes down.
# ------------------------------------------------------------------------------

library(WDI)
library(here)

raw_data_path <- here("pages", "project", "data", "world_bank_raw.Rds")

# Set to TRUE only if you deliberately want to replace the saved raw snapshot.
force_download <- FALSE

start_year <- 2000
end_year   <- 2024

# name in R = World Bank indicator code
indicators <- c(
  gdp_per_capita    = "NY.GDP.PCAP.PP.KD", # GDP per capita, PPP (constant 2021 intl $)
  market_cap_gdp    = "CM.MKT.LCAP.GD.ZS", # Market cap of listed domestic companies (% of GDP)
  stocks_traded_gdp = "CM.MKT.TRAD.GD.ZS", # Stocks traded, total value (% of GDP)
  listed_companies  = "CM.MKT.LDOM.NO",    # Listed domestic companies, total
  bank_credit_gdp   = "FD.AST.PRVT.GD.ZS", # Domestic credit to private sector by banks (% of GDP)
  population        = "SP.POP.TOTL"        # Population, total
)

if (file.exists(raw_data_path) && !force_download) {

  message(
    "Raw data already saved at:\n  ", raw_data_path,
    "\nNothing downloaded. Set force_download <- TRUE to replace it."
  )

} else {

  # extra = TRUE adds country metadata: iso3c, region, income group, etc.
  world_bank_raw <- WDI(
    country   = "all",
    indicator = indicators,
    start     = start_year,
    end       = end_year,
    extra     = TRUE
  )

  # Record when the snapshot was taken so the page can cite it.
  attr(world_bank_raw, "downloaded_on") <- Sys.Date()

  dir.create(dirname(raw_data_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(world_bank_raw, raw_data_path, compress = "xz")

  message(
    "Saved ", nrow(world_bank_raw), " rows and ", ncol(world_bank_raw),
    " columns to:\n  ", raw_data_path
  )
}

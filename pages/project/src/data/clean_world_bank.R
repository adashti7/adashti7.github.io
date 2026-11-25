# clean_world_bank.R
# ------------------------------------------------------------------------------
# Purpose : Turn the raw World Bank download into the analysis dataset:
#           one row per economy per year, aggregates removed, derived variables
#           added. Missing values stay missing (never converted to zero).
# Lives in: pages/project/src/data/clean_world_bank.R
# Reads   : pages/project/data/world_bank_raw.Rds
# Writes  : pages/project/data/cleaned/world_bank_cleaned.Rds
# Run in  : RStudio Console, after download_world_bank.R:
#             source(here::here("pages/project/src/data/clean_world_bank.R"))
# ------------------------------------------------------------------------------

library(dplyr)
library(here)

raw_data_path     <- here("pages", "project", "data", "world_bank_raw.Rds")
cleaned_data_path <- here("pages", "project", "data", "cleaned", "world_bank_cleaned.Rds")

world_bank_raw <- readRDS(raw_data_path)
downloaded_on  <- attr(world_bank_raw, "downloaded_on")

# ---- 1. Keep individual economies only ---------------------------------------
# The World Bank labels groups such as "World", "High income", and
# "European Union" with region == "Aggregates".

world_bank_economies <- world_bank_raw |>
  as_tibble() |>
  filter(
    !is.na(region), region != "Aggregates",
    !is.na(iso3c), iso3c != ""
  )

# ---- 2. Tidy names and metadata ----------------------------------------------

# Shorter display names for chart labels. Add more here if a label looks long.
short_names <- c(
  "Hong Kong SAR, China" = "Hong Kong",
  "Korea, Rep."          = "South Korea",
  "Iran, Islamic Rep."   = "Iran",
  "Egypt, Arab Rep."     = "Egypt",
  "Russian Federation"   = "Russia",
  "Slovak Republic"      = "Slovakia",
  "Venezuela, RB"        = "Venezuela",
  "West Bank and Gaza"   = "West Bank & Gaza"
)

income_levels <- c(
  "Low income", "Lower middle income", "Upper middle income", "High income"
)

world_bank_cleaned <- world_bank_economies |>
  select(
    country, iso3c, year, region, income,
    gdp_per_capita, market_cap_gdp, stocks_traded_gdp,
    listed_companies, bank_credit_gdp, population
  ) |>
  mutate(
    economy = if_else(
      country %in% names(short_names),
      unname(short_names[country]),
      country
    ),
    # NOTE: this is the World Bank's CURRENT income classification, applied to
    # every year. It is not the classification an economy had in the past.
    # "Not classified" becomes NA.
    income_group = factor(income, levels = income_levels)
  ) |>
  select(-income) |>
  arrange(iso3c, year)

# One row per economy per year, or stop.
stopifnot(anyDuplicated(select(world_bank_cleaned, iso3c, year)) == 0)

# ---- 3. Derived variables ----------------------------------------------------

world_bank_cleaned <- world_bank_cleaned |>
  mutate(
    log_gdp_per_capita = log10(gdp_per_capita),
    # Breadth of the market, adjusted for country size.
    listed_per_million = listed_companies / population * 1e6,
    # Yearly trading as a share of the market's own size. Both inputs are
    # "% of GDP", so GDP cancels out. Undefined when market cap is zero.
    trading_to_market_cap = if_else(
      market_cap_gdp > 0,
      stocks_traded_gdp / market_cap_gdp * 100,
      NA_real_
    )
  )

# ---- 4. The "expected" market size at each income level ----------------------
# A descriptive baseline, fitted separately for every year:
#
#     log10(market_cap_gdp) ~ log10(gdp_per_capita)
#
# Both sides are logged because both variables are strongly right-skewed.
# On the original scale a few very large markets would dominate the fit.
# The result is easy to read as a ratio:
#
#     market_cap_ratio = actual / expected
#
#     2   -> the market is twice as deep as expected for that income level
#     0.5 -> half as deep as expected
#
# The baseline describes the sample for that year. It is not causal, and the
# ratios change if the sample or the model changes.

min_economies_for_fit <- 30

fit_expected_market_cap <- function(market_cap_gdp, gdp_per_capita) {
  usable <- !is.na(market_cap_gdp) & !is.na(gdp_per_capita) &
    market_cap_gdp > 0 & gdp_per_capita > 0

  expected <- rep(NA_real_, length(market_cap_gdp))

  if (sum(usable) < min_economies_for_fit) {
    return(expected)
  }

  log_market_cap <- log10(market_cap_gdp[usable])
  log_gdp        <- log10(gdp_per_capita[usable])
  baseline_fit   <- lm(log_market_cap ~ log_gdp)

  expected[usable] <- 10^unname(fitted(baseline_fit))
  expected
}

world_bank_cleaned <- world_bank_cleaned |>
  group_by(year) |>
  mutate(
    expected_market_cap_gdp = fit_expected_market_cap(market_cap_gdp, gdp_per_capita)
  ) |>
  ungroup() |>
  mutate(
    market_cap_ratio        = market_cap_gdp / expected_market_cap_gdp,
    market_cap_residual_log = log10(market_cap_ratio)
  )

# Carry the download date along for chart captions.
attr(world_bank_cleaned, "downloaded_on") <- downloaded_on

# ---- 5. Save -----------------------------------------------------------------

dir.create(dirname(cleaned_data_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(world_bank_cleaned, cleaned_data_path, compress = "xz")

# ---- 6. Checks to read in the Console ----------------------------------------
# Nothing below changes the saved file. Read this output before trusting charts.

message("\nSaved: ", cleaned_data_path)
message("Economies kept: ", n_distinct(world_bank_cleaned$iso3c),
        " | Years: ", min(world_bank_cleaned$year), "-", max(world_bank_cleaned$year))

message("\nEconomies with data, by year (missing values are NOT zeros):")
coverage_by_year <- world_bank_cleaned |>
  group_by(year) |>
  summarize(
    gdp_per_capita    = sum(!is.na(gdp_per_capita)),
    market_cap_gdp    = sum(!is.na(market_cap_gdp)),
    in_baseline_fit   = sum(!is.na(market_cap_ratio)),
    stocks_traded_gdp = sum(!is.na(stocks_traded_gdp)),
    listed_companies  = sum(!is.na(listed_companies)),
    bank_credit_gdp   = sum(!is.na(bank_credit_gdp))
  )
print(coverage_by_year, n = Inf)

message("\nLast year with market-cap data for economies you named as interesting:")
watchlist <- c("USA", "GBR", "CHE", "SGP", "HKG", "JPN", "KOR",
               "DEU", "FRA", "CHN", "IND", "BRA", "ZAF")
watchlist_coverage <- world_bank_cleaned |>
  filter(iso3c %in% watchlist, !is.na(market_cap_gdp)) |>
  group_by(economy) |>
  summarize(
    first_year      = min(year),
    last_year       = max(year),
    years_with_data = n()
  ) |>
  arrange(last_year)
print(watchlist_coverage, n = Inf)

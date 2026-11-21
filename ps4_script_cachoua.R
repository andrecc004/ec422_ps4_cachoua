# EC 422 - Problem Set 4
# Parts 2-5
# NOTE: "___" marks a number to fill in from your output.

library(tidyverse)
library(fpp3)
library(BVAR)

# =====================================================================
# Part 2 - ARIMA vs. ETS
# =====================================================================

# ---- 2.1a ----
liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

# ---- 2.1b ----
liquor |>
  autoplot(sales) +
  labs(title = "U.S. Beer, Wine, and Liquor Store Sales",
       x = "Month", y = "Millions of dollars")

liquor |>
  gg_season(log(sales)) +
  labs(title = "Seasonal Plot: Log Liquor Store Sales",
       x = "Month", y = "log(sales)")

# Answer 2.1b:
# Sales trend steadily upward, from about $1.4 billion in 1992 to over
# $6 billion by 2019. December is the highest month by far (about 39%
# above an average month); January and February are the lowest (about
# 17% below). Summer months are somewhat above average. The seasonal
# swings grow with the level (the December-January gap went from about
# $940M in 1992 to $2,370M in 2019), so logs make the swings roughly
# constant and the seasonality additive.

# ---- 2.2a ----
liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

# Answer 2.2a:
# ARIMA chosen: ARIMA(2,0,3)(2,1,1)[12] w/ drift. No regular difference
# (d = 0) and one seasonal difference (D = 1): the model uses the change
# from the same month last year.
# ETS chosen: ETS(A,A,A). Additive errors, an additive trend, and
# additive seasonality (additive on the log scale).

# ---- 2.2b ----
liquor_fit |> select(arima) |> gg_tsresiduals()

# dof = p + q + P + Q (counted from the fitted model's coefficients)
arima_dof <- liquor_fit |>
  select(arima) |>
  tidy() |>
  filter(str_detect(term, "^s?(ar|ma)")) |>
  nrow()
arima_dof

liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = arima_dof)

# Answer 2.2b:
# dof = 8. Ljung-Box statistic = 95.2, p-value < 0.001, so the
# residuals do not look like white noise; some structure is left.

# ---- 2.3a ----
liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

liquor_cv_fit <- liquor_cv |>
  model(snaive = SNAIVE(sales),
        ets    = ETS(log(sales)),
        arima  = ARIMA(log(sales)))

saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")
# liquor_cv_fit <- readRDS("results/liquor_cv_fit.rds")

# Answer 2.3a: 216 forecast origins.

# ---- 2.3b ----
liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_acc <- liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))
saveRDS(liquor_acc, "results/liquor_acc.rds")
liquor_acc |> knitr::kable(digits = 2)

# Answer 2.3b:
# First forecast: January 2002. Last forecast: December 2019.
# RMSE / MAE: ARIMA 75.82 / 60.27, ETS 90.75 / 73.26,
#              SNAIVE 161.26 / 136.63
# ARIMA forecasts most accurately. Both metrics agree.

# ---- 2.4 ----
liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

dm_arima_ets    <- forecast::dm.test(liquor_errs$arima, liquor_errs$ets,
                                     h = 1, power = 2)
dm_arima_snaive <- forecast::dm.test(liquor_errs$arima, liquor_errs$snaive,
                                     h = 1, power = 2)
dm_arima_ets
dm_arima_snaive
saveRDS(list(arima_ets = dm_arima_ets, arima_snaive = dm_arima_snaive),
        "results/dm_liquor.rds")

# Answer 2.4:
# ARIMA vs ETS: DM = -4.28, p < 0.001.
# ARIMA vs SNAIVE: DM = -10.99, p < 0.001.
# ARIMA is significantly more accurate than both ETS and the seasonal
# random walk. I would use ARIMA.

# =====================================================================
# Part 3 - Forecast Combination
# =====================================================================

# ---- 3a ----
liquor_combo_fc <- liquor_cv_fit |>
  mutate(combo = (arima + ets) / 2) |>
  forecast(h = 1)

combo_acc <- liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))
saveRDS(combo_acc, "results/combo_acc.rds")
combo_acc |> knitr::kable(digits = 2)

# Answer 3a:
# Combo RMSE = 79.01, MAE = 63.18. It beats ETS (90.75) but not
# ARIMA (75.82).

# ---- 3b ----
err_cor <- cor(liquor_errs$arima, liquor_errs$ets)
err_cor

# Answer 3b:
# The correlation is 0.80, so the two models make similar errors and
# averaging cancels little. The DM test also showed ARIMA is
# significantly better than ETS, so adding ETS mostly pulls the forecast
# toward a worse model. That is why the combo did not beat ARIMA.

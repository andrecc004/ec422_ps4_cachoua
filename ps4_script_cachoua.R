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
# Sales trend steadily upward. December is the highest month by far;
# January and February are the lowest. The seasonal swings grow as the
# level rises, so taking logs makes them roughly constant in size.

# ---- 2.2a ----
liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

# Answer 2.2a:
# ARIMA chosen: ARIMA(___)(___)[12]. d = ___ regular difference(s) and
# D = ___ seasonal difference(s) were taken.
# ETS chosen: ETS(___,___,___). The letters give the error type,
# trend type, and seasonal type (A = additive, N = none, Ad = damped).

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
# Ljung-Box p-value = ___. If p > 0.05, the residuals look like white
# noise; if p < 0.05, some structure is left in the residuals.

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
# Most accurate model: ___. The two metrics ___ (agree / disagree).

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
# ARIMA vs ETS: DM = ___, p = ___.
# ARIMA vs SNAIVE: DM = ___, p = ___.
# A p-value below 0.05 means the two models' accuracy is significantly
# different. I would use ___, since it has the lowest RMSE and
# [is / is not] significantly better than the alternatives.

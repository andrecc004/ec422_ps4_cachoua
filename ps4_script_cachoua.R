# EC 422 - Problem Set 4

library(tidyverse)
library(fpp3)
library(BVAR)


# Part 2


# 2.1a=
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

# 2.1b
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

#2.2a
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

# 2.2b
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

# 2.3b
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

#2.4
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

#part 3

# 3a
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

#3b
err_cor <- cor(liquor_errs$arima, liquor_errs$ets)
err_cor

# Answer 3b:
# The correlation is 0.80, so the two models make similar errors and
# averaging cancels little. The DM test also showed ARIMA is
# significantly better than ETS, so adding ETS mostly pulls the forecast
# toward a worse model. That is why the combo did not beat ARIMA.



# Part 4 


#4.1a
qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |>
  slice(-(1:2))

#4.1b
growth <- function(x) 100 * difference(log(x))

qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(date,
            gdp    = growth(GDPC1),
            cons   = growth(PCECC96),
            inv    = growth(GPDIC1),
            emp    = growth(PAYEMS),
            infl   = growth(GDPCTPI),
            unrate = difference(UNRATE),
            ffr    = difference(FEDFUNDS),
            gs10   = difference(GS10)) |>
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

y <- qd |> select(-date) |> as.matrix()
nrow(y)
head(y)

#4.1c
N <- nrow(y)
origins <- 120:(N - 1)
actual <- y[121:N, ]

# Answer 4.1c: 120 forecasts per variable, from 1990 Q1 to 2019 Q4.

# 4.2
var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)
  M <- ncol(z)
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))
  x %*% B
}

fc_var1 <- NULL
fc_var4 <- NULL
for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}

rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))
rmse_by_var(fc_var1)
rmse_by_var(fc_var4)

# Answer 4.2:
# VAR(1) RMSE: gdp 0.513, cons 0.460, inv 2.493, emp 0.196, infl 0.208,
#   unrate 0.199, ffr 0.390, gs10 0.361
# VAR(4) RMSE: gdp 0.569, cons 0.527, inv 2.521, emp 0.228, infl 0.207,
#   unrate 0.215, ffr 0.497, gs10 0.379
# The VAR(1) forecasts better, for 7 of the 8 variables (all except
# inflation, where the two are nearly tied).
# The VAR(4) estimates many more coefficients from limited data. The
# extra estimation noise (overfitting) can outweigh what the extra lags add.

# 4.3a
bvar_cv <- function(lambda) {
  pr <- bv_priors(hyper = "lambda",
                  mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
                                                min = 0.999 * lambda,
                                                max = 1.001 * lambda),
                             b = 0))
  fc <- NULL
  for (T in origins) {
    fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
                  n_draw = 2000, n_burn = 1000, verbose = FALSE)
    x <- c(1, t(y[T:(T - 3), ]))
    fc <- rbind(fc, x %*% coef(fit_T))
  }
  fc
}

set.seed(422)
fc_bvar_03 <- bvar_cv(0.3)
saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")

#4.3b
set.seed(422)
fc_bvar_005 <- bvar_cv(0.05)
saveRDS(fc_bvar_005, "results/fc_bvar_005.rds")

var_results <- tibble(
  Variable = colnames(y),
  `VAR(1)` = rmse_by_var(fc_var1),
  `VAR(4)` = rmse_by_var(fc_var4),
  `BVAR(4), lambda = 0.3`  = rmse_by_var(fc_bvar_03),
  `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
)
saveRDS(var_results, "results/var_results.rds")
var_results |> knitr::kable(digits = 3)

# Answer 4.3b:
# - BVAR(4) with lambda = 0.3 has a lower RMSE than the OLS VAR(4) for
#   ___ of 8 variables. The only difference is the prior: shrinking
#   coefficients toward zero cuts estimation noise.
# - Compared with the VAR(1), the BVAR(4) is better for ___ of 8 variables.
# - With lambda = 0.05, RMSE [falls / rises] for most variables, so
#   [even more / less] shrinkage helps.
# - No. The winner's RMSE is too optimistic: the model was chosen
#   because it did best on this same sample, partly by luck.

# 4.3c
e_var4 <- actual[, "gdp"] - fc_var4[, 1]
e_bvar <- actual[, "gdp"] - fc_bvar_03[, 1]
dm_gdp <- forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)
dm_gdp

e_var4_infl <- actual[, "infl"] - fc_var4[, 5]
e_bvar_infl <- actual[, "infl"] - fc_bvar_03[, 5]
dm_infl <- forecast::dm.test(e_var4_infl, e_bvar_infl, h = 1, power = 2)
dm_infl

saveRDS(list(gdp = dm_gdp, infl = dm_infl), "results/dm_var.rds")

# Answer 4.3c:
# GDP: DM = ___, p = ___. Inflation: DM = ___, p = ___.
# [If p > 0.05] The differences are not statistically significant, so
# the RMSE rankings in 4.3b should be read with caution.
# [If p < 0.05] The BVAR is significantly more accurate, so the gains in
# 4.3b are likely real.

# Part 5


# 5.1a
md_raw <- read_csv("data/fredmd_2026-09.csv", show_col_types = FALSE)
tcode <- unlist(md_raw[1, -1])
md <- md_raw[-1, ] |> mutate(date = as.Date(sasdate, "%m/%d/%Y"))

transform_series <- function(x, code) {
  d <- function(v) c(NA, diff(v))
  switch(as.character(code),
         "1" = x,
         "2" = d(x),
         "3" = d(d(x)),
         "4" = log(x),
         "5" = 100 * d(log(x)),
         "6" = 100 * d(d(log(x))),
         "7" = d(x / dplyr::lag(x) - 1))
}

# 5.1b
fm <- map2_dfc(md[names(tcode)], tcode, transform_series) |>
  mutate(date = md$date, .before = 1) |>
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))
fm <- fm[, colSums(is.na(fm)) == 0]
dim(fm)

# 5.1c
tcode[c("INDPRO", "PAYEMS", "CPIAUCSL", "FEDFUNDS")]

# Answer 5.1c:
# INDPRO (5): monthly growth rate.
# PAYEMS (5): monthly growth rate.
# CPIAUCSL (6): change in the monthly inflation rate.
# FEDFUNDS (2): first difference.
# INDPRO is an index that grows over time, so growth rates make sense.
# FEDFUNDS is already a percent rate, so a simple difference is enough.

# 5.2a
Z <- fm |> select(-date) |> as.matrix()
pc <- prcomp(scale(Z))
summary(pc)$importance[, 1:5]

# Answer 5.2a: The first three factors explain 29.4% of the variation
# (15.0%, 7.4%, and 7.1%).

# 5.2b
F1 <- pc$x[, 1] * sign(cor(pc$x[, 1], fm$INDPRO))

tibble(date = fm$date, F1 = F1) |>
  ggplot(aes(date, F1)) +
  geom_line() +
  labs(title = "First Factor from FRED-MD", x = NULL, y = NULL) +
  theme_minimal()

# 5.2c
sort(abs(cor(pc$x[, 1], Z))[1, ], decreasing = TRUE)[1:8]
sort(abs(cor(pc$x[, 2], Z))[1, ], decreasing = TRUE)[1:8]

# Answer 5.2c:
# Factor 1 is most correlated with employment and industrial production
# (PAYEMS, IPMANSICS, USGOOD, INDPRO, MANEMP) and drops in recessions, so
# it is a real-activity factor.
# Factor 2 is most correlated with consumer price series (CPI and PCE
# price indexes), so it is a price/inflation factor.

# 5.3
ip <- fm$INDPRO
n <- nrow(Z)
init <- which(fm$date == as.Date("1989-12-01"))

e_ar <- NULL
e_fa <- NULL
for (T in init:(n - 1)) {
  F_T <- prcomp(scale(Z[1:T, ]))$x[, 1:3]
  d <- data.frame(y_next = ip[2:T], y = ip[1:(T - 1)], F_T[1:(T - 1), ])
  new <- data.frame(y = ip[T], t(F_T[T, ]))
  e_ar <- c(e_ar, ip[T + 1] - predict(lm(y_next ~ y, d), new))
  e_fa <- c(e_fa, ip[T + 1] - predict(lm(y_next ~ ., d), new))
}
c(RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)))

# Answer 5.3:
# RMSE_AR = 0.617 and RMSE_FA = 0.589, matching the results from class.
# Full-sample factors use future data that a forecaster would not have
# at the forecast origin, which makes the forecasts look better than
# they really are.

# 5.4
direct_race <- function(series, h = 12, r = 3) {
  lv <- md |>
    mutate(log_x = log(.data[[series]]),
           g = 100 * (log_x - lag(log_x))) |>
    filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))
  yh <- 100 * (lead(lv$log_x, h) - lv$log_x)
  g <- lv$g
  e_ar <- NULL
  e_fa <- NULL
  for (T in init:(n - h)) {
    F_T <- prcomp(scale(Z[1:T, ]))$x[, 1:r, drop = FALSE]
    i <- 1:(T - h)
    d <- data.frame(yh = yh[i], g = g[i], F_T[i, , drop = FALSE])
    new <- data.frame(g = g[T], F_T[T, , drop = FALSE])
    e_ar <- c(e_ar, yh[T] - predict(lm(yh ~ g, d), new))
    e_fa <- c(e_fa, yh[T] - predict(lm(yh ~ ., d), new))
  }
  tibble(series, forecasts = length(e_ar),
         RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)),
         MAE_AR = mean(abs(e_ar)), MAE_FA = mean(abs(e_fa)),
         DM_pvalue = forecast::dm.test(e_ar, e_fa, h = h, power = 2,
                                       varestimator = "bartlett")$p.value)
}

cpi_race <- direct_race("CPIAUCSL")
cpi_race
cpi_race |> mutate(pct_reduction = 100 * (1 - RMSE_FA / RMSE_AR))

# Answer 5.4:
# Yes. RMSE falls from 2.108 to 1.894, a 10.2% reduction (MAE: 1.578 to
# 1.349). DM p-value = 0.0004, so the difference is statistically
# significant.


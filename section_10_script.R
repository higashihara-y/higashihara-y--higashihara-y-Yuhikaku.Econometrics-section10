library(tidyverse)
library(readr)
library(estimatr)
library(readxl)
library(lmtest)
library(sandwich)
pacman::p_load(latex2exp)
pacman::p_load(estatapi)


# 図10-11 実験系列 -------------------------------------------------------------
set.seed(1)
data10_11_a <- rnorm(150)
data10_11_b <- data10_11_a[1]

for (i in 2:150) {
  data10_11_b[i] <- data10_11_b[i-1] + 0.5 * data10_11_a[i]
}

tibble(1:150, data10_11_a, data10_11_b) |> 
  pivot_longer(cols = c(data10_11_a, data10_11_b)) |> 
  ggplot() +
  geom_line(aes(x = `1:150`, y = value, color = name)) +
  scale_color_discrete(name = element_blank(),
                       labels = c("(a)系列相関なし", "(b)正の系列相関")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
  labs(x = element_blank(), y = element_blank())
  
  

# 図10-12 GDPギャップ（四半期データ） --------------------------------------------------

GDPgap_quarterly <- read_excel("Fig_12_GDPgap_quarterly.xlsx")
glimpse(GDPgap_quarterly)

GDPgap_quarterly |> 
  ggplot() +
  # yq()で四半期表示の文字列をDate型に変換
  geom_line(aes(x = yq(`...3`), y = `内閣府`, group = "")) +
  labs(x = element_blank(), y = "GDPギャップ")



# 図10-14 GDPギャップのコレログラム ---------------------------------------------------

# ベースRのacf関数によるプロット
data10_14 <- GDPgap_quarterly |> 
  filter(yq(...3) >= "1980-01-01" & yq(...3) <= "2016-10-01") |> 
  select(`内閣府`) |> 
  acf()

# ggplotによるプロット
acf10_14 <- tibble(
  Lag = data10_14$lag[-1],
  ACF = data10_14$acf[-1]
)

ci <- 0.95
N <- data10_14$n.used

acf10_14 |> 
  ggplot(aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", position = "identity") +
  # 臨界値の算出式については、P.479を参照
  geom_hline(yintercept = qnorm((1 + ci)/2) / sqrt(N), lty = "dashed") +
  geom_hline(yintercept = -qnorm((1 + ci)/2) / sqrt(N), lty = "dashed") +
  annotate(geom = "text", x = 20, y = 0.2, 
           label = latex2exp::TeX("$2 \\times $SE")) +
  annotate(geom = "text", x = 20, y = -0.2,
           label = latex2exp::TeX("$-2 \\times $SE")) +
  annotate(geom = "text", x = 5, y = 0.6, label = "自己相関")
  



# 実証例10.1 GDPギャップの系列相関 ----------------------------------------------------

data_j10.1 <- GDPgap_quarterly |> 
  filter(yq(...3) >= "1980-01-01" & yq(...3) <= "2016-10-01") |> 
  select("内閣府")

serialcol_table <- function(data) {
  rho <- acf(data, plot = FALSE)$acf[-1]
  ms <-  c(1, 2, 3, 4, 8, 12, 16, 20)
  table <- tibble()
  round_format <- function(x, digits = 2) {
    return(round(x, digits))
  }
  for (i in ms) {
    test = Box.test(data, lag = i, type = "Ljung-Box")
    table <- rbind(table, 
                   tibble(m = as.character(i),
                          rho_m = round_format(rho[i], 2),
                          Q_LB = round_format(test$statistic, 1),
                          臨界値 = round_format(qchisq(0.95, df = i)),
                          p値 = round_format(test$p.value)))
  }
  table <- t(table)
  rownames(table) <- c("$m$", "$\\hat{\\rho}_m$", "$Q_{LB}$",
                       "臨界値", "$P$値")
  
  table |> 
    kableExtra::kable(col.names = NULL, row.names = TRUE,
                      escape = FALSE)
}

serialcol_table(data_j10.1)




# 実証例10.2 ホワイトノイズの系列相関 ----------------------------------------------------
# コレログラム

data_j10.2 <- tibble( 
  Lag = acf(data10_11_a)$lag[-1],
  ACF = acf(data10_11_a)$acf[-1]
)

ci <- 0.95
N <- acf(data10_11_a)$n.used

data_j10.2 |> 
  ggplot(aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = qnorm((1 + ci)/2) / sqrt(N), lty = "dashed") +
  geom_hline(yintercept = -qnorm((1 + ci)/2) / sqrt(N), lty = "dashed") +
  annotate(geom = "text", x = 20, y = 0.17,
           label = latex2exp::TeX("$2 \\times $SE")) +
  annotate(geom = "text", x = 20, y = -0.17,
           label = latex2exp::TeX("$-2 \\times $SE")) +
  annotate(geom = "text", x = 2, y = 0.13, label = "自己相関")


# 系列相関の検定
serialcol_table <- function(data) {
  rho <- acf(data, plot = FALSE)$acf[-1]
  ms <-  c(1, 2, 3, 4, 8, 12, 16, 20)
  table <- tibble()
  round_format <- function(x, digits = 2) {
    return(round(x, digits = digits))
  }
  for(i in ms) {
    test <- Box.test(data, lag = i, type = "Ljung-Box")
    table <- rbind(table,
                   tibble(m = as.character(i),
                          rho_m = round_format(rho[i]),
                          Q_LB = round_format(test$statistic, 1),
                          臨界値 = round_format(qchisq(0.95, df = i)),
                          P値 = round_format(test$p.value)))
  }
  table <- t(table)
  rownames(table) <- c("$m$", "$\\hat{\\rho}_m$", "$Q_{LB}$",
                       "臨界値", "$P$値")
  table |> 
    kableExtra::kable(col.names = NULL, row.names = TRUE,
                      escape = FALSE)
}

serialcol_table(data10_11_a)




# 実証例10.3 フィリップス曲線の誤差項の系列相関 -----------------------------------------------

# 内閣府サイトより1980-1994のGDPデフレータのダウンロード
curl <- "https://www.esri.cao.go.jp/jp/sna/data/data_list/h27_retroactive/tables/def-qk_2780.csv"
cdestfile <- "def-qk_2780.csv"
download.file(curl, cdestfile)

# データの読み込みと整形
gdpdef_pre <- read.csv("def-qk_2780.csv", fileEncoding = "CP932")
gdpdef_pre <- gdpdef_pre[7:62, c(1, 2)] |> 
  rename(`時間軸（四半期）` = 四半期デフレーター季節調整系列) |> 
  rename(value = X) |> 
  mutate(value = as.numeric(value))


# e-Statより1994年移行のデータをダウンロードと整形
appId <- "a23c579662946176a2d27d4f4e78bb7ce51910a0"
gdpdef_after <- estat_getStatsData(appId = appId,
                                   statsDataId = "0003109787")

gdpdef_after <- gdpdef_after |> 
  filter(`国内総生産_四半期デフレーター 季節調整系列` ==
           "国内総生産(支出側)") |> 
  select(`時間軸（四半期）` , value)
  

# 時間軸を作成し、データを結合
year <- seq(ymd(19800101), ymd(20240701), by = "quarter")
gdpdef <- bind_rows(gdpdef_pre, gdpdef_after) |> 
  cbind(year) |> 
  tibble()

gdpdef <- gdpdef |> 
  mutate(inflation = 400 * (value - lag(value))/lag(value))


# GDPギャップの四半期データと内部結合し、OLS推定
gdp_def_gap <- GDPgap_quarterly |> 
  mutate(year = yq(...3)) |> 
  inner_join(gdpdef, by = join_by(year)) |> 
  select(year, "GDP_gap" = `内閣府`, inflation) |> 
  filter(year != "1980-01-01")
  
model10.3 <- lm_robust(inflation ~ GDP_gap, data = gdp_def_gap,
                       clusters = year, se_type = "stata")
summary(model10.3)


# モデル推定の誤差について系列相関を検定
# いずれのラグでも、Q値が臨界値を超え、系列相関の存在が確認できる
model10.3_resid <- gdp_def_gap$inflation - model10.3$fitted.values
serialcol_table(model10.3_resid)




# 実証例10.4 フィリップス曲線のHAC標準誤差 ------------------------------------------------

# ラグ次数の算出（推計では使用しない）
lag = floor(4 * (model10.3$nobs / 100)^(1/3))

# HAC標準誤差での推計
# vcov=NeweyWestとvcov=vcovHACでは推計値が異なる（NeweyWestがやや小さい）
model10.4 <- lm(inflation ~ GDP_gap, data = gdp_def_gap)
lmtest::coeftest(model10.4, vcov. = sandwich::NeweyWest) 
lmtest::coefci(model10.4, vcov. = sandwich::NeweyWest)
lmtest::coeftest(model10.4, vcov. = vcovHAC) 
lmtest::coefci(model10.4, vcov. = vcovHAC)

# ホワイト標準誤差での推計
lmtest::coeftest(model10.4, vcov. = vcovHC(model10.4, type = "HC1"))
lmtest::coefci(model10.4, vcov. = vcovHC)




# 実証例10.5 GDPギャップのARモデルのラグ選択 ----------------------------------------------
# 参照：ARIMAモデルについて　
# https://www.i-juse.co.jp/statistics/jirei/sympo/10/arima-model.html

data_10.5 <- GDPgap_quarterly |> 
  filter(ym(`...3`) < ym(201701)) |> 
  select(`内閣府`)
  
models_10.5 <- list(
  "AR(0)" = arima(data_10.5, order = c(0, 0, 0),
                  include.mean = FALSE, method = "ML"),
  "AR(1)" = arima(data_10.5, order = c(1, 0, 0),
                  include.mean = FALSE, method = "ML"),
  "AR(2)" = arima(data_10.5, order = c(2, 0, 0),
                  include.mean = FALSE, method = "ML"),
  "AR(3)" = arima(data_10.5, order = c(3, 0, 0),
                  include.mean = FALSE, method = "ML"),
  "AR(4)" = arima(data_10.5, order = c(4, 0, 0),
                  include.mean = FALSE, method = "ML")
)

table_10.5 <- tibble()
round_format <- function(x, digits = 3) {
  return(format(round(x, digits = digits), nsmall = digits))
}

for (each in models_10.5) {
  p <- each$arma[1]
  T <- 148 - p
  RSS_p <- sum(each$residuals^2)
  ln_RSS_p_div_T <- log(RSS_p / T)
  AIC_penalty <- (p + 1) * 2 / T
  BIC_penalty <- (p + 1) * log(T) / T
  table_10.5 <- rbind(table_10.5,
                      tibble(
                        ps = as.character(p),
                        AIC = round_format(ln_RSS_p_div_T + AIC_penalty),
                        BIC = round_format(ln_RSS_p_div_T + BIC_penalty),
                        RSS_p = round_format(RSS_p, 1),
                        ln_RSS_p_div_T = round_format(ln_RSS_p_div_T),
                        AIC_penalty = round_format(AIC_penalty),
                        BIC_penalty = round_format(BIC_penalty)
                      ))
}

table_10.5 <- t(table_10.5)

rownames(table_10.5) <- c(
  "$p$", "AIC", "BIC", "RSS($p$)", "ln[RSS($p$)/$T$]",
  "AICの罰則項", "AICの罰則項")

table_10.5 |> 
  kableExtra::kable(row.names = TRUE, col.names = NULL, escape = TRUE)


#AR(1)モデルの再推定
arima(data_10.5, order = c(1, 0, 0), method = "ML")




# 10-9［実証］ ----------------------------------------------------------------

# 後方移動平均、中心化移動平均を計算し、列として追加
# sides = 1 : 後方、sides = 2 : 中心化
# 参照：https://tips-r.blogspot.com/2015/01/r_1.html
MA_infl_10.9 <- gdp_def_gap |>
  mutate(MA_lag = stats::filter(inflation, rep(1, 4) / 4, 
                                sides = 1),
         MA_mid = stats::filter(inflation, c(1, 2, 2, 2, 1) / 8, 
                                sides = 2)) 

# 実証例10.5の結果より、inflationの系列相関をAR(1)モデルで推定
# 季節調整方法や有無により、推定結果が大きく異なる
# 後方移動平均
arima(MA_infl_10.9$MA_lag, order = c(1, 0, 0), method = "ML")
# 中心化移動平均
arima(MA_infl_10.9$MA_mid, order = c(1, 0, 0), method = "ML")
# 移動平均なし
arima(MA_infl_10.9$inflation, order = c(1, 0, 0), method = "ML")


# フィリップス曲線のHAC標準誤差による推定
# 後方移動平均
model10.9_lag <- lm(MA_lag ~ GDP_gap, data = MA_infl_10.9)
lmtest::coeftest(model10.9_lag, vcov. = sandwich::NeweyWest)
# 中心化移動平均
model10.9_mid <- lm(MA_mid ~ GDP_gap, data = MA_infl_10.9)
lmtest::coeftest(model10.9_mid, vcov. = sandwich::NeweyWest)
# 移動平均なし
model10.9 <- lm(inflation ~ GDP_gap, data = MA_infl_10.9)
lmtest::coeftest(model10.9, vcov. = sandwich::NeweyWest)

























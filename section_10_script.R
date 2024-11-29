library(tidyverse)
library(readr)
library(estimatr)
library(readxl)
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

# 1980-1994のGDPデフレータのダウンロード
curl <- "https://www.esri.cao.go.jp/jp/sna/data/data_list/h27_retroactive/tables/def-qk_2780.csv"
cdestfile <- "def-qk_2780.csv"
download.file(curl, cdestfile)

# データの読み込みと整形
# 1995以降のデータは無視して推計を行う
gdpdef_pre <- read.csv("def-qk_2780.csv", fileEncoding = "CP932")
gdpdef_pre <- gdpdef_pre[7:66, c(1, 2)] |> 
  rename(`時間軸（四半期）` = 四半期デフレーター季節調整系列) |> 
  rename(value = X) |> 
  mutate(value = as.numeric(value))

year <- seq(ymd(19800101), ymd(19941001), by = "quarter")
gdpdef <- cbind(gdpdef_pre, year) |> 
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
                       se_type = "stata")

# モデル推定の誤差について系列相関を検定
# GDPギャップの説明力が低すぎて、良い結果が得られない
model10.3_resid <- gdp_def_gap$inflation - model10.3$fitted.values
serialcol_table(model10.3_resid)

model10.3_resid |> 
  tibble() |> 
  ggplot() +
  geom_line(aes(x = 1:59, y = model10.3_resid))

gdp_def_gap |> 
  ggplot() +
  geom_line(aes(x = year, y = inflation))








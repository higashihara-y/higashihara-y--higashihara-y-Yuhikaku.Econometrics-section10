library(tidyverse)
library(readr)
library(estimatr)
library(readxl)
pacman::p_load(latex2exp)


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
  ms = c(1, 2, 3, 4, 8, 12, 16, 20)
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




























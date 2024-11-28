library(tidyverse)
library(readr)
library(estimatr)
library(readxl)


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













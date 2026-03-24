### Load Libraries
library(tidyverse)

### read in .csv
lrc_48 <- read.csv("lrc_48.csv")



### Curve 1
gg_1 <- lrc_48 %>% 
  filter(id == "gg_1")
ggplot(gg_1, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg1 <- lm(LEF ~ one.qL, data = gg_1, subset = one.qL < 0.9)
# creating slope and intercept
gg_1_slope <- coef(lm_gg1)[2]
gg_1_intercept <- coef(lm_gg1)[1]
# plot
ggplot(gg_1, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_1_slope,
    intercept = gg_1_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 10))



### Curve 2
gg_2 <- lrc_48 %>% 
  filter(id == "gg_2")
gg_2 <- gg_2 %>%
  filter(machine == "stan")
ggplot(gg_2, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg2 <- lm(LEF ~ one.qL, data = gg_2, subset = one.qL < 0.4)
# creating slope and intercept
gg_2_slope <- coef(lm_gg2)[2]
gg_2_intercept <- coef(lm_gg2)[1]
# plot
ggplot(gg_2, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_2_slope,
    intercept = gg_2_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 40))



### Curve 3
gg_3 <- lrc_48 %>% 
  filter(id == "gg_3")
ggplot(gg_3, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg3 <- lm(LEF ~ one.qL, data = gg_3, subset = one.qL < 0.9)
# creating slope and intercept
gg_3_slope <- coef(lm_gg3)[2]
gg_3_intercept <- coef(lm_gg3)[1]
# plot
ggplot(gg_3, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_3_slope,
    intercept = gg_3_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 15))



### Curve 4 no data



### Curve 5
gg_5 <- lrc_48 %>% 
  filter(id == "gg_5")
ggplot(gg_5, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg5 <- lm(LEF ~ one.qL, data = gg_5, subset = one.qL < 0.99)
# creating slope and intercept
gg_5_slope <- coef(lm_gg5)[2]
gg_5_intercept <- coef(lm_gg5)[1]
# plot
ggplot(gg_5, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_5_slope,
    intercept = gg_5_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 100))


### Curve 6
gg_6 <- lrc_48 %>% 
  filter(id == "gg_6")
ggplot(gg_6, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg6 <- lm(LEF ~ one.qL, data = gg_6, subset = one.qL < 0.5)
# creating slope and intercept
gg_6_slope <- coef(lm_gg6)[2]
gg_6_intercept <- coef(lm_gg6)[1]
# plot
ggplot(gg_6, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_6_slope,
    intercept = gg_6_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 12))



### Curve 7
gg_7 <- lrc_48 %>% 
  filter(id == "gg_7")
ggplot(gg_7, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg7 <- lm(LEF ~ one.qL, data = gg_7, subset = one.qL < 0.9)
# creating slope and intercept
gg_7_slope <- coef(lm_gg7)[2]
gg_7_intercept <- coef(lm_gg7)[1]
# plot
ggplot(gg_7, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_7_slope,
    intercept = gg_7_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 100))



### Curve 8
gg_8 <- lrc_48 %>% 
  filter(id == "gg_8")
ggplot(gg_8, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg8 <- lm(LEF ~ one.qL, data = gg_8, subset = one.qL < 0.5)
# creating slope and intercept
gg_8_slope <- coef(lm_gg8)[2]
gg_8_intercept <- coef(lm_gg8)[1]
# plot
ggplot(gg_8, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_8_slope,
    intercept = gg_8_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 100))



### Curve 9
gg_9 <- lrc_48 %>% 
  filter(id == "gg_9")
ggplot(gg_9, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg9 <- lm(LEF ~ one.qL, data = gg_9, subset = one.qL < 0.9)
# creating slope and intercept
gg_9_slope <- coef(lm_gg9)[2]
gg_9_intercept <- coef(lm_gg9)[1]
# plot
ggplot(gg_9, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_9_slope,
    intercept = gg_9_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 20))



### Curve 10
gg_10 <- lrc_48 %>% 
  filter(id == "gg_10")
gg_10 <- gg_10 %>%
  filter(machine == "ozzie")
ggplot(gg_10, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg10 <- lm(LEF ~ one.qL, data = gg_10, subset = one.qL < 0.7)
# creating slope and intercept
gg_10_slope <- coef(lm_gg10)[2]
gg_10_intercept <- coef(lm_gg10)[1]
# plot
ggplot(gg_10, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_10_slope,
    intercept = gg_10_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 30))



### Curve 11
gg_11 <- lrc_48 %>% 
  filter(id == "gg_11")
ggplot(gg_11, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg11 <- lm(LEF ~ one.qL, data = gg_11, subset = one.qL < 0.75)
# creating slope and intercept
gg_11_slope <- coef(lm_gg11)[2]
gg_11_intercept <- coef(lm_gg11)[1]
# plot
ggplot(gg_11, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_11_slope,
    intercept = gg_11_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 50))



### Curve 12
gg_12 <- lrc_48 %>% 
  filter(id == "gg_12")
ggplot(gg_12, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg12 <- lm(LEF ~ one.qL, data = gg_12, subset = one.qL < 0.6)
# creating slope and intercept
gg_12_slope <- coef(lm_gg12)[2]
gg_12_intercept <- coef(lm_gg12)[1]
# plot
ggplot(gg_12, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_12_slope,
    intercept = gg_12_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 30))



### Curve 13 no data



### Curve 14
gg_14 <- lrc_48 %>% 
  filter(id == "gg_14")
ggplot(gg_14, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg14 <- lm(LEF ~ one.qL, data = gg_14, subset = one.qL < 0.3)
# creating slope and intercept
gg_14_slope <- coef(lm_gg14)[2]
gg_14_intercept <- coef(lm_gg14)[1]
# plot
ggplot(gg_14, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_14_slope,
    intercept = gg_14_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 30))



### Curve 15
gg_15 <- lrc_48 %>% 
  filter(id == "gg_15")
ggplot(gg_15, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg15 <- lm(LEF ~ one.qL, data = gg_15, subset = one.qL < 0.8)
# creating slope and intercept
gg_15_slope <- coef(lm_gg15)[2]
gg_15_intercept <- coef(lm_gg15)[1]
# plot
ggplot(gg_15, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_15_slope,
    intercept = gg_15_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 30))



### Curve 16
gg_16 <- lrc_48 %>% 
  filter(id == "gg_16")
ggplot(gg_16, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg16 <- lm(LEF ~ one.qL, data = gg_16, subset = one.qL < 0.4)
# creating slope and intercept
gg_16_slope <- coef(lm_gg16)[2]
gg_16_intercept <- coef(lm_gg16)[1]
# plot
ggplot(gg_16, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_16_slope,
    intercept = gg_16_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 60))



### Curve 17 no data



### Curve 18
gg_18 <- lrc_48 %>% 
  filter(id == "gg_18")
ggplot(gg_18, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg18 <- lm(LEF ~ one.qL, data = gg_18, subset = one.qL < 0.5)
# creating slope and intercept
gg_18_slope <- coef(lm_gg18)[2]
gg_18_intercept <- coef(lm_gg18)[1]
# plot
ggplot(gg_18, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_18_slope,
    intercept = gg_18_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 50))



### Curve 19 no data



### Curve 20 no data



### Curve 21
gg_21 <- lrc_48 %>% 
  filter(id == "gg_21")
ggplot(gg_21, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg21 <- lm(LEF ~ one.qL, data = gg_21, subset = one.qL < 0.7)
# creating slope and intercept
gg_21_slope <- coef(lm_gg21)[2]
gg_21_intercept <- coef(lm_gg21)[1]
# plot
ggplot(gg_21, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_21_slope,
    intercept = gg_21_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 30))



### Curve 22
gg_22 <- lrc_48 %>% 
  filter(id == "gg_22")
ggplot(gg_22, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg22 <- lm(LEF ~ one.qL, data = gg_22, subset = one.qL < 0.4)
# creating slope and intercept
gg_22_slope <- coef(lm_gg22)[2]
gg_22_intercept <- coef(lm_gg22)[1]
# plot
ggplot(gg_22, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_22_slope,
    intercept = gg_22_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 100))



### Curve 23
gg_23 <- lrc_48 %>% 
  filter(id == "gg_23")
ggplot(gg_23, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg23 <- lm(LEF ~ one.qL, data = gg_23, subset = one.qL < 0.9)
# creating slope and intercept
gg_23_slope <- coef(lm_gg23)[2]
gg_23_intercept <- coef(lm_gg23)[1]
# plot
ggplot(gg_23, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_23_slope,
    intercept = gg_23_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 50))



### Curve 24
gg_24 <- lrc_48 %>% 
  filter(id == "gg_24")
ggplot(gg_24, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg24 <- lm(LEF ~ one.qL, data = gg_24, subset = one.qL < 0.5)
# creating slope and intercept
gg_24_slope <- coef(lm_gg24)[2]
gg_24_intercept <- coef(lm_gg24)[1]
# plot
ggplot(gg_24, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_24_slope,
    intercept = gg_24_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 40))



### Curve 25 no data



### Curve 26
gg_26 <- lrc_48 %>% 
  filter(id == "gg_26")
ggplot(gg_26, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg26 <- lm(LEF ~ one.qL, data = gg_26, subset = one.qL < 0.6)
# creating slope and intercept
gg_26_slope <- coef(lm_gg26)[2]
gg_26_intercept <- coef(lm_gg26)[1]
# plot
ggplot(gg_26, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_26_slope,
    intercept = gg_26_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 100))



### Curve 27 no data



### Curve 28
gg_28 <- lrc_48 %>% 
  filter(id == "gg_28")
ggplot(gg_28, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg28 <- lm(LEF ~ one.qL, data = gg_28, subset = one.qL < 0.6)
# creating slope and intercept
gg_28_slope <- coef(lm_gg28)[2]
gg_28_intercept <- coef(lm_gg28)[1]
# plot
ggplot(gg_28, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_28_slope,
    intercept = gg_28_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 100))



### Curve 29
gg_29 <- lrc_48 %>% 
  filter(id == "gg_29")
ggplot(gg_29, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg29 <- lm(LEF ~ one.qL, data = gg_29, subset = one.qL < 0.9)
# creating slope and intercept
gg_29_slope <- coef(lm_gg29)[2]
gg_29_intercept <- coef(lm_gg29)[1]
# plot
ggplot(gg_29, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_29_slope,
    intercept = gg_29_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 20))



### Curve 30
gg_30 <- lrc_48 %>% 
  filter(id == "gg_30")
ggplot(gg_30, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg30 <- lm(LEF ~ one.qL, data = gg_30, subset = one.qL < 0.7)
# creating slope and intercept
gg_30_slope <- coef(lm_gg30)[2]
gg_30_intercept <- coef(lm_gg30)[1]
# plot
ggplot(gg_30, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_30_slope,
    intercept = gg_30_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 50))



### Curve 31
gg_31 <- lrc_48 %>% 
  filter(id == "gg_31")
ggplot(gg_31, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg31 <- lm(LEF ~ one.qL, data = gg_31, subset = one.qL < 0.95)
# creating slope and intercept
gg_31_slope <- coef(lm_gg31)[2]
gg_31_intercept <- coef(lm_gg31)[1]
# plot
ggplot(gg_31, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_31_slope,
    intercept = gg_31_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 20))



### Curve 32
gg_32 <- lrc_48 %>% 
  filter(id == "gg_32")
ggplot(gg_32, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg32 <- lm(LEF ~ one.qL, data = gg_32, subset = one.qL < 0.5)
# creating slope and intercept
gg_32_slope <- coef(lm_gg32)[2]
gg_32_intercept <- coef(lm_gg32)[1]
# plot
ggplot(gg_32, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_32_slope,
    intercept = gg_32_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 20))



### Curve 33
gg_33 <- lrc_48 %>% 
  filter(id == "gg_33")
ggplot(gg_33, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg33 <- lm(LEF ~ one.qL, data = gg_33, subset = one.qL < 0.8)
# creating slope and intercept
gg_33_slope <- coef(lm_gg33)[2]
gg_33_intercept <- coef(lm_gg33)[1]
# plot
ggplot(gg_33, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_33_slope,
    intercept = gg_33_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 20))



### Curve 34
gg_34 <- lrc_48 %>% 
  filter(id == "gg_34")
gg_34 <- gg_34 %>%
  filter(machine == "albert")
ggplot(gg_34, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg34 <- lm(LEF ~ one.qL, data = gg_34, subset = one.qL < 0.4)
# creating slope and intercept
gg_34_slope <- coef(lm_gg34)[2]
gg_34_intercept <- coef(lm_gg34)[1]
# plot
ggplot(gg_34, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_34_slope,
    intercept = gg_34_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 100))



### Curve 35
gg_35 <- lrc_48 %>% 
  filter(id == "gg_35")
ggplot(gg_35, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg35 <- lm(LEF ~ one.qL, data = gg_35, subset = one.qL < 0.99)
# creating slope and intercept
gg_35_slope <- coef(lm_gg35)[2]
gg_35_intercept <- coef(lm_gg35)[1]
# plot
ggplot(gg_35, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_35_slope,
    intercept = gg_35_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 15))



### Curve 36
gg_36 <- lrc_48 %>% 
  filter(id == "gg_36")
ggplot(gg_36, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg36 <- lm(LEF ~ one.qL, data = gg_36, subset = one.qL < 0.5)
# creating slope and intercept
gg_36_slope <- coef(lm_gg36)[2]
gg_36_intercept <- coef(lm_gg36)[1]
# plot
ggplot(gg_36, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_36_slope,
    intercept = gg_36_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 30))



### Curve 37
gg_37 <- lrc_48 %>% 
  filter(id == "gg_37")
ggplot(gg_37, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg37 <- lm(LEF ~ one.qL, data = gg_37, subset = one.qL < 0.7)
# creating slope and intercept
gg_37_slope <- coef(lm_gg37)[2]
gg_37_intercept <- coef(lm_gg37)[1]
# plot
ggplot(gg_37, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_37_slope,
    intercept = gg_37_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 20))



### Curve 38
gg_38 <- lrc_48 %>% 
  filter(id == "gg_38")
ggplot(gg_38, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg38 <- lm(LEF ~ one.qL, data = gg_38, subset = one.qL < 0.7)
# creating slope and intercept
gg_38_slope <- coef(lm_gg38)[2]
gg_38_intercept <- coef(lm_gg38)[1]
# plot
ggplot(gg_38, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_38_slope,
    intercept = gg_38_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 30))



### Curve 39
gg_39 <- lrc_48 %>% 
  filter(id == "gg_39")
ggplot(gg_39, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg39 <- lm(LEF ~ one.qL, data = gg_39, subset = one.qL < 0.8)
# creating slope and intercept
gg_39_slope <- coef(lm_gg39)[2]
gg_39_intercept <- coef(lm_gg39)[1]
# plot
ggplot(gg_39, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_39_slope,
    intercept = gg_39_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 20))



### Curve 40 corrupted


### data frame
vqmax48_df <- data.frame(
  id = c("gg_1", "gg_2", "gg_3", "gg_5", "gg_6", "gg_7", "gg_9",
         "gg_10", "gg_12", "gg_14", "gg_15", "gg_16", "gg_18", 
         "gg_21", "gg_22", "gg_23", "gg_24", "gg_26", 
         "gg_28", "gg_29", "gg_30", "gg_31", "gg_32", "gg_34", 
         "gg_35", "gg_36", "gg_38", "gg_39"),
  slope_48 = c(gg_1_slope, gg_2_slope, gg_3_slope, gg_5_slope, gg_6_slope, gg_7_slope, gg_9_slope,
               gg_10_slope, gg_12_slope, gg_14_slope, gg_15_slope, gg_16_slope, gg_18_slope, 
               gg_21_slope, gg_22_slope, gg_23_slope, gg_24_slope, gg_26_slope, 
               gg_28_slope, gg_29_slope, gg_30_slope, gg_31_slope, gg_32_slope, gg_34_slope, 
               gg_35_slope, gg_36_slope, gg_38_slope, gg_39_slope),
  intercept_48 = c(gg_1_intercept, gg_2_intercept, gg_3_intercept, gg_5_intercept, gg_6_intercept, gg_7_intercept, gg_9_intercept,
                   gg_10_intercept, gg_12_intercept, gg_14_intercept, gg_15_intercept, gg_16_intercept, gg_18_intercept, 
                   gg_21_intercept, gg_22_intercept, gg_23_intercept, gg_24_intercept, gg_26_intercept, 
                   gg_28_intercept, gg_29_intercept, gg_30_intercept, gg_31_intercept, gg_32_intercept, gg_34_intercept, 
                   gg_35_intercept, gg_36_intercept, gg_38_intercept, gg_39_intercept)
)
vqmax48_df


### calculating Vqmax
vqmax48_df$vqmax_48 <- vqmax48_df$intercept + vqmax48_df$slope
vqmax48_df

### adding treatment column
vqmax48_df$replicate <- as.numeric(gsub("\\D", "", vqmax48_df$id))
vqmax48_df$treatment <- ifelse(vqmax48_df$replicate %% 2 == 1,
                               "25c",
                               "35c")
vqmax48_df$replicate <- NULL
vqmax48_df

### add leaf temp
vqmax48_df$tleaf_fac <- "48c"
vqmax48_df


### graph and stats time
ggplot(vqmax48_df, aes(x = treatment, y = vqmax_48)) + geom_boxplot() 
vqmax48_lm <- lm(vqmax_48 ~ treatment, data = vqmax48_df)
anova(vqmax48_lm)
plot(vqmax48_lm)

### write .csv
write.csv(vqmax48_df, "vqmax48_df.csv")

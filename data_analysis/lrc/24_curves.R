### Load Libraries
library(tidyverse)

### read in .csv
lrc_24 <- read.csv("lrc_24.csv")

### Curve 1 not good


### Curve 2
gg_2 <- lrc_24 %>% 
  filter(id == "gg_2")
ggplot(gg_2, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_2_tleaf <- mean(gg_2$Tleaf)
gg_2_tleaf
# creating linear model
lm_gg2 <- lm(LEF ~ one.qL, data = gg_2, subset = one.qL < 0.99)
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
    limits = c(0, 100))


### Curve 3
gg_3 <- lrc_24 %>% 
  filter(id == "gg_3")
ggplot(gg_3, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_3_tleaf <- mean(gg_3$Tleaf)
gg_3_tleaf
# creating linear model
lm_gg3 <- lm(LEF ~ one.qL, data = gg_3, subset = one.qL < 0.8)
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
    limits = c(0, 100))



### Curve 4
gg_4 <- lrc_24 %>% 
  filter(id == "gg_4")
ggplot(gg_4, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_4_tleaf <- mean(gg_4$Tleaf)
gg_4_tleaf
# creating linear model
lm_gg4 <- lm(LEF ~ one.qL, data = gg_4, subset = one.qL < 0.99)
# creating slope and intercept
gg_4_slope <- coef(lm_gg4)[2]
gg_4_intercept <- coef(lm_gg4)[1]
# plot
ggplot(gg_4, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_4_slope,
    intercept = gg_4_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 100))



### Curve 5
gg_5 <- lrc_24 %>% 
  filter(id == "gg_5")
ggplot(gg_5, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_5_tleaf <- mean(gg_5$Tleaf)
gg_5_tleaf
# creating linear model
lm_gg5 <- lm(LEF ~ one.qL, data = gg_5, subset = one.qL < 0.8)
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
    limits = c(0, 80))



### Curve 6
gg_6 <- lrc_24 %>% 
  filter(id == "gg_6")
ggplot(gg_6, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_6_tleaf <- mean(gg_6$Tleaf)
gg_6_tleaf
# creating linear model
lm_gg6 <- lm(LEF ~ one.qL, data = gg_6, subset = one.qL < 0.9)
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
    limits = c(0, 30))


### Curve 7
gg_7 <- lrc_24 %>% 
  filter(id == "gg_7")
ggplot(gg_7, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_7_tleaf <- mean(gg_7$Tleaf)
gg_7_tleaf
# creating linear model
lm_gg7 <- lm(LEF ~ one.qL, data = gg_7, subset = one.qL < 0.875)
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
gg_8 <- lrc_24 %>% 
  filter(id == "gg_8")
ggplot(gg_8, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_8_tleaf <- mean(gg_8$Tleaf)
gg_8_tleaf
# creating linear model
lm_gg8 <- lm(LEF ~ one.qL, data = gg_8, subset = one.qL < 0.8)
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
    limits = c(0, 70))



### Curve 9
gg_9 <- lrc_24 %>% 
  filter(id == "gg_9")
ggplot(gg_9, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_9_tleaf <- mean(gg_9$Tleaf)
gg_9_tleaf
# creating linear model
lm_gg9 <- lm(LEF ~ one.qL, data = gg_9, subset = one.qL < 0.8)
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
    limits = c(0, 100))



### Curve 10
gg_10 <- lrc_24 %>% 
  filter(id == "gg_10")
ggplot(gg_10, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_10_tleaf <- mean(gg_10$Tleaf)
gg_10_tleaf
# creating linear model
lm_gg10 <- lm(LEF ~ one.qL, data = gg_10, subset = one.qL < 0.8)
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
    limits = c(0, 50))



### Curve 11
gg_11 <- lrc_24 %>% 
  filter(id == "gg_11")
ggplot(gg_11, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_11_tleaf <- mean(gg_11$Tleaf)
gg_11_tleaf
# creating linear model
lm_gg11 <- lm(LEF ~ one.qL, data = gg_11, subset = one.qL < 0.8)
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
    limits = c(0, 100))



### Curve 12
gg_12 <- lrc_24 %>% 
  filter(id == "gg_12")
ggplot(gg_12, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_12_tleaf <- mean(gg_12$Tleaf)
gg_12_tleaf
# creating linear model
lm_gg12 <- lm(LEF ~ one.qL, data = gg_12, subset = one.qL < 0.85)
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
    limits = c(0, 40))



### Curve 13 no data 



### Curve 14
gg_14 <- lrc_24 %>% 
  filter(id == "gg_14")
ggplot(gg_14, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_14_tleaf <- mean(gg_14$Tleaf)
gg_14_tleaf
# creating linear model
lm_gg14 <- lm(LEF ~ one.qL, data = gg_14, subset = one.qL < 0.9)
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
    limits = c(0, 60))



### Curve 15
gg_15 <- lrc_24 %>% 
  filter(id == "gg_15")
ggplot(gg_15, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_15_tleaf <- mean(gg_15$Tleaf)
gg_15_tleaf
# creating linear model
lm_gg15 <- lm(LEF ~ one.qL, data = gg_15, subset = one.qL < 0.75)
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
    limits = c(0, 100))



### Curve 16
gg_16 <- lrc_24 %>% 
  filter(id == "gg_16")
ggplot(gg_16, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_16_tleaf <- mean(gg_16$Tleaf)
gg_16_tleaf
# creating linear model
lm_gg16 <- lm(LEF ~ one.qL, data = gg_16, subset = one.qL < 0.8)
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
    limits = c(0, 100))



### Curve 17 no data

### Curve 18
gg_18 <- lrc_24 %>% 
  filter(id == "gg_18")
ggplot(gg_18, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_18_tleaf <- mean(gg_18$Tleaf)
gg_18_tleaf
# creating linear model
lm_gg18 <- lm(LEF ~ one.qL, data = gg_18, subset = one.qL < 0.99)
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
    limits = c(0, 70))



### Curve 19
gg_19 <- lrc_24 %>% 
  filter(id == "gg_19")
ggplot(gg_19, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_19_tleaf <- mean(gg_19$Tleaf)
gg_19_tleaf
# creating linear model
lm_gg19 <- lm(LEF ~ one.qL, data = gg_19, subset = one.qL < 0.9)
# creating slope and intercept
gg_19_slope <- coef(lm_gg19)[2]
gg_19_intercept <- coef(lm_gg19)[1]
# plot
ggplot(gg_19, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_19_slope,
    intercept = gg_19_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 100))



### Curve 20 no data

### Curve 21
gg_21 <- lrc_24 %>% 
  filter(id == "gg_21")
ggplot(gg_21, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_21_tleaf <- mean(gg_21$Tleaf)
gg_21_tleaf
# creating linear model
lm_gg21 <- lm(LEF ~ one.qL, data = gg_21, subset = one.qL < 0.9)
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
    limits = c(0, 100))



### Curve 22
gg_22 <- lrc_24 %>% 
  filter(id == "gg_22")
gg_22 <- gg_22 %>% 
  filter(machine == "albert")
ggplot(gg_22, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_22_tleaf <- mean(gg_22$Tleaf)
gg_22_tleaf
# creating linear model
lm_gg22 <- lm(LEF ~ one.qL, data = gg_22, subset = one.qL < 0.99)
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
    limits = c(0, 70))



### Curve 23
gg_23 <- lrc_24 %>% 
  filter(id == "gg_23")
ggplot(gg_23, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_23_tleaf <- mean(gg_23$Tleaf)
gg_23_tleaf
# creating linear model
lm_gg23 <- lm(LEF ~ one.qL, data = gg_23, subset = one.qL < 0.8)
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
    limits = c(0, 100))



### Curve 24
gg_24 <- lrc_24 %>% 
  filter(id == "gg_24")
ggplot(gg_24, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_24_tleaf <- mean(gg_24$Tleaf)
gg_24_tleaf
# creating linear model
lm_gg24 <- lm(LEF ~ one.qL, data = gg_24, subset = one.qL < 0.85)
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
    limits = c(0, 60))



### Curve 25
gg_25 <- lrc_24 %>% 
  filter(id == "gg_25")
ggplot(gg_25, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_25_tleaf <- mean(gg_25$Tleaf)
gg_25_tleaf
# creating linear model
lm_gg25 <- lm(LEF ~ one.qL, data = gg_25, subset = one.qL < 1)
# creating slope and intercept
gg_25_slope <- coef(lm_gg25)[2]
gg_25_intercept <- coef(lm_gg25)[1]
# plot
ggplot(gg_25, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_25_slope,
    intercept = gg_25_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 100))



### Curve 26
gg_26 <- lrc_24 %>% 
  filter(id == "gg_26")
ggplot(gg_26, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_26_tleaf <- mean(gg_26$Tleaf)
gg_26_tleaf
# creating linear model
lm_gg26 <- lm(LEF ~ one.qL, data = gg_26, subset = one.qL < 0.95)
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
    limits = c(0, 70))



### Curve 27
gg_27 <- lrc_24 %>% 
  filter(id == "gg_27")
ggplot(gg_27, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_27_tleaf <- mean(gg_27$Tleaf)
gg_27_tleaf
# creating linear model
lm_gg27 <- lm(LEF ~ one.qL, data = gg_27, subset = one.qL < 0.99)
# creating slope and intercept
gg_27_slope <- coef(lm_gg27)[2]
gg_27_intercept <- coef(lm_gg27)[1]
# plot
ggplot(gg_27, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_27_slope,
    intercept = gg_27_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 70))


### Curve 28
gg_28 <- lrc_24 %>% 
  filter(id == "gg_28")
ggplot(gg_28, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_28_tleaf <- mean(gg_28$Tleaf)
gg_28_tleaf
# creating linear model
lm_gg28 <- lm(LEF ~ one.qL, data = gg_28, subset = one.qL < 0.9)
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
gg_29 <- lrc_24 %>% 
  filter(id == "gg_29")
ggplot(gg_29, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_29_tleaf <- mean(gg_29$Tleaf)
gg_29_tleaf
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
    limits = c(0, 100))


### Curve 30
gg_30 <- lrc_24 %>% 
  filter(id == "gg_30")
ggplot(gg_30, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_30_tleaf <- mean(gg_30$Tleaf)
gg_30_tleaf
# creating linear model
lm_gg30 <- lm(LEF ~ one.qL, data = gg_30, subset = one.qL < 0.99)
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
    limits = c(0, 70))


### Curve 31
gg_31 <- lrc_24 %>% 
  filter(id == "gg_31")
ggplot(gg_31, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_31_tleaf <- mean(gg_31$Tleaf)
gg_31_tleaf
# creating linear model
lm_gg31 <- lm(LEF ~ one.qL, data = gg_31, subset = one.qL < 0.8)
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
    limits = c(0, 40))


### Curve 32
gg_32 <- lrc_24 %>% 
  filter(id == "gg_32")
ggplot(gg_32, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_32_tleaf <- mean(gg_32$Tleaf)
gg_32_tleaf
# creating linear model
lm_gg32 <- lm(LEF ~ one.qL, data = gg_32, subset = one.qL < 0.875)
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
    limits = c(0, 50))



### Curve 33
gg_33 <- lrc_24 %>% 
  filter(id == "gg_33")
ggplot(gg_33, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_33_tleaf <- mean(gg_33$Tleaf)
gg_33_tleaf
# creating linear model
lm_gg33 <- lm(LEF ~ one.qL, data = gg_33, subset = one.qL < 0.9)
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
    limits = c(0, 100))



### Curve 34
gg_34 <- lrc_24 %>% 
  filter(id == "gg_34")
ggplot(gg_34, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_34_tleaf <- mean(gg_34$Tleaf)
gg_34_tleaf
# creating linear model
lm_gg34 <- lm(LEF ~ one.qL, data = gg_34, subset = one.qL < 0.99)
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
gg_35 <- lrc_24 %>% 
  filter(id == "gg_35")
ggplot(gg_35, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg35 <- lm(LEF ~ one.qL, data = gg_35, subset = one.qL < 0.9)
# Temp
gg_35_tleaf <- mean(gg_35$Tleaf)
gg_35_tleaf
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
    limits = c(0, 200))



### Curve 36
gg_36 <- lrc_24 %>% 
  filter(id == "gg_36")
ggplot(gg_36, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_36_tleaf <- mean(gg_36$Tleaf)
gg_36_tleaf
# creating linear model
lm_gg36 <- lm(LEF ~ one.qL, data = gg_36, subset = one.qL < 0.8)
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
    limits = c(0, 100))



### Curve 37
gg_37 <- lrc_24 %>% 
  filter(id == "gg_37")
ggplot(gg_37, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_37_tleaf <- mean(gg_37$Tleaf)
gg_37_tleaf
# creating linear model
lm_gg37 <- lm(LEF ~ one.qL, data = gg_37, subset = one.qL < 0.8)
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
    limits = c(0, 100))



### Curve 38
gg_38 <- lrc_24 %>% 
  filter(id == "gg_38")
ggplot(gg_38, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_38_tleaf <- mean(gg_3$Tleaf)
gg_38_tleaf
# creating linear model
lm_gg38 <- lm(LEF ~ one.qL, data = gg_38, subset = one.qL < 0.9)
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
    limits = c(0, 50))



### Curve 39
gg_39 <- lrc_24 %>% 
  filter(id == "gg_39")
ggplot(gg_39, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_39_tleaf <- mean(gg_39$Tleaf)
gg_39_tleaf
# creating linear model
lm_gg39 <- lm(LEF ~ one.qL, data = gg_39, subset = one.qL < 098)
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
    limits = c(0, 50))


### Curve 40
gg_40 <- lrc_24 %>% 
  filter(id == "gg_40")
ggplot(gg_40, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# Temp
gg_40_tleaf <- mean(gg_40$Tleaf)
gg_40_tleaf
# creating linear model
lm_gg40 <- lm(LEF ~ one.qL, data = gg_40, subset = one.qL < 0.9)
# creating slope and intercept
gg_40_slope <- coef(lm_gg40)[2]
gg_40_intercept <- coef(lm_gg40)[1]
# plot
ggplot(gg_40, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_40_slope,
    intercept = gg_40_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 40))


### Creating dataframe
vqmax24_df <- data.frame(
  id = c("gg_2", "gg_3", "gg_4", "gg_5", "gg_6", "gg_7", "gg_8", "gg_9",
         "gg_10", "gg_11", "gg_12", "gg_14", "gg_15", "gg_16", "gg_18", 
         "gg_19", "gg_21", "gg_22", "gg_23", "gg_24", "gg_25", "gg_26", 
         "gg_27", "gg_28", "gg_29", "gg_30", "gg_31", "gg_32", "gg_33", "gg_34", 
         "gg_35", "gg_36", "gg_37", "gg_38", "gg_39", "gg_40" ),
  slope_24 = c(gg_2_slope, gg_3_slope, gg_4_slope, gg_5_slope, gg_6_slope, gg_7_slope, gg_8_slope, gg_9_slope,
              gg_10_slope, gg_11_slope, gg_12_slope, gg_14_slope, gg_15_slope, gg_16_slope, gg_18_slope, 
              gg_19_slope, gg_21_slope, gg_22_slope, gg_23_slope, gg_24_slope, gg_25_slope, gg_26_slope, 
              gg_27_slope, gg_28_slope, gg_29_slope, gg_30_slope, gg_31_slope, gg_32_slope, gg_33_slope, gg_34_slope, 
              gg_35_slope, gg_36_slope, gg_37_slope, gg_38_slope, gg_39_slope, gg_40_slope),
  intercept_24 = c(gg_2_intercept, gg_3_intercept, gg_4_intercept, gg_5_intercept, gg_6_intercept, gg_7_intercept, gg_8_intercept, gg_9_intercept,
              gg_10_intercept, gg_11_intercept, gg_12_intercept, gg_14_intercept, gg_15_intercept, gg_16_intercept, gg_18_intercept, 
              gg_19_intercept, gg_21_intercept, gg_22_intercept, gg_23_intercept, gg_24_intercept, gg_25_intercept, gg_26_intercept, 
              gg_27_intercept, gg_28_intercept, gg_29_intercept, gg_30_intercept, gg_31_intercept, gg_32_intercept, gg_33_intercept, gg_34_intercept, 
              gg_35_intercept, gg_36_intercept, gg_37_intercept, gg_38_intercept, gg_39_intercept, gg_40_intercept),
  tleaf_24 = c(gg_2_tleaf, gg_3_tleaf, gg_4_tleaf, gg_5_tleaf, gg_6_tleaf, gg_7_tleaf, gg_8_tleaf, gg_9_tleaf,
                   gg_10_tleaf, gg_11_tleaf, gg_12_tleaf, gg_14_tleaf, gg_15_tleaf, gg_16_tleaf, gg_18_tleaf, 
                   gg_19_tleaf, gg_21_tleaf, gg_22_tleaf, gg_23_tleaf, gg_24_tleaf, gg_25_tleaf, gg_26_tleaf, 
                   gg_27_tleaf, gg_28_tleaf, gg_29_tleaf, gg_30_tleaf, gg_31_tleaf, gg_32_tleaf, gg_33_tleaf, gg_34_tleaf, 
                   gg_35_tleaf, gg_36_tleaf, gg_37_tleaf, gg_38_tleaf, gg_39_tleaf, gg_40_tleaf)
)
vqmax24_df

### calculating Vqmax
vqmax24_df$vqmax_24 <- vqmax24_df$intercept + vqmax24_df$slope
vqmax24_df

### adding treatment column
vqmax24_df$replicate <- as.numeric(gsub("\\D", "", vqmax24_df$id))
vqmax24_df$treatment <- ifelse(vqmax24_df$replicate %% 2 == 1,
                               "25c",
                               "35c")
vqmax24_df$replicate <- NULL
vqmax24_df

### add leaf temp
vqmax24_df$tleaf_fac <- "24c"
vqmax24_df


### graph and stats time
ggplot(vqmax24_df, aes(x = treatment, y = vqmax_24)) + geom_boxplot() 
vqmax24_lm <- lm(vqmax_24 ~ treatment, data = vqmax24_df)
anova(vqmax24_lm)
plot(vqmax24_lm)

### write .csv
write.csv(vqmax24_df, "vqmax24_df.csv")



### Load Libraries
library(tidyverse)

### read in .csv
lrc_36 <- read.csv("lrc_36.csv")

### Curve 1 bad

### Curve 2
gg_2 <- lrc_36 %>% 
  filter(id == "gg_2")
ggplot(gg_2, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg2 <- lm(LEF ~ one.qL, data = gg_2, subset = one.qL < 0.7)
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
    limits = c(0, 60))



### Curve 3
gg_3 <- lrc_36 %>% 
  filter(id == "gg_3")
ggplot(gg_3, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg3 <- lm(LEF ~ one.qL, data = gg_3, subset = one.qL < 0.7)
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
    limits = c(0, 150))



### Curve 4
gg_4 <- lrc_36 %>% 
  filter(id == "gg_4")
ggplot(gg_4, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg4 <- lm(LEF ~ one.qL, data = gg_4, subset = one.qL < 0.7)
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
gg_5 <- lrc_36 %>% 
  filter(id == "gg_5")
ggplot(gg_5, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
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
    limits = c(0, 150))


### Curve 6
gg_6 <- lrc_36 %>% 
  filter(id == "gg_6")
ggplot(gg_6, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg6 <- lm(LEF ~ one.qL, data = gg_6, subset = one.qL < 0.8)
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
    limits = c(0, 120))



### Curve 7
gg_7 <- lrc_36 %>% 
  filter(id == "gg_7")
ggplot(gg_7, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg7 <- lm(LEF ~ one.qL, data = gg_7, subset = one.qL < 0.8)
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
    limits = c(0, 200))


### Curve 8
gg_8 <- lrc_36 %>% 
  filter(id == "gg_8")
gg_8 <- gg_8 %>% 
  filter(machine == "stan")
ggplot(gg_8, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg8 <- lm(LEF ~ one.qL, data = gg_8, subset = one.qL < 0.7)
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
    limits = c(0, 150))




### Curve 9
gg_9 <- lrc_36 %>% 
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
    limits = c(0, 150))



### Curve 10
gg_10 <- lrc_36 %>% 
  filter(id == "gg_10")
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
    limits = c(0, 50))


### Curve 11
gg_11 <- lrc_36 %>% 
  filter(id == "gg_11")
ggplot(gg_11, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
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
gg_12 <- lrc_36 %>% 
  filter(id == "gg_12")
gg_12 <- gg_12 %>% 
  filter(machine == "stan")
ggplot(gg_12, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg12 <- lm(LEF ~ one.qL, data = gg_12, subset = one.qL < 0.7)
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
    limits = c(0, 80))


### Curve 13 no data

### Curve 14
gg_14 <- lrc_36 %>% 
  filter(id == "gg_14")
gg_14 <- gg_14 %>%
  filter(machine == "ozzie")
ggplot(gg_14, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg14 <- lm(LEF ~ one.qL, data = gg_14, subset = one.qL < 0.8)
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
    limits = c(0, 80))



### Curve 15
gg_15 <- lrc_36 %>% 
  filter(id == "gg_15")
gg_15_filtered <- gg_15 %>%
  filter(machine == "albert")
ggplot(gg_15_filtered, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg15 <- lm(LEF ~ one.qL, data = gg_15_filtered, subset = one.qL < 0.8)
# creating slope and intercept
gg_15_slope <- coef(lm_gg15)[2]
gg_15_intercept <- coef(lm_gg15)[1]
# plot
ggplot(gg_15_filtered, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_15_slope,
    intercept = gg_15_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 150))



### Curve 16 & Curve 17 corrupted



### Curve 18
gg_18 <- lrc_36 %>% 
  filter(id == "gg_18")
ggplot(gg_18, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg18 <- lm(LEF ~ one.qL, data = gg_18, subset = one.qL < 0.75)
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
    limits = c(0, 60))



### Curve 19
gg_19 <- lrc_36 %>% 
  filter(id == "gg_19")
ggplot(gg_19, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
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
    limits = c(0, 150))


### Curve 20 corrupted



### Curve 21
gg_21 <- lrc_36 %>% 
  filter(id == "gg_21")
ggplot(gg_21, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
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
    limits = c(0, 150))


### Curve 22 corrupted


### Curve 23
gg_23 <- lrc_36 %>% 
  filter(id == "gg_23")
ggplot(gg_23, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg23 <- lm(LEF ~ one.qL, data = gg_23, subset = one.qL < 0.7)
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
    limits = c(0, 120))



### Curve 24
gg_24 <- lrc_36 %>% 
  filter(id == "gg_24")
ggplot(gg_24, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg24 <- lm(LEF ~ one.qL, data = gg_24, subset = one.qL < 0.75)
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



### Curve 25 from 15, not sure but it is a good 25c curve
gg_25 <- gg_15 %>%
  filter(machine == "albert")
ggplot(gg_25, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg25 <- lm(LEF ~ one.qL, data = gg_25, subset = one.qL < 0.8)
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
    limits = c(0, 150))


### Curve 26
gg_26 <- lrc_36 %>% 
  filter(id == "gg_26")
ggplot(gg_26, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg26 <- lm(LEF ~ one.qL, data = gg_26, subset = one.qL < 0.7)
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
    limits = c(0, 80))


### Curve 27 no data


### Curve 28
gg_28 <- lrc_36 %>% 
  filter(id == "gg_28")
ggplot(gg_28, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg28 <- lm(LEF ~ one.qL, data = gg_28, subset = one.qL < 0.7)
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
    limits = c(0, 60))



### Curve 29 no data



### Curve 30 corrupted



#### Curve 31
gg_31 <- lrc_36 %>% 
  filter(id == "gg_31")
ggplot(gg_31, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg31 <- lm(LEF ~ one.qL, data = gg_31, subset = one.qL < 0.9)
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
    limits = c(0, 250))



### Curve 32
gg_32 <- lrc_36 %>% 
  filter(id == "gg_32")
ggplot(gg_32, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg32 <- lm(LEF ~ one.qL, data = gg_32, subset = one.qL < 0.7)
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
    limits = c(0, 40))



### Curve 33
gg_33 <- lrc_36 %>% 
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
    limits = c(0, 150))



### Curve 34 no data



### Curve 35
gg_35 <- lrc_36 %>% 
  filter(id == "gg_35")
ggplot(gg_35, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg35 <- lm(LEF ~ one.qL, data = gg_35, subset = one.qL < 0.8)
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
    limits = c(0, 300))



### Curve 36
gg_36 <- lrc_36 %>% 
  filter(id == "gg_36")
ggplot(gg_36, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
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
gg_37 <- lrc_36 %>% 
  filter(id == "gg_37")
ggplot(gg_37, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg37 <- lm(LEF ~ one.qL, data = gg_37, subset = one.qL < 0.9)
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
    limits = c(0, 60))



### Curve 38
gg_38 <- lrc_36 %>% 
  filter(id == "gg_38")
ggplot(gg_38, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg38 <- lm(LEF ~ one.qL, data = gg_38, subset = one.qL < 0.6)
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
    limits = c(0, 100))



### Curve 39
gg_39 <- lrc_36 %>% 
  filter(id == "gg_39")
ggplot(gg_39, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg39 <- lm(LEF ~ one.qL, data = gg_39, subset = one.qL < 0.7)
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
    limits = c(0, 80))



### Curve 40
gg_40 <- lrc_36 %>% 
  filter(id == "gg_40")
ggplot(gg_40, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg40 <- lm(LEF ~ one.qL, data = gg_40, subset = one.qL < 0.7)
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
    limits = c(0, 80))



### Creating dataframe
vqmax36_df <- data.frame(
  id = c("gg_2", "gg_3", "gg_4" , "gg_5", "gg_6", "gg_7", "gg_8", "gg_9", "gg_10", "gg_11",
         "gg_12", "gg_14", "gg_15", "gg_18", "gg_19", "gg_21", "gg_23", "gg_24", "gg_25",
         "gg_26", "gg_28", "gg_31", "gg_33", "gg_35", "gg_37", "gg_38", "gg_39", "gg_40"),
  slope_30 = c(gg_2_slope, gg_3_slope, gg_4_slope, gg_5_slope, gg_6_slope, gg_7_slope, 
                   gg_8_slope, gg_9_slope, gg_10_slope, gg_11_slope, gg_12_slope, gg_14_slope, 
                   gg_15_slope, gg_18_slope, gg_19_slope, gg_21_slope, gg_23_slope, gg_24_slope, 
                   gg_25_slope, gg_26_slope, gg_28_slope, gg_31_slope, gg_33_slope, gg_35_slope, 
                   gg_37_slope, gg_38_slope, gg_39_slope, gg_40_slope),
  intercept_30 = c(gg_2_intercept, gg_3_intercept, gg_4_intercept, gg_5_intercept, gg_6_intercept, gg_7_intercept, 
               gg_8_intercept, gg_9_intercept, gg_10_intercept, gg_11_intercept, gg_12_intercept, gg_14_intercept, 
               gg_15_intercept, gg_18_intercept, gg_19_intercept, gg_21_intercept, gg_23_intercept, gg_24_intercept, 
               gg_25_intercept, gg_26_intercept, gg_28_intercept, gg_31_intercept, gg_33_intercept, gg_35_intercept, 
               gg_37_intercept, gg_38_intercept, gg_39_intercept, gg_40_intercept)
)
vqmax36_df


### calculating Vqmax
vqmax36_df$vqmax_36 <- vqmax36_df$intercept + vqmax36_df$slope
vqmax36_df


### adding treatment column
vqmax36_df$replicate <- as.numeric(gsub("\\D", "", vqmax36_df$id))
vqmax36_df$treatment <- ifelse(vqmax36_df$replicate %% 2 == 1,
                               "25c",
                               "35c")
vqmax36_df$replicate <- NULL
vqmax36_df

### add leaf temp
vqmax36_df$tleaf_fac <- "36c"
vqmax36_df

### graph and stats time
ggplot(vqmax36_df, aes(x = treatment, y = vqmax_36)) + geom_boxplot()
vqmax36_lm <- lm(vqmax_36 ~ treatment, data = vqmax36_df)
anova(vqmax36_lm)
plot(vqmax36_lm)

#write.csv
write.csv(vqmax36_df, "vqmax36_df.csv")

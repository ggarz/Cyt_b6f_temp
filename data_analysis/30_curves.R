### Load Libraries
library(tidyverse)

### read in .csv
lrc_30 <- read.csv("lrc_30.csv")


### no data for gg_1 and gg_2

### Curve 3
gg_3 <- lrc_30 %>% 
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
    limits = c(0, 250))



### Curve 4
gg_4 <- lrc_30 %>% 
  filter(id == "gg_4")
ggplot(gg_4, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg4 <- lm(LEF ~ one.qL, data = gg_4, subset = one.qL < 0.9)
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



### no curves for gg_5

### Curve 6
gg_6 <- lrc_30 %>% 
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
gg_7 <- lrc_30 %>% 
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
    limits = c(0, 150))



### Curve 8
gg_8 <- lrc_30 %>% 
  filter(id == "gg_8")
ggplot(gg_8, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
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
    limits = c(0, 150))


### Curve 9
gg_9 <- lrc_30 %>% 
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
gg_10 <- lrc_30 %>% 
  filter(id == "gg_10")
ggplot(gg_10, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg10 <- lm(LEF ~ one.qL, data = gg_10, subset = one.qL < 0.9)
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
gg_11 <- lrc_30 %>% 
  filter(id == "gg_11")
ggplot(gg_11, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg11 <- lm(LEF ~ one.qL, data = gg_11, subset = one.qL < 0.85)
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
gg_12 <- lrc_30 %>% 
  filter(id == "gg_12")
ggplot(gg_12, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
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
    limits = c(0, 60))


### Curve 13 has no data...

### Curve 14
gg_14 <- lrc_30 %>% 
  filter(id == "gg_14")
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
    limits = c(0, 60))

### no data for 15

### Curve 16
gg_16 <- lrc_30 %>% 
  filter(id == "gg_16")
gg_16 <- gg_16 %>% 
  filter(machine == "stan")
ggplot(gg_16, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg16 <- lm(LEF ~ one.qL, data = gg_16, subset = one.qL < 0.9)
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
    limits = c(0, 150))



### no data for Curve 17...

### Curve 18
gg_18 <- lrc_30 %>% 
  filter(id == "gg_18")
ggplot(gg_18, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg18 <- lm(LEF ~ one.qL, data = gg_18, subset = one.qL < 0.85)
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


### Curve 19
gg_19 <- lrc_30 %>% 
  filter(id == "gg_19")
ggplot(gg_19, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg19 <- lm(LEF ~ one.qL, data = gg_19, subset = one.qL < 0.8)
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


### Curve 20 & 21 no data

### Curve 22
gg_22 <- lrc_30 %>% 
  filter(id == "gg_22")
ggplot(gg_22, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg22 <- lm(LEF ~ one.qL, data = gg_22, subset = one.qL < 0.7)
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
    limits = c(0, 60))



### Curve 23
gg_23 <- lrc_30 %>% 
  filter(id == "gg_23")
ggplot(gg_23, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
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
    limits = c(0, 150))



### Curve 24
gg_24 <- lrc_30 %>% 
  filter(id == "gg_24")
ggplot(gg_24, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg24 <- lm(LEF ~ one.qL, data = gg_24, subset = one.qL < 0.9)
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
gg_25 <- lrc_30 %>% 
  filter(id == "gg_25")
ggplot(gg_25, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg25 <- lm(LEF ~ one.qL, data = gg_25, subset = one.qL < 0.91)
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
gg_26 <- lrc_30 %>% 
  filter(id == "gg_26")
ggplot(gg_26, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg26 <- lm(LEF ~ one.qL, data = gg_26, subset = one.qL < 0.75)
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
gg_27 <- lrc_30 %>% 
  filter(id == "gg_27")
ggplot(gg_27, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg27 <- lm(LEF ~ one.qL, data = gg_27, subset = one.qL < 0.9)
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
    limits = c(0, 100))



### Curve 28 corrupted

### Curve 29
gg_29 <- lrc_30 %>% 
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
    limits = c(0, 100))


### Curve 30
gg_30 <- lrc_30 %>% 
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
    limits = c(0, 60))



### Curve 31
gg_31 <- lrc_30 %>% 
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



### Curve 32 (may have been included w/ gg_34... still 35c)
gg_34 <- lrc_30 %>% 
  filter(id == "gg_34")
gg_32 <- gg_34 %>% 
  filter(machine == "ozzie")
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
    limits = c(0, 150))



### Curve 33
gg_33 <- lrc_30 %>% 
  filter(id == "gg_33")
ggplot(gg_33, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
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
    limits = c(0, 150))



### Curve 34
gg_34 <- lrc_30 %>% 
  filter(id == "gg_34")
gg_34_filtered <- gg_34 %>% 
  filter(machine == "stan")
ggplot(gg_34_filtered, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg34 <- lm(LEF ~ one.qL, data = gg_34_filtered, subset = one.qL < 0.85)
# creating slope and intercept
gg_34_slope <- coef(lm_gg34)[2]
gg_34_intercept <- coef(lm_gg34)[1]
# plot
ggplot(gg_34_filtered, aes(one.qL, LEF)) +
  geom_point() +
  geom_abline(
    slope = gg_34_slope,
    intercept = gg_34_intercept) +
  scale_x_continuous(
    limits = c(0, 1)) + 
  scale_y_continuous(
    limits = c(0, 60))



### Curve 35
gg_35 <- lrc_30 %>% 
  filter(id == "gg_35")
ggplot(gg_35, aes(x = one.qL, y = LEF, color = treatment, group = id)) + geom_point() + geom_line()
# creating linear model
lm_gg35 <- lm(LEF ~ one.qL, data = gg_35, subset = one.qL < 0.6)
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


### Curve 36 is corrupted


### Curve 37
gg_37 <- lrc_30 %>% 
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
    limits = c(0, 50))

### curve 38-39 no data


### Curve 40
gg_40 <- lrc_30 %>% 
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
    limits = c(0, 30))



### Creating dataframe
vqmax30_df <- data.frame(
  id = c("gg_3", "gg_4", "gg_6", "gg_7", "gg_8", "gg_9",
         "gg_10", "gg_11", "gg_12", "gg_14", "gg_16", "gg_18", 
         "gg_19", "gg_22", "gg_23", "gg_24", "gg_25", "gg_26", 
         "gg_27", "gg_29", "gg_30", "gg_31", "gg_32", "gg_33",
         "gg_34", "gg_35", "gg_37", "gg_40" ),
  slope_30 = c(gg_3_slope, gg_4_slope, gg_6_slope, gg_7_slope, gg_8_slope, gg_9_slope,
            gg_10_slope, gg_11_slope, gg_12_slope, gg_14_slope, gg_16_slope, gg_18_slope, 
            gg_19_slope, gg_22_slope, gg_23_slope, gg_24_slope, gg_25_slope, gg_26_slope, 
            gg_27_slope, gg_29_slope, gg_30_slope, gg_31_slope, gg_32_slope, gg_33_slope, 
            gg_34_slope, gg_35_slope, gg_37_slope, gg_40_slope),
  intercept_30 = c(gg_3_intercept, gg_4_intercept, gg_6_intercept, gg_7_intercept, gg_8_intercept,
            gg_9_intercept, gg_10_intercept, gg_11_intercept, gg_12_intercept, gg_14_intercept, 
            gg_16_intercept, gg_18_intercept, gg_19_intercept, gg_22_intercept, gg_23_intercept,
            gg_24_intercept, gg_25_intercept, gg_26_intercept, gg_27_intercept, gg_29_intercept, 
            gg_30_intercept,  gg_31_intercept, gg_32_intercept, gg_33_intercept, gg_34_intercept, 
            gg_35_intercept, gg_37_intercept, gg_40_intercept)
)
vqmax30_df


### calculating Vqmax
vqmax30_df$vqmax_30 <- vqmax30_df$intercept + vqmax30_df$slope
vqmax30_df


### adding treatment column
vqmax30_df$replicate <- as.numeric(gsub("\\D", "", vqmax30_df$id))
vqmax30_df$treatment <- ifelse(vqmax30_df$replicate %% 2 == 1,
                              "25c",
                              "35c")
vqmax30_df$replicate <- NULL
vqmax30_df


### add leaf temp
vqmax30_df$tleaf_fac <- "30c"
vqmax30_df


### graph and stats time
ggplot(vqmax30_df, aes(x = treatment, y = vqmax_30)) + geom_boxplot()
vqmax30_lm <- lm(vqmax_30 ~ treatment, data = vqmax30_df)
anova(vqmax30_lm)
plot(vqmax30_lm)

### write .csv
write.csv(vqmax30_df, "vqmax30_df.csv")

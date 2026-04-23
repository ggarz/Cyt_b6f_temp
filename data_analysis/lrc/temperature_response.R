### Load Libraries
library(tidyverse)
library(emmeans)
library(multcomp)
library(multcompView)
library(lme4)

### Read .csv
vqmax18_df <- read.csv("vqmax18_df.csv")
vqmax24_df <- read.csv("vqmax24_df.csv")
vqmax30_df <- read.csv("vqmax30_df.csv")
vqmax36_df <- read.csv("vqmax36_df.csv")
vqmax42_df <- read.csv("vqmax42_df.csv")
vqmax48_df <- read.csv("vqmax48_df.csv")

### check
head(vqmax18_df)
head(vqmax24_df)
head(vqmax30_df)
head(vqmax36_df)
head(vqmax42_df)
head(vqmax48_df)

### rbind
colnames(vqmax18_df) <- c("x", 'id', 'slope', 'intercept', 'tleaf', 'vqmax', 'treatment', 'tleaf_fac')
colnames(vqmax24_df) <- c("x", 'id', 'slope', 'intercept', 'tleaf', 'vqmax', 'treatment', 'tleaf_fac')
colnames(vqmax30_df) <- c("x", 'id', 'slope', 'intercept', 'tleaf', 'vqmax', 'treatment', 'tleaf_fac')
colnames(vqmax36_df) <- c("x", 'id', 'slope', 'intercept', 'tleaf', 'vqmax', 'treatment', 'tleaf_fac')
colnames(vqmax42_df) <- c("x", 'id', 'slope', 'intercept', 'tleaf', 'vqmax', 'treatment', 'tleaf_fac')
colnames(vqmax48_df) <- c("x", 'id', 'slope', 'intercept', 'tleaf', 'vqmax', 'treatment', 'tleaf_fac')

### check again
head(vqmax18_df)
head(vqmax24_df)
head(vqmax30_df)
head(vqmax36_df)
head(vqmax42_df)
head(vqmax48_df)

### create master
master_df <- rbind(vqmax18_df, vqmax24_df, vqmax30_df, vqmax36_df, vqmax42_df, vqmax48_df)
head(master_df)

vqmax_lm <- lm(vqmax ~ treatment * tleaf_fac, data = master_df)
summary(vqmax_lm)
plot(resid(vqmax_lm) ~ fitted(vqmax_lm))
anova(vqmax_lm)

cld(emmeans(vqmax_lm, pairwise ~ treatment))
cld(emmeans(vqmax_lm, pairwise ~ tleaf_fac))
cld(emmeans(vqmax_lm, pairwise ~ tleaf_fac | treatment))

### plotting with ggplot
ggplot(master_df, aes(x = tleaf_fac, y = vqmax, fill = treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("25c" = "#0072B2", "35c" = "#C44E52"))
  theme(
    legend.title = element_blank(), 
    legend.text = element_text(size = rel(1.5)),
    axis.title.x = element_text(size = rel(2.75)),
    axis.title.y = element_text(size = rel(2.75)),
    axis.text.x = element_text(size = rel(2.25)),
    axis.text.y = element_text(size = rel(2.25)),
    panel.background = element_rect(fill = 'white', colour = 'black'),
    panel.grid.major = element_line(colour = 'grey')) +
  xlab("Leaf temperature (factorial)(°C)") +
  ylab(expression("Vqmax"))


### creatting curves...
# adding tresp2
master_df$tleaf2 <- master_df$tleaf^2
head(master_df)



### gg_1 
gg_1 <- master_df %>% 
  filter(id == "gg_1")
head(gg_1)
# only has 2 points, no go



### gg_2
gg_2 <- master_df %>% 
  filter(id == "gg_2")
head(gg_2)
# missing only 30, good enough
gg_2_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_2)
summary(gg_2_lm)
# coef
gg_2_coef <- gg_2_lm$coefficients
gg_2_coef
# t opt
gg_2_topt <- -gg_2_coef[2] / (2 * gg_2_coef[3])
gg_2_topt
# adding coef to df
gg_2_df <- data.frame(
  id = "gg_2",
  treatment = "35c",
  a = gg_2_coef[1],
  b = gg_2_coef[2],
  c = gg_2_coef[3],
  topt = gg_2_topt
)
gg_2_df



### gg_3
gg_3 <- master_df %>% 
  filter(id == "gg_3")
head(gg_3)
# got all 6
gg_3_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_3)
summary(gg_3_lm)
# coef
gg_3_coef <- gg_3_lm$coefficients
gg_3_coef
# t opt
gg_3_topt <- -gg_3_coef[2] / (2 * gg_3_coef[3])
gg_3_topt
# adding coef to df
gg_3_df <- data.frame(
  id = "gg_3",
  treatment = "25c",
  a = gg_3_coef[1],
  b = gg_3_coef[2],
  c = gg_3_coef[3],
  topt = gg_3_topt
)
gg_3_df



### gg_4
gg_4 <- master_df %>% 
  filter(id == "gg_4")
head(gg_4)
# missing 48, might ask
gg_4_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_4)
summary(gg_4_lm)
# coef
gg_4_coef <- gg_4_lm$coefficients
gg_4_coef
# t opt
gg_4_topt <- -gg_4_coef[2] / (2 * gg_4_coef[3])
gg_4_topt
# adding coef to df
gg_4_df <- data.frame(
  id = "gg_4",
  treatment = "35c",
  a = gg_4_coef[1],
  b = gg_4_coef[2],
  c = gg_4_coef[3],
  topt = gg_4_topt
)
gg_4_df



### gg_5
gg_5 <- master_df %>% 
  filter(id == "gg_5")
head(gg_5)
# missing 30, should be fine
gg_5_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_5)
summary(gg_5_lm)
# coef
gg_5_coef <- gg_5_lm$coefficients
gg_5_coef
# t opt
gg_5_topt <- -gg_5_coef[2] / (2 * gg_5_coef[3])
gg_5_topt
# adding coef to df
gg_5_df <- data.frame(
  id = "gg_5",
  treatment = "25c",
  a = gg_5_coef[1],
  b = gg_5_coef[2],
  c = gg_5_coef[3],
  topt = gg_5_topt
)
gg_5_df



### gg_6
gg_6 <- master_df %>% 
  filter(id == "gg_6")
head(gg_6)
# all 6
gg_6_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_6)
summary(gg_6_lm)
# coef
gg_6_coef <- gg_6_lm$coefficients
gg_6_coef
# t opt
gg_6_topt <- -gg_6_coef[2] / (2 * gg_6_coef[3])
gg_6_topt
# adding coef to df
gg_6_df <- data.frame(
  id = "gg_6",
  treatment = "35c",
  a = gg_6_coef[1],
  b = gg_6_coef[2],
  c = gg_6_coef[3],
  topt = gg_6_topt
)
gg_6_df



### gg_7
gg_7 <- master_df %>% 
  filter(id == "gg_7")
head(gg_7)
# all 6
gg_7_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_7)
summary(gg_7_lm)
# coef
gg_7_coef <- gg_7_lm$coefficients
gg_7_coef
# t opt
gg_7_topt <- -gg_7_coef[2] / (2 * gg_7_coef[3])
gg_7_topt
# adding coef to df
gg_7_df <- data.frame(
  id = "gg_7",
  treatment = "25c",
  a = gg_7_coef[1],
  b = gg_7_coef[2],
  c = gg_7_coef[3],
  topt = gg_7_topt
)
gg_7_df



### gg_8
gg_8 <- master_df %>% 
  filter(id == "gg_8")
head(gg_8)
# missing 42 and 48... dont use for now



### gg_9
gg_9 <- master_df %>% 
  filter(id == "gg_9")
head(gg_9)
# missing 30, should be fine
gg_9_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_9)
summary(gg_9_lm)
# coef
gg_9_coef <- gg_9_lm$coefficients
gg_9_coef
# t opt
gg_9_topt <- -gg_9_coef[2] / (2 * gg_9_coef[3])
gg_9_topt
# adding coef to df
gg_9_df <- data.frame(
  id = "gg_9",
  treatment = "25c",
  a = gg_9_coef[1],
  b = gg_9_coef[2],
  c = gg_9_coef[3],
  topt = gg_9_topt
)
gg_9_df



### gg_10
gg_10 <- master_df %>% 
  filter(id == "gg_10")
head(gg_10)
# all 6
gg_10_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_10)
summary(gg_10_lm)
# coef
gg_10_coef <- gg_10_lm$coefficients
gg_10_coef
# t opt
gg_10_topt <- -gg_10_coef[2] / (2 * gg_10_coef[3])
gg_10_topt
# adding coef to df
gg_10_df <- data.frame(
  id = "gg_10",
  treatment = "35c",
  a = gg_10_coef[1],
  b = gg_10_coef[2],
  c = gg_10_coef[3],
  topt = gg_10_topt
)
gg_10_df



### gg_11
gg_11 <- master_df %>% 
  filter(id == "gg_11")
head(gg_11)
# missing 42 and 48, dont use for now



### gg_12
gg_12 <- master_df %>% 
  filter(id == "gg_12")
head(gg_12)
# all 6
gg_12_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_12)
summary(gg_12_lm)
# coef
gg_12_coef <- gg_12_lm$coefficients
gg_12_coef
# t opt
gg_12_topt <- -gg_12_coef[2] / (2 * gg_12_coef[3])
gg_12_topt
# adding coef to df
gg_12_df <- data.frame(
  id = "gg_12",
  treatment = "35c",
  a = gg_12_coef[1],
  b = gg_12_coef[2],
  c = gg_12_coef[3],
  topt = gg_12_topt
)
gg_12_df



### gg_13
gg_13 <- master_df %>% 
  filter(id == "gg_13")
head(gg_13)
# nothing for 13...



### gg_14
gg_14 <- master_df %>% 
  filter(id == "gg_14")
head(gg_14)
# all 6
gg_14_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_14)
summary(gg_14_lm)
# coef
gg_14_coef <- gg_14_lm$coefficients
gg_14_coef
# t opt
gg_14_topt <- -gg_14_coef[2] / (2 * gg_14_coef[3])
gg_14_topt
# adding coef to df
gg_14_df <- data.frame(
  id = "gg_14",
  treatment = "35c",
  a = gg_14_coef[1],
  b = gg_14_coef[2],
  c = gg_14_coef[3],
  topt = gg_14_topt
)
gg_14_df



### gg_15
gg_15 <- master_df %>% 
  filter(id == "gg_15")
head(gg_15)
# missing 30 and 42... proceed?
gg_15_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_15)
summary(gg_15_lm)
# coef
gg_15_coef <- gg_15_lm$coefficients
gg_15_coef
# t opt
gg_15_topt <- -gg_15_coef[2] / (2 * gg_15_coef[3])
gg_15_topt
# adding coef to df
gg_15_df <- data.frame(
  id = "gg_15",
  treatment = "25c",
  a = gg_15_coef[1],
  b = gg_15_coef[2],
  c = gg_15_coef[3],
  topt = gg_15_topt
)
gg_15_df



### gg_16
gg_16 <- master_df %>% 
  filter(id == "gg_16")
head(gg_16)
# all missing 36...
gg_16_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_16)
summary(gg_16_lm)
# coef
gg_16_coef <- gg_16_lm$coefficients
gg_16_coef
# t opt
gg_16_topt <- -gg_16_coef[2] / (2 * gg_16_coef[3])
gg_16_topt
# adding coef to df
gg_16_df <- data.frame(
  id = "gg_16",
  treatment = "35c",
  a = gg_16_coef[1],
  b = gg_16_coef[2],
  c = gg_16_coef[3],
  topt = gg_16_topt
)
gg_16_df




### gg_17
gg_17 <- master_df %>% 
  filter(id == "gg_17")
head(gg_17)
# no points...



### gg_18
gg_18 <- master_df %>% 
  filter(id == "gg_18")
head(gg_18)
# all 6
gg_18_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_18)
summary(gg_18_lm)
# coef
gg_18_coef <- gg_18_lm$coefficients
gg_18_coef
# t opt
gg_18_topt <- -gg_18_coef[2] / (2 * gg_18_coef[3])
gg_18_topt
# adding coef to df
gg_18_df <- data.frame(
  id = "gg_18",
  treatment = "35c",
  a = gg_18_coef[1],
  b = gg_18_coef[2],
  c = gg_18_coef[3],
  topt = gg_18_topt
)
gg_18_df



### gg_19
gg_19 <- master_df %>% 
  filter(id == "gg_19")
head(gg_19)
# missing 42 and 48...



### gg_20
gg_20 <- master_df %>% 
  filter(id == "gg_20")
head(gg_20)
# missing all...



### gg_21
gg_21 <- master_df %>% 
  filter(id == "gg_21")
head(gg_21)
# all missing 30
gg_21_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_21)
summary(gg_21_lm)
# coef
gg_21_coef <- gg_21_lm$coefficients
gg_21_coef
# t opt
gg_21_topt <- -gg_21_coef[2] / (2 * gg_21_coef[3])
gg_21_topt
# adding coef to df
gg_21_df <- data.frame(
  id = "gg_21",
  treatment = "25c",
  a = gg_21_coef[1],
  b = gg_21_coef[2],
  c = gg_21_coef[3],
  topt = gg_21_topt
)
gg_21_df



### gg_22
gg_22 <- master_df %>% 
  filter(id == "gg_22")
head(gg_22)
# missing 36, proceed
gg_22_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_22)
summary(gg_22_lm)
# coef
gg_22_coef <- gg_22_lm$coefficients
gg_22_coef
# t opt
gg_22_topt <- -gg_22_coef[2] / (2 * gg_22_coef[3])
gg_22_topt
# adding coef to df
gg_22_df <- data.frame(
  id = "gg_22",
  treatment = "35c",
  a = gg_22_coef[1],
  b = gg_22_coef[2],
  c = gg_22_coef[3],
  topt = gg_22_topt
)
gg_22_df



### gg_23
gg_23 <- master_df %>% 
  filter(id == "gg_23")
head(gg_23)
# missing 42 should be fine
gg_23_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_23)
summary(gg_23_lm)
# coef
gg_23_coef <- gg_23_lm$coefficients
gg_23_coef
# t opt
gg_23_topt <- -gg_23_coef[2] / (2 * gg_23_coef[3])
gg_23_topt
# adding coef to df
gg_23_df <- data.frame(
  id = "gg_23",
  treatment = "25c",
  a = gg_23_coef[1],
  b = gg_23_coef[2],
  c = gg_23_coef[3],
  topt = gg_23_topt
)
gg_23_df



### gg_24
gg_24 <- master_df %>% 
  filter(id == "gg_24")
head(gg_24)
# all 6
gg_24_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_24)
summary(gg_24_lm)
# coef
gg_24_coef <- gg_24_lm$coefficients
gg_24_coef
# t opt
gg_24_topt <- -gg_24_coef[2] / (2 * gg_24_coef[3])
gg_24_topt
# adding coef to df
gg_24_df <- data.frame(
  id = "gg_24",
  treatment = "35c",
  a = gg_24_coef[1],
  b = gg_24_coef[2],
  c = gg_24_coef[3],
  topt = gg_24_topt
)
gg_24_df



### gg_25
gg_25 <- master_df %>% 
  filter(id == "gg_25")
head(gg_25)
# missing 48, should be fine
gg_25_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_25)
summary(gg_25_lm)
# coef
gg_25_coef <- gg_25_lm$coefficients
gg_25_coef
# t opt
gg_25_topt <- -gg_25_coef[2] / (2 * gg_25_coef[3])
gg_25_topt
# adding coef to df
gg_25_df <- data.frame(
  id = "gg_25",
  treatment ="25c",
  a = gg_25_coef[1],
  b = gg_25_coef[2],
  c = gg_25_coef[3],
  topt = gg_25_topt
)
gg_25_df



### gg_26
gg_26 <- master_df %>% 
  filter(id == "gg_26")
head(gg_26)
# all 6
gg_26_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_26)
summary(gg_26_lm)
# coef
gg_26_coef <- gg_26_lm$coefficients
gg_26_coef
# t opt
gg_26_topt <- -gg_26_coef[2] / (2 * gg_26_coef[3])
gg_26_topt
# adding coef to df
gg_26_df <- data.frame(
  id = "gg_26",
  treatment = "35c",
  a = gg_26_coef[1],
  b = gg_26_coef[2],
  c = gg_26_coef[3],
  topt = gg_26_topt
)
gg_26_df




### gg_27
gg_27 <- master_df %>% 
  filter(id == "gg_27")
head(gg_27)
# missing 42 and 48... leave out



### gg_28
gg_28 <- master_df %>% 
  filter(id == "gg_28")
head(gg_28)
#  cut for fitting purposes



### gg_29
gg_29 <- master_df %>% 
  filter(id == "gg_29")
head(gg_29)
# all missing 30
gg_29_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_29)
summary(gg_29_lm)
# coef
gg_29_coef <- gg_29_lm$coefficients
gg_29_coef
# t opt
gg_29_topt <- -gg_29_coef[2] / (2 * gg_29_coef[3])
gg_29_topt
# adding coef to df
gg_29_df <- data.frame(
  id = "gg_29",
  treatment = "25c",
  a = gg_29_coef[1],
  b = gg_29_coef[2],
  c = gg_29_coef[3],
  topt = gg_29_topt
)
gg_29_df



### gg_30
gg_30 <- master_df %>% 
  filter(id == "gg_30")
head(gg_30)
# missing 36
gg_30_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_30)
summary(gg_30_lm)
# coef
gg_30_coef <- gg_30_lm$coefficients
gg_30_coef
# t opt
gg_30_topt <- -gg_30_coef[2] / (2 * gg_30_coef[3])
gg_30_topt
# adding coef to df
gg_30_df <- data.frame(
  id = "gg_30",
  treatment = "35c",
  a = gg_30_coef[1],
  b = gg_30_coef[2],
  c = gg_30_coef[3],
  topt = gg_30_topt
)
gg_30_df



### gg_31
gg_31 <- master_df %>% 
  filter(id == "gg_31")
head(gg_31)
# missing 42
gg_31_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_31)
summary(gg_31_lm)
# coef
gg_31_coef <- gg_31_lm$coefficients
gg_31_coef
# t opt
gg_31_topt <- -gg_31_coef[2] / (2 * gg_31_coef[3])
gg_31_topt
# adding coef to df
gg_31_df <- data.frame(
  id = "gg_31",
  treatment = "25c",
  a = gg_31_coef[1],
  b = gg_31_coef[2],
  c = gg_31_coef[3],
  topt = gg_31_topt
)
gg_31_df



### gg_32
gg_32 <- master_df %>% 
  filter(id == "gg_32")
head(gg_32)
# missing 36
gg_32_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_32)
summary(gg_32_lm)
# coef
gg_32_coef <- gg_32_lm$coefficients
gg_32_coef
# t opt
gg_32_topt <- -gg_32_coef[2] / (2 * gg_32_coef[3])
gg_32_topt
# adding coef to df
gg_32_df <- data.frame(
  id = "gg_32",
  treatment = "35c",
  a = gg_32_coef[1],
  b = gg_32_coef[2],
  c = gg_32_coef[3],
  topt = gg_32_topt
)
gg_32_df



### gg_33
gg_33 <- master_df %>% 
  filter(id == "gg_33")
head(gg_33)
# missing 42 and 48... null



### gg_34
gg_34 <- master_df %>% 
  filter(id == "gg_34")
head(gg_34)
# cut for fitting



### gg_35
gg_35 <- master_df %>% 
  filter(id == "gg_35")
head(gg_35)
# all 6
gg_35_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_35)
summary(gg_35_lm)
# coef
gg_35_coef <- gg_35_lm$coefficients
gg_35_coef
# t opt
gg_35_topt <- -gg_35_coef[2] / (2 * gg_35_coef[3])
gg_35_topt
# adding coef to df
gg_35_df <- data.frame(
  id = "gg_35",
  treatment = "25c",
  a = gg_35_coef[1],
  b = gg_35_coef[2],
  c = gg_35_coef[3],
  topt = gg_35_topt
)
gg_35_df



### gg_36
gg_36 <- master_df %>% 
  filter(id == "gg_36")
head(gg_36)
# missing 30 and 36, dont use



### gg_37
gg_37 <- master_df %>% 
  filter(id == "gg_37")
head(gg_37)
# all misssing 42 and 48


### gg_38
gg_38 <- master_df %>% 
  filter(id == "gg_38")
head(gg_38)
# missing 30
gg_38_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_38)
summary(gg_38_lm)
# coef
gg_38_coef <- gg_38_lm$coefficients
gg_38_coef
# t opt
gg_38_topt <- -gg_38_coef[2] / (2 * gg_38_coef[3])
gg_38_topt
# adding coef to df
gg_38_df <- data.frame(
  id = "gg_38",
  treatment = "35c",
  a = gg_38_coef[1],
  b = gg_38_coef[2],
  c = gg_38_coef[3],
  topt = gg_38_topt
)
gg_38_df



### gg_39
gg_39 <- master_df %>% 
  filter(id == "gg_39")
head(gg_39)
# missing 30
gg_39_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_39)
summary(gg_39_lm)
# coef
gg_39_coef <- gg_39_lm$coefficients
gg_39_coef
# t opt
gg_39_topt <- -gg_39_coef[2] / (2 * gg_39_coef[3])
gg_39_topt
# adding coef to df
gg_39_df <- data.frame(
  id = "gg_39",
  treatment = "25c",
  a = gg_39_coef[1],
  b = gg_39_coef[2],
  c = gg_39_coef[3],
  topt = gg_39_topt
)
gg_39_df



### gg_40
gg_40 <- master_df %>% 
  filter(id == "gg_40")
head(gg_40)
# missing 48
gg_40_lm <- lm(log(vqmax) ~ tleaf + tleaf2, data = gg_40)
summary(gg_40_lm)
# coef
gg_40_coef <- gg_40_lm$coefficients
gg_40_coef
# t opt
gg_40_topt <- -gg_40_coef[2] / (2 * gg_40_coef[3])
gg_40_topt
# adding coef to df
gg_40_df <- data.frame(
  id = "gg_40",
  treatment = "25c",
  a = gg_40_coef[1],
  b = gg_40_coef[2],
  c = gg_40_coef[3],
  topt = gg_40_topt
)
gg_40_df



### creating master coef data frame
coef_master <- combined_df <- rbind(
  gg_2_df, gg_3_df, gg_4_df, gg_5_df, gg_6_df, gg_7_df, gg_9_df, gg_10_df,
  gg_12_df, gg_14_df, gg_15_df, gg_16_df, gg_18_df, gg_21_df, gg_22_df, 
  gg_23_df, gg_24_df, gg_25_df, gg_26_df, gg_29_df, gg_30_df, 
  gg_31_df, gg_32_df, gg_35_df, gg_38_df, gg_39_df, gg_40_df
)
coef_master



### testing stats


# topt
hist(coef_master$topt)
topt_lm <- lm(topt ~ treatment, data = coef_master)
plot(resid(topt_lm)~fitted(topt_lm))
summary(topt_lm)
ggplot(coef_master, aes(x = treatment, y = topt)) + geom_boxplot()

# a 
hist(coef_master$a)
a_lm <- lm(a ~ treatment, data = coef_master)
plot(resid(a_lm) ~ fitted(a_lm))
summary(a_lm)
ggplot(coef_master, aes(x = treatment, y = a)) + geom_boxplot()

# b 
hist(coef_master$b)
b_lm <- lm(b ~ treatment, data = coef_master)
plot(resid(b_lm) ~ fitted(b_lm))
summary(b_lm)
ggplot(coef_master, aes(x = treatment, y = b)) + geom_boxplot()

# c
hist(coef_master$c)
c_lm <- lm(c ~ treatment, data = coef_master)
plot(resid(c_lm) ~ fitted(c_lm))
summary(c_lm)
ggplot(coef_master, aes(x = treatment, y = c)) + geom_boxplot()



### stats to grpah

# string of temperature values
temp_values <- seq(15, 50, 1)

# emmeans
topt_df <- as.data.frame(emmeans(topt_lm, ~ treatment))
a_df <- as.data.frame(emmeans(a_lm, ~ treatment))
b_df <- as.data.frame(emmeans(b_lm, ~ treatment))
c_df <- as.data.frame(emmeans(c_lm, ~ treatment))

# extract
topt_25 <- topt_df$emmean[topt_df$treatment == "25c"]
topt_35 <- topt_df$emmean[topt_df$treatment == "35c"]
a_25 <-  a_df$emmean[a_df$treatment == "25c"]
a_35 <-  a_df$emmean[a_df$treatment == "35c"]
b_25 <-  b_df$emmean[b_df$treatment == "25c"]
b_35 <-  b_df$emmean[b_df$treatment == "35c"]
c_25 <-  c_df$emmean[c_df$treatment == "25c"]
c_35 <-  c_df$emmean[c_df$treatment == "35c"]

# temp response
trc_25 <- exp(a_25 + b_25 * temp_values + c_25 * temp_values^2)
trc_35 <- exp(a_35 + b_35 * temp_values + c_35 * temp_values^2)
trc_df <- as.data.frame(cbind(temp_values, trc_25, trc_35))
head(trc_df)


### Plotting!
trc_plot <- ggplot(trc_df, aes(x = temp_values)) +
  theme(
    legend.title = element_blank(), 
    legend.text = element_text(size = rel(1.5)),
    axis.title.x = element_text(size = rel(2.75)),
    axis.title.y = element_text(size = rel(2.75)),
    axis.text.x = element_text(size = rel(2.25)),
    axis.text.y = element_text(size = rel(2.25)),
    panel.background = element_rect(fill = 'white', colour = 'black'),
    panel.grid.major = element_line(colour = 'grey')) +
  geom_line(aes(y = trc_25, color = "25°C"), linewidth = 2) +
  geom_line(aes(y = trc_35, color = "35°C"), linewidth = 2) +
  scale_color_manual(values = c("25°C" = "#0072B2", "35°C" = "#C44E52")) +
  xlab('Leaf temperature \u00b0C\n') +
  ylab(expression("Vqmax"))
trc_plot


### Load Libraries
library(tidyverse)
library(emmeans)
library(multcomp)
library(multcompView)

### Read .csv
vqmax18_df <- read.csv("vqmax18_df.csv")
vqmax24_df <- read.csv("vqmax24_df.csv")
vqmax30_df <- read.csv("vqmax30_df.csv")
vqmax36_df <- read.csv("vqmax36_df.csv")
vqmax42_df <- read.csv("vqmax42_df.csv")
vqmax48_df <- read.csv("vqmax48_df.csv")

### check
vqmax18_df
vqmax24_df
vqmax30_df
vqmax36_df
vqmax42_df
vqmax48_df

### rbind
colnames(vqmax18_df) <- c("x", 'id', 'slope', 'intercept', 'vqmax', 'treatment', 'tleaf_fac')
colnames(vqmax24_df) <- c("x", 'id', 'slope', 'intercept', 'vqmax', 'treatment', 'tleaf_fac')
colnames(vqmax30_df) <- c("x", 'id', 'slope', 'intercept', 'vqmax', 'treatment', 'tleaf_fac')
colnames(vqmax36_df) <- c("x", 'id', 'slope', 'intercept', 'vqmax', 'treatment', 'tleaf_fac')
colnames(vqmax42_df) <- c("x", 'id', 'slope', 'intercept', 'vqmax', 'treatment', 'tleaf_fac')
colnames(vqmax48_df) <- c("x", 'id', 'slope', 'intercept', 'vqmax', 'treatment', 'tleaf_fac')

### check again
vqmax18_df
vqmax24_df
vqmax30_df
vqmax36_df
vqmax42_df
vqmax48_df


### create master
master_df <- rbind(vqmax18_df, vqmax24_df, vqmax30_df, vqmax36_df, vqmax42_df, vqmax48_df)
master_df

vqmax_lm <- lm(vqmax ~ treatment * tleaf_fac, data = master_df)
summary(vqmax_lm)
plot(resid(vqmax_lm) ~ fitted(vqmax_lm))
anova(vqmax_lm)

cld(emmeans(vqmax_lm, pairwise ~ treatment))
cld(emmeans(vqmax_lm, pairwise ~ tleaf_fac))
cld(emmeans(vqmax_lm, pairwise ~ tleaf_fac | treatment))

### plotting with ggplot
ggplot(master_df, aes(x = tleaf_fac, y = vqmax, color = treatment)) + geom_boxplot()

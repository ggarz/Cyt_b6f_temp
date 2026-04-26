### Load Libraries
library(tidyverse)
options(max.print = 1000)

### Read .csv file
lrc_total <- read.csv('lrc_total.csv')
head(lrc_total)

### get rid of bad rows in phipsii
lrc_clean <- lrc_total %>%
  filter(!is.na(Fs), !is.na(Fm.), Fs != 0, Fm. != 0)
head(lrc_clean)

### make phipsii numeric
lrc_clean$PhiPS2 <- as.numeric(lrc_clean$PhiPS2)


### make replicate column to seperate odds and evens
lrc_clean$replicate <- as.numeric(gsub("\\D", "", lrc_clean$id))
head(lrc_clean)

### organizing by odds and evens 
lrc_clean$treatment <- ifelse(lrc_clean$replicate %% 2 == 1,
                             "25c",
                             "35c")
head(lrc_clean)

### Rounding temperatures
lrc_clean$TRounded <- round(lrc_clean$Tleaf, digits = 0)
head(lrc_clean$TRounded)

### Rounding temperatures further
lrc_clean$TRounded <- with(lrc_clean,
                        ifelse(TRounded < 21, 18,
                               ifelse(TRounded < 27, 24,
                                      ifelse(TRounded < 33, 30,
                                             ifelse(TRounded < 39, 36,
                                                    ifelse(TRounded < 45, 42, 48)
                                             )
                                      )
                               )
                        )
)
head(lrc_clean$TRounded)

### Creating LEF
alpha <- 0.85
beta <- 0.5
lrc_clean$LEF <- lrc_clean$PhiPS2 * lrc_clean$Q * alpha * beta
head(lrc_clean$LEF)  

### Creating separate data frames for each temperature
lrc_18 <- lrc_clean %>% 
  filter(TRounded == 18)
lrc_24 <- lrc_clean %>% 
  filter(TRounded == 24)
lrc_30 <- lrc_clean %>% 
  filter(TRounded == 30)
lrc_36 <- lrc_clean %>% 
  filter(TRounded == 36)
lrc_42 <- lrc_clean %>% 
  filter(TRounded == 42)
lrc_48 <- lrc_clean %>% 
  filter(TRounded == 48)

### Test with all temps vs treatment
ggplot(lrc_18, aes(x = one.qL, y = LEF, 
                   color = treatment, group = id)) + geom_point() + geom_line()
ggplot(lrc_24, aes(x = one.qL, y = LEF, 
                   color = treatment, group = id)) + geom_point() + geom_line()
ggplot(lrc_30, aes(x = one.qL, y = LEF, 
                   color = treatment, group = id)) + geom_point() + geom_line()
ggplot(lrc_36, aes(x = one.qL, y = LEF,
                   color = treatment, group = id)) + geom_point() + geom_line()
ggplot(lrc_42, aes(x = one.qL, y = LEF, 
                   color = treatment, group = id)) + geom_point() + geom_line()
ggplot(lrc_48, aes(x = one.qL, y = LEF,
                   color = treatment, group = id)) + geom_point() + geom_line()
### Very messy, but potentially telling...

### getting phipsii just for fun
ggplot(lrc_18, aes(x = treatment, y = PhiPS2)) + geom_boxplot()
ggplot(lrc_24, aes(x = treatment, y = PhiPS2)) + geom_boxplot()
ggplot(lrc_30, aes(x = treatment, y = PhiPS2)) + geom_boxplot()
ggplot(lrc_36, aes(x = treatment, y = PhiPS2)) + geom_boxplot()
ggplot(lrc_42, aes(x = treatment, y = PhiPS2)) + geom_boxplot()
ggplot(lrc_48, aes(x = treatment, y = PhiPS2)) + geom_boxplot()

### Redoing 1-qL vs LEF based on machine
albert_18 <- lrc_18 %>% 
  filter(machine == "albert")
#gibson_18 <- lrc_18 %>% 
#  filter(machine == "gibson")
ozzie_18 <- lrc_18 %>% 
  filter(machine == "ozzie")
stan_18 <- lrc_18 %>% 
  filter(machine == "stan")

### testing via machine for 18c
ggplot(albert_18, aes(x = one.qL, y = LEF, 
                   color = treatment, group = id)) + geom_point() + geom_line()
#ggplot(gibson_18, aes(x = one.qL, y = LEF, 
#                     color = treatment, group = id)) + geom_point() + geom_line()
ggplot(ozzie_18, aes(x = one.qL, y = LEF, 
                      color = treatment, group = id)) + geom_point() + geom_line()
ggplot(stan_18, aes(x = one.qL, y = LEF, 
                      color = treatment, group = id)) + geom_point() + geom_line()

### machines for 24c
albert_24 <- lrc_24 %>% 
  filter(machine == "albert")
#gibson_24 <- lrc_24 %>% 
#  filter(machine == "gibson")
ozzie_24 <- lrc_24 %>% 
  filter(machine == "ozzie")
stan_24 <- lrc_24 %>% 
  filter(machine == "stan")

### testing via machine for 24c
ggplot(albert_24, aes(x = one.qL, y = LEF, 
                      color = treatment, group = id)) + geom_point() + geom_line()
ggplot(gibson_24, aes(x = one.qL, y = LEF, 
                      color = treatment, group = id)) + geom_point() + geom_line()
ggplot(ozzie_24, aes(x = one.qL, y = LEF, 
                     color = treatment, group = id)) + geom_point() + geom_line()
ggplot(stan_24, aes(x = one.qL, y = LEF, 
                    color = treatment, group = id)) + geom_point() + geom_line()

### machines for 30c
albert_30 <- lrc_30 %>% 
  filter(machine == "albert")
gibson_30 <- lrc_30 %>% 
  filter(machine == "gibson")
ozzie_30 <- lrc_30 %>% 
  filter(machine == "ozzie")
stan_30 <- lrc_30 %>% 
  filter(machine == "stan")

### testing via machine for 30c
ggplot(albert_30, aes(x = one.qL, y = LEF, 
                      color = treatment, group = id)) + geom_point() + geom_line()
ggplot(gibson_30, aes(x = one.qL, y = LEF, 
                      color = treatment, group = id)) + geom_point() + geom_line()
ggplot(ozzie_30, aes(x = one.qL, y = LEF, 
                     color = treatment, group = id)) + geom_point() + geom_line()
ggplot(stan_30, aes(x = one.qL, y = LEF, 
                    color = treatment, group = id)) + geom_point() + geom_line()

### machines for 36c
albert_36 <- lrc_36 %>% 
  filter(machine == "albert")
#gibson_36 <- lrc_36 %>% 
#  filter(machine == "gibson")
ozzie_36 <- lrc_36 %>% 
  filter(machine == "ozzie")
stan_36 <- lrc_36 %>% 
  filter(machine == "stan")

### testing via machine for 30c
ggplot(albert_36, aes(x = one.qL, y = LEF, 
                      color = treatment, group = id)) + geom_point() + geom_line()
#ggplot(gibson_36, aes(x = one.qL, y = LEF, 
#                     color = treatment, group = id)) + geom_point() + geom_line()
ggplot(ozzie_36, aes(x = one.qL, y = LEF, 
                     color = treatment, group = id)) + geom_point() + geom_line()
ggplot(stan_36, aes(x = one.qL, y = LEF, 
                    color = treatment, group = id)) + geom_point() + geom_line()

### machines for 42c
albert_42 <- lrc_42 %>% 
  filter(machine == "albert")
#gibson_42 <- lrc_42 %>% 
#  filter(machine == "gibson")
ozzie_42 <- lrc_42 %>% 
  filter(machine == "ozzie")
stan_42 <- lrc_42 %>% 
  filter(machine == "stan")

### testing via machine for 30c
ggplot(albert_42, aes(x = one.qL, y = LEF, 
                      color = treatment, group = id)) + geom_point() + geom_line()
#ggplot(gibson_42, aes(x = one.qL, y = LEF, 
#                     color = treatment, group = id)) + geom_point() + geom_line()
ggplot(ozzie_42, aes(x = one.qL, y = LEF, 
                     color = treatment, group = id)) + geom_point() + geom_line()
ggplot(stan_42, aes(x = one.qL, y = LEF, 
                    color = treatment, group = id)) + geom_point() + geom_line()

### machines for 48c
albert_48 <- lrc_48 %>% 
  filter(machine == "albert")
#gibson_48 <- lrc_48 %>% 
#  filter(machine == "gibson")
ozzie_48 <- lrc_48 %>% 
  filter(machine == "ozzie")
stan_48 <- lrc_48 %>% 
  filter(machine == "stan")

### testing via machine for 48c
ggplot(albert_48, aes(x = one.qL, y = LEF, 
                      color = treatment, group = id)) + geom_point() + geom_line()
#ggplot(gibson_48, aes(x = one.qL, y = LEF, 
#                      color = treatment, group = id)) + geom_point() + geom_line()
ggplot(ozzie_48, aes(x = one.qL, y = LEF, 
                     color = treatment, group = id)) + geom_point() + geom_line()
ggplot(stan_48, aes(x = one.qL, y = LEF, 
                    color = treatment, group = id)) + geom_point() + geom_line()



### write .csv for each temperature
write.csv(lrc_18, "lrc_18.csv")
write.csv(lrc_24, "lrc_24.csv")
write.csv(lrc_30, "lrc_30.csv")
write.csv(lrc_36, "lrc_36.csv")
write.csv(lrc_42, "lrc_42.csv")
write.csv(lrc_48, "lrc_48.csv")

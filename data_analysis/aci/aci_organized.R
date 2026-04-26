### Load Libraries
library(tidyverse)
options(max.print = 1000)

### Read .csv file
aci_total <- read.csv('aci_total.csv')
head(aci_total)

### make replicate column to seperate odds and evens
aci_total$replicate <- as.numeric(gsub("\\D", "", aci_total$id))
head(aci_total)

### organizing by odds and evens 
aci_total$treatment <- ifelse(aci_total$replicate %% 2 == 1,
                              "25c",
                              "35c")
head(aci_total)

### Rounding temperatures
aci_total$TRounded <- round(aci_total$Tleaf, digits = 0)
head(aci_total$TRounded)

### Rounding temperatures further
aci_total$TRounded <- with(aci_total,
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
aci_total$TRounded

### Creating separate data frames for each temperature
aci_18 <- aci_total %>% 
  filter(TRounded == 18)
aci_24 <- aci_total %>% 
  filter(TRounded == 24)
aci_30 <- aci_total %>% 
  filter(TRounded == 30)
aci_36 <- aci_total %>% 
  filter(TRounded == 36)
aci_42 <- aci_total %>% 
  filter(TRounded == 42)
aci_48 <- aci_total %>% 
  filter(TRounded == 48)

### Test with all temps vs treatment
ggplot(aci_18, aes(x = Ci, y = A, 
                   color = treatment, group = id)) + geom_point() + geom_line()
ggplot(aci_24, aes(x = Ci, y = A, 
                   color = treatment, group = id)) + geom_point() + geom_line()
ggplot(aci_30, aes(x = Ci, y = A, 
                   color = treatment, group = id)) + geom_point() + geom_line()
ggplot(aci_36, aes(x = Ci, y = A, 
                   color = treatment, group = id)) + geom_point() + geom_line()
ggplot(aci_42, aes(x = Ci, y = A, 
                   color = treatment, group = id)) + geom_point() + geom_line()
ggplot(aci_48, aes(x = Ci, y = A, 
                   color = treatment, group = id)) + geom_point() + geom_line()

### write .csv for each temperature
write.csv(aci_18, "aci_18.csv")
write.csv(aci_24, "aci_24.csv")
write.csv(aci_30, "aci_30.csv")
write.csv(aci_36, "aci_36.csv")
write.csv(aci_42, "aci_42.csv")
write.csv(aci_48, "aci_48.csv")


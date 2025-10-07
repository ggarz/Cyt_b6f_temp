# Test 

library(ggplot2)
library(ggthemes)

source('garry_model.R')

test_1 <- model(PAR =  seq(0, 1500, 100))

test_2 <- model(PAR =  seq(0, 1500, 100), CO2 = 800)

test_3 <- model(PAR =  seq(0, 1500, 100), temp_c = 35)

test_4 <- model(temp_c = seq(0, 50, 5))

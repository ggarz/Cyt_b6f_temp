# Test 

library(ggplot2)

source('garry_model.R')

test_1 <- model(PAR =  seq(0, 2000, 200))

test_2 <- model(PAR =  seq(0, 1500, 100), CO2 = 800)

test_3 <- model(PAR =  seq(0, 2000, 200), temp_c = 35)

test_4 <- model(temp_c = seq(15, 50, 1))

test_5 <- model(CO2 = seq(0, 1000, 50), PAR = 2000)


### plotting
vqmax_temp <- ggplot(test_4, aes(x = Temp, y = Vqmax)) + geom_line(linewidth = 2, color = "red") + theme(
  legend.title = element_blank(), 
  legend.text = element_text(size = rel(1.5)),
  axis.title.x = element_text(size = rel(2.75)),
  axis.title.y = element_text(size = rel(2.75)),
  axis.text.x = element_text(size = rel(2.25)),
  axis.text.y = element_text(size = rel(2.25)),
  panel.background = element_rect(fill = 'white', colour = 'black'),
  panel.grid.major = element_line(colour = 'grey')) +
  xlab('Leaf temperature \u00b0C\n') +
  ylab(expression("Vqmax"))
vqmax_temp

aj_temp <- ggplot(test_4, aes(x = Temp, y = Ag_j)) + geom_line(linewidth = 2, color = "purple") + theme(
  legend.title = element_blank(), 
  legend.text = element_text(size = rel(1.5)),
  axis.title.x = element_text(size = rel(2.75)),
  axis.title.y = element_text(size = rel(2.75)),
  axis.text.x = element_text(size = rel(2.25)),
  axis.text.y = element_text(size = rel(2.25)),
  panel.background = element_rect(fill = 'white', colour = 'black'),
  panel.grid.major = element_line(colour = 'grey')) +
  xlab('Leaf temperature \u00b0C\n') +
  ylab(expression("Aj"))
aj_temp

aj_co2 <- ggplot(test_4, aes(x = CO2, y = Ag_j)) + geom_line(linewidth = 2, color = "blue") + theme(
  legend.title = element_blank(), 
  legend.text = element_text(size = rel(1.5)),
  axis.title.x = element_text(size = rel(2.75)),
  axis.title.y = element_text(size = rel(2.75)),
  axis.text.x = element_text(size = rel(2.25)),
  axis.text.y = element_text(size = rel(2.25)),
  panel.background = element_rect(fill = 'white', colour = 'black'),
  panel.grid.major = element_line(colour = 'grey')) +
  xlab('CO2') +
  ylab(expression("Aj"))
aj_co2

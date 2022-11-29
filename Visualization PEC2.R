library(readxl)

library(tidyverse)
library(ggplot2)
library(ggridges)
library(ggjoy)
library(cowplot)

########################
### Ridgeline Chart ###
#######################

## Temperaturas Barcelona 2020


data_temp_bcn_2020<- read_excel(paste0(getwd(),"/Temperaturas Barcelona 2020.xlsx"))
data_temp_bcn_2020 <- as.data.frame(data_temp_bcn_2020)
names(data_temp_bcn_2020) <- c("fecha", "temp_max", "temp_min")
data_temp_bcn_2020$fecha <- as.Date(data_temp_bcn_2020$fecha)
head(data_temp_bcn_2020)
data_temp_bcn_2020$mes <- as.factor(format(data_temp_bcn_2020$fecha,'%m'))
data_temp_bcn_2020$temp_max <- as.numeric(data_temp_bcn_2020$temp_max)
data_temp_bcn_2020$temp_min <- as.numeric(data_temp_bcn_2020$temp_min)
head(data_temp_bcn_2020)
meses = c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio", "Julio","Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")
levels(data_temp_bcn_2020$mes) <- meses
head(data_temp_bcn_2020)



ggplot_temp_min <- ggplot(data_temp_bcn_2020, aes(y=mes, 
                               x=temp_min, 
                               fill=stat(x)))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+
  scale_fill_viridis_c(name = "Promedio Temp. Min.", option = "H")+
  labs(title= "Promedio Temperaturas Mínimas de Barcelona en 2020")+
  theme(
    axis.title = element_text(color='black', face='bold', size=10),
    axis.text = element_text(color='black', size=10)
  )



ggplot_temp_max <- ggplot(data_temp_bcn_2020, aes(y=mes, 
                               x=temp_max, 
                               fill=stat(x)))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+
  scale_fill_viridis_c(name = "Promedio Temp. Max.", option = "H")+
  labs(title= "Promedio Temperaturas Máximas de Barcelona en 2020")+
  theme(
    axis.title = element_text(color='black', face='bold', size=10),
    axis.text = element_text(color='black', size=10)
  )



p <- plot_grid(ggplot_temp_min, ggplot_temp_max, labels = "AUTO")
p2 <- ggplot_temp_max
ggsave("Ridgeline Chart.pdf", p2)




################
## SCATERPLOT ##
################

library(ggplot2)

data_muertes_2021<- read_excel(paste0(getwd(),"/muertes_2021.xlsx"))
data_muertes_2021 <- as.data.frame(data_muertes_2021)
head(data_muertes_2021)

ggplot(data_muertes_2021, aes(x = EDAD, y = DEFUNCIONES)) +
  geom_point(size=2)+
  labs(title= "Defunciones por edad a causa del Covid-19 en 2021 ")

p <- ggplot(data_muertes_2021, aes(x = EDAD, y = DEFUNCIONES)) +
  geom_point(size=2)+
  labs(title= "Defunciones por edad a causa del Covid-19 en 2021 ")

ggsave("Scatterplot.pdf", p)



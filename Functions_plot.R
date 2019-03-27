rm(list = ls())
library(ggplot2)
library(RColorBrewer)
library(rlang)

myboxplot <- function(mydata, myexposure, myoutcome, mytitle, mylabel_x, mylabel_y, my_fill)

{
  ###########################################################
  ###########################################################
  #### Crear grafico boxplot                          #######
  #### Recibe los siguientes par?metros:              #######
  #### data = Dataframe con datos                     #######
  #### myexposure = Variable eje x para agrupar datos #######
  #### myoutcome = Variable eje y valores datos       #######
  #### mytitle = Titulo para la grafica               ####### 
  #### mylabel_x = Label descripcion eje x            #######
  #### mylabel_y = Label descripcion eje y            #######
  #### my_fill = Variable para color de relleno       #######
  ###########################################################
  ###########################################################

  
  bp <- ggplot(
    mydata, 
    aes(x = as.factor(mydata[[myexposure]]), 
        y = mydata[[myoutcome]],
        color = as.factor(mydata[[myexposure]]))) +
    geom_boxplot() + 
    labs(title = mytitle,
         x= mylabel_x, 
         y = mylabel_y,
         fill = my_fill) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "none") + 
    scale_colour_brewer(palette = "Paired") 
}




mygeom_bar <- function(mydata, myexposure, myoutcome, mytitle, mylabel_x, mylabel_y, my_fill, my_angle, my_legend)

{
  ###########################################################
  ###########################################################
  #### Crear grafico barras                           #######
  #### Recibe los siguientes par?metros:              #######
  #### data = Dataframe con datos                     #######
  #### myexposure = Variable eje x para agrupar datos #######
  #### myoutcome = Variable eje y valores datos       #######
  #### mytitle = Titulo para la grafica               ####### 
  #### mylabel_x = Label descripcion eje x            #######
  #### mylabel_y = Label descripcion eje y            #######
  #### my_fill = Variable para color de relleno       #######
  #### my_angle = Angulo al cual girar grafico        #######
  #### my_legend = Leyenda para el grafico            #######
  ###########################################################
  ###########################################################

  
  bp <- 
    ggplot(
      mydata, 
      aes(x = mydata[[myexposure]])) +
    geom_bar(
      aes(fill = as.factor(mydata[[myoutcome]]),
          y = (..count..)/sum(..count..)),
      na.rm = TRUE) +
    theme_minimal() +
    labs(x = mylabel_x, 
         y = "Frecuencia Relativa", 
         fill = my_fill) +
    ggtitle(mytitle) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_fill_brewer(palette = "Paired") + 
    scale_y_continuous(labels=scales::percent) +
    theme(legend.position = my_legend) +
    
    { # angle
    if (!is.null(my_angle)) theme(axis.text.x = element_text(angle = my_angle))  } +
    
    { # angle
    if (!is.null(my_angle)) coord_flip()}
}


outlier <- function(mydata, value, q_min, q_max){
  
  
  ##########################################################
  ##########################################################
  #### Imputa los outliers de las variables          #######
  #### numéricas y los reemplaza por la media,       #######
  #### seleccionando los valores que estén por encima#######
  #### y por debajo de los percentiles límites.      #######
  #### Recibe los siguientes parámetros:             #######
  #### mydata = Dtaframe con los datos mensuales     #######
  #### value = variable o aributo                    #######
  #### q_min = valor del pecentil mínimo             #######
  #### q_max = valor del pecentil máximo             ####### 
  ##########################################################
  ##########################################################
  
  quantiles <- quantile(mydata[[value]], 
                        c(q_min,q_max), 
                        na.rm = TRUE)
  ifelse(mydata[[value]] < quantiles[1] | mydata[[value]] > quantiles[2], 
         ceiling(mean(mydata[[value]])), 
         mydata[[value]])
}

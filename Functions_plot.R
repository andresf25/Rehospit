rm(list = ls())
library(ggplot2)
library(RColorBrewer)
library(rlang)

myboxplot <- function(mydata, myexposure, myoutcome, mytitle, mylabel_x, mylabel_y, my_fill)

{
  ##########################################################
  ##########################################################
  #### Modifica el a?o base de la inflaci?n          #######
  #### con base al periodo empleado.                 #######
  #### Recibe los siguientes par?metros:             #######
  #### data = Dtaframe con los datos mensuales       #######
  #### variable = serie historica de la inflaci?n    #######
  #### fecha = periodo de tiempo empleado            #######
  #### per_bef = periodo de la base anterior         ####### 
  #### per_aft = periodo de la nueva base            #######
  ##########################################################
  ##########################################################
  
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




mygeom_bar <- function(mydata, myexposure, myoutcome, mytitle, mylabel_x, mylabel_y, my_fill, my_angle)

{
  
  ##########################################################
  ##########################################################
  #### Modifica el a?o base de la inflaci?n          #######
  #### con base al periodo empleado.                 #######
  #### Recibe los siguientes par?metros:             #######
  #### data = Dtaframe con los datos mensuales       #######
  #### variable = serie historica de la inflaci?n    #######
  #### fecha = periodo de tiempo empleado            #######
  #### per_bef = periodo de la base anterior         ####### 
  #### per_aft = periodo de la nueva base            #######
  ##########################################################
  ##########################################################
  
  
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
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none") + 
    scale_fill_brewer(palette = "Paired") + 
    scale_y_continuous(labels=scales::percent) +
    
    { # angle
      if (!is.null(my_angle)) theme(axis.text.x = element_text(angle = my_angle), 
                                    legend.position = "none")  } +
    
                                    { # angle
                                      if (!is.null(my_angle)) coord_flip()} 
}
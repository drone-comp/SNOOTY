"""
Python package `SNOOTY`: smoothing and evaluation for data analysis.

"""

import rpy2.robjects.numpy2ri as numpy2ri
import rpy2.robjects. pandas2ri as pandas2ri
from rpy2.robjects.packages import STAP
import rpy2.robjects as ro


def smooth(data, smooth_size, before, after):
    """
    Smoothing data using R functions

    The normal column is used to calculate a smooth value using the slide_index_dbl from R language.
    After that it's made a time dependent linear interpolation applied to all columns
    
    Parameters
    ----------
    data = datafram which has a time column and a normal column.
    

    before --> trata-se da quantidade de células anteriores à célula atual que serão aglutinadas a esta no processo de slide window.

    after --> trata-se da quantidade de células posteriores à célula atual que serão aglutinadas a esta no processo de slide window.

    smooth size --> trata-se do tamanho do intervalo entre duas células consecutivas. O valor se refere a alguma variável que é escolhida como referência. No caso do experimento, foi escolhido o tempo.

    """
    r_function_str = """

    smooth <- function(data, smooth_size, before, after) {

        library(tidyverse)
        library(slider)

        #Smooth the data:
        data <- 
            data %>%
            mutate(smooth = slide_index_dbl(normal, time, mean, .before = before, .after = before))

        
        ntime <- seq(min(data$time), max(data$time), by = smooth_size)
        ndata <- tibble(time = ntime, value = approx(data$time, data$smooth, ntime)$y)
        names <- colnames(data)
        names <- names[names != 'time' & names != 'normal' & names != 'smooth']
        
        
        for(name in names){
            ndata <- ndata %>% mutate(approx(data$time, data[[name]], ntime)$y)
        }
        
        #Change col names:
        naming <- function(x){
                return (paste('alt', x))
        }
        alt_names <- lapply(1:(length(ndata)-2), naming)
        colnames(ndata) <- c("time", "value", alt_names)
        
        return(ndata)
    }


"""
    r_pkg = STAP(r_function_str, "r_pkg")
    pandas2ri.activate()
    datapandas = r_pkg.smooth(data, smooth_size, before, after)

    return datapandas
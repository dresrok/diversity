# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function(n1) {
  #resultado <- n1*3
  library(jsonlite)
  resultado <- fromJSON("https://api.github.com/users/hadley/orgs")
  print(resultado$id)
}

getIv <- function(url){
  library(jsonlite)
  data <- fromJSON(url)
  forest <- data.frame(table(data$Especie), stringsAsFactors=FALSE, row.names = NULL);
  names(forest) <- c('Especie', 'abunA');
  forest$Especie <- as.character(forest$Especie);
  forest$abunR <- (forest$abunA/sum(forest$abunA))*100;
  freqSpecies <- data.frame(table(data$Subparcela, data$Especie));
  names(freqSpecies) <- c('subparcela', 'especie', 'frecA');
  dtFreq <- data.frame(table(freqSpecies$especie[freqSpecies$frec > 0]));
  forest$frecA <- dtFreq$Freq;
  forest$frecR <- (forest$frecA/sum(forest$frecA))*100;
  data$dA <- (pi/40000) * (data$Dn^2);
  dtDn <- data.frame(tapply(data$dA, data$Especie, sum), row.names=NULL);
  names(dtDn) <- c('sumDn');
  forest$domA <-  dtDn$sumDn;
  forest$domR <- (forest$domA/sum(forest$domA))*100;
  forest$IVI <- forest$abunR + forest$frecR + forest$domR;
  total <- data.frame(Especie = "Total", abunA = sum(forest$abunA), abunR = sum(forest$abunR), frecA = sum(forest$frecA), frecR = sum(forest$frecR), domA = sum(forest$domA), domR = sum(forest$domR), IVI = sum(forest$IVI));
  forest <- rbind(forest, total);
  return(forest);
}

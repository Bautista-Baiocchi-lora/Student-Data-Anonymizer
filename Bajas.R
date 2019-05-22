library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(xlsx)

load(file = "./output-data/master.key.RData")
bajas <- read.csv2(file = "./input-data/bajas2018.csv")

compose_names <- function(name) {
  composed.name <- name %>%
    strsplit(split = ",") %>%
    unlist() %>%
    rev() %>%
    str_c(collapse = " ") %>%
    trimws() %>%
    str_to_title()
}

get_similatiries <- function(name) {
  return(RecordLinkage::levenshteinSim(name, as.vector(name.master.key$original.names)))
}

decide_name <- function(name) {
  similarities <- get_similatiries(name)
  possible.names <- as.vector(name.master.key[similarities > .85, 2])
  if (length(possible.names) > 0) {
    return(possible.names[1])
  }
  print(c("Removing:", name))
  return(NA) # Return NA to later remove column
}


bajas$Alumno <- bajas %>%
  pull(Alumno) %>%
  as.vector() %>%
  sapply(compose_names) %>%
  sapply(decide_name)


write.xlsx(bajas, file = './output-data/Bajas-ANON.xlsx')

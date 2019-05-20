library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(xlsx)
library(RecordLinkage)

load(file = "./output-data/master.key.RData")
notas.infor <- read.csv2(file = "./input-data/ALGEBRA INFORMATICOS.csv", row.names = c("X"))
notas.indus <- read.csv2(file = "./input-data/ALGEBRA I INDUSTRIALES.csv")


compose_names <- function(name) {
  composed.name <- name %>%
    strsplit(split = ",") %>%
    unlist() %>%
    rev() %>%
    str_c(collapse = " ") %>%
    trimws() %>%
    str_to_title()
}

# Remove X row, set all empty cells to NA, remove all rows with Nombre = NA
format_notas <- function(df) {
  df <- df %>%
    mutate_all(na_if, "") %>%
    filter(!is.na(Nombre))

  df$Nombre <- df$Nombre %>%
    droplevels(exclude = c("0", "1", "2")) %>%
    as.vector() %>%
    sapply(compose_names)
  return(df)
}

notas.indus <- format_notas(notas.indus)
notas.infor <- format_notas(notas.infor)

get_similatiries <- function(name) {
  return(RecordLinkage::levenshteinSim(name, as.vector(name.master.key$original.names)))
}

decide_name <- function(name) {
  similarities <- get_similatiries(name)
  possible.names <- as.vector(name.master.key[similarities > .85,2])
  if (length(possible.names) > 0) {
    return(possible.names[1])
  }
  return(NA) # Return NA to later remove column
}

fix_names_column <- function(name.col) {
  new.names <- vector()
  for (name in name.col) {
    if (!is.na(name)) {
      current.name <- decide_name(name)
    }
    new.names <- c(new.names, current.name)
  }
  return(new.names)
}

notas.infor$Nombre <- fix_names_column(notas.infor$Nombre)
notas.infor <- filter(notas.infor, !is.na(Date) & !is.na(Nombre))

notas.indus$Nombre <- fix_names_column(notas.indus$Nombre)
notas.indus <- filter(notas.indus, !is.na(Date) & !is.na(Nombre))

write.xlsx(notas.infor, "./output-data/ALGEBRA INFORMATICOS-ANON.xlsx") 

write.xlsx(notas.indus, "./output-data/ALGEBRA I INDUSTRIALES-ANON.xlsx") 
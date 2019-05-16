# Install package if missing
if ("xlsx" %in% rownames(installed.packages()) == FALSE) {
  install.packages("xlsx")
}


# Import packages
library(dplyr)
library(xlsx)
library(RecordLinkage)
library(stringr)

format_names <- function(names) {
  names <- names %>%
    trimws() %>%
    str_to_title()
  return(gsub("  ", " ", names))
}

ingresos.both <- read.csv2(file = "./input-data/INGRESO 2018-2019.csv", row.names = c("X")) # Import data

ingresos.both <- rename(ingresos.both, Nombres = Nombre.completo)

# Names to Title and trim leading/trailing spaces
ingresos.both$Nombres <- format_names(ingresos.both$Nombres)

ingresos.both <- ingresos.both[!duplicated(ingresos.both$Nombres), ] # delete duplicate rows based on name

ingresos.2018 <- read.csv2(file = "./input-data/INGRESOS 2018.csv", row.names = c("X"))

compose_names <- function(df) {
  df %>%
    mutate(Nombres = paste(Nombres, Apellidos)) %>%
    select(-Apellidos)
}

ingresos.2018 <- ingresos.2018 %>% compose_names()

ingresos.2018$Nombres <- format_names(ingresos.2018$Nombres)

ingresos.2018 <- ingresos.2018[!duplicated(ingresos.2018$Nombres), ]

ingresos.2019 <- read.csv2(file = "./input-data/INGRESO 2019.csv", row.names = c("X"))

ingresos.2019 <- ingresos.2019 %>% compose_names()

ingresos.2019$Nombres <- format_names(ingresos.2019$Nombres)

ingresos.2019 <- ingresos.2019[!duplicated(ingresos.2019$Nombres), ]

# Create vector of original names from all files
original.names <- c(as.vector(ingresos.both$Nombres), ingresos.2018$Nombres, ingresos.2019$Nombres)

# Remove all duplicates
original.names <- original.names[!duplicated(original.names)]

get_similatiries <- function(name) {
  return(RecordLinkage::levenshteinSim(name, original.names))
}

# LANG=en_US.UTF-8

decide_name <- function(name) {
  similarities <- get_similatiries(name)
  possible.names <- original.names[which(similarities > .85 & similarities < 1.0)]
  # No similar names found
  if (length(possible.names) == 0) {
    return(name)
  } else if (any(grepl("á|í", possible.names))) {
    return(possible.names[1]) # Return name with accent
  }
  return(NA) # Return NA to later remove column
}

original.names <- as.vector(sapply(original.names, decide_name))

original.names <- original.names[-which(is.na(original.names))]

# Removes all names that have been deprecated
filter_deprecated_names <- function(df) {
  return(filter(df, Nombres %in% original.names))
}

ingresos.2018 <- filter_deprecated_names(ingresos.2018)

ingresos.2019 <- filter_deprecated_names(ingresos.2019)

ingresos.both <- filter_deprecated_names(ingresos.both)

# Initiate random name generator
source("./Name.Generator.R")

generated.random.names <- as.vector(sapply(original.names, function(name) {
  return(random_name())
}))

name.master.key <- data.frame(original.names, generated.random.names)

swap_name <- function(name) {
  return(name.master.key[(name.master.key$original.names == name), 2])
}

ingresos.2018$Nombres <- as.vector(sapply(ingresos.2018$Nombres, swap_name))

write.xlsx(ingresos.2018, "./output-data/INGRESO 2018-ANON.xlsx") # Finished anonymizing INGRESO 2018, Save it.

ingresos.2019$Nombres <- as.vector(sapply(ingresos.2019$Nombres, swap_name))

write.xlsx(ingresos.2019, "./output-data/INGRESO 2019-ANON.xlsx") # Finished anonymizing INGRESO 2018, Save it.

ingresos.both$Nombres <- as.vector(sapply(ingresos.both$Nombres, swap_name))

write.xlsx(ingresos.both, "./output-data/INGRESO 2018-2019-ANON.xlsx") # Finished anonymizing INGRESO 2018-2019, Save it

write.csv2(name.master.key, "./output-data/INGRESO 2018-2019-KEY.csv", row.names = F) # Save master key as csv for future use

save(name.master.key, file = './output-data/master.key.RData')
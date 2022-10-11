
library(dplyr)
library(gdata)
library(stringr)
library(openxlsx)
library(tidyr)

# 1. Get Data -------------------------------------------------------------

wave2 <- read.csv("./Data/data_w2.csv", header = TRUE, sep = ",", fill = TRUE,
                  fileEncoding = "UTF-8")
wave3 <- read.csv("./Data/data_w3.csv", header = TRUE, sep = ",", fill = TRUE, 
                  fileEncoding = "UTF-8")

# All observations of wave 2 
data_all <- read.csv("./Data/data_total_wave2.csv", header = TRUE, 
                        sep = ";", fill = TRUE, fileEncoding = "UTF-8") 

# waves including "Sonntagsfrage"
wave2_s <- read.csv("./Data/data_w2_s.csv", fileEncoding = "UTF-8", sep = ",")
wave3_s <- read.csv("./Data/data_w3_s.csv", fileEncoding = "UTF-8", sep = ";")

# merging "Sonntagsfrage" to dataset
wave2 <- 
  merge(x = wave2, y = wave2_s[, c("Nutzer", "Sonntagsfrage")], by = "Nutzer")

wave3 <- 
  merge(x = wave3, y = wave3_s[, c("Nutzer", "Sonntagsfrage")], by = "Nutzer")


# 2. First overview -------------------------------------------------------

glimpse(wave2)
glimpse(wave3)

# how many people are decided/undecided before cleaning data?
prop.table(table(wave2$Antwort))
prop.table(table(wave3$Antwort))


# 3. Clean data -----------------------------------------------------------

parties <- c("CDU.CSU", "SPD", "Grüne", "FDP", "AfD", "Linke")

# People who were "undecided" but still voted for only one party are now "decided"
# Input: dataset - data frame
# Output: data frame with corrected decided people
change_ontic <- function(dataset) {
  for (i in seq_len(nrow(dataset))) {
    if (dataset[["ontic_bin"]][i] == 1 && sum(dataset[i, parties]) <= 1 ) {
      dataset[["ontic_bin"]][i] <- 0
    }
  }
  dataset
}
# 148 more people are decided 

# Keep only Ontic values with combinations that more than 20 people have chosen 
# Input: dataset - data frame
# Output: data frame with Ontic combinations of more than 20 
minimize_ontic <- function(dataset) {
  dataset %>%
    group_by(Ontic) %>%
    filter(n() >= 20) # > 21 to get the 8 most frequent combinations
}


# Remove people that chose "Eine andere Partei" or "Sonstige" for the variable Ontic
# Input:  dataset - data frame
#         variable - character vector of parties (either one element or more)
# Output: data frame without "Eine andere Partei", "Sonstige", "Nichtwähler"
no_others <- function(dataset, variable = "Ontic"){
  dataset <- subset(dataset, !is.element(dataset[, variable], 
                                         c("Sonstige", "Eine andere Partei",
                                           "Nichtwähler")))
  # Remove "Eine andere Partei" from variable "Ontic"
  dataset[, variable] <- str_remove_all(dataset[, variable], '-.-Eine andere Partei')
  
  # dataset[, variable] <- replace(dataset[, variable], dataset[, variable] == 
  #                            "Eine andere Partei", "Sonstige")
  dataset
}


# Change "Weiß nicht / keine Angabe" in every row and column into NA
# Input:  dataset - data frame
# Output: data frame with NA instead of "Weiß nicht / keine Angabe" 
show_na <- function(dataset){
  dataset[, 1:ncol(dataset)][dataset[, 1:ncol(dataset)] == 
                               "Weiß nicht / keine Angabe"] <- NA 
  dataset
}

# Remove these NA
# Input:  dataset - data frame
# Output: data frame without NAs
remove_na <- function(dataset){
  dataset[, 1:ncol(dataset)][dataset[, 1:ncol(dataset)] == 
                               "Weiß nicht / keine Angabe"] <- NA 
  dataset <- dataset[complete.cases(dataset), ]
  dataset <- drop.levels(dataset)
  dataset
}

# Uniform names for the parties
wave2[["Ontic"]] <- gsub(pattern = "GRÜNE", replacement = "Grüne", wave2[["Ontic"]])
wave2[["Ontic"]] <- gsub(pattern = "LINKE", replacement = "Linke", wave2[["Ontic"]])
wave3[["Ontic"]] <- gsub(pattern = "GRÜNE", replacement = "Grüne", wave3[["Ontic"]])
wave3[["Ontic"]] <- gsub(pattern = "LINKE", replacement = "Linke", wave3[["Ontic"]])


# Checking if Ontic and Sonntagsfrage are identical, if the person is decided 
# and remove it, if it's not the case
ontic_dec_wave2 <- subset(wave2, ontic_bin == 0)
ontic_dec_wave2 <- subset(ontic_dec_wave2, Ontic != Sonntagsfrage) 
# 0 observations
# wave2 <- anti_join(wave2, ontic_dec_wave2)

ontic_dec_wave3 <- subset(wave3, ontic_bin == 0)
ontic_dec_wave3 <- subset(ontic_dec_wave3, Ontic != Sonntagsfrage) 
# 0 observations
# wave3 <- anti_join(wave3, ontic_dec_wave3)


#####????????
# Check for undecided people if Sonntagsfrage is an element from Ontic
ontic_undec_wave2 <- subset(wave2, ontic_bin == 1)
ontic_undec_wave2$Identisch <- 
  ontic_undec_wave2$Sonntagsfrage %in% ontic_undec_wave2$Ontic

ontic_undec_wave3 <- subset(wave3, ontic_bin == 1)
ontic_undec_wave3$Identisch <- 
  ontic_undec_wave3$Sonntagsfrage %in% ontic_undec_wave3$Ontic
#####????????


# get Variable "Ontic_bin" for data_all
data_all <- data_all %>%
  mutate(ontic_bin = ifelse(Antwort == "Bin schon fest entschieden", 0,
                     ifelse(Antwort == "Bin noch nicht fest entschieden", 1, NA)))


# 4. Get new datasets -----------------------------------------------------

# Dataset including na's wave2 for the missing plot in describe_data.R
wave2_missings <- select(wave2, - c(X)) %>%
  show_na

# Use all defined functions on the datasets
wave2 <- select(wave2, - c(X)) %>%
  no_others %>%
  no_others(variable = "Sonntagsfrage") %>% 
  change_ontic %>% 
  show_na %>%
  remove_na %>%
  minimize_ontic

wave3 <- select(wave3, - c(X)) %>%
  no_others %>%
  no_others(variable = "Sonntagsfrage") %>%
  change_ontic %>% 
  show_na %>%
  remove_na %>%
  minimize_ontic


data_all<- data_all %>%
  show_na 

# Scaling of the weight to get again the number of observations 
# as the sum of the weight
wave2$Gewicht <- wave2$Gewicht + ((nrow(wave2) - sum(wave2$Gewicht))/sum(wave2$Gewicht))
wave3$Gewicht <- wave3$Gewicht + ((nrow(wave3) - sum(wave3$Gewicht))/sum(wave3$Gewicht))

write.csv(wave2, './Data/wave2_clean.csv')
write.csv(wave2_missings, './Data/wave2_missings.csv')
write.csv(wave3, './Data/wave3_clean.csv')
write.csv(data_all, './Data/data_all_clean.csv')





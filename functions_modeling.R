
# Functions that are used for modeling. R
library(randomForest)
library(ranger)
library(splitTools)
library(Metrics) # rmse
library(RColorBrewer)
library(patchwork)
library(radiant.data)
library(splitstackshape)
library(ggpubr)
library(gridExtra)
library(tidyverse)


# 1. Random Forest =======================================================


# A) Stratified CV --------------------------------------------------------

# Function for getting the accuracy values for mtry from 1 to 12
# The accuracy is the mean of an 5-fold Cross validation
# Input:  dataset - data frame
#         seed - (optional) random generator seed
#         weight_type - which weight should be used for fitting the model. 
#                       civey: using weights given in dataset
#                       none: using no weights
#                       simulation: using simulated weights
# Output: list of 12 accuracy values as a result of tuning mtry. 
#         The first value e.g. is for mtry = 1
#         For weight_type == "simulation" the output is a list of the mean
#         and variance of 100 accuracy values
getting_accuracy <- function(dataset, seed = NULL,
                             weight_type = c("civey", "none", "simulation")){
  
  dataset_decided <- subset(dataset, ontic_bin == 0)
  dataset_decided <- select(dataset_decided, -c(X, Nutzer, Frage, Zeitpunkt, 
                                                Antwort, Sonntagsfrage, CDU.CSU, 
                                                SPD, Grüne, FDP, AfD, Linke, 
                                                ontic_bin))
  
  dataset_decided$Ontic <- as.factor(dataset_decided$Ontic)
  
  set.seed(seed) # seed for getting same results
  weight_type <- match.arg(weight_type) # only 3 options possible 
  
  
  # Get stratified cross-validation in-sample indices
  folds <- create_folds(dataset_decided$Ontic, k = 5, type = "stratified")
  
  # proportion check for each fold
  if (!all.equal(prop.table(table(dataset_decided[folds$Fold1, ]$Ontic)),
                 prop.table(table(dataset_decided[folds$Fold2, ]$Ontic)),
                 prop.table(table(dataset_decided[folds$Fold3, ]$Ontic)),
                 prop.table(table(dataset_decided[folds$Fold4, ]$Ontic)),
                 prop.table(table(dataset_decided[folds$Fold5, ]$Ontic)),
                 prop.table(table(dataset_decided$Ontic)), tolerance = 0.1)) {
    stop("Proportions are not the same")
  }
  
  # Tune mtry by GridSearchCV
  valid_mtry <- numeric(ncol(dataset_decided) - 2) 
  # 2 because of "Ontic" and excluding "Gewicht" later
  
  # for loop: i = amount of variables to tune mtry 
  for (i in seq_along(valid_mtry)) {
    cv_mtry <- numeric()
    # for loop k = 5 
    for (fold in folds) {
      
      # so that function is running for weight_type == "simulation"
      weights <- NULL
      # defining weights and excluding the variable then from the training dataset
      # 1. case: civey weights
      if(weight_type == "civey") {
        weights <- dataset_decided[fold, ]$Gewicht
      }
      # 2. case: no weights
      if(weight_type == "none") {
        weights <- NULL
      }
      
      # split into train and test dataset
      train <- select(dataset_decided[fold, ], -c(Gewicht))
      test <- select(dataset_decided[-fold, ], -c(Gewicht))
      
      # fit with mtry from 1 to 12
      fit <- ranger(Ontic ~ ., data = train, mtry = i,
                    case.weights = weights)
      pred_cv <- predict(fit, data = test)
      # confusion matrix for calculating the accuracy
      confusion_matrix <- as.matrix(table(Actual = test$Ontic, 
                                          Predicted = pred_cv$predictions))
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      cv_mtry <- c(cv_mtry, accuracy) # get the current accuracy and the ones before
      
    }
    valid_mtry[i] <- mean(cv_mtry) # get the mean of the k = 5 accuracy values
  }
  
  # best mtry with highest accuracy 
  best_mtry <- which.max(valid_mtry)
  
  # 3. case: simulated weights
  # for simulated weights: no cross validaiton but a replication of the final_fit 
  # 100 times for calculating the mean accuracy 
  if(weight_type == "simulation") {
    weights_sim <- rexp(10000, 1)
    
    sim_replicate <- replicate(100, {
      set.seed(seed)
      # split data into train and test using stratified sampling
      add_id <- rowid_to_column(dataset_decided, "ID")
      train <- add_id %>% stratified(., group = "Ontic", size = 0.8)
      test <- add_id[-train$ID, ]
      train <- select(train, -c(ID, Gewicht))
      test <- select(test, -c(ID, Gewicht))
      
      ind <- sample(1:length(weights_sim), nrow(train))
      weights <- weights_sim[ind]
      fit <- ranger(Ontic ~ ., data = train, mtry = best_mtry,
                    case.weights = weights)
      pred_cv <- predict(fit, data = test)
      confusion_matrix <- as.matrix(table(Actual = test$Ontic,
                                          Predicted = pred_cv$predictions))
      
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      accuracy
    })
    # get mean and variance of the 100 accuracy values
    accuracy_decided_sim <- list(mean_accuracy_sim = mean(sim_replicate),
                                 var_accuracy_sim = var(sim_replicate))
    return(accuracy_decided_sim) # stopping function here
  }
  
  
  # list of all accuracy values from CV and from the final and the best mtry
  # to use for predicting the undecided people
  accuracy_decided <- list(accuracy_tuned_mtry = valid_mtry,
                           best_mtry = best_mtry, 
                           accuracy = max(valid_mtry)) 
  accuracy_decided
  
}

# Function for plotting an example of a simulated weight
# Input:  dataset - data frame
#         title - charcter describing the title of the plot
# Output: ggplot of simulated weight distribution
plotting_simulation_example <- function(dataset, title) {
  # One example for showing the distribution of the simulated weight
  weight_example_sim <- rexp(nrow(dataset), 1)
  data_sim <- cbind(select(dataset, -c(X, Nutzer, Frage, Zeitpunkt, Antwort,
                                       Sonntagsfrage)), 
                    Gewicht_sim = weight_example_sim)
  
  sim_weight <- ggplot(data_sim, aes(x = Gewicht_sim)) +
    geom_histogram(bins = 150, position = "dodge", fill = "#0072B2") +
    scale_fill_manual(values = c("#0072B2")) +
    geom_vline(xintercept = 1, linetype = 'dashed', col = 'darkgrey') +
    labs(title = title,
         y = "Anzahl Beobachtungen",
         x = "Simuliertes Gewicht") + 
    theme(plot.title = element_text(size = 16))
}



# B) Prediction -----------------------------------------------------------


# Function for plotting the predictions of the random forest model to 
# predict the parties of undecided people
# Input:  dataset - data frame
#         weight_type - which weight should be used for fitting the model. 
#                       civey: using weights given in dataset
#                       none: using no weights
#                       simulation: using simulated weights
#         title - title of the plot
# Output: ggplot of predicted frequencies of the undecided for each party 
predicting_undecided <- function(dataset,
                                 weight_type = c("civey", "none"), 
                                 title, best_mtry){
  # Ranger package including weights, all possible variables and 
  # growing a probability forest
  
  # Defining new data sets 
  dataset_decided <- subset(select(dataset, -c(X, Nutzer, Frage, Zeitpunkt, Antwort)),
                            ontic_bin == 0)
  dataset_undecided <- subset(select(dataset, -c(X, Nutzer, Frage, Zeitpunkt, Antwort)),
                              ontic_bin == 1)
  x_undecided <- select(dataset_undecided, -c(Ontic))
  
  # defining weights and excluding the variable then from the training dataset
  # 1. case: civey weights
  if(weight_type == "civey") {
    weights <- dataset_decided$Gewicht
  }
  # 2. case: no weights
  if(weight_type == "none") {
    weights <- NULL
  }
  dataset_decided_rf <- select(dataset_decided, -c(Gewicht, ontic_bin, CDU.CSU, 
                                                   SPD, Grüne, FDP, AfD, Linke,
                                                   Sonntagsfrage))
  x_undecided <- select(x_undecided, -c(Gewicht, ontic_bin, CDU.CSU, SPD, Grüne, 
                                        FDP, AfD, Linke, Sonntagsfrage))
  
  rf <- ranger(as.factor(Ontic) ~., data = dataset_decided_rf, mtry = best_mtry,
               case.weights = weights, probability = TRUE) 
  pred <- predict(rf, data = x_undecided) 
  
  # Getting matrix for normalization with probabilities
  prob <- pred$predictions 
  
  # Matrix for normalization (get the same order as in prediction)
  normalization_matrix <-
    as.matrix(dataset_undecided[, c("AfD", "CDU.CSU", "FDP", "Grüne", "Linke", "SPD")])
  
  # multiplication of both matrices 
  matrix_multiplication <- prob*normalization_matrix
  
  # Include only the possible sets per person
  set_prediction <- matrix_multiplication/rowSums(matrix_multiplication)
  
  # get the mean of the probabilities per party as probability
  pred_mean <- colMeans(set_prediction)
  
  # get absolute numbers
  pred_absolute <- rep(356, 6)*colMeans(set_prediction)
  
  # get relative numbers
  pred_relative <- data.frame(party = names(pred_absolute), 
                              predictions = as.numeric(pred_absolute/
                                                         sum(pred_absolute)))
  
  # combining results of undecided with results of decided
  pred_relative$predictions <- round((prop.table(table(dataset_decided$Ontic)) *
                                        nrow(dataset_decided) +
                                        pred_relative$predictions *
                                        nrow(dataset_undecided))/nrow(dataset), 3) * 100
  
  # plotting the results
  rf_predictions <- 
    ggplot(pred_relative, aes(x = predictions, y = reorder(party, predictions))) +
    geom_bar(stat = "identity",
             fill = c("blue3", "black", "gold",
                      "green4", "mediumvioletred", "firebrick3")) +
    geom_text(aes(label = predictions), position = position_stack(vjust = 0.75), 
              size = 4, col = "white") +
    labs(title = title,
         x = "Anteil (in %)",
         y = "Partei")
  
  rf_predictions
}

# 2. Dempster Bounds ======================================================

# Defining functions

# Function getting the lower and upper bound of the dempster bounds 
# (with/without weights and with/without restricitons)
# Input:  dataset - data frame
#         party (optional) - possible options: SPD, CDU.CSU, FPD, Linke, Grüne, AfD
#         type - possible options: demp, weighted_demp, res_demp
#         coalition (optional) - choose any combinations of the 6 parties 
#                                to build a coalition
# Output: data frame for the chosen party with upper and lower bound
#         -> depending on the chosen type, the correct output is shown
get_dempster_bounds <- function(dataset, party = c("SPD", "CDU.CSU", "FDP",
                                                   "Linke", "Grüne", "AfD"),
                                type = c("demp", "weighted_demp", 
                                         "res_demp", "coalitions"),
                                coalition = c("CDU.CSU", "SPD")) {
  party <- match.arg(party) # only 6 options possible 
  type <- match.arg(type)
  dataset_upper <- dataset[, c("ontic_bin", "CDU.CSU", "FDP", "SPD", "Linke", 
                               "Grüne", "AfD", "Gewicht")]
  dataset_lower <- dataset[, c("ontic_bin", "CDU.CSU", "FDP", "SPD", "Linke", 
                               "Grüne", "AfD", "Gewicht")]
  #dataset_without_party <- dataset[, !names(dataset) %in% party]
  sum_undecided_lower <- 0 # count for 80/20 restriction
  sum_decided_lower <- 0 # count for 80/20 restriction
  sum_undecided_upper <- 0 
  sum_decided_upper <- 0 
  
  # depending on dempster bound type - different output
  
  # 1. Normal dempster bounds by using the count
  if(type == "demp"){
    # lower bound
    for(i in 1:nrow(dataset_lower)) {
      # if person is undecided and party is part of the considerationset,
      # then set entry of this party from 1 to 0
      if(dataset_lower[["ontic_bin"]][i] == 1 && dataset_lower[[party]][i] == 1){
        dataset_lower[[party]][i] <- 0
      }
    }
    demp_lower <- sum(dataset_lower[[party]]) / nrow(dataset_lower) * 100
    # upper bound
    demp_upper <- sum(dataset_upper[[party]]) / nrow(dataset_upper) * 100
    
    bounds <- data.frame(party = party,
                         lower_bound = demp_lower,
                         upper_bound = demp_upper)
  }
  # 2. Dempster bounds using "Gewichte" instead of count
  if(type == "weighted_demp"){
    # lower bound
    for(i in 1:nrow(dataset_lower)) {
      # if person is undecided and party is part of the considerationset,
      # then set entry of this party from 1 to 0
      if(dataset_lower[["ontic_bin"]][i] == 1 && dataset_lower[[party]][i] == 1){
        dataset_lower[[party]][i] <- 0
      }
    }
    subset_lower <- dataset_lower[c(party, "Gewicht")]
    subset_lower[subset_lower == 0] <- NA
    subset_lower <- na.omit(subset_lower)
    demp_weights_lower <- sum(subset_lower$Gewicht) / nrow(dataset_lower) * 100
    # upper bound
    subset_upper <- dataset_upper[c(party, "Gewicht")]
    subset_upper[subset_upper == 0] <- NA # Exclude zeros
    subset_upper <- na.omit(subset_upper) 
    demp_weights_upper <- sum(subset_upper$Gewicht) / nrow(dataset_upper) * 100
    
    bounds <- data.frame(party = party,
                         lower_bound = demp_weights_lower,
                         upper_bound = demp_weights_upper)
  }
  # 3. Dempster bounds wiht 80/20 restriction
  if(type == "res_demp"){
    # lower bound
    # for loop: for each row in dataset_lower
    for(i in 1:nrow(dataset_lower)) {
      # if person is undecided and party is part of the considerationset,
      # then set entry of this party from 1 to 0
      if(dataset_lower[["ontic_bin"]][i] == 1 && dataset_lower[[party]][i] == 1){
        dataset_lower[[party]][i] <- 0
        # using "Gewicht" instead of the count
        sum_undecided_lower <- sum_undecided_lower + dataset_lower[["Gewicht"]][i]
      }
      # if person is decided and the party is the chosen one,
      # then count Gewicht 
      if(dataset_lower[["ontic_bin"]][i] == 0 && dataset_lower[[party]][i] == 1){
        sum_decided_lower <- sum_decided_lower + dataset_lower[["Gewicht"]][i]
      }
    }
    
    # upper bound
    # for loop: for each row in dataset_upper
    for(i in 1:nrow(dataset_upper)) {
      # if person is undecided and the party is part of the considerationset,
      # then count Gewicht 
      if(dataset_upper[["ontic_bin"]][i] == 1 && dataset_upper[[party]][i] == 1){
        # using "Gewicht" instead of the count
        sum_undecided_upper <- sum_undecided_upper + dataset_upper[["Gewicht"]][i] 
        # used for res_demp
      }
      # if person is decided and the party isthe chosen one,
      # then count Gewicht 
      if(dataset_upper[["ontic_bin"]][i] == 0 && dataset_upper[[party]][i] == 1){
        sum_decided_upper <- sum_decided_upper + dataset_upper[["Gewicht"]][i] 
        # used for res_demp
      }
    }
    
    res_demp_lower <- ((sum_undecided_lower - sum_undecided_lower*0.8) +
                         sum_decided_lower) / nrow(dataset_lower) * 100
    res_demp_upper <- (sum_undecided_upper * 0.8 + sum_decided_upper) /
      nrow(dataset_upper) * 100
    bounds <- data.frame(party = party,
                         lower_bound = res_demp_lower,
                         upper_bound = res_demp_upper)
  }
  
  # 4. Dempster bounds with coalitions instead of parties
  if(type == "coalitions"){
    # lower bound
    for(i in 1:nrow(dataset_lower)) {
      # if person is undecided, the coalition is part of the considerationset and 
      # no other party is part of it: then count sum_undecided_coalition
      if(dataset_lower[["ontic_bin"]][i] == 1 &&
         sum(dataset_lower[coalition][i, ]) == length(coalition) &&
         sum(select(dataset_lower[i, ], -c(ontic_bin, Gewicht))) == length(coalition)){
        
        sum_undecided_lower <- sum_undecided_lower + dataset_lower[["Gewicht"]][i]
      }
      # if person is decided and one of the party from the chosen coalition is the 
      # chosen one: count sum_decided_lower
      if(dataset_lower[["ontic_bin"]][i] == 0 && sum(dataset_lower[coalition][i, ]) == 1){
        sum_decided_lower <- sum_decided_lower + dataset_lower[["Gewicht"]][i]
      }
    }
    # upper bound
    for(i in 1:nrow(dataset_upper)) { 
      # if person is undecided, the coalition is part of the considerationset: 
      # count sum_undecided_upper
      if(dataset_upper[["ontic_bin"]][i] == 1 && 
         sum(dataset_lower[coalition][i, ]) == length(coalition)){
        sum_undecided_upper <- sum_undecided_upper + dataset_upper[["Gewicht"]][i]
      }
      # if person is decided and one of the party from the chosen coalition is the 
      # chosen one: count sum_decided_upper
      if(dataset_upper[["ontic_bin"]][i] == 0 && sum(dataset_upper[coalition][i, ]) == 1){
        sum_decided_upper <- sum_decided_upper + dataset_upper[["Gewicht"]][i]
      }
    }
    
    # lower bound: sum of decided people with one of the party chosen and
    # of undecided people with coalition being only part of considerationset
    coalition_demp_lower <- (sum_undecided_lower + sum_decided_lower) /
      nrow(dataset_lower) * 100
    # upper bound: sum of decided people with one of the party chosen and 
    # of undecided people with coalition being part of the considerationset
    coalition_demp_upper <- (sum_undecided_upper + sum_decided_upper) /
      nrow(dataset_upper) * 100
    bounds <- data.frame(coalition = paste(coalition, collapse = ', '),
                         lower_bound = coalition_demp_lower,
                         upper_bound = coalition_demp_upper)
  }
  
  bounds
}


# Function for plotting the dempster bounds
# Input:  dataset - data frame
#         party (optional) - possible options: SPD, CDU.CSU, FPD, Linke, Grüne, AfD
#         demp_type - possible options: demp, weighted_demp, res_demp
# Output: data frame that combines lower_bound, upper_bound and middle point
#         for every party
combining_bounds <- function(dataset,
                             demp_type = c("demp", "weighted_demp", 
                                           "res_demp", "coalitions")) {
  
  dempster_bounds <- rbind(
    get_dempster_bounds(dataset, party = "CDU.CSU", type = demp_type),
    get_dempster_bounds(dataset, party = "FDP", type = demp_type),
    get_dempster_bounds(dataset, party = "SPD", type = demp_type),
    get_dempster_bounds(dataset, party = "Linke", type = demp_type),
    get_dempster_bounds(dataset, party = "Grüne", type = demp_type),
    get_dempster_bounds(dataset, party = "AfD", type = demp_type)
  )
  
  # get middle point for 50/50 restriction
  for(i in 1:nrow(dempster_bounds)){
    dempster_bounds[["middle"]][i] <- mean(c(dempster_bounds$lower_bound[i],
                                             dempster_bounds$upper_bound[i]))
    
  }
  dempster_bounds
}



# 3. Only decided including weights =======================================


# Getting prediction of decided people including the weights 
# Function to plot the parties of the decided ones using the sample weights
# Input:  dataset - data frame
# Output: ggplot of parties of the decided people
plotting_decided_weights <- function(dataset) {
  dataset %>%
    group_by(Ontic) %>% 
    subset(ontic_bin == 0) %>%
    summarise(count = sum(Gewicht)) %>%  # using Gewicht instead of count 
    mutate(freq = round(count / sum(count), 3) * 100) %>% 
    ggplot(aes(x = reorder(Ontic, freq), y = freq)) +
    geom_bar(stat = "identity", 
             fill = c("blue3", "black", "gold",
                      "green4", "mediumvioletred", "firebrick3")) +
    coord_flip() +
    geom_text(aes(label = freq), position = position_stack(vjust = 0.8), 
              size = 4, col = "white") +
    labs(title = "Entschlossene",
         y = "Anteil (in %)",
         x = "Partei",
         fill = "Gewicht")
}


# 4. Comparison ===========================================================

# Getting prediction of Sonntagsfrage including the weights 
# Function to plot the Sonntagsfrage of the decided and undecided ones
# Input:  dataset - data frame
# Output: ggplot of Sonntagsfrage of the decided and undecided people
plotting_forced_decision <- function(dataset){
  dataset %>%
    group_by(Sonntagsfrage) %>%
    summarise(count = sum(Gewicht)) %>% 
    mutate(freq = round(count / sum(count), 3) * 100) %>% 
    ggplot(aes(x = reorder(Sonntagsfrage, freq), y = freq)) +
    geom_bar(stat = "identity",
             fill = c("blue3", "black", "gold",
                      "green4", "mediumvioletred", "firebrick3")) +
    coord_flip() +
    geom_text(aes(label = freq), position = position_stack(vjust = 0.75), 
              size = 4, col = "white") + 
    labs(title = "Sonntagsfrage",
         x = "Partei",
         y = "Anteil (in %)")
}


# Getting prediction of Sonntagsfrage without weights 
# Function to plot the Sonntagsfrage of the decided and undecided ones
# Input:  dataset - data frame
# Output: ggplot of Sonntagsfrage of the decided and undecided people
plotting_forced_decision_no_weights <- function(dataset){
  dataset %>%
    group_by(Sonntagsfrage) %>%
    summarise(count = n()) %>% 
    mutate(freq = round(count / sum(count), 3) * 100) %>% 
    ggplot(aes(x = reorder(Sonntagsfrage, freq), y = freq)) +
    geom_bar(stat = "identity",
             fill = c("blue3", "black", "gold",
                      "green4", "mediumvioletred", "firebrick3")) +
    coord_flip() +
    geom_text(aes(label = freq), position = position_stack(vjust = 0.75), 
              size = 4, col = "white") + 
    labs(title = "Sonntagsfrage",
         x = "Partei",
         y = "Anteil (in %)")
}

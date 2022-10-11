library(tidyr)
library(ggplot2)
library(dplyr)
library(cowplot)
library(forcats)
library(gridExtra)
library(grid)
library(patchwork)

# read clean data 
wave2 <- read.csv("./Data/wave2_clean.csv", header = TRUE, sep = ",", fill = TRUE,
                  fileEncoding = "UTF-8")
wave2_missings <- read.csv("./Data/wave2_missings.csv", header = TRUE, sep = ",",
                           fill = TRUE, fileEncoding = "UTF-8")
wave3 <- read.csv("./Data/wave3_clean.csv", header = TRUE, sep = ",", fill = TRUE, 
                  fileEncoding = "UTF-8")

data_all <- read.csv("./Data/data_all_clean.csv", header = TRUE, sep = ",",
                        fill = TRUE, fileEncoding = "UTF-8")

# set theme for all of the plots
theme_set(theme_light() +
            theme(panel.background = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 14), 
                  axis.title.x = element_text(size = 12),
                  axis.title.y = element_text(size = 12)))


# how many people are decided after cleaning data?
round(prop.table(table(wave2$ontic_bin)), 3)
round(prop.table(table(wave3$ontic_bin)), 3)


# 1. Describing missing values --------------------------------------------

# Function for getting percentage of missings
# Input:  dataset - data frame
# Output: grouped data frame showing the missing percantage of the variables
get_missings_info <- function(dataset) {
  dataset[, c("Berufliche.Stellung", "Berufsbildung", 
            "Beschäftigungsstatus", "Familienstand", 
            "Kinder.im.Haushalt", "Religion", 
            "Bildungsabschluss")] %>% 
    gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    group_by(key) %>%
    mutate(total = n()) %>% 
    group_by(key, total, isna) %>%
    summarise(num.isna = n()) %>% # get n of NA per column
    mutate(pct = num.isna / total * 100) # get percentage per column
}

# Function for plotting percantage of missings per variable
# Input:  dataset - data frame
#         title - charcter describing the title of the plot
# Output: ggplot of missing percentage per variable
missings_plot <- function(dataset, title) {
  get_missings_info(dataset) %>% 
    mutate(pct_m = ifelse(isna == FALSE, pct-50, pct)) %>% 
    mutate(key = fct_rev(factor(key, levels =  
                                  c("Berufliche.Stellung", "Berufsbildung", 
                                    "Beschäftigungsstatus", "Familienstand", 
                                    "Kinder.im.Haushalt", "Religion",
                                    "Bildungsabschluss")))) %>% 
    ggplot() +
    geom_bar(aes(x = key, y = pct_m, fill = isna), 
             stat = 'identity', alpha = 0.8) +
    scale_fill_manual(name = "", 
                      values = c('steelblue', 'grey'), 
                      labels = c("Präsent", "Fehlend")) +
    coord_flip() +
    labs(title = title, 
         x = 'Variable', y = "% der fehlenden Werte") +
    scale_x_discrete(labels = c('Berufliche.Stellung' = "Berufliche Stellung",
                                'Berufsbildung' = "Berufsbildung",
                                'Beschäftigungsstatus' = "Beschäftigungsstatus",
                                'Familienstand' = "Familienstand",
                                'Kinder.im.Haushalt' = "Kinder im Haushalt", 
                                "Religion" = "Religion",
                                "Bildungsabschluss" = "Bildungsabschluss"))
}

missings_plot_wave2 <- missings_plot(wave2_missings, "Stichprobe")
missings_plot_all <- missings_plot(data_all, "Gesamter Datensatz")

# combining the plots
comparison_missings <- missings_plot_all + 
  missings_plot_wave2 + plot_annotation(
  title =  'Anteil fehlender Werte', 
  theme = theme(plot.title = element_text(size = 16)))

ggsave("./Plots/comparison_missings.png",
       width = 11,
       height = 6,
       dpi = 1000)

# remove na's from data_all
data_all <- na.omit(data_all) 


# 2. Sample Generation ----------------------------------------------------

# A) Comparing Covariables "Geschlecht" and "Alter" in both data frames:
#    data_all and wave2

# Function for plotting a covariable by decided/undecided 
# Input:  dataset - data frame
#         variable - character of variable that should be plotted
#         title - charcter describing the title of the plot
# Output: ggplot of covariable by decided/undecided
plotting_covariables <- function(dataset, variable, title) {
  dataset %>% 
  ggplot(aes(x = factor(dataset[, variable]), 
                    fill = factor(ontic_bin))) +
    geom_bar(aes(y = (..count..)/sum(..count..)), position = "stack") +
    scale_fill_manual(values = c("#0072B2", "#56B4E9"), labels = c("nein", "ja")) +
    labs(title = title,
         y = "Anteil",
         x = variable,
         fill = "Person unentschieden?")
  
}

plot_sex_all <- plotting_covariables(data_all, variable = "Geschlecht",
                                     title = "Gesamter Datensatz")
plot_sex_wave2 <- plotting_covariables(wave2, variable ="Geschlecht",
                                       title = "Stichprobe")


comparison_sex <- plot_sex_all + 
  plot_sex_wave2 + plot_annotation(
    title = 'Relative Häufigkeit des Geschlechts', 
    theme = theme(plot.title = element_text(size = 16)))

ggsave("./Plots/comparison_sex.png",
       width = 11,
       height = 6,
       dpi = 1000)

plot_age_all <- plotting_covariables(data_all, variable = "Alter",
                                     title = "Gesamter Datensatz")
plot_age_wave2 <- plotting_covariables(wave2, variable ="Alter",
                                       title = "Stichprobe")

comparison_age <- plot_age_all + 
  plot_age_wave2 + plot_annotation(
    title = 'Relative Häufigkeit des Alters', 
    theme = theme(plot.title = element_text(size = 16)))


ggsave("./Plots/comparison_age.png",
       width = 11,
       height = 6,
       dpi = 1000)


# B) Considering the variable weight

# Distribution of the variable weight
distribution_weight_wave2 <- ggplot(wave2, aes(x = Gewicht)) +
  geom_histogram(bins = 150, position = "dodge", fill = "#0072B2") +
  scale_fill_manual(values = c("#0072B2")) +
  xlim(0, 4) +
  geom_vline(xintercept = 1, linetype = 'dashed', col = 'darkgrey') +
  labs(title = "Verteilung der Variable Gewicht",
       y = "Anzahl",
       x = "Gewicht") + 
  theme(plot.title = element_text(size = 16))

ggsave("./Plots/distribution_weight_wave2.png",
       width = 11,
       height = 6,
       dpi = 1000)


# Plotting variable age concluding the weights
distribution_age <- wave2 %>% 
  mutate(Gewicht = fct_rev(cut(Gewicht, c(0, 1, 3, 9)))) %>%
  ggplot(aes(x = factor(Alter), fill = Gewicht)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), colour = "white", size = 0.2) +
  scale_fill_manual(values = c("darkred", "#FE7773", "#FFC2C3")) +
  labs(title = "Verteilung Alter",
       y = "Anteil",
       x = "Alter",
       fill = "Gewicht")


# Getting the frequencies of the weight for each age group
wave2_weights <- wave2 %>% 
  group_by(Alter) %>% 
  summarise(Freq = round(sum(Gewicht)/nrow(wave2), 2))

# Plotting the weights instead of the count
distribution_age_weights <- 
  ggplot(wave2_weights, aes(x = Alter, y = Freq)) +
  geom_bar(stat = "identity", fill = "grey60") + #, alpha = 0.7
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.9), 
            size = 4, col = "black") +
  labs(title = "Anteil Gewicht pro Altersgruppe",
       y = "Anteil",
       x = "Alter",
       fill = "Gewicht")


age_including_weights <- distribution_age + 
  distribution_age_weights + plot_annotation(
    title = 'Berücksichtigung der Variable Gewicht', 
    theme = theme(plot.title = element_text(size = 16)))


ggsave("./Plots/age_including_weights.png",
       width = 11,
       height = 6,
       dpi = 1000)




# 3. Descriptive Analysis -------------------------------------------------


# Plot zur Darstellung der Entschiedenen/Unentschiedenen -> absolute Häufigkeit
# Für 2. und 3. Welle

# Chosen Parties of the decided Persons 
# Function to plot the data of the decided ones
# Input:  dataset - data frame
# Output: ggplot of choices of the decided people
plotting_decided <- function(dataset){
  dataset %>%
    group_by(Ontic) %>% 
    subset(ontic_bin == 0) %>%
    summarise(count = n()) %>% 
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

# Function to plot the data of the decided ones
# Input:  dataset - data frame
# Output: ggplot of choices of the decided people
plotting_undecided <- function(dataset){
  dataset %>%
    group_by(Ontic) %>%
    subset(ontic_bin == 1) %>% 
    summarise(count = n()) %>%
    mutate(freq = round(count / sum(count), 3) * 100) %>% 
    ggplot(aes(x = reorder(Ontic, freq), y = freq)) +
    geom_bar(stat = "identity", 
             fill = "grey50") + 
    coord_flip() +
    geom_text(aes(label = freq), position = position_stack(vjust = 0.85), 
              size = 4, col = "white") +
    scale_x_discrete(labels = c("CDU/CSU-.-FDP" = "CDU/CSU, FDP", 
                                "SPD-.-Grüne" = "SPD, Grüne", 
                                "SPD-.-Grüne-.-Linke" = "SPD, Grüne, Linke",
                                "CDU/CSU-.-FDP-.-AfD" = "CDU/CSU, FDP, AfD", 
                                "CDU/CSU-.-SPD-.-FDP" = "CDU/CSU, SPD, FDP", 
                                "CDU/CSU-.-SPD" = "CDU/CSU, SPD",
                                "CDU/CSU-.-SPD-.-Grüne" = "CDU/CSU, SPD, Grüne",
                                "FDP-.-AfD" = "FDP, AfD",
                                "Grüne-.-Linke" = "Grüne, Linke",
                                "CDU/CSU-.-SPD-.-Grüne-.-FDP" = 
                                  "CDU/CSU, SPD, Grüne, FDP")) +
    labs(title = "Unentschlossene",
         y = "Anteil (in %)",
         x = "Parteikombination")
}

decided_wave2 <- plotting_decided(wave2)
undecided_wave2 <- plotting_undecided(wave2)

comparison_decided_undecided_wave2 <- decided_wave2 + 
  undecided_wave2 + plot_annotation(
    title = 'Verteilung der Parteien - 2. Welle', 
    theme = theme(plot.title = element_text(size = 16)))

ggsave("./Plots/comparison_decided_undecided_wave2.png",
       width = 11,
       height = 6,
       dpi = 1000)



decided_wave3 <- plotting_decided(wave3)
undecided_wave3 <- plotting_undecided(wave3)

comparison_decided_undecided_wave3 <- decided_wave3 + 
  undecided_wave3 + plot_annotation(
    title = 'Verteilung der Parteien - 3. Welle', 
    theme = theme(plot.title = element_text(size = 16)))

ggsave("./Plots/comparison_decided_undecided_wave3.png",
       width = 11,
       height = 6,
       dpi = 1000)





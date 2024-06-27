library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

# tidying
df <- read_excel("../excel files/DD_Cali_001.xlsx")

## create subset for G/R greater than 0
positive_GbyR <- subset(df, `G / R` > 0)

## add concentration column
positive_GbyR$Conc <- 0
positive_GbyR$Conc <- ifelse(positive_GbyR$Well == "B04" | positive_GbyR$Well == "B05", "DMSO",
                          ifelse(positive_GbyR$Well == "C04" | positive_GbyR$Well == "C05", "1uM",
                                   ifelse(positive_GbyR$Well == "D04" | positive_GbyR$Well == "D05", "10uM","100uM")))

##create simplified G/R and O/R dataframe with only columns of interest
simple_GbyR <- positive_GbyR[, c("Well", "G / R", "Conc")]


## Change (reverse) order of factors to DMSO, 1uM, 10uM, 100uM rather than alphabetical order
simple_GbyR$Conc <- factor(simple_GbyR$Conc, levels=c("DMSO", "1uM", "10uM", "100uM"))

#find mean G/R values by well
bywell <- simple_GbyR %>%
  group_by(Well) %>%
  summarise_at(c("G / R"), list(mean))

# Repeat tidying for O/R
positive_ObyR <- subset(df, `O / R` > 0)

## add concentration column
positive_ObyR$Conc <- 0
positive_ObyR$Conc <- ifelse(positive_ObyR$Well == "B04" | positive_ObyR$Well == "B05", "DMSO",
                             ifelse(positive_ObyR$Well == "C04" | positive_ObyR$Well == "C05", "1uM",
                                    ifelse(positive_ObyR$Well == "D04" | positive_ObyR$Well == "D05", "10uM","100uM")))

##create simplified O/R dataframe with only columns of interest
simple_ObyR <- positive_ObyR[, c("Well", "O / R", "Conc")]

#find mean O/R values by well
bywell <- simple_ObyR %>%
  group_by(Well) %>%
  summarise_at(c("O / R"), list(mean))

## Change (reverse) order of factors to DMSO, 1uM, 10uM, 100uM rather than alphabetical order
simple_ObyR$Conc <- factor(simple_ObyR$Conc, levels=c("DMSO", "1uM", "10uM", "100uM"))

# plot
ggplot(data = simple_GbyR, aes(x = Conc, y = `G / R`)) + 
  geom_boxplot(aes(fill = Conc)) +
  ylim(0, 1)

ggplot(data = simple_GbyR, aes(x = `G / R`, fill = Conc)) + 
  geom_histogram(bins = 50) +
  xlim(0, 1) +
  facet_wrap( ~ Conc, ncol = 1)

ggplot(data = simple_ObyR, aes(x = Conc, y = `O / R`)) + 
  geom_boxplot(aes(fill = Conc)) +
  ylim(0, 1)

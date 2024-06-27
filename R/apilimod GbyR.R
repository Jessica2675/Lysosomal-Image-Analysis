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

##create simplified G/R dataframe with only columns of interest
simple_GbyR <- positive_GbyR[, c("Well", "G / R", "Conc")]

## Change (reverse) order of factors to DMSO, 1uM, 10uM, 100uM rather than alphabetical order
simple_GbyR$Conc <- factor(simple_GbyR$Conc, levels=c("DMSO", "1uM", "10uM", "100uM"))

# plot
qplot(data = simple_GbyR, x = Conc, y = `G / R`) +
  ylim(0, 1)
ggplot(data = simple_GbyR, aes(x = Conc, y = `G / R`)) + 
  geom_boxplot(aes(fill = Conc)) +
  ylim(0, 1)

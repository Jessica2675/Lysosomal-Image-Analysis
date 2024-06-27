library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

# tidying
df <- read_excel("../excel files/apilimod plate 2/DD_Cells_002.xlsx")

## create subset for G/R greater than 0
positive_GbyR <- subset(df, `G / R` > 0)

## add concentration column
positive_GbyR$Conc <- 0
positive_GbyR$Conc <- ifelse(positive_GbyR$Well == "B02" | positive_GbyR$Well == "B03" | positive_GbyR$Well == "C02" | positive_GbyR$Well == "C03", "DMSO_L4992",
                             ifelse(positive_GbyR$Well == "B04" | positive_GbyR$Well == "B05" | positive_GbyR$Well == "C04" | positive_GbyR$Well == "C05","DMSO_L3200", 
                                    ifelse(positive_GbyR$Well == "D02" | positive_GbyR$Well == "E02" | positive_GbyR$Well == "E03" | positive_GbyR$Well == "F02","100uM_L4992","100uM_L3200")))

##create simplified G/R dataframe with only columns of interest
simple_GbyR <- positive_GbyR[, c("Well", "G / R", "Conc")]


## Change (reverse) order of factors to DMSO, 1uM, 10uM, 100uM rather than alphabetical order
simple_GbyR$Conc <- factor(simple_GbyR$Conc, levels=c("DMSO_L4992", "DMSO_L3200", "100uM_L4992", "100uM_L3200"))

#find mean G/R values by well
bywell <- simple_GbyR %>%
  group_by(Well) %>%
  summarise_at(c("G / R"), list(mean))

# Repeat tidying for O/R
positive_ObyR <- subset(df, `O / R` > 0)

## add concentration column
positive_ObyR$Conc <- 0
positive_ObyR$Conc <- ifelse(positive_ObyR$Well == "B02" | positive_ObyR$Well == "B03" | positive_ObyR$Well == "C02" | positive_ObyR$Well == "C03", "DMSO_L4992",
                             ifelse(positive_ObyR$Well == "B04" | positive_ObyR$Well == "B05" | positive_ObyR$Well == "C04" | positive_ObyR$Well == "C05","DMSO_L3200", 
                                    ifelse(positive_ObyR$Well == "D02" | positive_ObyR$Well == "E02" | positive_ObyR$Well == "E03" | positive_ObyR$Well == "F02","100uM_L4992","100uM_L3200")))

##create simplified O/R dataframe with only columns of interest
simple_ObyR <- positive_ObyR[, c("Well", "O / R", "Conc")]



##create overall simple dataframe with G/R and O/R
simple <- df[, c("Area", "Well", "G / R", "O / R")]
simple <- subset(simple, `G / R` > 0)
simple <- subset(simple, `O / R` > 0)
simple$Conc <- ifelse(simple$Well == "B02" | simple$Well == "B03" | simple$Well == "C02" | simple$Well == "C03", "DMSO_L4992",
                             ifelse(simple$Well == "B04" | simple$Well == "B05" | simple$Well == "C04" | simple$Well == "C05","DMSO_L3200", 
                                    ifelse(simple$Well == "D02" | simple$Well == "E02" | simple$Well == "E03" | simple$Well == "F02","100uM_L4992","100uM_L3200")))
write.csv(simple, "GbyR ObyR Area plate002 cells.csv")


#find mean O/R values by well
bywell <- simple_ObyR %>%
  group_by(Well) %>%
  summarise_at(c("O / R"), list(mean))

## Change (reverse) order of factors to DMSO, 1uM, 10uM, 100uM rather than alphabetical order
simple_ObyR$Conc <- factor(simple_ObyR$Conc, levels=c("DMSO_L4992", "DMSO_L3200", "100uM_L4992", "100uM_L3200"))

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

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

# tidying
df <- read_excel("../excel files/processed apilimod data.xlsx", .name_repair = "minimal")

df$`B04-DMSO` <- "B04_DMSO"
df$`B05_DMSO` <- "B05_DMSO"
df$C04_1uM_Api <- "C04_1uM_Api"
df$C05_1uM_Api <- "C05_1uM_Api"
df$D04_10uM_Api <- "D04_10uM_Api"
df$D05_10uM_Api <- "D05_10uM_Api"
df$E04_100uM_Api <- "E04_100uM_Api"
df$E05_100uM_Api <- "E05_100uM_Api"


## separate dataframes for each well
B04_DMSO <- df[1:5]
names(B04_DMSO)[1] <- "Well"
B05_DMSO <- df[6:10]
names(B05_DMSO)[1] <- "Well"
C04_1uM_Api <- df[11:15]
names(C04_1uM_Api)[1] <- "Well"
C05_1uM_Api <- df[16:20]
names(C05_1uM_Api)[1] <- "Well"
D04_10uM_Api <- df[21:25]
names(D04_10uM_Api)[1] <- "Well"
D05_10uM_Api <- df[26:30]
names(D05_10uM_Api)[1] <- "Well"
E04_100uM_Api <- df[31:35]
names(E04_100uM_Api)[1] <- "Well"
E05_100uM_Api <- df[36:40]
names(E05_100uM_Api)[1] <- "Well"

## combine data from all wells in long format
overall <- bind_rows(B04_DMSO, B05_DMSO, C04_1uM_Api, C05_1uM_Api, D04_10uM_Api, D05_10uM_Api, E04_100uM_Api, E05_100uM_Api)
overall <- na.omit(overall)

## separate treatment and well number into separate columns
overall <- separate_wider_delim(overall, cols = Well, delim = "_", names = c("Well", "Treatment"), too_many = "merge")

## Change (reverse) order of factors to DMSO, 1uM, 10uM, 100uM rather than alphabetical order
overall$Treatment <- factor(overall$Treatment, levels=c("DMSO", "1uM_Api", "10uM_Api", "100uM_Api"))

##filter out pH < 3 or pH > 7
overall <- subset(overall, pH > 3)
overall <- subset(overall, pH < 7)

# plotting
comparisons <- list( c("DMSO", "1uM_Api"), c("DMSO", "10uM_Api"), c("DMSO", "100_Api"))
##pH
compare_means(pH ~ Treatment,  data = overall)
ggplot(data = overall, aes(x = Treatment, y = pH)) +
  geom_boxplot(aes(fill = Treatment)) +
  stat_compare_means() +
  geom_pwc(
    method = "t_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0.2
  )

##Ca
ggplot(data = overall, aes(x = Treatment, y = Ca)) + 
  geom_boxplot(aes(fill = Treatment)) + 
  ylim(0, 100) + 
  stat_compare_means() +   
  geom_pwc(
    method = "t_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0.2
  )

##Area
ggplot(data = overall, aes(x = Treatment, y = Area)) + 
  geom_boxplot(aes(fill = Treatment)) + 
  ylim(0, 350) + 
  stat_compare_means() +   
  geom_pwc(
    method = "t_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0.2
  )

##cumulative plot pH
ggplot(overall, aes(pH, colour = Treatment)) + stat_ecdf()

##cumulative plot Ca
ggplot(overall, aes(Ca, colour = Treatment)) + stat_ecdf()

##cumulative plot Area
ggplot(overall, aes(Area, colour = Treatment)) + stat_ecdf()


qplot(data = overall, x = Ca, y = pH, colour = Treatment) 
qplot(data = overall, x = Area, y = Ca, colour = Treatment)
qplot(data = overall, x = Area, y = pH, colour = Treatment)
ggplot(data = overall, aes(x = Treatment, y = Ca)) + geom_boxplot(aes(fill = Treatment)) + ylim(0, 100) + stat_compare_means()


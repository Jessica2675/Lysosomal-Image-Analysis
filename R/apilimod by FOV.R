library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

# tidying
df <- read_excel("../excel files/DD_Cali_001.xlsx")

##filter out pH < 3 or pH > 7
overall <- subset(df, pH > 3)
overall <- subset(overall, pH < 7)

##filter out Ca < 0 or Ca > 10000
overall <- subset(overall, Ca > 0)
overall <- subset(overall, Ca < 10000)

##combine well info and FOV info in new column
overall$Well_FOV <- paste(overall$Well, overall$FOV, sep="_")

#find mean values by FOV
byFOV <- overall %>%
  group_by(Well_FOV) %>%
  summarise_at(c("Area", "log(Ca)", "pH", "Ca"), list(mean))

## add concentration values to byFOV
byFOV$Conc <- ifelse(grepl("B04", byFOV$Well_FOV) | grepl("B05", byFOV$Well_FOV), "DMSO",
                             ifelse(grepl("C04", byFOV$Well_FOV) | grepl("C05", byFOV$Well_FOV), "1uM",
                                    ifelse(grepl("D04", byFOV$Well_FOV) | grepl("D05", byFOV$Well_FOV), "10uM","100uM")))


## Change (reverse) order of factors to DMSO, 1uM, 10uM, 100uM rather than alphabetical order
byFOV$Conc <- factor(byFOV$Conc, levels=c("DMSO", "1uM", "10uM", "100uM"))

# Plot
##pH
ggplot(data = byFOV, aes(x = Conc, y = pH)) +
  geom_boxplot(aes(fill = Conc)) +
  stat_compare_means() +
  geom_pwc(
    method = "t_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0.2
  )

##logCa
ggplot(data = byFOV, aes(x = Conc, y = `log(Ca)`)) +
  geom_boxplot(aes(fill = Conc)) +
  stat_compare_means() +
  geom_pwc(
    method = "t_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0.2
  )

##Area
ggplot(data = byFOV, aes(x = Conc, y = Area)) + 
  geom_boxplot(aes(fill = Conc)) + 
  stat_compare_means() +   
  geom_pwc(
    method = "t_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0.2
  )

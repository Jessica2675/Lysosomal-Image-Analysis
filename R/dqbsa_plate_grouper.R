library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

plate13 <- read.csv("../excel files/apilimod bafilomycin plate 13/excluded_dqbsa13.csv")
plate15 <- read.csv("../excel files/apilimod bafilomycin plate 15/excluded_dqbsa15.csv")
plate16 <- read.csv("../excel files/apilimod bafilomycin plate 16/excluded_dqbsa16.csv")
plate17 <- read.csv("../excel files/apilimod bafilomycin plate 17/excluded_dqbsa17.csv")
plate18 <- read.csv("../excel files/apilimod bafilomycin plate 18/excluded_dqbsa18.csv")


df <- rbind(plate13) %>% rbind(plate15) %>% rbind(plate16) %>% rbind(plate17) %>% rbind(plate18)

# means by well (i don't think this has been edited yet to suit all plates grouped)
#bywell <- df %>%
 # group_by(Well, Time, condition) %>%
  #summarise(count = n())%>% 
  #inner_join( 
   # df %>%
    #  group_by(Well) %>%
     # summarise_at(c("Area", "Mean", "IntDen", "RawIntDen"), list(mean)))

# means by condition
bycondition <- df %>%
  group_by(Time, condition) %>%
  summarise(count = n())%>% 
  inner_join( 
    df %>%
      group_by(Time, condition) %>%
      summarise_at(c("Area", "Mean", "IntDen", "RawIntDen"), list(mean)))

df$Cell <- gsub('\\D','', df$condition)       # replaces non-digits with blanks
df$Drug <- gsub('\\d','', df$condition)       # replaces digits with blanks

# Change order of factors
df$Cell <- factor(df$Cell, levels=c("4992", "6004", "10900", "3200", "2572", "7364", "8373"))
df$Drug <- factor(df$Drug, levels=c("Ldmso","Lapi", "Lbafi", "Ldmsoneg", "Lapineg", "Lbafineg"))
df$Time <- factor(df$Time, levels=c("2h", "5h"))


#plotting
#means by condition x is time
ggplot(data = bycondition, aes(x = Time, y = Mean, colour = condition)) + 
  geom_point(position = position_dodge(width = 0.5))

#means by condition x is condition
ggplot(data = bycondition, aes(x = condition, y = Mean, colour = Time)) + 
  geom_point(position = position_dodge(width = 0.5))

#all data boxplot x is condition
condition <- ggplot(data = df, aes(x = condition, y = Mean, colour = Time)) + 
  geom_boxplot() + ylim(0, 2500) + coord_flip()

condition + facet_grid(rows = vars(Drug))

#all data boxplot x is Well
ggplot(data = df, aes(x =Well, y = Mean, colour = condition)) + 
  geom_boxplot() + ylim(0, 2500)

#################best#################
#all data boxplot, x is cell, colour is time, facet is drug
condition <- ggplot(data = df, aes(x = Cell, y = Mean, colour = Time)) + 
  geom_boxplot() + ylim(0, 2500) + coord_flip()

condition + facet_grid(rows = vars(Drug))
########################################


#all data boxplot, x is cell, colour is time
condition <- ggplot(data = df, aes(x = Cell, y = Mean, colour = Time)) + 
  geom_boxplot() + ylim(0, 2500)

condition + facet_grid(cols = vars(Drug))


#all data boxplot, x is drug, colour is time, facet is cell
conditiondrug <- ggplot(data = df, aes(x = Drug, y = Mean, colour = Time)) + 
  geom_boxplot() + ylim(0, 2500) + coord_flip()

conditiondrug + facet_grid(rows = vars(Cell))

#######check for consistency between plates#######
df$PlateTimecondition <- paste(df$Plate, df$Time, df$condition, sep = "_")
df$Timecondition <- paste(df$Time, df$condition, sep = "_")

plate <- ggplot(data = df, aes(x = condition, y = Mean, colour = as.factor(Plate))) + 
  geom_boxplot() + ylim(0, 2500) + coord_flip()

plate + facet_grid(cols = vars(Time))

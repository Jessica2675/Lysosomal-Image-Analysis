library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(stringr)

df <- read.csv("../excel files/apilimod bafilomycin plate 17/DQBSA_Results.csv")
df <- df %>% separate(Image.Name, c('Duplicate', 'Project', 'Assay', 'Plate','Well','Time','condition','Multichannel','Date','Image_number')) 
df <- df %>% select(c(Area, Mean, IntDen, RawIntDen, Plate, Well, Time, condition, Date, Image_number))



# means by well
bywell <- df %>%
  group_by(Well, Time, condition) %>%
  summarise(count = n())%>% 
  inner_join( 
    df %>%
      group_by(Well) %>%
      summarise_at(c("Area", "Mean", "IntDen", "RawIntDen"), list(mean)))

# means by condition
bycondition <- df %>%
  group_by(Time, condition) %>%
  summarise(count = n())%>% 
  inner_join( 
    df %>%
      group_by(Time, condition) %>%
      summarise_at(c("Area", "Mean", "IntDen", "RawIntDen"), list(mean)))


#exclude outlier wells - doesn't look necessary for this plate
#exclude <- df %>% subset(Well != "B02") %>% subset(Well != "C03") %>% subset(Well != "G05")
#excludedcondition <- exclude %>%
 # group_by(Time, condition) %>%
  #summarise(count = n())%>% 
  #inner_join( 
   # exclude %>%
    #  group_by(Time, condition) %>%
     # summarise_at(c("Area", "Mean", "IntDen", "RawIntDen"), list(mean)))

write.csv(df, "../excel files/apilimod bafilomycin plate 17/excluded_dqbsa17.csv")


#plotting
#means by condition x is time
ggplot(data = bycondition, aes(x = Time, y = Mean, colour = condition)) + 
  geom_point(position = position_dodge(width = 0.5))

#means by condition x is condition
ggplot(data = bycondition, aes(x = condition, y = Mean, colour = Time)) + 
  geom_point(position = position_dodge(width = 0.5))

#all data boxplot x is condition
ggplot(data = df, aes(x = condition, y = Mean, colour = Time)) + 
  geom_boxplot() + ylim(0, 2500)

#all data boxplot x is Well
ggplot(data = df, aes(x =Well, y = Mean, colour = condition)) + 
  geom_boxplot() + ylim(0, 2500)

#all data boxplot x is condition, outlier wells are excluded
ggplot(data = exclude, aes(x = condition, y = Mean, colour = Time)) + 
  geom_boxplot() + ylim(0, 2500)

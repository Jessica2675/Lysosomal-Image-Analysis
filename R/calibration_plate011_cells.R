library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

## coefficients with pH  in calibration curve
lowGtoR = 0.095637796
highGtoR = 0.937991928
highOtoR = 1.203121
pKa = 4.591590271
slope = 0.555829694


df <- read_excel("../excel files/apilimod bafilomycin plate 11/DD_Cells_011.xlsx")
df <- df %>% select(c(Plate, Well, FOV, Area, `G / R`, `O / R`,`log(Ca)`,pH, Ca))

#calculate pH w/ in calibration curve
df$pH <- pKa + slope*log((lowGtoR - df$`G / R`)/(df$`G / R` - highGtoR))

#calculate log(Ca) and Ca w/  in calibration curve
df$Kd <- 1.03 + 5.4*10^12 * exp(-df$pH/0.189) + 3.11*10^6 * exp(-df$pH/0.412)
df$lowOtoR <- highOtoR/(4.24 + 0.12*exp(0.5*df$pH))
df$logCa <- log10(df$Kd*(df$`O / R` - df$lowOtoR)/(highOtoR - df$`O / R`))

df$Ca <- 10^(df$logCa)

##########start with the pH values excluding 
#apply pH and Ca filters
df <- subset(df, pH < 7)
df <- subset(df, pH > 3)
df <- subset(df, Ca < 10000)
df <- subset(df, Ca > 0)

#mean values by well

bywell <- df %>%
  group_by(Well) %>%
  summarise(count = n())%>% 
  inner_join( 
    df %>%
      group_by(Well) %>%
      summarise_at(c("G / R", "O / R", "pH", "Area", "log(Ca)", "Ca"), list(mean)))
    
##add treatment info to bywell and df
bywell$Conc <- ifelse(bywell$Well == "B02" | bywell$Well == "C02", "100uM_L4992",
                      ifelse(bywell$Well == "B03" | bywell$Well == "C03"| bywell$Well == "D03", "baf_L4992", 
                                    ifelse(bywell$Well == "B04" | bywell$Well == "C04"| bywell$Well == "D04", "DMSO_L8373",
                                           ifelse(bywell$Well == "B05" | bywell$Well == "C05"| bywell$Well == "D05", "100uM_L8373",
                                                  ifelse(bywell$Well == "E05" | bywell$Well == "F05" | bywell$Well == "G05", "baf_L8373", "none_U2OS")))))

df$Conc <- ifelse(df$Well == "B02" | df$Well == "C02", "100uM_L4992",
                      ifelse(df$Well == "B03" | df$Well == "C03"| df$Well == "D03", "baf_L4992", 
                             ifelse(df$Well == "B04" | df$Well == "C04"| df$Well == "D04", "DMSO_L8373",
                                    ifelse(df$Well == "B05" | df$Well == "C05"| df$Well == "D05", "100uM_L8373",
                                           ifelse(df$Well == "E05" | df$Well == "F05" | df$Well == "G05", "baf_L8373", "none_U2OS")))))

#separate conc into separate columns for cell line and concentration
df <- df %>% separate(Conc, c('Conc', 'cell_line'))
bywell <- bywell %>% separate(Conc, c('Conc', 'cell_line'))

# Change order of factors
df$Conc <- factor(df$Conc, levels=c("none", "DMSO", "100uM", "baf"))
df$cell_line <- factor(df$cell_line, levels=c("U2OS", "L4992", "L8373"))

bywell$Conc <- factor(bywell$Conc, levels=c("none", "DMSO", "100uM", "baf"))
bywell$cell_line <- factor(bywell$cell_line, levels=c("U2OS", "L4992", "L8373"))


##############save df   ##but first make sure you remove the filter!!!! >:(
write.csv(df, "../excel files/apilimod bafilomycin plate 11/df_Cells_011.csv")

#plot
##all data
ggplot(data = df, aes(x = cell_line, y = `G / R`)) + 
  geom_boxplot(aes(fill = Conc)) +
  ylim(0, 1)

ggplot(data = df, aes(x = cell_line, y = pH)) +
  geom_boxplot(aes(fill = Conc)) +
  ylim(3,7)

ggplot(data = df, aes(x = cell_line, y = `O / R`)) + 
  geom_boxplot(aes(fill = Conc)) +
  ylim(0, 1)

ggplot(data = df, aes(x = cell_line, y = Ca)) +
  geom_boxplot(aes(fill = Conc)) +
  ylim(0, 1000)

ggplot(data = df, aes(x = cell_line, y = Area)) +
  geom_boxplot(aes(fill = Conc)) +
  ylim(0, 1000)

##by well
GbyR <- ggplot(data = bywell, aes(x = cell_line, y = `G / R`, colour = Conc)) + 
  geom_point(position = position_dodge(width = 0.5))

ObyR <- ggplot(data = bywell, aes(x = cell_line, y = `O / R`, colour = Conc)) + 
  geom_point(position = position_dodge(width = 0.5))

count <- ggplot(data = bywell, aes(x = cell_line, y = count, colour = Conc)) + 
  geom_point(position = position_dodge(width = 0.5))

area <- ggplot(data = bywell, aes(x = cell_line, y = Area, colour = Conc)) + 
  geom_point(position = position_dodge(width = 0.5))

pH <- ggplot(data = bywell, aes(x = cell_line, y = pH, colour = Conc)) + 
  geom_point(position = position_dodge(width = 0.5))

Ca <- ggplot(data = bywell, aes(x = cell_line, y = Ca, colour = Conc)) + 
  geom_point(position = position_dodge(width = 0.5))


ggarrange(
  pH, Ca, count, area,
  common.legend = TRUE)

ggarrange(
  GbyR, ObyR,
  common.legend = TRUE)

##colour by well to check for outlier well
##by well

count <- ggplot(data = bywell, aes(x = cell_line, y = count)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Well))

area <- ggplot(data = bywell, aes(x = cell_line, y = Area)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Well))

pH <- ggplot(data = bywell, aes(x = cell_line, y = pH)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Well))

Ca <- ggplot(data = bywell, aes(x = cell_line, y = Ca)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Well))


##heatmap

###add grouping factor by pH and add grouping factor by logCa 
heatmap <- transform(df, pH_group = cut(pH, breaks = c(0, 4, 5, 6, 7), labels = c("3.5", "4.5", "5.5", "6.5")))
heatmap <- transform(heatmap, logCa_group  = cut(log.Ca., breaks = c( 0,1, 2,3,4), labels = c( "0.5","1.5", "2.5", "3.5")))

###LRRK2

LRRK2 <- subset(heatmap, cell_line == "L2572")
LRRK2_Apilimod <- subset(LRRK2, Conc == "100uM")
LRRK2_DMSO <- subset(LRRK2, Conc == "DMSO")

####LRRK2 DMSO
frequencyLRRK2DMSO <- table(LRRK2_DMSO$pH_group, LRRK2_DMSO$logCa_group)
frequencyLRRK2DMSO <- prop.table(frequencyLRRK2DMSO)*100

h1 <- ggplot(data = as.data.frame(frequencyLRRK2DMSO), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("LRRK2 + DMSO") + 
  theme(legend.position = "none")
  

####LRRK2 Apilimod
frequencyLRRK2_Apilimod <- table(LRRK2_Apilimod$pH_group, LRRK2_Apilimod$logCa_group)
frequencyLRRK2_Apilimod <- prop.table(frequencyLRRK2_Apilimod)*100

h2 <- ggplot(data = as.data.frame(frequencyLRRK2_Apilimod), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("LRRK2  + Apilimod") + 
  theme(legend.position = "none")

###control

control <- subset(heatmap, cell_line == "L6004")
control_Apilimod <- subset(control, Conc == "100uM")
control_DMSO <- subset(control, Conc == "DMSO")

####control DMSO
frequencycontrolDMSO <- table(control_DMSO$pH_group, control_DMSO$logCa_group)
frequencycontrolDMSO <- prop.table(frequencycontrolDMSO)*100

h3 <- ggplot(data = as.data.frame(frequencycontrolDMSO), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("HI  + DMSO")+ 
  theme(legend.position = "none")


####control Apilimod
frequencycontrol_Apilimod <- table(control_Apilimod$pH_group, control_Apilimod$logCa_group)
frequencycontrol_Apilimod <- prop.table(frequencycontrol_Apilimod)*100

h4 <- ggplot(data = as.data.frame(frequencycontrol_Apilimod), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("HI  + Apilimod")+ 
  theme(legend.position = "none")


ggarrange(
  h3, h4, h1, h2)


###########
#check best FOVs result for 2 L3200 wells
bestF06 <- df %>% filter(Well == "F06") %>% filter(FOV %in% c("008", "006", "012", "014"))
bestF07 <- df %>% filter(Well == "F07") %>% filter(FOV %in% c("007", "017", "18", "019"))


best <- rbind(bestF06, bestF07)

byFOV <- best %>%
  group_by(FOV, Well) %>%
  summarise(count = n())%>% 
  inner_join( 
    df %>%
      group_by(FOV, Well) %>%
      summarise_at(c("G / R", "O / R", "pH", "Area", "logCa", "Ca"), list(mean)))

byFOV$Conc <- ifelse(byFOV$Well == "F06", "DMSO_L3200", "100uM_L3200")
byFOV <- byFOV %>% separate(Conc, c('Conc', 'cell_line'))
byFOV$Conc <- factor(byFOV$Conc, levels=c("DMSO", "100uM"))

FOVcount <- ggplot(data = byFOV, aes(x = cell_line, y = count, colour = Conc)) + 
  geom_point(position = position_dodge(width = 0.5))

FOVarea <- ggplot(data = byFOV, aes(x = cell_line, y = Area, colour = Conc)) + 
  geom_point(position = position_dodge(width = 0.5))

FOVpH <- ggplot(data = byFOV, aes(x = cell_line, y = pH, colour = Conc)) + 
  geom_point(position = position_dodge(width = 0.5))

FOVCa <- ggplot(data = byFOV, aes(x = cell_line, y = Ca, colour = Conc)) + 
  geom_point(position = position_dodge(width = 0.5))

ggarrange(
  FOVpH, FOVCa, FOVcount, FOVarea,
  common.legend = TRUE)


FOVGbyR <- ggplot(data = byFOV, aes(x = cell_line, y = `G / R`, colour = Conc)) + 
  geom_point(position = position_dodge(width = 0.5))

FOVObyR <- ggplot(data = byFOV, aes(x = cell_line, y = `O / R`, colour = Conc)) + 
  geom_point(position = position_dodge(width = 0.5))

ggarrange(
  FOVGbyR, FOVObyR, 
  common.legend = TRUE)

ggplot(data = best, aes(x = cell_line, y = `G / R`)) + 
  geom_boxplot(aes(fill = Conc)) +
  ylim(0, 1)

ggplot(data = best, aes(x = cell_line, y = pH)) +
  geom_boxplot(aes(fill = Conc)) +
  ylim(3,7)

ggplot(data = best, aes(x = cell_line, y = `O / R`)) + 
  geom_boxplot(aes(fill = Conc)) +
  ylim(0, 1)

ggplot(data = best, aes(x = cell_line, y = Ca)) +
  geom_boxplot(aes(fill = Conc)) +
  ylim(0, 1000)
###########

##combine plate006 mean values by well
plate6 <- read.csv("../excel files/apilimod plate 6/mean_values_bywell.csv")
plate6 <- plate6[2:11] #remove the first column of row numbers

names(plate6) <- names(bywell) #set both to have same column names
overall <- rbind(plate6, bywell)
bywell <- overall

#change order of factors
bywell$Conc <- factor(bywell$Conc, levels=c("none", "DMSO", "100uM"))
bywell$cell_line <- factor(bywell$cell_line, levels=c("U2OS","L4992","L6004", "L3200", "L2572"))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

lowGtoR = 0.02
highGtoR = 0.4
highOtoR = 0.21
pKa = 4.82
slope = 0.467

df <- read_excel("../excel files/apilimod plate 1/DD_Cali_001.xlsx")
df <- df %>% select(c(Well, FOV, Area, `G / R`, `O / R`, pH, Ca, logCa))

#calculate pH
df$pH <- pKa + slope*log((lowGtoR - df$`G / R`)/(df$`G / R` - highGtoR))

#calculate log(Ca) and Ca
df$Kd <- 1.03 + 5.4*10^12 * exp(-df$pH/0.189) + 3.11*10^6 * exp(-df$pH/0.412)
df$lowOtoR <- highOtoR/(4.24 + 0.12*exp(0.5*df$pH))
df$logCa <- log10(df$Kd*(df$`O / R` - df$lowOtoR)/(highOtoR - df$`O / R`))

df$Ca <- 10^(df$logCa)


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
bywell$Conc <- ifelse(bywell$Well == "B04" | bywell$Well == "B05", "DMSO",
                     ifelse(bywell$Well == "C04" | bywell$Well == "C05", "1 μM",
                            ifelse(bywell$Well == "D04" | bywell$Well == "D05", "10 μM", "100 μM")))


bywell <- subset(bywell, Well != "B06")

df$Conc <- ifelse(df$Well == "B04" | df$Well == "B05", "DMSO",
                     ifelse(df$Well == "C04" | df$Well == "C05", "1 μM",
                            ifelse(df$Well == "D04" | df$Well == "D05", "10 μM", "100 μM")))
df <- subset(df, Well != "B06")

#separate conc into separate columns for cell line and concentration
df <- df %>% separate(Conc, c('Conc', 'cell_line'))
bywell <- bywell %>% separate(Conc, c('Conc', 'cell_line'))

# Change order of factors
df$Conc <- factor(df$Conc, levels=c("DMSO", "1 μM", "10 μM" ,"100 μM"))
df$cell_line <- factor(df$cell_line, levels=c("L6004", "L2572", "L3200"))

bywell$Conc <- factor(bywell$Conc, levels=c("DMSO", "1 μM", "10 μM" ,"100 μM"))
bywell$cell_line <- factor(bywell$cell_line, levels=c("L6004", "L2572", "L3200"))


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

area <- ggplot(data = bywell, aes(x = Conc, y = Area, colour = Conc)) + 
  geom_point(position = position_dodge(width = 0.5)) + theme_classic() + xlab("Concentration")

pH <- ggplot(data = bywell, aes(x = Conc, y = pH, colour = Conc)) + 
  geom_point(position = position_dodge(width = 0.5)) + theme_classic() + xlab("Concentration")


Ca <- ggplot(data = bywell, aes(x = Conc, y = Ca, colour = Conc)) + 
  geom_point(position = position_dodge(width = 0.5)) + theme_classic() + xlab("Concentration")


ggarrange(
  Ca, area,
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
heatmap <- transform(heatmap, logCa_group  = cut(log.Ca., breaks = c(0, 1, 2, 3, 4), labels = c("0.5", "1.5", "2.5", "3.5")))

###concs
a100um <- subset(heatmap, Conc == "100 μM")
a10um <- subset(heatmap, Conc == "10 μM")
DMSO <- subset(heatmap, Conc == "DMSO")
a1um <- subset(heatmap, Conc == "1 μM")
#### DMSO
frequencyDMSO <- table(DMSO$pH_group, DMSO$logCa_group)
frequencyDMSO <- prop.table(frequencyDMSO)*100

h1 <- ggplot(data = as.data.frame(frequencyDMSO), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("DMSO") + 
  theme(legend.position = "none")
  

####1um Apilimod
frequency1 <- table(a1um$pH_group, a1um$logCa_group)
frequency1 <- prop.table(frequency1)*100

h2 <- ggplot(data = as.data.frame(frequency1), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("1 μM Apilimod") + 
  theme(legend.position = "none")

###10um
frequency10 <- table(a10um$pH_group, a10um$logCa_group)
frequency10 <- prop.table(frequency10)*100

h3 <- ggplot(data = as.data.frame(frequency10), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("10 μM Apilimod") + 
  theme(legend.position = "none")

####control Apilimod
frequency100 <- table(a100um$pH_group, a100um$logCa_group)
frequency100 <- prop.table(frequency100)*100

h4 <- ggplot(data = as.data.frame(frequency100), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("100 μM Apilimod") + 
  theme(legend.position = "none")

###LRRK2 L3200

L3200 <- subset(heatmap, cell_line == "L3200")
L3200_Apilimod <- subset(L3200, Conc == "100uM")
L3200_DMSO <- subset(L3200, Conc == "DMSO")

####L3200 DMSO
frequencyL3200DMSO <- table(L3200_DMSO$pH_group, L3200_DMSO$logCa_group)
frequencyL3200DMSO <- prop.table(frequencyL3200DMSO)*100

h5 <- ggplot(data = as.data.frame(frequencyL3200DMSO), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("LRRK2 L3200 + DMSO") + 
  theme(legend.position = "none")


####L3200 Apilimod
frequencyL3200_Apilimod <- table(L3200_Apilimod$pH_group, L3200_Apilimod$logCa_group)
frequencyL3200_Apilimod <- prop.table(frequencyL3200_Apilimod)*100

h6 <- ggplot(data = as.data.frame(frequencyL3200_Apilimod), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("LRRK2 L3200 + Apilimod") + 
  theme(legend.position = "none")

ggarrange(
  h3, h4, h1, h2, h5, h6, ncol = 2, nrow = 3 )

ggarrange(h1, h2, h3, h4, nrow = 1 )
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
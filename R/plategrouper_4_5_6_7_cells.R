library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(ggExtra)

## coefficients with pH 3.85 in calibration curve
lowGtoR = 0.154418045
highGtoR = 0.664011779
highOtoR = 1.009826
pKa = 4.343784887
slope = 0.300947664

p007 <- read_excel("../excel files/apilimod plate 7/DD_Cells_007.xlsx")[,1:45]  #columns excluding the pH and Ca
p004 <- read_excel("../excel files/apilimod plate 4/DD_well c04 dmso L4992_004.xlsx")[,1:45] 
p004 <- p004 %>% rbind(read_excel("../excel files/apilimod plate 4/DD_U2OS_004.xlsx")[,1:45])
p005 <- read_excel("../excel files/apilimod plate 5/DD_Cells_005.xlsx")[,1:45]
p006 <- read_excel("../excel files/apilimod plate 6/DD_Cells_006.xlsx")[,1:45]

df <- p004
df <- df %>% rbind(p005) %>% rbind(p006) %>% rbind(p007)
df <- df %>% select(c(Plate, Well, FOV, Area, `G / R`, `O / R`))

#calculate pH 
df$pH <- pKa + slope*log((lowGtoR - df$`G / R`)/(df$`G / R` - highGtoR))

#calculate log(Ca) and Ca 
df$Kd <- 1.03 + 5.4*10^12 * exp(-df$pH/0.189) + 3.11*10^6 * exp(-df$pH/0.412)
df$lowOtoR <- highOtoR/(4.24 + 0.12*exp(0.5*df$pH))
df$logCa <- log10(df$Kd*(df$`O / R` - df$lowOtoR)/(highOtoR - df$`O / R`))

df$Ca <- 10^(df$logCa)

##########start with the pH values excluding 3.85
#apply pH and Ca filters
df <- subset(df, pH < 7)
df <- subset(df, pH > 3)
df <- subset(df, Ca < 10000)
df <- subset(df, Ca > 0)

#combine plate and well info in one column
df$Plate_Well <- paste(df$Plate, df$Well, sep = "_")

#mean values by well

bywell <- df %>%
  group_by(Plate_Well) %>%
  summarise(count = n())%>% 
  inner_join( 
    df %>%
      group_by(Plate_Well) %>%
      summarise_at(c("G / R", "O / R", "pH", "Area", "logCa", "Ca"), list(mean)))
    
##add treatment info to bywell and df
bywell$Conc <- ifelse(bywell$Plate_Well =="007_B02" | bywell$Plate_Well =="007_C02" | bywell$Plate_Well =="007_E02" ,"DMSO_L6004",
                             ifelse(bywell$Plate_Well =="007_F02" | bywell$Plate_Well =="007_C03" | bywell$Plate_Well =="007_D03" ,"100uM_L6004", 
                                    ifelse(bywell$Plate_Well =="007_B04" | bywell$Plate_Well =="007_C04" | bywell$Plate_Well =="007_D04","DMSO_L2572",
                                           ifelse(bywell$Plate_Well =="007_B05" | bywell$Plate_Well =="007_C05" | bywell$Plate_Well =="007_D05","100uM_L2572",
  ifelse(bywell$Plate_Well =="005_B09" | bywell$Plate_Well =="005_C09", "DMSO_L3200",
                      ifelse(bywell$Plate_Well =="005_B05" | bywell$Plate_Well =="005_C05", "DMSO_L4992",
                             ifelse(bywell$Plate_Well =="005_B07" | bywell$Plate_Well =="005_C07", "DMSO_L6004",
  ifelse(bywell$Plate_Well =="006_B02" | bywell$Plate_Well =="006_C02" | bywell$Plate_Well =="006_D02" , "DMSO_L4992",
                      ifelse(bywell$Plate_Well =="006_B03" | bywell$Plate_Well =="006_C03" | bywell$Plate_Well =="006_D03" ,"100uM_L4992", 
                             ifelse(bywell$Plate_Well =="006_B04" | bywell$Plate_Well =="006_C04" | bywell$Plate_Well =="006_D04","DMSO_L3200",
                                    ifelse(bywell$Plate_Well =="006_B05" | bywell$Plate_Well =="006_C05" | bywell$Plate_Well =="006_D05","100uM_L3200",
  ifelse(bywell$Plate_Well =="004_C04", "DMSO_L4992","none_U2OS"))))))))))))
                      


df$Conc <- ifelse(df$Plate_Well =="007_B02" | df$Plate_Well =="007_C02" | df$Plate_Well =="007_E02" ,"DMSO_L6004",
                      ifelse(df$Plate_Well =="007_F02" | df$Plate_Well =="007_C03" | df$Plate_Well =="007_D03" ,"100uM_L6004", 
                             ifelse(df$Plate_Well =="007_B04" | df$Plate_Well =="007_C04" | df$Plate_Well =="007_D04","DMSO_L2572",
                                    ifelse(df$Plate_Well =="007_B05" | df$Plate_Well =="007_C05" | df$Plate_Well =="007_D05","100uM_L2572",
                                           ifelse(df$Plate_Well =="005_B09" | df$Plate_Well =="005_C09", "DMSO_L3200",
                                                  ifelse(df$Plate_Well =="005_B05" | df$Plate_Well =="005_C05", "DMSO_L4992",
                                                         ifelse(df$Plate_Well =="005_B07" | df$Plate_Well =="005_C07", "DMSO_L6004",
                                                                ifelse(df$Plate_Well =="006_B02" | df$Plate_Well =="006_C02" | df$Plate_Well =="006_D02" , "DMSO_L4992",
                                                                       ifelse(df$Plate_Well =="006_B03" | df$Plate_Well =="006_C03" | df$Plate_Well =="006_D03" ,"100uM_L4992", 
                                                                              ifelse(df$Plate_Well =="006_B04" | df$Plate_Well =="006_C04" | df$Plate_Well =="006_D04","DMSO_L3200",
                                                                                     ifelse(df$Plate_Well =="006_B05" | df$Plate_Well =="006_C05" | df$Plate_Well =="006_D05","100uM_L3200",
                                                                                            ifelse(df$Plate_Well =="004_C04", "DMSO_L4992","none_U2OS"))))))))))))


#separate conc into separate columns for cell line and concentration
df <- df %>% separate(Conc, c('Conc', 'cell_line'))
df <- df %>% separate(Plate_Well, c('Plate', 'Well'))
bywell <- bywell %>% separate(Conc, c('Conc', 'cell_line'))
bywell <- bywell %>% separate(Plate_Well, c('Plate', 'Well'))

# Change order of factors
df$Conc <- factor(df$Conc, levels=c("none", "DMSO", "100uM"))
df$cell_line <- factor(df$cell_line, levels=c("U2OS","L4992", "L6004", "L3200", "L2572"))

bywell$Conc <- factor(bywell$Conc, levels=c("none", "DMSO", "100uM"))
bywell$cell_line <- factor(bywell$cell_line, levels=c("U2OS","L4992", "L6004", "L3200", "L2572"))

#plot
##u2os scatterplot with marginal histograms
u2osdf <- subset(df, cell_line == "U2OS") %>% na.omit()
u2os_scatter <- ggplot((data = u2osdf), aes(x = logCa, y = pH)) + geom_point(aes(colour = Plate))
ggMarginal(u2os_scatter, groupColour = TRUE, groupFill = TRUE)


u2osdf %>% group_by(Plate) %>% summarise_at(c("pH", "logCa"), mean)
u2osdf %>% group_by(Plate) %>% summarise_at(c("pH", "logCa"), sd)

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

##colour by plate
##by well

count <- ggplot(data = bywell, aes(x = cell_line, y = count)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))

area <- ggplot(data = bywell, aes(x = cell_line, y = Area)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))

pH <- ggplot(data = bywell, aes(x = cell_line, y = pH)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))

Ca <- ggplot(data = bywell, aes(x = cell_line, y = Ca)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))


##heatmap

###add grouping factor by pH and add grouping factor by logCa 
heatmap <- transform(df, pH_group = cut(pH, breaks = c(0, 4, 5, 6, 7), labels = c("3.5", "4.5", "5.5", "6.5")))
heatmap <- transform(heatmap, logCa_group  = cut(logCa, breaks = c( 0,1, 2,3,4), labels = c( "0.5","1.5", "2.5", "3.5")))

###L3200

L3200 <- subset(heatmap, cell_line == "L3200")
L3200_Apilimod <- subset(L3200, Conc == "100uM")
L3200_DMSO <- subset(L3200, Conc == "DMSO")

####L3200 DMSO
frequencyL3200DMSO <- table(L3200_DMSO$pH_group, L3200_DMSO$logCa_group)
frequencyL3200DMSO <- prop.table(frequencyL3200DMSO)*100

h1 <- ggplot(data = as.data.frame(frequencyL3200DMSO), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("L3200 + DMSO") + 
  theme(legend.position = "none")
  

####L3200 Apilimod
frequencyL3200_Apilimod <- table(L3200_Apilimod$pH_group, L3200_Apilimod$logCa_group)
frequencyL3200_Apilimod <- prop.table(frequencyL3200_Apilimod)*100

h2 <- ggplot(data = as.data.frame(frequencyL3200_Apilimod), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("L3200  + Apilimod") + 
  theme(legend.position = "none")

###L2572

L2572 <- subset(heatmap, cell_line == "L2572")
L2572_Apilimod <- subset(L2572, Conc == "100uM")
L2572_DMSO <- subset(L2572, Conc == "DMSO")

####L2572 DMSO
frequencyL2572DMSO <- table(L2572_DMSO$pH_group, L2572_DMSO$logCa_group)
frequencyL2572DMSO <- prop.table(frequencyL2572DMSO)*100

h3 <- ggplot(data = as.data.frame(frequencyL2572DMSO), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("L2572 + DMSO") + 
  theme(legend.position = "none")


####L2572 Apilimod
frequencyL2572_Apilimod <- table(L2572_Apilimod$pH_group, L2572_Apilimod$logCa_group)
frequencyL2572_Apilimod <- prop.table(frequencyL2572_Apilimod)*100

h4 <- ggplot(data = as.data.frame(frequencyL2572_Apilimod), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("L2572  + Apilimod") + 
  theme(legend.position = "none")

###L6004

L6004 <- subset(heatmap, cell_line == "L6004")
L6004_Apilimod <- subset(L6004, Conc == "100uM")
L6004_DMSO <- subset(L6004, Conc == "DMSO")

####L6004 DMSO
frequencyL6004DMSO <- table(L6004_DMSO$pH_group, L6004_DMSO$logCa_group)
frequencyL6004DMSO <- prop.table(frequencyL6004DMSO)*100

h5 <- ggplot(data = as.data.frame(frequencyL6004DMSO), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("L6004  + DMSO")+ 
  theme(legend.position = "none")


####L6004 Apilimod
frequencyL6004_Apilimod <- table(L6004_Apilimod$pH_group, L6004_Apilimod$logCa_group)
frequencyL6004_Apilimod <- prop.table(frequencyL6004_Apilimod)*100

h6 <- ggplot(data = as.data.frame(frequencyL6004_Apilimod), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("L6004  + Apilimod")+ 
  theme(legend.position = "none")


###L4992

L4992 <- subset(heatmap, cell_line == "L4992")
L4992_Apilimod <- subset(L4992, Conc == "100uM")
L4992_DMSO <- subset(L4992, Conc == "DMSO")

####L4992 DMSO
frequencyL4992DMSO <- table(L4992_DMSO$pH_group, L4992_DMSO$logCa_group)
frequencyL4992DMSO <- prop.table(frequencyL4992DMSO)*100

h7 <- ggplot(data = as.data.frame(frequencyL4992DMSO), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("L4992  + DMSO")+ 
  theme(legend.position = "none")


####L4992 Apilimod
frequencyL4992_Apilimod <- table(L4992_Apilimod$pH_group, L4992_Apilimod$logCa_group)
frequencyL4992_Apilimod <- prop.table(frequencyL4992_Apilimod)*100

h8 <- ggplot(data = as.data.frame(frequencyL4992_Apilimod), aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = round(Freq, 2)), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("log(Ca)") +
  ylab("pH") +
  ggtitle("L4992  + Apilimod")+ 
  theme(legend.position = "none")



ggarrange(
  h7, h8, h5, h6, h3, h4, h1, h2, nrow = 4, ncol = 2)


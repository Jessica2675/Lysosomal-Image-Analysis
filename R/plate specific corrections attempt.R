library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(ggExtra)

df <- read.csv("../excel files/apilimod plate 4 5 6 7 combined/no filter df.csv")

##u2os scatterplot with marginal histograms
u2osdf <- subset(df, cell_line == "U2OS") %>% na.omit()
u2os_scatter <- ggplot((data = u2osdf), aes(x = logCa, y = pH)) + geom_point(aes(colour = as.factor(Plate)))
ggMarginal(u2os_scatter, groupColour = TRUE, groupFill = TRUE)


means <- u2osdf %>% group_by(Plate) %>% summarise_at(c("pH", "logCa"), mean)
sds <- u2osdf %>% group_by(Plate) %>% summarise_at(c("pH", "logCa"), sd)

# set reference plate values (here plate 6 cos that is the plate from which we took the calibration curve)
refmean <- subset(means, Plate == 6) 
refsds <- subset(sds, Plate == 6)

#try correction by multiplication
multicorrect <- means
multicorrect$pHcorrection <- multicorrect$pH/refmean$pH
multicorrect$logCacorrection <- multicorrect$logCa/refmean$logCa
multicorrect <- multicorrect %>% select(c("Plate", "pHcorrection", "logCacorrection"))

multidf <- inner_join(df, multicorrect, by="Plate") %>% mutate(newpH = pH / pHcorrection) %>% mutate(newlogCa = logCa / logCacorrection)
multidf$newCa <- 10^multidf$newlogCa 

## visualise multidf correct u2os
u2osmultidf <- subset(multidf, cell_line == "U2OS") %>% na.omit()
u2os_multiscatter <- ggplot((data = u2osmultidf), aes(x = newlogCa, y = newpH)) + geom_point(aes(colour = as.factor(Plate)))
ggMarginal(u2os_multiscatter, groupColour = TRUE, groupFill = TRUE)

##examine multidf corrected means for each condition
###apply filter for pH 3-7, Ca<10000
multidf <- subset(multidf, newpH < 7)
multidf <- subset(multidf, newpH > 3)
multidf <- subset(multidf, newCa < 10000)
multidf <- subset(multidf, newCa > 0)

### multidf means bywell
multidf$Plate_Well <- paste(multidf$Plate, multidf$Well, sep = "_")
multibywell <- multidf %>%
  group_by(Plate_Well) %>%
  summarise(count = n())%>% 
  inner_join( 
    multidf %>%
      group_by(Plate_Well) %>%
      summarise_at(c("G...R", "O...R", "newpH", "Area", "newlogCa", "newCa"), mean, na.rm = TRUE))

#norm conc and cell_line info to multibywell  
multibywell$Conc <- ifelse(multibywell$Plate_Well =="7_B02" | multibywell$Plate_Well =="7_C02" | multibywell$Plate_Well =="7_E02" ,"DMSO_L6004",
                      ifelse(multibywell$Plate_Well =="7_F02" | multibywell$Plate_Well =="7_C03" | multibywell$Plate_Well =="7_D03" ,"1uM_L6004", 
                             ifelse(multibywell$Plate_Well =="7_B04" | multibywell$Plate_Well =="7_C04" | multibywell$Plate_Well =="7_D04","DMSO_L2572",
                                    ifelse(multibywell$Plate_Well =="7_B05" | multibywell$Plate_Well =="7_C05" | multibywell$Plate_Well =="7_D05","1uM_L2572",
                                           ifelse(multibywell$Plate_Well =="5_B09" | multibywell$Plate_Well =="5_C09", "DMSO_L3200",
                                                  ifelse(multibywell$Plate_Well =="5_B05" | multibywell$Plate_Well =="5_C05", "DMSO_L4992",
                                                         ifelse(multibywell$Plate_Well =="5_B07" | multibywell$Plate_Well =="5_C07", "DMSO_L6004",
                                                                ifelse(multibywell$Plate_Well =="6_B02" | multibywell$Plate_Well =="6_C02" | multibywell$Plate_Well =="6_D02" , "DMSO_L4992",
                                                                       ifelse(multibywell$Plate_Well =="6_B03" | multibywell$Plate_Well =="6_C03" | multibywell$Plate_Well =="6_D03" ,"1uM_L4992", 
                                                                              ifelse(multibywell$Plate_Well =="6_B04" | multibywell$Plate_Well =="6_C04" | multibywell$Plate_Well =="6_D04","DMSO_L3200",
                                                                                     ifelse(multibywell$Plate_Well =="6_B05" | multibywell$Plate_Well =="6_C05" | multibywell$Plate_Well =="6_D05","1uM_L3200",
                                                                                            ifelse(multibywell$Plate_Well =="4_C04", "DMSO_L4992","none_U2OS"))))))))))))

multibywell <- multibywell %>% separate(Plate_Well, c('Plate', 'Well'))
multibywell <- multibywell %>% separate(Conc, c('Conc', 'cell_line'))
multibywell$Conc <- factor(multibywell$Conc, levels=c("none", "DMSO", "100uM"))
multibywell$cell_line <- factor(multibywell$cell_line, levels=c("U2OS","L4992", "L6004", "L3200", "L2572"))

###plot by well
multicount <- ggplot(data = multibywell, aes(x = cell_line, y = count)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))

multiarea <- ggplot(data = multibywell, aes(x = cell_line, y = Area)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))

multipH <- ggplot(data = multibywell, aes(x = cell_line, y = newpH)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))

multiCa <- ggplot(data = multibywell, aes(x = cell_line, y = newCa)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))

ggarrange(
  multipH, multiCa, multicount, multiarea,
  common.legend = TRUE)

#try correction by addition
addcorrect <- means
addcorrect$pHcorrection <- addcorrect$pH - refmean$pH
addcorrect$logCacorrection <- addcorrect$logCa - refmean$logCa
addcorrect <- addcorrect %>% select(c("Plate", "pHcorrection", "logCacorrection"))

adddf <- inner_join(df, addcorrect, by="Plate") %>% mutate(newpH = pH - pHcorrection) %>% mutate(newlogCa = logCa - logCacorrection)
adddf$newCa <- 10^adddf$newlogCa 

## visualise adddf correct u2os
u2osadddf <- subset(adddf, cell_line == "U2OS") %>% na.omit()
u2os_addscatter <- ggplot((data = u2osadddf), aes(x = newlogCa, y = newpH)) + geom_point(aes(colour = as.factor(Plate)))
ggMarginal(u2os_addscatter, groupColour = TRUE, groupFill = TRUE)

##examine adddf corrected means for each condition
###apply filter for pH 3-7, Ca<10000
adddf <- subset(adddf, newpH < 7)
adddf <- subset(adddf, newpH > 3)
adddf <- subset(adddf, newCa < 10000)
adddf <- subset(adddf, newCa > 0)

### adddf means bywell
adddf$Plate_Well <- paste(adddf$Plate, adddf$Well, sep = "_")
addbywell <- adddf %>%
  group_by(Plate_Well) %>%
  summarise(count = n())%>% 
  inner_join( 
    adddf %>%
      group_by(Plate_Well) %>%
      summarise_at(c("G...R", "O...R", "newpH", "Area", "newlogCa", "newCa"), mean, na.rm = TRUE))

#add conc and cell_line info to addbywell  
addbywell$Conc <- ifelse(addbywell$Plate_Well =="7_B02" | addbywell$Plate_Well =="7_C02" | addbywell$Plate_Well =="7_E02" ,"DMSO_L6004",
                           ifelse(addbywell$Plate_Well =="7_F02" | addbywell$Plate_Well =="7_C03" | addbywell$Plate_Well =="7_D03" ,"1uM_L6004", 
                                  ifelse(addbywell$Plate_Well =="7_B04" | addbywell$Plate_Well =="7_C04" | addbywell$Plate_Well =="7_D04","DMSO_L2572",
                                         ifelse(addbywell$Plate_Well =="7_B05" | addbywell$Plate_Well =="7_C05" | addbywell$Plate_Well =="7_D05","1uM_L2572",
                                                ifelse(addbywell$Plate_Well =="5_B09" | addbywell$Plate_Well =="5_C09", "DMSO_L3200",
                                                       ifelse(addbywell$Plate_Well =="5_B05" | addbywell$Plate_Well =="5_C05", "DMSO_L4992",
                                                              ifelse(addbywell$Plate_Well =="5_B07" | addbywell$Plate_Well =="5_C07", "DMSO_L6004",
                                                                     ifelse(addbywell$Plate_Well =="6_B02" | addbywell$Plate_Well =="6_C02" | addbywell$Plate_Well =="6_D02" , "DMSO_L4992",
                                                                            ifelse(addbywell$Plate_Well =="6_B03" | addbywell$Plate_Well =="6_C03" | addbywell$Plate_Well =="6_D03" ,"1uM_L4992", 
                                                                                   ifelse(addbywell$Plate_Well =="6_B04" | addbywell$Plate_Well =="6_C04" | addbywell$Plate_Well =="6_D04","DMSO_L3200",
                                                                                          ifelse(addbywell$Plate_Well =="6_B05" | addbywell$Plate_Well =="6_C05" | addbywell$Plate_Well =="6_D05","1uM_L3200",
                                                                                                 ifelse(addbywell$Plate_Well =="4_C04", "DMSO_L4992","none_U2OS"))))))))))))

addbywell <- addbywell %>% separate(Plate_Well, c('Plate', 'Well'))
addbywell <- addbywell %>% separate(Conc, c('Conc', 'cell_line'))
addbywell$Conc <- factor(addbywell$Conc, levels=c("none", "DMSO", "100uM"))
addbywell$cell_line <- factor(addbywell$cell_line, levels=c("U2OS","L4992", "L6004", "L3200", "L2572"))

###plot by well
addcount <- ggplot(data = addbywell, aes(x = cell_line, y = count)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))

addarea <- ggplot(data = addbywell, aes(x = cell_line, y = Area)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))

addpH <- ggplot(data = addbywell, aes(x = cell_line, y = newpH)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))

addCa <- ggplot(data = addbywell, aes(x = cell_line, y = newCa)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))

ggarrange(
  addpH, addCa, addcount, addarea,
  common.legend = TRUE)

#try mean addition plus sd multiplicative correction (i.e. matching the normal distributions of the curves) :( currently not working
normcorrect <- sds
normcorrect$pHcorrection <- refsds$pH/normcorrect$pH
normcorrect$logCacorrection <-refsds$logCa/ normcorrect$logCa 
normcorrect <- normcorrect %>% select(c("Plate", "pHcorrection", "logCacorrection"))

normadddf <- 
normadddf <- adddf[, !(colnames(adddf) %in% c("pHcorrection", "logCacorrection"))] #remove previous phcorrection and logCacorrection from adddf
normdf <- inner_join(normadddf, normcorrect, by="Plate") %>% mutate(newpH = pH * pHcorrection) %>% mutate(newlogCa = logCa * logCacorrection)
normdf$newCa <- 10^normdf$newlogCa 

## visualise normdf correct u2os
u2osnormdf <- subset(normdf, cell_line == "U2OS") %>% na.omit()
u2os_normscatter <- ggplot((data = u2osnormdf), aes(x = newlogCa, y = newpH)) + geom_point(aes(colour = as.factor(Plate)))
ggMarginal(u2os_normscatter, groupColour = TRUE, groupFill = TRUE)

##examine normdf corrected means for each condition
###apply filter for pH 3-7, Ca<10000
normdf <- subset(normdf, newpH < 7)
normdf <- subset(normdf, newpH > 3)
normdf <- subset(normdf, newCa < 10000)
normdf <- subset(normdf, newCa > 0)

### normdf means bywell
normdf$Plate_Well <- paste(normdf$Plate, normdf$Well, sep = "_")
normbywell <- normdf %>%
  group_by(Plate_Well) %>%
  summarise(count = n())%>% 
  inner_join( 
    normdf %>%
      group_by(Plate_Well) %>%
      summarise_at(c("G...R", "O...R", "newpH", "Area", "newlogCa", "newCa"), mean, na.rm = TRUE))

#norm conc and cell_line info to normbywell  
normbywell$Conc <- ifelse(normbywell$Plate_Well =="7_B02" | normbywell$Plate_Well =="7_C02" | normbywell$Plate_Well =="7_E02" ,"DMSO_L6004",
                         ifelse(normbywell$Plate_Well =="7_F02" | normbywell$Plate_Well =="7_C03" | normbywell$Plate_Well =="7_D03" ,"1uM_L6004", 
                                ifelse(normbywell$Plate_Well =="7_B04" | normbywell$Plate_Well =="7_C04" | normbywell$Plate_Well =="7_D04","DMSO_L2572",
                                       ifelse(normbywell$Plate_Well =="7_B05" | normbywell$Plate_Well =="7_C05" | normbywell$Plate_Well =="7_D05","1uM_L2572",
                                              ifelse(normbywell$Plate_Well =="5_B09" | normbywell$Plate_Well =="5_C09", "DMSO_L3200",
                                                     ifelse(normbywell$Plate_Well =="5_B05" | normbywell$Plate_Well =="5_C05", "DMSO_L4992",
                                                            ifelse(normbywell$Plate_Well =="5_B07" | normbywell$Plate_Well =="5_C07", "DMSO_L6004",
                                                                   ifelse(normbywell$Plate_Well =="6_B02" | normbywell$Plate_Well =="6_C02" | normbywell$Plate_Well =="6_D02" , "DMSO_L4992",
                                                                          ifelse(normbywell$Plate_Well =="6_B03" | normbywell$Plate_Well =="6_C03" | normbywell$Plate_Well =="6_D03" ,"1uM_L4992", 
                                                                                 ifelse(normbywell$Plate_Well =="6_B04" | normbywell$Plate_Well =="6_C04" | normbywell$Plate_Well =="6_D04","DMSO_L3200",
                                                                                        ifelse(normbywell$Plate_Well =="6_B05" | normbywell$Plate_Well =="6_C05" | normbywell$Plate_Well =="6_D05","1uM_L3200",
                                                                                               ifelse(normbywell$Plate_Well =="4_C04", "DMSO_L4992","none_U2OS"))))))))))))

normbywell <- normbywell %>% separate(Plate_Well, c('Plate', 'Well'))
normbywell <- normbywell %>% separate(Conc, c('Conc', 'cell_line'))
normbywell$Conc <- factor(normbywell$Conc, levels=c("none", "DMSO", "100uM"))
normbywell$cell_line <- factor(normbywell$cell_line, levels=c("U2OS","L4992", "L6004", "L3200", "L2572"))

###plot by well
normcount <- ggplot(data = normbywell, aes(x = cell_line, y = count)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))

normarea <- ggplot(data = normbywell, aes(x = cell_line, y = Area)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))

normpH <- ggplot(data = normbywell, aes(x = cell_line, y = newpH)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))

normCa <- ggplot(data = normbywell, aes(x = cell_line, y = newCa)) + 
  geom_point(position = position_dodge(width = 0.5), aes(group = Conc, colour = Plate))

ggarrange(
  normpH, normCa, normcount, normarea,
  common.legend = TRUE)


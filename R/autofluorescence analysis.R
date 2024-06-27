library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)


df <- read_excel("../excel files/apilimod plate 4/DD_d04 autofluorescence_004.xlsx")
df$G <- df$`G | Mean` - df$`G | Background`
df$O <- df$`O | Mean` - df$`O | Background`
df$R <- df$`R | Mean` - df$`R | Background`

#filter 
df <- subset(df, G > 0)
df <- subset(df, O > 0)

summary(df$G)
summary(df$O)
summary(df$R)

#calculate pH and ca artificial from autofluorescence
#calculate pH
df$pH <- pKa + slope*log((lowGtoR - df$`G / R`)/(df$`G / R` - highGtoR))

#calculate log(Ca) and Ca
df$Kd <- 1.03 + 5.4*10^12 * exp(-df$pH/0.189) + 3.11*10^6 * exp(-df$pH/0.412)
df$lowOtoR <- highOtoR/(4.24 + 0.12*exp(0.5*df$pH))
df$logCa <- log10(df$Kd*(df$`O / R` - df$lowOtoR)/(highOtoR - df$`O / R`))

df$Ca <- 10^(df$logCa)

# plot
G <- ggplot(data = df, aes(x = 0, y = G)) + geom_point(position = "jitter") + ggtitle("G")
O <- ggplot(data = df, aes(x = 0, y = O)) + geom_point(position = "jitter") + ggtitle("O")
pH <- ggplot(data = df, aes(x = 0, y = pH)) + geom_point(position = "jitter") + ggtitle("pH")
Ca <- ggplot(data = df, aes(x = 0, y = Ca)) + geom_point(position = "jitter") + ggtitle("Ca")

ggarrange(G,O,pH,Ca)


######try the filter on dmso l4992 well
autoG <- mean(df$`G | Mean`) #summary(df$G)[4]
autoO <- mean(df$`O | Mean`) #summary(df$O)[4]

lowGtoR = 0.0463
highGtoR = 0.677
highOtoR = 0.590
pKa = 4.48
slope = 0.437

l4992 <- read_excel("../excel files/apilimod plate 4/DD_well c04 dmso L4992_004.xlsx")
l4992$G_corrected <- l4992$`G | Mean` - autoG #- l4992$`G | Background`- autoG
l4992$O_corrected <- l4992$`O | Mean` -autoO #- l4992$`O | Background`- autoO

l4992$testGtoR<- (l4992$`G | Mean` - l4992$`G | Background`)/(l4992$`R | Mean` - l4992$`R | Background`)
l4992 <- l4992 %>% select(c(Well, FOV, Area, `G / R`, `O / R`, pH, Ca, `log(Ca)`))

#calculate pH
l4992$pH <- pKa + slope*log((lowGtoR - l4992$`G / R`)/(l4992$`G / R` - highGtoR))

#calculate log(Ca) and Ca
l4992$Kd <- 1.03 + 5.4*10^12 * exp(-l4992$pH/0.189) + 3.11*10^6 * exp(-l4992$pH/0.412)
l4992$lowOtoR <- highOtoR/(4.24 + 0.12*exp(0.5*l4992$pH))
l4992$logCa <- log10(l4992$Kd*(l4992$`O / R` - l4992$lowOtoR)/(highOtoR - l4992$`O / R`))

l4992$Ca <- 10^(l4992$logCa)

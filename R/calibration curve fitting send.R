library(readxl)
library(dplyr)
library(tidyr)

df <- read_excel("../excel files/apilimod plate 6/DD_Calibration_006.xlsx")

#filter G/R and O/R
df <- subset(df, `G / R` >= 0)
df <- subset(df, `O / R` >= 0)

#find median G/R and O/R by well
byWell <- df %>%
  group_by(Well) %>%
  summarise_at(c("G / R", "O / R"), list(median)) %>%
  inner_join(df %>%
               group_by(Well) %>%
               summarise(count = n()))


#assign pH values by well (assuming correct order for pH is same for as alphabetical order of wells)
byWell[order(byWell$Well),]
byWell$pH <- c(3.45, 3.85, 4.09,4.2, 4.5, "4.5HC", 4.9, 5.2,5.5, 6.35)

highOtoR = byWell[[which(byWell$pH == "4.5HC"), "O / R"]]
LC <- subset(byWell, pH != "4.5HC")
LC$pH <- as.numeric(LC$pH)

#LC <- subset(LC, pH < 6) #exclude outlier at ph 6.35

####curve fitting
x = LC$pH
y = LC$`G / R`

fitmodel <- nls(y ~ highGtoR + (lowGtoR - highGtoR)/(1 + exp((x - pKa)/slope)), start=list(highGtoR = 0.8, lowGtoR = 0.1, pKa = 4.5, slope = 0.3))

#output results
coef(summary(fitmodel))
highOtoR

plot(LC$pH, LC$`G / R`)

r <- range(LC$pH)
xNew <- seq(r[1],r[2],length.out = 5000)
yNew <- predict(fitmodel,list(x = xNew))

lines(xNew,yNew)



#############write files
write.csv(byWell, "../excel files/apilimod plate 6/byWell.csv")
write.csv(coef(summary(fitmodel)), "../excel files/apilimod plate 6/coefficients-wo3.85.csv")

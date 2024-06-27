library(readxl)
library(dplyr)
library(tidyr)

df <- read_excel("../excel files/apilimod bafilomycin plate 10/DD_Calibration_010.xlsx")

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
byWell$pH <- c(4.5, 4.75, "4.5HC", 4.9,4.3, 5.2, 4.09, 5.5, 3.45, 6.35, 2.65, 7)

highOtoR = byWell[[which(byWell$pH == "4.5HC"), "O / R"]]
LC <- subset(byWell, pH != "4.5HC")
LC$pH <- as.numeric(LC$pH)

LC <- subset(LC, pH != 7) #exclude pH7 outlier

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



#plot curve from plate 9 on same graph
byWell9 <- read.csv("../excel files/apilimod plate 9/byWell.csv")
highOtoR9 = byWell9[[which(byWell9$pH == "4.5HC"), "O...R"]]
LC9 <- subset(byWell9, pH != "4.5HC")
LC9$pH <- as.numeric(LC9$pH)

points(LC9$pH, LC9$G...R, col = "red")
y9 <- 0.740830732 + (0.152360181 - 0.740830732)/(1 + exp((xNew - 4.884814844)/0.539255075))
lines(xNew, y9, col = "red")

#############write files
write.csv(byWell, "../excel files/apilimod bafilomycin plate 10/byWell.csv")
write.csv(coef(summary(fitmodel)), "../excel files/apilimod bafilomycin plate 10/coefficients.csv")

##zd
df <- df %>% select(Plate, Well, Area, `G / R`, `O / R`)
df$pH <- ifelse(df$Well == "B10" ,4.5,
                  ifelse(df$Well == "C10", "4.5HC",
                  ifelse(df$Well == "D10", 4.3,
                  ifelse(df$Well == "E10", 4.09,
                  ifelse(df$Well == "F10", 3.45,
                  ifelse(df$Well == "G10", 2.65,
                  ifelse(df$Well == "B11", 4.75,
                  ifelse(df$Well == "C11", 4.9,
                  ifelse(df$Well == "D11", 5.2,
                  ifelse(df$Well == "E11", 5.5,
                  ifelse(df$Well == "F11", 6.35, 7)))))))))))

df2 <- read_excel("../excel files/apilimod plate 9/DD_Calibration_009.xlsx")
df2 <- subset(df, `G / R` >= 0)
df2 <- subset(df, `O / R` >= 0)
df2 <- df2 %>% select(Plate, Well, Area, `G / R`, `O / R`)
df$pH <- ifelse(df$Well == "B10" ,"4.5HC",
                ifelse(df$Well == "C10", 4.5,
                       ifelse(df$Well == "D10", 4.3,
                              ifelse(df$Well == "E10", 4.09,
                                     ifelse(df$Well == "F10", 3.45,
                                            ifelse(df$Well == "G10", 2.65,
                                                   ifelse(df$Well == "B11", 4.75,
                                                          ifelse(df$Well == "C11", 4.9,
                                                                 ifelse(df$Well == "D11", 5.2,
                                                                        ifelse(df$Well == "E11", 5.5,
                                                                               ifelse(df$Well == "F11", 6.35, 7)))))))))))

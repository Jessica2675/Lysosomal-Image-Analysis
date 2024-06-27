library(readxl)
library(writexl)
library(dplyr)
library(tidyr)

df <- read_excel("../MJFF/R_fovanalysed2.xlsx")

#paticle analysis
##calculate yellow/total from df
df$YellowtoTotal <- 100 * df$`count_>0.1` / df$count_unfiltered

##combine cell line and treatment conditions
df$condition <- paste(df$`Cell line`, df$Treatment, sep="_")

##find mean yellow/total by treatment
means <- df %>%
  group_by(condition) %>%
  summarise_at("YellowtoTotal", list(mean))

# separate diff. treatments into diff columns
particle_analysis <- means %>%
  spread(condition, YellowtoTotal)

#order columns alphabetically so DMSO and torin of same cell line are next to each other (..dmso, ...torin, ...dmso, ...torin)
particle_analysis <- particle_analysis[,order(colnames(particle_analysis))]

## odd columns/even columns (i.e. induced/uninduced)
s <- seq(1,ncol(particle_analysis),2)
particle_analysis[paste0(names(particle_analysis[-s]), "/", names(particle_analysis)[s])] <- particle_analysis[-s] / particle_analysis[s]
particle_analysis


write_xlsx(list("all yellowtototal" = df, "mean by treatment" = means, "means and P_indtoUnind" = particle_analysis), "particle analysis.xlsx")

library(readxl)
library(writexl)
library(dplyr)
library(tidyr)

df <- read_excel("../MJFF/MJFF_AutophagyPREMOvalidation_001.xlsx")

#combine well info and FOV info in new column
df$Well_FOV <- paste(df$Well, df$FOV, sep="_")

#find count and median values by FOV without filters
nofilter <- df %>%
  group_by(Well_FOV) %>%
  summarise(count = n())%>% 
  inner_join(df %>%
  group_by(Well_FOV) %>%
  summarise_at(c("Area", "Green / Red"), list(median)))


#find count and median values by FOV above 0.1
df_filtered <- subset(df, `Green / Red` > 0.1)

above_0.1 <- df_filtered %>%
  group_by(Well_FOV) %>%
  summarise(count = n())%>% 
  inner_join(df_filtered %>%
  group_by(Well_FOV) %>%
  summarise_at(c("Area", "Green / Red"), list(median)))


#merge both filtered and unfiltered into one table
final <- merge(nofilter, above_0.1, by = "Well_FOV", suffixes = c("_unfiltered", "_>0.1"))

#write final as excel file
write_xlsx(final, "MJFF_001.xlsx")



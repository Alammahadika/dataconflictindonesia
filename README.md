# SOCIAL CONFLICT'S IN INDONESIA 
From R-Studio, this project tries to do a new innovation analysis of social conflict in Indonesia, collecting conflict data from [The Humanitarian Data Exchange](https://data.humdata.org/dataset/ucdp-data-for-indonesia), and this project also explains a little about social conflict in Indonesia by region. I hope this data becomes an open source for peoples.

## Affected Kills Social Conflict in Indonesia 1989-2023
### Read Data Base & Manipulation Data 
```r{}
dataconflictindonesia <-read.table("/Users/mymac/Desktop/Data Github/data_conflict", sep = ",", header = FALSE)
colnames(dataconflictindonesia) <-c("Years", "Province", "Affected")
View(dataconflictindonesia)
print(dataconflictindonesia)

library(tidyr)
dataconflictindonesia <- dataconflictindonesia %>% arrange(Years) #sort data to years
View(dataconflictindonesia)

library(writexl)
write_xlsx(dataconflictindonesia, "dataconflictindonesia.xlsx") # Import Data set to xlsx Excel
getwd()

library(readxl)
datasocialconflictidn <- read_excel("~/Desktop/Data Github/datasocialconflictidn.xlsx")
View(datasocialconflictidn) # manipulation data from 1887 to 68 with excel

```

### Death Toll Social Conflict in Indonesia 1989-2023

```r{}
ggplot(datasocialconflictidn, aes(x = Years, y = Affected)) +
  geom_line(size = 1, alpha = 0.8, color = "black") +
  geom_point(size = 1) +
#shaded for conflict
  annotate("rect", xmin = 1991, xmax = 1996, ymin = 0, ymax = Inf, fill = "red", alpha = 0.3, color = "darkred") +
  annotate("rect", xmin = 1997, xmax = 1998, ymin = 0, ymax = Inf, fill = "blue", alpha = 0.3, color = "darkblue") +
  annotate("rect", xmin = 1999, xmax = 2000, ymin = 0, ymax = Inf, fill = "green", alpha = 0.3, color ="darkgreen") +
  annotate("rect", xmin = 2001, xmax = 2002, ymin = 0, ymax = Inf, fill = "grey", alpha = 0.3, color = "darkgrey") +
  annotate("rect", xmin = 2003, xmax = 2005, ymin = 0, ymax = Inf, fill = "yellow", alpha = 0.3, color = "darkgoldenrod") +
  # add text for conflict
  annotate("text", x = 1991.5, y = max(datasocialconflictidn$Affected) -500,
           label = "Conflict Timor-Timur", color = "black", angle = 90, fontface = "bold") +
  annotate("text", x = 1997.5, y = max(datasocialconflictidn$Affected) -1000,
           label = "Conflict West Kalimantan & Irian Jaya", color = "black", angle = 90, fontface = "bold") +
  annotate("text", x = 1999.5, y = max(datasocialconflictidn$Affected) -1500,
           label = "Conflict Timor-Timor & Maluku", color = "black", angle = 90, fontface = "bold") +
  annotate("text", x = 2001.5, y = max(datasocialconflictidn$Affected) -2000,
           label = "Conflict Sampit & Bali", color = "black", angle = 90, fontface = "bold") +
  annotate("text", x = 2003.5, y = max(datasocialconflictidn$Affected) -2200,
           label = "Conflict Aceh", color = "black", angle = 90, fontface = "bold") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1898, 2023, by = 1)) +
  labs(x = "Years", y = NULL, 
       title = "Death Toll Social Conflict (Affected Kills) in Indonesia 1989-2023",
       subtitle = "Source: Humanitarian Data Exchange") +
  theme(plot.title = element_text(face = "bold"))

```

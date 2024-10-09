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

library(knitr)
datasocialconflictidnmarkdown <-kable(datasocialconflictidn, format = "markdown") 
print(datasocialconflictidnmarkdown)
```

| Years| Affected|Region                                       |
|-----:|--------:|:--------------------------------------------|
|  1989|        7|NA                                           |
|  1990|      200|NA                                           |
|  1991|      429|Conflict Timor Timor                         |
|  1992|       53|NA                                           |
|  1993|       27|NA                                           |
|  1994|       12|NA                                           |
|  1995|       65|NA                                           |
|  1996|       36|NA                                           |
|  1997|      441|Conflict West Kalimantan & Conflict Irian    |
|  1998|      141|NA                                           |
|  1999|     2421|Conflict Timor Timor & Conflict Maluku       |
|  2000|      917|NA                                           |
|  2001|      884|Conflict Sampit & Conflict Bali              |
|  2002|      726|NA                                           |
|  2003|     1069|Conflict Aceh                                |
|  2004|     1082|NA                                           |
|  2005|      242|NA                                           |
|  2006|       13|NA                                           |
|  2007|        2|NA                                           |
|  2008|        6|NA                                           |
|  2009|        0|NA                                           |
|  2010|        0|NA                                           |
|  2011|        9|NA                                           |
|  2012|        1|NA                                           |
|  2013|       20|NA                                           |
|  2014|       12|NA                                           |
|  2015|        4|NA                                           |
|  2016|        0|NA                                           |
|  2017|        2|NA                                           |
|  2018|       32|NA                                           |
|  2019|       16|NA                                           |
|  2020|       24|NA                                           |
|  2021|       68|NA                                           |
|  2022|       64|NA                                           |
|  2023|       89|NA                                           |



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
![Indonesia Conflict Social](./datavisual/dataaffectedkills.png)

### Death Toll of Social Conflict in Indonesia 
The graph above shows the number of first fatalities from 1999 to 2001 the highest number of fatalities in the East Timor and Maluku conflicts, showing a range of 2500 victims in this period. Then from 2004 - 2005 there was a significant increase in the Aceh conflict with the number of victims above 500. After 2005, the graph shows a decrease in the number of victims of the conflict during this period. Social conflict in Indonesia is low, and the number of fatalities is very low due to social conditions.

##  [Indonesia: National Violence Monitoring System 2014](https://microdata.worldbank.org/index.php/catalog/2626/study-description)
This section tries to analyze the database of the 2014 National Violence Monitoring System, I will make innovations in data analysis, collecting databases on information on acts of violence between individuals or groups that have psychological impacts. The total observation data collected amounted to 28046 data case and 100 variable.
This section only [analyzes variables](https://microdata.worldbank.org/index.php/catalog/2626/data-dictionary/F1?file_name=DATA%20SNPK%202014_JRI%2030%2011%202015) Affiliation involved in conflict and violence & Number of fatalities.

### Data Bese & Data Manipulation  
```r{}
library(haven) # read format data (sav) from spss 
datasnpk <- read_sav("/Users/mymac/Desktop/Data Github/datasnpk2015.sav")

library(writerxl) # export data from spss (sav) to (xlsx) excel
write_xlsx(datasnpk, "datasnpk2014.xlsx") # for save data to in device
getwd()

library(readxl)
datasnpk2014 <- read_excel("~/Desktop/Data Github/dataspnk2014.xlsx") #import data in environment R
datasnpk2014$tanggal_kejadian <- as.Date(datasnpk2014$tanggal_kejadian, format = "%d/%m/%Y") # adjustment variable date to format R



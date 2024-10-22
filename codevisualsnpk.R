# National Violence Monitoring System 2014

#data from;
# https://microdata.worldbank.org/index.php/catalog/2626/study-description

library(haven)
datasnpk <- read_sav("/Users/mymac/Desktop/Data Github/datasnpk2015.sav")

library(writexl) # import to 
write_xlsx(datasnpk, "datasnpk2014.xlsx")
getwd()

library(readxl)
datasnpk2014 <- read_excel("~/Desktop/Data Github/datasnpk2014.xlsx")
datasnpk2014$tanggal_kejadian <- as.Date(datasnpk2014$tanggal_kejadian, format = "%d/%m/%Y")
print(datasnpk2014) # view data


# Manipulation data from general data 

# (1) Unclear                      : 6652
# (2) Others                       : 70
# (3) Militia                      : 18
# (4) Society                      : 13238
# (5) Affiliation with Government  : 973
# (6) Selected Institution         : 13
# (7) NGOs International           : 0
# (8) NGOs Local                   : 16
# (9) Private Sector               : 2858
# (10) Political Party             : 215
# (11) Religion Institution        : 26
# (12) Labor                       : 25
# (13) Mass Group                  : 96
# (14) Army                        : 126
# (15) Police                      : 2062
# (16) Police Brimob               : 49
# (17) Separatism                  : 38
# (18) Student                     : 1559
# (19) Security                    : 13

# import data set & subset data 

library(readxl)
totalactors <- read_excel("~/Desktop/Data Github/totalactors.xlsx")
View(totalactors) 

# Create graph analysis tree maps
library(treemap)
library(treemapify)
library(ggplot2)
library(viridis)


ggplot(totalactors, aes(area = Total, fill = Actors, label = Actors)) +
  geom_treemap() +
  geom_treemap_text(colour = "yellow3", place = "centre", grow = FALSE, size = 15) +
  scale_fill_viridis_d(option = "magma") +
  labs(title = "Affiliates Involved in Conflict and Violence in Indonesia 2014") +
  theme(legend.background = "none",
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        plot.title = element_text(color = "white")) +
  theme(plot.title = element_text(face = "bold")) +
  theme_classic()



# Data Victim/Fatalities Died conflict in Indonesia in 2014

library(dplyr)
filtered_dataspnk2024 <- datasnpk2014 %>%
  filter(kil_total != 0)

print(filtered_dataspnk2024$jenis_kek)

ggplot(filtered_dataspnk2024, aes(tanggal_kejadian, y = kil_total)) +
  geom_line(color = "black") +
  geom_smooth(method = "lm", formula = y~log(x)) +
  labs(x = "Date", y = "Total",
       subtitle = "Source: Government of Indonesia & The World Bank") +
  ggtitle("Total Death Toll Social Conflict Indonesia in 2014") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  theme(axis.text = element_blank()) +
  scale_y_continuous(breaks = seq(1, 19, by = 1)) +
  theme_bw() 

# Analysis death victim people for month 
# graph analysis with geom_smooth

library(readxl)
totalmonthdeath <- read_excel("totalmonthdeath.xlsx") # data set from datasnpnk2014
print(totalmonthdeath)


totalmonthdeath$Month <- factor(totalmonthdeath$Month, 
                                levels = c("January", "February", "March",
                                           "April", "May", "June", "July", 
                                           "August", "September", "October", 
                                           "November", "December"))

library(ggplot2)
library(dplyr)

totalmonthdeath %>%
  tail(12) %>%
  ggplot(aes(x = Month, y = Total, group = 1)) +
  geom_line(aes(x = Month, y = Total), color = "black", size = 0.1) +
  geom_point(shape = 20, color = "darkblue", size = 3) +
  theme_bw() +
  geom_smooth(method = "loess", span = 1, se = FALSE) +
  labs(title = "Total Death Toll Social Conflict Indonesia in 2014 Per Month",
       subtitle = "Source: Government of Indonesia & The World Bank", 
       y = "Total",
       x = "Month") +
  scale_y_continuous(breaks = seq(min(totalmonthdeath$Total),max(totalmonthdeath$Total),
                                  by = 10)) +
  theme(plot.title = element_text(face = "bold"))

# Number of Victims Province in 2014
library(readxl)
deatheachprovince <- read_excel("~/Desktop/Data Github/deatheachprovince.xlsx")
View(deatheachprovince)

library(knitr)
deatheachprovincemarkdown <-kable(deatheachprovince, format = "markdown")
print(deatheachprovincemarkdown)


library(ggplot2)
library(dplyr)

deatheachprovince %>%
  mutate(Province = str_remove(Province, "-.*$")) %>%
  ggplot(aes(y = reorder(Province, Total), x = Total)) +
  geom_segment(aes(x = 0, xend = Total, y = reorder(Province, Total),
                   yend = reorder(Province, Total)), color = "black") +
  geom_point(size = 4, color = "red", fill = "darkred", shape = 21) +
  geom_text(aes(label = Total),
            nudge_x = 10, hjust = -0.3, vjust = 0.5, size = 3.5, color = "black") +
  labs(title = "Number of Victims Province in 2014",
       subtitle = "Source: Government of Indonesia & The World Bank",
       y = "Province",
       x = "Victims") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(angle = 90, hjust = 1))

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


library(ggplot2)
library(lubridate)
library(gridExtra)



ggplot(datasnpk2014, aes(tanggal_kejadian, y = actor_s1_tp)) +
  geom_jitter(aes(color = factor(actor_s1_tp))) +
  ggtitle("Affiliates Involved in Conflict and Violence in Indonesia 2014") +
  labs(x = "Date", y = "Actor Affiliation", color = "Actor Affiliations",
       subtitle = "Source: Government of Indonesia & The World Bank") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(1, 19, by = 1)) +
  theme(axis.text = element_blank()) +
  theme_bw() +
  scale_color_manual(values = c(
    "brown4", "aquamarine4", "azure4", "coral3", 
    "cornsilk4", "darkslategray", "deeppink4", "royalblue3", 
    "firebrick4","green4", "indianred4", "maroon4", 
    "steelblue4", "pink4", "blue2", "red4", 
    "tan4", "slateblue3", "tomato"), 
    labels = c(
      "Unclear", "Others", "Militia", "Society", 
      "Affiliation with Government", "Selected Institutions", 
      "NGOs International", "NGOs Local", "Private Sector", 
      "Political Party", "Religion Institutions", "Labour", 
      "Mass Group", "Army", "Police", "Police Brimob", 
      "Separatism", "Student", "Security"
    ))


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

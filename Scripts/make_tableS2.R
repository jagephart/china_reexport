# TITLE: make_tableS2
# AUTHOR: Bixaun Yang

# Load cleaned data
source("Scripts/clean_data.R")

#----------------------------------------------------------------------------------------------------#
#                                       Table S2
#----------------------------------------------------------------------------------------------------#
## Input 2019 import data
imports_q.partner <- read.csv("Data/import_by_partner_quantity.csv")
imports_v.partner <- read.csv("Data/import_by_partner_value.csv")

## Label EU countries
imports_q.partner$Country[imports_q.partner$Country=="Austria" | imports_q.partner$Country=="Belgium" |
                            imports_q.partner$Country=="Bulgaria" |imports_q.partner$Country=="Croatia" |
                            imports_q.partner$Country=="Cyprus" |imports_q.partner$Country=="Czechia" |
                            imports_q.partner$Country=="Denmark" |imports_q.partner$Country=="Estonia" |
                            imports_q.partner$Country=="Finland" |imports_q.partner$Country=="France" |
                            imports_q.partner$Country=="Germany" |imports_q.partner$Country=="Greece" |
                            imports_q.partner$Country=="Hungary" |imports_q.partner$Country=="Ireland" |
                            imports_q.partner$Country=="Italy" |imports_q.partner$Country=="Latvia" |
                            imports_q.partner$Country=="Lithuania" |imports_q.partner$Country=="Luxembourg" |
                            imports_q.partner$Country=="Malta" |imports_q.partner$Country=="Netherlands" |
                            imports_q.partner$Country=="Poland" |imports_q.partner$Country=="Portugal" |
                            imports_q.partner$Country=="Romania" |imports_q.partner$Country=="Slovak Republic" |
                            imports_q.partner$Country=="Slovenia" |imports_q.partner$Country=="Spain" |
                            imports_q.partner$Country=="Sweden"] <- "EU"

imports_v.partner$Country[imports_v.partner$Country=="Austria" | imports_v.partner$Country=="Belgium" |
                            imports_v.partner$Country=="Bulgaria" |imports_v.partner$Country=="Croatia" |
                            imports_v.partner$Country=="Cyprus" |imports_v.partner$Country=="Czechia" |
                            imports_v.partner$Country=="Denmark" |imports_v.partner$Country=="Estonia" |
                            imports_v.partner$Country=="Finland" |imports_v.partner$Country=="France" |
                            imports_v.partner$Country=="Germany" |imports_v.partner$Country=="Greece" |
                            imports_v.partner$Country=="Hungary" |imports_v.partner$Country=="Ireland" |
                            imports_v.partner$Country=="Italy" |imports_v.partner$Country=="Latvia" |
                            imports_v.partner$Country=="Lithuania" |imports_v.partner$Country=="Luxembourg" |
                            imports_v.partner$Country=="Malta" |imports_v.partner$Country=="Netherlands" |
                            imports_v.partner$Country=="Poland" |imports_v.partner$Country=="Portugal" |
                            imports_v.partner$Country=="Romania" |imports_v.partner$Country=="Slovak Republic" |
                            imports_v.partner$Country=="Slovenia" |imports_v.partner$Country=="Spain" |
                            imports_v.partner$Country=="Sweden"] <- "EU"
## Group imports by species and countries
imports_q.partner <- merge(imports_q.partner,match_trade[,1:2],by="Commodity..Name.",all.x = T)
imports_q.partner <- imports_q.partner %>% 
  group_by(Species,Country) %>% 
  summarize(Quantity=sum(Quantity))

imports_v.partner <- merge(imports_v.partner,match_trade[,1:2],by="Commodity..Name.",all.x = T)
imports_v.partner <- imports_v.partner %>% 
  group_by(Species,Country) %>% 
  summarize(Value=sum(Value))

## Subset datasets to only include selected species
imports_q.partner <- merge(imports_q.partner,Top_15_imports[,c(1,4)],by="Species",all.x = T) %>% 
  filter(select==1)
imports_v.partner <- merge(imports_v.partner,Top_15_imports[,c(1,4)],by="Species",all.x = T) %>% 
  filter(select==1)

## Select top three source countries for China's seafood imports by species
imports.partner <- merge(imports_q.partner[,1:3],imports_v.partner[,1:3],by=c("Species","Country"),all = T)
imports.partner <- imports.partner %>% 
  mutate(unit_value=Value/Quantity) %>% ## Calculate unit value
  group_by(Species) %>% 
  mutate(share=Quantity/sum(Quantity)) %>% 
  arrange(Species,desc(Quantity)) %>% 
  group_by(Species) %>% 
  slice(1:3)

write.csv(imports.partner, "Outputs/TableS2.csv", row.names = FALSE)

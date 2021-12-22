# TITLE: make_table1
# AUTHOR: Bixuan Yang

# Load cleaned data
source("Scripts/clean_data.R")

# Load data from Table S4
table_S4 <- read.csv("Outputs/TableS4.csv")

#----------------------------------------------------------------------------------------------------#
#                                        Table 1
#----------------------------------------------------------------------------------------------------#
table_1 <- table_S4[,c(1,3,5,6)] ## select columns that contain species and production
table_1 <- merge(table_1,Top_15_imports,by="Species",all = T) %>% filter(select==1)
table_1$reexport_share <- table_1$Export_live_weight/(table_1$Production+table_1$Import_live_weight)
table_1 <- table_1 %>% 
  select(Species,Import_quantity,share,Production,reexport_share) %>% 
  arrange(desc(Import_quantity))

write.csv(table_1, "Outputs/Table1.csv", row.names = FALSE)

## Note: The estimates in the article are done with excel, which may lead to slight discrepencies due to rounding

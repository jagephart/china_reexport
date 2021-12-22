# TITLE: make_tableS4
# AUTHOR: Bixaun Yang

# Load cleaned data
source("Scripts/clean_data.R")

# Load data from table S1
production.dwf <- read.csv("Outputs/TableS1_FAOdwf.csv") # Produced by make_tableS1.R

#----------------------------------------------------------------------------------------------------#
#                                         Table S4
#----------------------------------------------------------------------------------------------------#
table_S4 <- merge(imports_q.lw[,c(1,6,7)],exports_q.lw[,c(1,6,7)],by="Species",all=T)
table_S4 <- merge(table_S4,production.s,by="Species",all=T)
table_S4 <- merge(table_S4,production.dwf,by="Species",all.x=T)
table_S4[is.na(table_S4)] <- 0
table_S4$Production_domestic <- table_S4$Production-table_S4$Production_dwf

table_S4.1 <- table_S4 %>% 
  mutate(Import_live_weight=Import_live_weight+Production_dwf, ## dwf landings are considered as imports to compute apparent consumption, consumer destination indicator, and production origin indicastor
         apparent_consumption=Production_domestic+Import_live_weight-Export_live_weight,
         consumer_dest_indicator=100-100*(apparent_consumption/(Production_domestic+Import_live_weight)),
         production_orig_indicator=100*Import_live_weight/(Production_domestic+Import_live_weight))

table_S4.1$apparent_consumption[table_S4.1$apparent_consumption<0] <- 0 ## Replace any negative apparent consumption values (due to live weight conversions/data errors) with zero
table_S4.1$consumer_dest_indicator[table_S4.1$consumer_dest_indicator>100] <- 100 ## Replace any indicator that exceeds 100 with 100

write.csv(table_S4.1, "Outputs/TableS4.csv", row.names = FALSE)

## Note: The estimates in the article are done with excel, which may lead to slight discrepancies due to rounding


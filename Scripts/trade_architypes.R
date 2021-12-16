# Example trade architype figure
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggthemes)

# Load data
df_all <- read.csv("Data/china_reexport_summary_20210618.csv")

df_all <- df_all %>%
  replace(is.na(.), 0) %>%
  # Revise production and import to account for distant water fishing (DWF)
  mutate(Production_1000t = Production_1000t-DWF_1000t,
         Import_lw_1000t = Import_lw_1000t + DWF_1000t) %>%
  # Calculate apparent consumption
  mutate(apparent_consumption = Production_1000t+Import_lw_1000t-Export_lw_1000t) %>%
  # Replace any negative apparent consumption values (due to live weight conversions/data errors) with zero
  mutate(apparent_consumption = ifelse(apparent_consumption < 0, 0, apparent_consumption)) %>%
  # Calculate production origin and consumer destination indicators
  mutate(consumer_destination_indicator = 100-(100*apparent_consumption/(Production_1000t+Import_lw_1000t)),
         production_origin_indicator = 100*Import_lw_1000t/(Production_1000t+Import_lw_1000t)) %>%
  # Calculate re-export upper and lower from FAO data
  mutate(
    # Fill exports with production first
    reexport_lower = ifelse(Production_1000t>Export_lw_1000t, 
                            # If production is greater than exports, no processed imports
                            0, 
                            # If production can't explain all exports:
                            ifelse(Import_lw_1000t>(Export_lw_1000t-Production_1000t),
                                   Export_lw_1000t-Production_1000t,
                                   Import_lw_1000t)),
    # Fill exports with imports first
    reexport_upper = ifelse(Import_lw_1000t>Export_lw_1000t, 
                            Export_lw_1000t, 
                            Import_lw_1000t))
# Min re-export rate
sum(df_all$reexport_lower)/sum(df_all$Import_lw_1000t)
# Max re-export rate
sum(df_all$reexport_upper)/sum(df_all$Import_lw_1000t)

# Leave processing import share NA's as NA's (by turning 0's back into NAs)
df_all$processing_import_share[df_all$processing_import_share == 0] <- NA

architypes <- data.frame(x = c(5, 5, 95, 95), 
                         y = c(45, 55, 45, 55),
                         label = c("Domestic-oriented production", 
                                   "Imports for domestic market", 
                                   "Export-oriented production",
                                   "Re-exports"))

# Plot production origin versus consumer destination, with points scaled by apparent consumption
png("Outputs/architype_fig_scale_consumption.png", width = 8, height = 5, units = "in", res = 300)
ggplot(df_all, aes(x = consumer_destination_indicator, y = production_origin_indicator)) +
  geom_jitter(aes(size = apparent_consumption, alpha = 0.8), width = 2.5, height = 2.5) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(-5, 105)) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(-5, 105)) +
  scale_size_continuous(range = c(3, 8)) + # change the size of the points
  scale_colour_distiller(palette = 5, na.value = "black", direction = 1) +
  geom_text_repel(aes(label = Label), 
                  force = 1, point.padding = 0.2, box.padding = 0.2,
                  size = 3.25) + 
  geom_hline(yintercept = 50) +
  geom_vline(xintercept = 50) +
  geom_text(data = architypes, aes(x = x, y = y, label = label), size = 3.5) +
  labs(x = "Consumer destination indicator",
       y = "Production origin indicator",
       size = "Apparent consumption (1000t live weight)",
       alpha = element_blank()) +
  guides(size = "legend", color = "legend", alpha = "none") +
  theme_minimal() + 
  theme(legend.position="bottom", legend.box="vertical")
dev.off()

# Architypes table
architypes_table <- df_all %>%
  mutate(Architype = case_when(
    (production_origin_indicator < 50 & consumer_destination_indicator < 50) ~ "Domestic-oriented production", 
    (production_origin_indicator < 50 & consumer_destination_indicator > 50) ~ "Export-oriented production", 
    (production_origin_indicator > 50 & consumer_destination_indicator < 50) ~ "Imports for domestic market", 
    (production_origin_indicator > 50 & consumer_destination_indicator > 50) ~ "Re-exports" 
  )) %>%
  arrange(Architype)
write.csv(architypes_table, "Outputs/architypes_table_20210621.csv")

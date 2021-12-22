# TITLE: make_figureS3
# AUTHOR: Jessica Gephart 

# Load packages
library(viridis)
library(ggpubr)
library(ggthemes)

# Load cleaned data
source("Scripts/clean_data.R")

# Load data from Table S4
table_S4 <- read.csv("Outputs/TableS4.csv") # Produced by make_tableS4.R

#----------------------------------------------------------------------------------------------------#
#                                        Figure S3
#----------------------------------------------------------------------------------------------------#
df <-  merge(table_S4,Top_15_imports,by="Species",all.y = T) %>% 
  select(Species,Import_live_weight,Export_live_weight,Production)
colnames(df)[colnames(df)=="Import_live_weight"] <- "Import_lw_1000t"
colnames(df)[colnames(df)=="Export_live_weight"] <- "Export_lw_1000t"
colnames(df)[colnames(df)=="Production"] <- "Production_1000t"

df <- df %>%
  # Calculate apparent consumption
  mutate(apparent_consumption = Production_1000t+Import_lw_1000t-Export_lw_1000t) %>%
  # Replace any negative apparent consumption values (due to live weight conversions/data errors) with zero
  mutate(apparent_consumption = ifelse(apparent_consumption < 0, 0, apparent_consumption)) %>%
  # Calculate production origin and consumer destination indicators
  mutate(consumer_destination_indicator = 100-(100*apparent_consumption/(Production_1000t+Import_lw_1000t)),
         production_origin_indicator = 100*Import_lw_1000t/(Production_1000t+Import_lw_1000t)) %>%
  # Calculate re-export upper and lower from FAO data
  mutate(
    # Fill exports with imports first
    Reexport_Upper = ifelse(Import_lw_1000t>Export_lw_1000t, 
                            Export_lw_1000t, 
                            Import_lw_1000t),
    # Lower option based on a proportional mix of production and import
    Reexport_Lower = ifelse(
      # Imports as proportion of available product times export
      ((Import_lw_1000t/(Import_lw_1000t + Production_1000t))*Export_lw_1000t)>Import_lw_1000t,
      Import_lw_1000t,
      (Import_lw_1000t/(Import_lw_1000t + Production_1000t))*Export_lw_1000t)) %>%
  # Calculate the export coming from production in each case
  mutate(
    Production_Upper = ifelse(Production_1000t > (Export_lw_1000t - Reexport_Upper),
                              Export_lw_1000t - Reexport_Upper, Production_1000t),
    Production_Lower = ifelse(Production_1000t > (Export_lw_1000t - Reexport_Lower),
                              Export_lw_1000t - Reexport_Lower, Production_1000t)
  ) %>%
  # Calculate unexplained portion
  mutate(
    Error_Upper = Export_lw_1000t - Reexport_Upper - Production_Upper,
    Error_Lower = Export_lw_1000t - Reexport_Lower - Production_Lower
  ) %>%
  mutate(Species = fct_reorder(Species, Export_lw_1000t))

df_scenarios <- df %>%
  select(Species, Reexport_Upper:Error_Lower) %>%
  pivot_longer(cols = Reexport_Upper:Error_Lower, names_to = c("Source", "scenario"),
               names_sep = "_") %>%
  mutate(source = factor(Source, levels = c("Reexport", "Error", "Production")), 
         scenario = factor(scenario, levels = c("Lower", "Upper")))

g_quant <- ggplot(df_scenarios, aes(x = value/1000, y = Species, fill = Source)) + 
  geom_bar(position = "stack", stat = "identity") + 
  xlab("Exports by source (million t)") +
  ylab("") +
  scale_fill_ptol() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~scenario, ncol = 3)
g_quant

g_prop <- ggplot(df_scenarios, aes(x = value, y = Species, fill = Source)) + 
  geom_bar(position = "fill", stat = "identity") + 
  xlab("Proportion of exports by source") +
  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=2)) +
  ylab("") +
  scale_fill_ptol() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~scenario, ncol = 3)
g_prop

png("Outputs/FigureS3.png", width = 5, height = 5.5, units = "in", res = 300)
ggarrange(g_quant, g_prop, ncol = 1, labels = c("a", "b"), 
          common.legend = TRUE, legend = "bottom")
dev.off()

write.csv(df, "Outputs/FigureS3_data.csv", row.names = FALSE)


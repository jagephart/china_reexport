# China’s seafood imports: not for domestic consumption?

Frank Asche, Bixuan Yang, Jessica A. Gephart, Martin D. Smith, James L. Anderson, Edward V. Camp, Taryn M. Garlock, David C. Love, Atle Oglend, and Hans-Martin Straume

Data availability
All data used in the paper is publicly available. This repository  contains the data necessary for our analysis and figures. 

## Data sources

1.	Trade data: the data on China's seafood imports and exports are obtained from the FAO FishStat database (https://www.fao.org/fishery/en/topic/166235), covering the period from 1985 to 2019. Imports by trading partners are available only in 2019.

2.	Production data: the data on China's seafood production comes from the FAO FishStat database (https://www.fao.org/fishery/en/topic/166235), covering the period from 1950 to 2019.

3.	Conversion factors for live weight equivalents data: conversion factors by Combined Nomenclature (CN) 8-digit code are obtained from European Market Observatory for Fisheries and Aquaculture Products (https://www.eumofa.eu/documents/20178/24415/Metadata+2+-+DM+-+Annex+7+CF+per+CN8_%252707-%252714.pdf/7e98ac0c-a8cc-4223-9114-af64ab670532). Given that the Harmonized System (HS) codes have been updated in 2017, conversion factors based on 2012 and 2017 HS codes are both collected.

4.	Distant-water landings data: total distant-water catches and catches of tuna and cuttlefish and squid are collected from the China Fishery Statistical Yearbook (2013-2020).

## Description of the Data 

The trade data and production data that we used for our analysis are contained in the "Data" folder. The files "import_quantity.csv", "export_quantity.csv" and "production_quantity.csv" are at the year-product level, while "imports_by_partner_quantity.csv" and "imports_by_partner_value.csv" are at the year-country-product level. We manually classified each commodity into a species group and assign an HS code to each imported and exported item. The classification is available in "match_names_prod.csv" and "match_names_trade.csv" Conversion factors are provided in files "conversion_factors_2012.csv" and "conversion_factors_2017.csv". Distant-water fishing landings are presented in "DWF_fishery_yearbook.csv".

## Description of R code

The "clean_data.R" script does all pre-processing of the raw data that is then loaded into each respective figure and table file. All figure and summary tables are written out to the "Outputs" folder. 

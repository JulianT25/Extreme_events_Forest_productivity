##Installing packages
library(tidyverse)

setwd("C:\\Users\\julia\\OneDrive - Universidad de Alcala\\Doctorado\\CHAPTER_II_BIOMASS\\new_CC\\nuevo_SPSW_all\\Final_vars")


####Data cleaning
ClimaSPSW_new <- ClimaSPSW_final %>% 
  filter(sumPrcp_ref < 2150) %>% #Limpiamos los valores raros encontrados en las vars de Prcp
  filter(!is.na(minSPEI_sp_3)) #Eliminamos los NA's que encontramos en esta variable de SPEI



####New_vars####
ClimaSPSW_new$year32 <- ClimaSPSW_new$year3 - ClimaSPSW_new$year2
ClimaSPSW_new$year43 <- ClimaSPSW_new$year4 - ClimaSPSW_new$year3
ClimaSPSW_new$AB32 <- ClimaSPSW_new$ba_ha3 - ClimaSPSW_new$ba_ha2
ClimaSPSW_new$AB43 <- ClimaSPSW_new$ba_ha4 - ClimaSPSW_new$ba_ha3
ClimaSPSW_new$ratesAB32 <- (ClimaSPSW_new$ba_ha3 - ClimaSPSW_new$ba_ha2)/ClimaSPSW_new$year32
ClimaSPSW_new$ratesAB43 <- (ClimaSPSW_new$ba_ha4 - ClimaSPSW_new$ba_ha3)/ClimaSPSW_new$year43
ClimaSPSW_new$rAB32rel <- (((ClimaSPSW_new$ba_ha3 - ClimaSPSW_new$ba_ha2)/ClimaSPSW_new$ba_ha2)/ClimaSPSW_new$year32)*100
ClimaSPSW_new$rAB43rel <- (((ClimaSPSW_new$ba_ha4 - ClimaSPSW_new$ba_ha3)/ClimaSPSW_new$ba_ha3)/ClimaSPSW_new$year43)*100
ClimaSPSW_new$AB32rel <- ((ClimaSPSW_new$ba_ha3 - ClimaSPSW_new$ba_ha2)/ClimaSPSW_new$ba_ha2)*100
ClimaSPSW_new$AB43rel <- ((ClimaSPSW_new$ba_ha4 - ClimaSPSW_new$ba_ha3)/ClimaSPSW_new$ba_ha3)*100




###########
ClimaSPSW_new$Prcp_LP <- ClimaSPSW_new$sumPrcp_ref
ClimaSPSW_new$PrcpCC12 <- ClimaSPSW_new$sumPrcp_p2 - ClimaSPSW_new$sumPrcp_ref
ClimaSPSW_new$PrcpCC23 <- ClimaSPSW_new$sumPrcp_p3 - ClimaSPSW_new$sumPrcp_ref 
###########

ClimaSPSW_new$Temp_LP <- ClimaSPSW_new$meanTemp_t_ref1
ClimaSPSW_new$TempCC12 <- ClimaSPSW_new$meanTemp_t1_p2 - ClimaSPSW_new$meanTemp_t_ref1 
ClimaSPSW_new$TempCC23 <- ClimaSPSW_new$meanTemp_t1_p3 - ClimaSPSW_new$meanTemp_t_ref1 
###########

ClimaSPSW_new$Hw_LP <- ClimaSPSW_new$nEventsHeatwave_t_ref2
ClimaSPSW_new$HwCC12 <- ClimaSPSW_new$nEventsHeatwave_t2_p2 - ClimaSPSW_new$nEventsHeatwave_t_ref2
ClimaSPSW_new$HwCC23 <- ClimaSPSW_new$nEventsHeatwave_t2_p3 - ClimaSPSW_new$nEventsHeatwave_t_ref2 
###########

ClimaSPSW_new$SPEImin_LP <- ClimaSPSW_new$minSPEI_sp_ref
ClimaSPSW_new$SPEIminCC12 <- ClimaSPSW_new$minSPEI_sp_2 - ClimaSPSW_new$minSPEI_sp_ref 
ClimaSPSW_new$SPEIminCC23 <- ClimaSPSW_new$minSPEI_sp_3 - ClimaSPSW_new$minSPEI_sp_ref
###########

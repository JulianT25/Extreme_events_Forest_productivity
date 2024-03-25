Table_Biome_Temp <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%  
  summarise(
    Temp1_sd = sd(meanTemp_t1_p1, na.rm = T),
    Temp1 = mean(meanTemp_t1_p1, na.rm = T),
    Temp1max = max(meanTemp_t1_p1, na.rm = T),
    Temp1min = min(meanTemp_t1_p1, na.rm = T),
    Temp2_sd = sd(meanTemp_t1_p2, na.rm = T),
    Temp2 = mean(meanTemp_t1_p2, na.rm = T),
    Temp2max = max(meanTemp_t1_p2, na.rm = T),
    Temp2min = min(meanTemp_t1_p2, na.rm = T),
    Temp3_sd = sd(meanTemp_t1_p3, na.rm = T),
    Temp3 = mean(meanTemp_t1_p3, na.rm = T),
    Temp3max = max(meanTemp_t1_p3, na.rm = T),
    Temp3min = min(meanTemp_t1_p3, na.rm = T),
    TempLP_sd = sd(meanTemp_t_ref1, na.rm = T),
    TempLP = mean(meanTemp_t_ref1, na.rm = T),
    TempLPmax = max(meanTemp_t_ref1, na.rm = T),
    TempLPmin = min(meanTemp_t_ref1, na.rm = T)
  )
Table_Biome_Temp

Table_Rates_Temp <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>%
  group_by(Biome) %>% 
  summarise(
    Temp12_sd = sd(TempCC12, na.rm = T),
    Temp12 = mean(TempCC12, na.rm = T),
    Temp23_sd = sd(TempCC23, na.rm = T),
    Temp23 = mean(TempCC23, na.rm = T)
  )
Table_Rates_Temp  

##########################################
###############################

Table_Biome_Prcp <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>% 
  summarise(
    Prcp1_sd = sd(sumPrcp_p1, na.rm = T),
    Prcp1 = mean(sumPrcp_p1, na.rm = T),
    Prcp1max = max(sumPrcp_p1, na.rm = T),
    Prcp1min = min(sumPrcp_p1, na.rm = T),
    Prcp2_sd = sd(sumPrcp_p2, na.rm = T),
    Prcp2 = mean(sumPrcp_p2, na.rm = T),
    Prcp2max = max(sumPrcp_p2, na.rm = T),
    Prcp2min = min(sumPrcp_p2, na.rm = T),
    Prcp3_sd = sd(sumPrcp_p3, na.rm = T),
    Prcp3 = mean(sumPrcp_p3, na.rm = T),
    Prcp3max = max(sumPrcp_p3, na.rm = T),
    Prcp3min = min(sumPrcp_p3, na.rm = T),
    PrcpLP_sd = sd(sumPrcp_ref, na.rm = T),
    PrcpLP = mean(sumPrcp_ref, na.rm = T),
    PrcpLPmax = max(sumPrcp_ref, na.rm = T),
    PrcpLPmin = min(sumPrcp_ref, na.rm = T)
  )
Table_Biome_Prcp

Table_Rates_Prcp <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%   
  summarise(
    Prcp12_sd = sd(PrcpCC12, na.rm = T),
    Prcp12 = mean(PrcpCC12, na.rm = T),
    Prcp23_sd = sd(PrcpCC23, na.rm = T),
    Prcp23 = mean(PrcpCC23, na.rm = T)
  )
Table_Rates_Prcp

##########################################
###############################

Table_Biome_SPEImin <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%     
  summarise(
    SPEImin1_sd = sd(minSPEI_sp_1, na.rm = T),
    SPEImin1 = mean(minSPEI_sp_1, na.rm = T),
    SPEImin1max = max(minSPEI_sp_1, na.rm = T),
    SPEImin1min = min(minSPEI_sp_1, na.rm = T),
    SPEImin2_sd = sd(minSPEI_sp_2, na.rm = T),
    SPEImin2 = mean(minSPEI_sp_2, na.rm = T),
    SPEImin2max = max(minSPEI_sp_2, na.rm = T),
    SPEImin2min = min(minSPEI_sp_2, na.rm = T),
    SPEImin3_sd = sd(minSPEI_sp_3, na.rm = T),
    SPEImin3 = mean(minSPEI_sp_3, na.rm = T),
    SPEImin3max = max(minSPEI_sp_3, na.rm = T),
    SPEImin3min = min(minSPEI_sp_3, na.rm = T),
    SPEIminLP_sd = sd(minSPEI_sp_ref, na.rm = T),
    SPEIminLP = mean(minSPEI_sp_ref, na.rm = T),
    SPEIminLPmax = max(minSPEI_sp_ref, na.rm = T),
    SPEIminLPmin = min(minSPEI_sp_ref, na.rm = T),
  )
Table_Biome_SPEImin

Table_Rates_SPEImin <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%   
  summarise(
    SPEImin12_sd = sd(SPEIminCC12, na.rm = T),
    SPEImin12 = mean(SPEIminCC12, na.rm = T),
    SPEImin23_sd = sd(SPEIminCC23, na.rm = T),
    SPEImin23 = mean(SPEIminCC23, na.rm = T)
  )
Table_Rates_SPEImin


##########################################
###############################

Table_Biome_Hw <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%   
  summarise(
    Hw1_sd = sd(nEventsHeatwave_t2_p1, na.rm = T),
    Hw1 = mean(nEventsHeatwave_t2_p1, na.rm = T),
    Hw1max = max(nEventsHeatwave_t2_p1, na.rm = T),
    Hw1min = min(nEventsHeatwave_t2_p1, na.rm = T),
    Hw2_sd = sd(nEventsHeatwave_t2_p2, na.rm = T),
    Hw2 = mean(nEventsHeatwave_t2_p2, na.rm = T),
    Hw2max = max(nEventsHeatwave_t2_p2, na.rm = T),
    Hw2min = min(nEventsHeatwave_t2_p2, na.rm = T),
    Hw3_sd = sd(nEventsHeatwave_t2_p3, na.rm = T),
    Hw3 = mean(nEventsHeatwave_t2_p3, na.rm = T),
    Hw3max = max(nEventsHeatwave_t2_p3, na.rm = T),
    Hw3min = min(nEventsHeatwave_t2_p3, na.rm = T),
    HwLP_sd = sd(nEventsHeatwave_t_ref2, na.rm = T),
    HwLP = mean(nEventsHeatwave_t_ref2, na.rm = T),
    HwLPmax = max(nEventsHeatwave_t_ref2, na.rm = T),
    HwLPmin = min(nEventsHeatwave_t_ref2, na.rm = T),
  )
Table_Biome_Hw

Table_Rates_Hw <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>% 
  summarise(
    Hw12_sd = sd(HwCC12, na.rm = T),
    Hw12 = mean(HwCC12, na.rm = T),
    Hw23_sd = sd(HwCC23, na.rm = T),
    Hw23 = mean(HwCC23, na.rm = T)
  )
Table_Rates_Hw

##########################################
###############################

Table_Biome_BA <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome, FG1) %>%   
  summarise(
    ba1_sd = sd(ba_ha2, na.rm = T),
    ba1 = mean(ba_ha2, na.rm = T),
    ba1max = max(ba_ha2, na.rm = T),
    ba1min = min(ba_ha2, na.rm = T),
    ba2_sd = sd(ba_ha3, na.rm = T),
    ba2 = mean(ba_ha3, na.rm = T),
    ba2max = max(ba_ha3, na.rm = T),
    ba2min = min(ba_ha3, na.rm = T),
    ba3_sd = sd(ba_ha4, na.rm = T),
    ba3 = mean(ba_ha4, na.rm = T),
    ba3max = max(ba_ha4, na.rm = T),
    ba3min = min(ba_ha4, na.rm = T)
  )
Table_Biome_BA

##########################################
###############################

Table_Biome_BArates <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome, FG1) %>%   
  summarise(
    ba1_sd = sd(ratesAB32, na.rm = T),
    ba1 = mean(ratesAB32, na.rm = T),
    ba1max = max(ratesAB32, na.rm = T),
    ba1min = min(ratesAB32, na.rm = T),
    ba2_sd = sd(ratesAB43, na.rm = T),
    ba2 = mean(ratesAB43, na.rm = T),
    ba2max = max(ratesAB43, na.rm = T),
    ba2min = min(ratesAB43, na.rm = T)
  )
Table_Biome_BArates


##########################################
###############################

Table_Biome_percProduct <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome, FG1) %>%   
  summarise(
    ba1_sd = sd(rAB32rel, na.rm = T),
    ba1 = mean(rAB32rel, na.rm = T),
    ba1max = max(rAB32rel, na.rm = T),
    ba1min = min(rAB32rel),
    ba2_sd = sd(rAB43rel, na.rm = T),
    ba2 = mean(rAB43rel, na.rm = T),
    ba2max = max(rAB43rel, na.rm = T),
    ba2min = min(rAB43rel, na.rm = T)
  ) 
Table_Biome_percProduct


#################Test de wilcoxon

Mediterranean <- ClimaSPSW_new %>% 
  select(Biome, rAB32rel, rAB43rel) %>% 
  filter(Biome == "Mediterranean")

wilcox.test(Mediterranean$rAB32rel, Mediterranean$rAB43rel)

Temp_south <- ClimaSPSW_new %>% 
  select(Biome, rAB32rel, rAB43rel) %>% 
  filter(Biome == "Temp_south")

wilcox.test(Temp_south$rAB32rel, Temp_south$rAB43rel)

Temp_north <- ClimaSPSW_new %>% 
  select(Biome, rAB32rel, rAB43rel) %>% 
  filter(Biome == "Temp_north")

wilcox.test(Temp_north$rAB32rel, Temp_north$rAB43rel)

Boreal <- ClimaSPSW_new %>% 
  select(Biome, rAB32rel, rAB43rel) %>% 
  filter(Biome == "Boreal")

wilcox.test(Boreal$rAB32rel, Boreal$rAB43rel)

##########################################
###############################

Table_Biome_BA <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%   
  summarise(
    ba1_sd = sd(ba_ha2, na.rm = T),
    ba1 = mean(ba_ha2, na.rm = T),
    ba1max = max(ba_ha2, na.rm = T),
    ba1min = min(ba_ha2, na.rm = T),
    ba2_sd = sd(ba_ha3, na.rm = T),
    ba2 = mean(ba_ha3, na.rm = T),
    ba2max = max(ba_ha3, na.rm = T),
    ba2min = min(ba_ha3, na.rm = T),
    ba3_sd = sd(ba_ha4, na.rm = T),
    ba3 = mean(ba_ha4, na.rm = T),
    ba3max = max(ba_ha4, na.rm = T),
    ba3min = min(ba_ha4, na.rm = T)
  )
Table_Biome_BA

##########################################
###############################

Table_Biome_BArates <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%   
  summarise(
    ba1_sd = sd(ratesAB32, na.rm = T),
    ba1 = mean(ratesAB32, na.rm = T),
    ba1max = max(ratesAB32, na.rm = T),
    ba1min = min(ratesAB32),
    ba2_sd = sd(ratesAB43, na.rm = T),
    ba2 = mean(ratesAB43, na.rm = T),
    ba2max = max(ratesAB43, na.rm = T),
    ba2min = min(ratesAB43, na.rm = T)
  )
Table_Biome_BArates


##########################################
###############################

Table_Biome_percProduct <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%   
  summarise(
    ba1_sd = sd(rAB32rel, na.rm = T),
    ba1 = mean(rAB32rel, na.rm = T),
    ba1max = max(rAB32rel, na.rm = T),
    ba1min = min(rAB32rel),
    ba2_sd = sd(rAB43rel, na.rm = T),
    ba2 = mean(rAB43rel, na.rm = T),
    ba2max = max(rAB43rel, na.rm = T),
    ba2min = min(rAB43rel, na.rm = T)
  ) 
Table_Biome_percProduct

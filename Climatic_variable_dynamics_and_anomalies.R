#####CLIMATIC VARIABLES DYNAMICS AND ANOMALIES

####Vars representation 
####Temperature per perid and reference period

#####
Temp_123LP_B <- ClimaSPSW_new %>% 
  filter(FG1 %in% c("bldec", "bleve", "nleve")) %>% 
  #filter(Country=="SP") %>%
  mutate( 
    FG1 = str_replace(FG1, "bldec", "Broad-leved \n deciduous"),
    FG1 = str_replace(FG1, "bleve", "Broad-leaved \n evergreen"),
    FG1 = str_replace(FG1, "nleve", "Needle-leaved \n evergreen"),
    Biome = str_replace(Biome, "Temp_north", "Temperate \n north"),
    Biome = str_replace(Biome, "Temp_south", "Temperate \n south")
  ) %>% 
  rename(
    NFI1 = meanTemp_t1_p1,
    NFI2 = meanTemp_t1_p2,
    NFI3 = meanTemp_t1_p3,
    NFILP = meanTemp_t_ref1
  ) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate \n south", "Temperate \n north", "Boreal")) %>% 
  select(NFI1,NFI2,NFI3,NFILP, FG1, Biome) %>% 
  pivot_longer(cols = starts_with("NFI"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x=years, y=values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.1, 0.25, 0.5, 1.5)) +
  scale_fill_manual(values = c("#FFEB3B","#7CB342","#2E7D32","#2196F3")) +
  #geom_signif(comparisons = list(c(2,3),c(3,4),c(2,4)), y_position = c(1700, 1900, 2100), textsize = 4, na.rm = T,
  #            test = "wilcox.test", map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))+
  coord_cartesian(ylim= c(-3,25))+
  facet_wrap(~Biome, nrow = 1)+
  #stat_summary(fun = mean, geom = "point", shape=18, size = 3, color = "black")+
  labs(y = "", x = "")+ 
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 14),
    strip.text.x = element_text(size =17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none")

Temp_123LP_B


#Precipitation per perid and reference period
#####


Prcp_123LP_B <- ClimaSPSW_new %>% 
  filter(FG1 %in% c("bldec", "bleve", "nleve")) %>% 
  #filter(Country=="SP") %>%
  mutate( 
    FG1 = str_replace(FG1, "bldec", "Broad-leved \n deciduous"),
    FG1 = str_replace(FG1, "bleve", "Broad-leaved \n evergreen"),
    FG1 = str_replace(FG1, "nleve", "Needle-leaved \n evergreen"),
    Biome = str_replace(Biome, "Temp_north", "Temperate \n north"),
    Biome = str_replace(Biome, "Temp_south", "Temperate \n south")
  ) %>% 
  rename(
    NFI1 = sumPrcp_p1,
    NFI2 = sumPrcp_p2,
    NFI3 = sumPrcp_p3,
    NFILP = sumPrcp_ref
  ) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate \n south", "Temperate \n north", "Boreal")) %>% 
  select(NFI1,NFI2,NFI3,NFILP, FG1, Biome) %>% 
  pivot_longer(cols = starts_with("NFI"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x=years, y=values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.1, 0.25, 0.5, 1.5)) +
  scale_fill_manual(values = c("#FFEB3B","#7CB342","#2E7D32","#2196F3")) +
  #geom_signif(comparisons = list(c(2,3),c(3,4),c(2,4)), y_position = c(1700, 1900, 2100), textsize = 4, na.rm = T,
  #            test = "wilcox.test", map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))+
  coord_cartesian(ylim= c(100,1800))+
  facet_wrap(~ Biome, nrow = 1)+
  #stat_summary(fun = mean, geom = "point", shape=18, size = 3, color = "black")+
  labs(y = "", x = "")+ 
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 14),
    strip.text.x = element_text(size =17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none")

Prcp_123LP_B

#Drought per perid and reference period
#####

SPEImin_123LP_B <- ClimaSPSW_new %>% 
  filter(FG1 %in% c("bldec", "bleve", "nleve")) %>% 
  #filter(Country=="SP") %>%
  mutate( 
    FG1 = str_replace(FG1, "bldec", "Broad-leved \n deciduous"),
    FG1 = str_replace(FG1, "bleve", "Broad-leaved \n evergreen"),
    FG1 = str_replace(FG1, "nleve", "Needle-leaved \n evergreen"),
    Biome = str_replace(Biome, "Temp_north", "Temperate \n north"),
    Biome = str_replace(Biome, "Temp_south", "Temperate \n south")
  ) %>% 
  rename(
    NFI1 = minSPEI_sp_1,
    NFI2 = minSPEI_sp_2,
    NFI3 = minSPEI_sp_3,
    NFILP = minSPEI_sp_ref
  ) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate \n south", "Temperate \n north", "Boreal")) %>% 
  select(NFI1,NFI2,NFI3,NFILP, FG1, Biome) %>% 
  pivot_longer(cols = starts_with("NFI"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x=years, y=values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.1, 0.25, 0.5, 1.5)) +
  scale_fill_manual(values = c("#FFEB3B","#7CB342","#2E7D32","#2196F3")) +
  #geom_signif(comparisons = list(c(2,3),c(3,4),c(2,4)), y_position = c(1700, 1900, 2100), textsize = 4, na.rm = T,
  #            test = "wilcox.test", map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))+
  coord_cartesian(ylim= c(-2,1))+
  facet_wrap(~ Biome, nrow = 1)+
  #stat_summary(fun = mean, geom = "point", shape=18, size = 3, color = "black")+
  labs(y = "", x = "")+ 
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 14),
    strip.text.x = element_text(size =17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none")

SPEImin_123LP_B


#Heatwavesper perid and reference period
#####

Hw_123LP_B <- ClimaSPSW_new %>% 
  filter(FG1 %in% c("bldec", "bleve", "nleve")) %>% 
  #filter(Country=="SP") %>%
  mutate( 
    FG1 = str_replace(FG1, "bldec", "Broad-leved \n deciduous"),
    FG1 = str_replace(FG1, "bleve", "Broad-leaved \n evergreen"),
    FG1 = str_replace(FG1, "nleve", "Needle-leaved \n evergreen"),
    Biome = str_replace(Biome, "Temp_north", "Temperate \n north"),
    Biome = str_replace(Biome, "Temp_south", "Temperate \n south")
  ) %>% 
  rename(
    NFI1 = nEventsHeatwave_t2_p1,
    NFI2 = nEventsHeatwave_t2_p2,
    NFI3 = nEventsHeatwave_t2_p3,
    NFILP = nEventsHeatwave_t_ref2
  ) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate \n south", "Temperate \n north", "Boreal")) %>% 
  select(NFI1,NFI2,NFI3,NFILP, FG1, Biome) %>% 
  pivot_longer(cols = starts_with("NFI"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x=years, y=values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.1, 0.25, 0.5, 1.5)) +
  scale_fill_manual(values = c("#FFEB3B","#7CB342","#2E7D32","#2196F3")) +
  #geom_signif(comparisons = list(c(2,3),c(3,4),c(2,4)), y_position = c(1700, 1900, 2100), textsize = 4, na.rm = T,
  #            test = "wilcox.test", map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))+
  #coord_cartesian(ylim= c(0 , 1.9))+
  facet_wrap(~ Biome, nrow = 1)+
  #stat_summary(fun = mean, geom = "point", shape=18, size = 3, color = "black")+
  labs(y = "", x = "")+ 
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 14),
    strip.text.x = element_text(size =17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none")

Hw_123LP_B




###################################################ANOMALIES######################################
#Temperature changes between periods (Biome)
#####
Temp_12_23_B <- ClimaSPSW_new %>% 
  filter(FG1 %in% c("bldec", "bleve", "nleve")) %>% 
  #filter(Country=="SP") %>%
  mutate( 
    FG1 = str_replace(FG1, "bldec", "Broad-leved \n deciduous"),
    FG1 = str_replace(FG1, "bleve", "Broad-leaved \n evergreen"),
    FG1 = str_replace(FG1, "nleve", "Needle-leaved \n evergreen"),
    Biome = str_replace(Biome, "Temp_north", "Temperate \n north"),
    Biome = str_replace(Biome, "Temp_south", "Temperate \n south")
  ) %>% 
  rename(
    NFI12 = TempCC12,
    NFI23 = TempCC23) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate \n south", "Temperate \n north", "Boreal")) %>% 
  select(NFI12,NFI23, FG1, Biome) %>% 
  pivot_longer(cols = starts_with("NFI"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x=years, y=values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.4, 0.9)) +
  scale_fill_manual(values = c("#FFEB3B","#7CB342","#2E7D32","#2196F3")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_signif(comparisons = list(c(2,3),c(3,4),c(2,4)), y_position = c(1700, 1900, 2100), textsize = 4, na.rm = T,
  #            test = "wilcox.test", map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))+
  coord_cartesian(ylim= c(-0.5,2))+
  facet_wrap(~Biome, nrow = 1)+
  #stat_summary(fun = mean, geom = "point", shape=18, size = 3, color = "black")+
  labs(y = "", x = "")+ 
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 14),
    strip.text.x = element_text(size =17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none")

Temp_12_23_B


############################################################################

#Precipitation changes between periods (FG/Biome) 

#############
#Precipitation changes between periods (Biome)
#####
Prcp_12_23_B <- ClimaSPSW_new %>% 
  filter(FG1 %in% c("bldec", "bleve", "nleve")) %>% 
  #filter(Country=="SP") %>%
  mutate( 
    FG1 = str_replace(FG1, "bldec", "Broad-leved \n deciduous"),
    FG1 = str_replace(FG1, "bleve", "Broad-leaved \n evergreen"),
    FG1 = str_replace(FG1, "nleve", "Needle-leaved \n evergreen"),
    Biome = str_replace(Biome, "Temp_north", "Temperate \n north"),
    Biome = str_replace(Biome, "Temp_south", "Temperate \n south")
  ) %>% 
  rename(
    NFI12 = PrcpCC12,
    NFI23 = PrcpCC23) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate \n south", "Temperate \n north", "Boreal")) %>% 
  select(NFI12,NFI23, FG1, Biome) %>% 
  filter(NFI12> -500) %>%
  filter(NFI23> -500) %>% 
  pivot_longer(cols = starts_with("NFI"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x=years, y=values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.4, 0.9)) +
  scale_fill_manual(values = c("#FFEB3B","#7CB342","#2E7D32","#2196F3")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_signif(comparisons = list(c(2,3),c(3,4),c(2,4)), y_position = c(1700, 1900, 2100), textsize = 4, na.rm = T,
  #            test = "wilcox.test", map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))+
  #coord_cartesian(ylim= c(-200,200))+
  facet_wrap(~Biome, nrow = 1)+
  #stat_summary(fun = mean, geom = "point", shape=18, size = 3, color = "black")+
  labs(y = "", x = "")+ 
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 14),
    strip.text.x = element_text(size =17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none")

Prcp_12_23_B 
############################################################################

##SPEImin changes between periods (FG/Biome) 


#############
#SPEImin changes between periods (Biome)
#####
SPEImin_12_23_B <- ClimaSPSW_new %>% 
  filter(FG1 %in% c("bldec", "bleve", "nleve")) %>% 
  #filter(Country=="SP") %>%
  mutate( 
    FG1 = str_replace(FG1, "bldec", "Broad-leved \n deciduous"),
    FG1 = str_replace(FG1, "bleve", "Broad-leaved \n evergreen"),
    FG1 = str_replace(FG1, "nleve", "Needle-leaved \n evergreen"),
    Biome = str_replace(Biome, "Temp_north", "Temperate \n north"),
    Biome = str_replace(Biome, "Temp_south", "Temperate \n south")
  ) %>% 
  rename(
    NFI12 = SPEIminCC12,
    NFI23 = SPEIminCC23) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate \n south", "Temperate \n north", "Boreal")) %>% 
  select(NFI12,NFI23, FG1, Biome) %>% 
  pivot_longer(cols = starts_with("NFI"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x=years, y=values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.4, 1)) +
  scale_fill_manual(values = c("#FFEB3B","#7CB342","#2E7D32","#2196F3")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_signif(comparisons = list(c(2,3),c(3,4),c(2,4)), y_position = c(1700, 1900, 2100), textsize = 4, na.rm = T,
  #            test = "wilcox.test", map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))+
  #coord_cartesian(ylim= c(-0.5,2.2))+
  facet_wrap(~Biome, nrow = 1)+
  #stat_summary(fun = mean, geom = "point", shape=18, size = 3, color = "black")+
  labs(y = "", x = "")+ 
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 14),
    strip.text.x = element_text(size =17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none")

SPEImin_12_23_B 



############################################################################

#############
#Heatwaves changes between periods (Biome)
#####
Hw_12_23_B <- ClimaSPSW_new %>% 
  filter(FG1 %in% c("bldec", "bleve", "nleve")) %>% 
  #filter(Country=="SP") %>%
  mutate( 
    FG1 = str_replace(FG1, "bldec", "Broad-leved \n deciduous"),
    FG1 = str_replace(FG1, "bleve", "Broad-leaved \n evergreen"),
    FG1 = str_replace(FG1, "nleve", "Needle-leaved \n evergreen"),
    Biome = str_replace(Biome, "Temp_north", "Temperate \n north"),
    Biome = str_replace(Biome, "Temp_south", "Temperate \n south")
  ) %>% 
  rename(
    NFI12 = HwCC12,
    NFI23 = HwCC23) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate \n south", "Temperate \n north", "Boreal")) %>% 
  select(NFI12,NFI23, FG1, Biome) %>% 
  pivot_longer(cols = starts_with("NFI"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x=years, y=values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.4, 0.9)) +
  scale_fill_manual(values = c("#FFEB3B","#7CB342","#2E7D32","#2196F3")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_signif(comparisons = list(c(2,3),c(3,4),c(2,4)), y_position = c(1700, 1900, 2100), textsize = 4, na.rm = T,
  #            test = "wilcox.test", map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))+
  #coord_cartesian(ylim= c(-0.35,0.75))+
  facet_wrap(~Biome, nrow = 1)+
  #stat_summary(fun = mean, geom = "point", shape=18, size = 3, color = "black")+
  labs(y = "", x = "")+ 
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 14),
    strip.text.x = element_text(size =17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none")

Hw_12_23_B
#####BASAL AREA DYNAMICS AND PRODUCTIVITY
library(ggsignif)
##################################BASAL AREA DYNAMICS
BA_evol <- ClimaSPSW_new %>% 
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
    NFI1 = ba_ha2,
    NFI2 = ba_ha3,
    NFI3 = ba_ha4
  ) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate south", "Temperate north", "Boreal")) %>% 
  select(NFI1, NFI2, NFI3,  FG1, Biome) %>% 
  pivot_longer(cols = starts_with("NFI"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x=years, y=values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.2, 0.4, 0.7)) +
  scale_fill_manual(values = c("#FFEB3B","#7CB342","#2E7D32","#2196F3")) +
  #geom_signif(comparisons = list(c(1,2),c(2,3),c(1,3)), y_position = c(63, 65, 67), textsize = 4, na.rm = T,
  #            test = "wilcox.test", map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))+
  coord_cartesian(ylim = c(0,70))+
  facet_wrap(~Biome, nrow = 1)+
  #stat_summary(fun = mean, geom = "point", shape=18, size = 3, color = "black")+
  labs(y = "Stand basal area (m2/ha-1)", x = "")+ 
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text = element_text(size = 22, family="TT Arial"),
    strip.text.x = element_text(size =22, family="TT Arial"),
    strip.text.y = element_text(size = 22, family="TT Arial"),
    axis.title = element_text(size = 20, family="TT Arial"),
    legend.position = "none")

BA_evol 

########################BASAL AREA ABSOLUTE RATES 

ratesBA_evol <- ClimaSPSW_new %>% 
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
    NFI12 = ratesAB32,
    NFI23 = ratesAB43
  ) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate \n south", "Temperate \n north", "Boreal")) %>% 
  select(NFI12, NFI23,  FG1, Biome) %>% 
  pivot_longer(cols = starts_with("NFI"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x=years, y=values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.4, 0.7)) +
  scale_fill_manual(values = c("#FFEB3B","#7CB342","#2E7D32","#2196F3")) +
  #geom_signif(comparisons = list(c(1,2)), y_position = c(10), textsize = 4, na.rm = T,
  #            test = "wilcox.test", map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))+
  coord_cartesian(ylim = c(-1.5,2))+
  facet_wrap(~Biome, nrow = 1)+
  #stat_summary(fun = mean, geom = "point", shape=18, size = 3, color = "black")+
  labs(y = "Absolute basal area change (m2 ha-1)", x = "")+ 
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text = element_text(size = 22, family="TT Arial"),
    strip.text.x = element_text(size =22, family="TT Arial"),
    strip.text.y = element_text(size = 22, family="TT Arial"),
    axis.title = element_text(size = 20, family="TT Arial"),
    legend.position = "none")

ratesBA_evol

########################BASAL AREA RELATIVE RATES
ratesBA_evol <- ClimaSPSW_new %>% 
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
    NFI12 = rAB32rel,
    NFI23 = rAB43rel
  ) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate \n south", "Temperate \n north", "Boreal")) %>% 
  select(NFI12, NFI23,  FG1, Biome) %>% 
  pivot_longer(cols = starts_with("NFI"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x=years, y=values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.4, 0.7)) +
  scale_fill_manual(values = c("#FFEB3B","#7CB342","#2E7D32","#2196F3")) +
  #geom_signif(comparisons = list(c(1,2)), y_position = c(10), textsize = 4, na.rm = T,
  #            test = "wilcox.test", map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))+
  coord_cartesian(ylim = c(-10,20))+
  facet_wrap(~Biome, nrow = 1)+
  #stat_summary(fun = mean, geom = "point", shape=18, size = 3, color = "black")+
  labs(y = "Relative basal area change (% ha-1)", x = "")+ 
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text = element_text(size = 22, family="TT Arial"),
    strip.text.x = element_text(size =22, family="TT Arial"),
    strip.text.y = element_text(size = 22, family="TT Arial"),
    axis.title = element_text(size = 20, family="TT Arial"),
    legend.position = "none")

ratesBA_evol



####################NFI YEARS

Boxplot_year <- ClimaSPSW_new %>% 
  select(Country, year2, year3, year4) %>% 
  rename(
    NFI1 = year2,
    NFI2 = year3,
    NFI3 = year4) %>%
  mutate(
    Country = as.factor(Country)) %>%
  pivot_longer(cols = starts_with("NFI"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x=Country, y=values, fill = years)) +
  geom_boxplot(aes(fill = Country, alpha = factor(years))) +
  scale_alpha_manual(values = c(0.4, 0.6, 0.9)) +
  scale_fill_manual(values = c("#FFEB3B","#7CB342","#2E7D32","#2196F3")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_signif(comparisons = list(c(2,3),c(3,4),c(2,4)), y_position = c(1700, 1900, 2100), textsize = 4, na.rm = T,
  #            test = "wilcox.test", map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))+
  coord_cartesian(ylim= c(1985,2025))+
  facet_wrap(~Country)+
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

Boxplot_year



Boxplot_year <- ClimaSPSW_new %>% 
  select(Country, year2, year3, year4) %>% 
  rename(
    NFI1 = year2,
    NFI2 = year3,
    NFI3 = year4) %>%
  mutate(
    Country = as.factor(Country)) %>%
  pivot_longer(cols = starts_with("NFI"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x = Country, y = values, fill = years)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = rep("black", 3)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_cartesian(ylim = c(1985,2025)) +
  facet_wrap(~Country, scales = "free") +
  labs(y = "", x = "") + 
  theme_minimal() +
  theme(
    axis.line = element_line(size = 1, color = "black"),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none"
  )

Boxplot_year
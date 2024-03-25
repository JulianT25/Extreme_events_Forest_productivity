library(dominanceanalysis)
library(tidyverse)
library(patchwork)
library(caret)
library(car)

###Preparamos y escalamos variables para cada modelo
Mediterranean <- ClimaSPSW_new %>% 
  filter(Biome == "Mediterranean") %>%
  #filter(FG1 == "nleve") %>% 
  rename(
    BA1 = ba_ha2,
    BA2 = ba_ha3,
    BA3 = ba_ha4,
    FunctGroup = FG1,
    ratesAB12 = ratesAB32,
    ratesAB23 = ratesAB43,
    Tmean_LP = Temp_LP,
    Prcp_LP = Prcp_LP,
    SPEImin_LP = SPEImin_LP,
    Heatwaves_LP = Hw_LP,
    TempCC12 = TempCC12,
    TempCC23 = TempCC23,
    PrcpCC12 = PrcpCC12,
    PrcpCC23 = PrcpCC23,
    SPEIminCC12 = SPEIminCC12,
    SPEIminCC23 = SPEIminCC23,
    HeatwavesCC12 = HwCC12,
    HeatwavesCC23 = HwCC23
  )%>% 
  select(BA1, BA2, BA3,
         ratesAB12, ratesAB23,
         Tmean_LP, Prcp_LP, SPEImin_LP, Heatwaves_LP,
         TempCC12, TempCC23, 
         PrcpCC12, PrcpCC23, 
         SPEIminCC12, SPEIminCC23 , 
         HeatwavesCC12, HeatwavesCC23) %>% 
  mutate(across(
    everything(),
    scale,
    .names = "scaled_{.col}"
  )) %>% 
  mutate(across(
    c(-ratesAB12, -ratesAB23),
    as.numeric
  ))


###############################
##########################
##############################
Temp_South <- ClimaSPSW_new %>% 
  filter(Biome == "Temp_south") %>%
  #filter(FG1 == "nleve") %>% 
  rename(
    BA1 = ba_ha2,
    BA2 = ba_ha3,
    BA3 = ba_ha4,
    FunctGroup = FG1,
    ratesAB12 = ratesAB32,
    ratesAB23 = ratesAB43,
    Tmean_LP = Temp_LP,
    Prcp_LP = Prcp_LP,
    SPEImin_LP = SPEImin_LP,
    Heatwaves_LP = Hw_LP,
    TempCC12 = TempCC12,
    TempCC23 = TempCC23,
    PrcpCC12 = PrcpCC12,
    PrcpCC23 = PrcpCC23,
    SPEIminCC12 = SPEIminCC12,
    SPEIminCC23 = SPEIminCC23,
    HeatwavesCC12 = HwCC12,
    HeatwavesCC23 = HwCC23
  )%>% 
  select(BA1, BA2, BA3,
         ratesAB12, ratesAB23,
         Tmean_LP, Prcp_LP, SPEImin_LP, Heatwaves_LP,
         TempCC12, TempCC23, 
         PrcpCC12, PrcpCC23, 
         SPEIminCC12, SPEIminCC23 , 
         HeatwavesCC12, HeatwavesCC23) %>% 
  mutate(across(
    everything(),
    scale,
    .names = "scaled_{.col}"
  )) %>% 
  mutate(across(
    c(-ratesAB12, -ratesAB23),
    as.numeric
  ))


####################
###########
##################

Temp_North <- ClimaSPSW_new %>% 
  filter(Biome == "Temp_north") %>%
  #filter(FG1 == "nleve") %>% 
  rename(
    BA1 = ba_ha2,
    BA2 = ba_ha3,
    BA3 = ba_ha4,
    FunctGroup = FG1,
    ratesAB12 = ratesAB32,
    ratesAB23 = ratesAB43,
    Tmean_LP = Temp_LP,
    Prcp_LP = Prcp_LP,
    SPEImin_LP = SPEImin_LP,
    Heatwaves_LP = Hw_LP,
    TempCC12 = TempCC12,
    TempCC23 = TempCC23,
    PrcpCC12 = PrcpCC12,
    PrcpCC23 = PrcpCC23,
    SPEIminCC12 = SPEIminCC12,
    SPEIminCC23 = SPEIminCC23,
    HeatwavesCC12 = HwCC12,
    HeatwavesCC23 = HwCC23
  )%>% 
  select(BA1, BA2, BA3,
         ratesAB12, ratesAB23,
         Tmean_LP, Prcp_LP, SPEImin_LP, Heatwaves_LP,
         TempCC12, TempCC23, 
         PrcpCC12, PrcpCC23, 
         SPEIminCC12, SPEIminCC23 , 
         HeatwavesCC12, HeatwavesCC23) %>% 
  mutate(across(
    everything(),
    scale,
    .names = "scaled_{.col}"
  )) %>% 
  mutate(across(
    c(-ratesAB12, -ratesAB23),
    as.numeric
  ))

############################
######################3
##########################

Boreal <- ClimaSPSW_new %>% 
  filter(Biome == "Boreal") %>%
  #filter(FG1 == "nleve") %>% 
  rename(
    BA1 = ba_ha2,
    BA2 = ba_ha3,
    BA3 = ba_ha4,
    FunctGroup = FG1,
    ratesAB12 = ratesAB32,
    ratesAB23 = ratesAB43,
    Tmean_LP = Temp_LP,
    Prcp_LP = Prcp_LP,
    SPEImin_LP = SPEImin_LP,
    Heatwaves_LP = Hw_LP,
    TempCC12 = TempCC12,
    TempCC23 = TempCC23,
    PrcpCC12 = PrcpCC12,
    PrcpCC23 = PrcpCC23,
    SPEIminCC12 = SPEIminCC12,
    SPEIminCC23 = SPEIminCC23,
    HeatwavesCC12 = HwCC12,
    HeatwavesCC23 = HwCC23
  )%>% 
  select(BA1, BA2, BA3,
         ratesAB12, ratesAB23,
         Tmean_LP, Prcp_LP, SPEImin_LP, Heatwaves_LP,
         TempCC12, TempCC23, 
         PrcpCC12, PrcpCC23, 
         SPEIminCC12, SPEIminCC23 , 
         HeatwavesCC12, HeatwavesCC23) %>% 
  mutate(across(
    everything(),
    scale,
    .names = "scaled_{.col}"
  )) %>% 
  mutate(across(
    c(-ratesAB12, -ratesAB23),
    as.numeric
  ))



####Comenzamos a correr los modelos 

####MEDITERRANEAN  0.5(-Prcp, -HW)  0.6(-Prcp) 0.65(-Prcp)
Mediterranean <- as.data.frame(Mediterranean)
Mediterranean$ratesAB12 <- Mediterranean$ratesAB12 + 1 - min(Mediterranean$ratesAB12)

Mediterranean$ratesAB23 <- Mediterranean$ratesAB23 + 1 - min(Mediterranean$ratesAB23)


Model1 <- glm(formula = ratesAB12 ~ 
                scaled_BA1 + 
                scaled_Tmean_LP +
                scaled_TempCC12 +
                scaled_Prcp_LP + 
                scaled_PrcpCC12 +
                scaled_SPEImin_LP +
                scaled_SPEIminCC12 +
                scaled_Heatwaves_LP +
                scaled_HeatwavesCC12,
              family = Gamma(link = "log"),
              data = Mediterranean)
summary(Model1)
varImp(Model1)


Model1b <- glm(formula = ratesAB23 ~ 
                 offset(0.0062244*scaled_BA2) +
                 offset(-0.0074155*scaled_Tmean_LP) +
                 offset(0.0061258*scaled_TempCC23) +
                 offset(0.0061452*scaled_Prcp_LP) +
                 offset(-0.0085332*scaled_PrcpCC23) +
                 offset(-0.0063545*scaled_SPEImin_LP) +
                 offset(-0.0033107*scaled_SPEIminCC23) +
                 offset(-0.0042898*scaled_Heatwaves_LP) +
                 offset(-0.0053576*scaled_HeatwavesCC23) +
                 scaled_TempCC23 + scaled_PrcpCC23 + 
                 scaled_SPEIminCC23 + scaled_HeatwavesCC23, 
               family = Gamma(link = "log"), 
               data = Mediterranean)
summary(Model1b)
varImp(Model1b)

####TEMPERATE SOUTH 0.5(-Prcp, -Temp)  0.6(-Prcp) 0.65(-Prcp)
TempSouth <- as.data.frame(Temp_South)
TempSouth$ratesAB12 <- TempSouth$ratesAB12 +1 -min(TempSouth$ratesAB12)
TempSouth$ratesAB23 <- TempSouth$ratesAB23 +1 -min(TempSouth$ratesAB23)

Model2 <- glm(formula = ratesAB12 ~ 
                scaled_BA1 + 
                scaled_Tmean_LP + 
                scaled_TempCC12 +
                scaled_SPEImin_LP + 
                scaled_SPEIminCC12 +
                scaled_Heatwaves_LP+
                scaled_HeatwavesCC12,
              family = Gamma(link = "log"),
              data = TempSouth)
summary(Model2)
varImp(Model2)


Model2b <- glm(formula = ratesAB23 ~ 
                 offset(-0.0083852*scaled_BA2) +
                 offset(-0.0004020*scaled_Tmean_LP) +
                 offset(-0.0088025*scaled_TempCC23) +
                 offset(-0.0297388*scaled_SPEImin_LP) +
                 offset(-0.0339428*scaled_SPEIminCC23) +
                 offset(-0.0048592*scaled_Heatwaves_LP) +
                 offset(0.0009552*scaled_HeatwavesCC23) +
                 scaled_TempCC23 + scaled_SPEIminCC23 + scaled_HeatwavesCC23,
               family = Gamma(link = "log"), 
               data = TempSouth)
summary(Model2b)
varImp(Model2b)



####TEMPERATE NORTH 0.5(-Prcp)  0.6(No Extraction) 0.65(No Extraction)
TempNorth <- as.data.frame(Temp_North)

TempNorth$ratesAB12 <- TempNorth$ratesAB12 +1 -min(TempNorth$ratesAB12)
TempNorth$ratesAB23 <- TempNorth$ratesAB23 +1 -min(TempNorth$ratesAB23)


Model3 <- glm(formula = ratesAB12 ~ 
                scaled_BA1 + 
                scaled_Tmean_LP +
                scaled_TempCC12 +
                scaled_SPEImin_LP +
                scaled_SPEIminCC12 +
                scaled_Heatwaves_LP +
                scaled_HeatwavesCC12,
              family = Gamma(link = "log"),
              data = TempNorth)
summary(Model3)
varImp(Model3)


Model3b <- glm(formula = ratesAB23 ~ 
                 offset(-0.0067859*scaled_BA2) +
                 offset(-0.0004030*scaled_Tmean_LP) +
                 offset(0.0003244*scaled_TempCC23) +
                 offset(0.0046074*scaled_SPEImin_LP) +
                 offset(-0.0009286*scaled_SPEIminCC23) +
                 offset(-0.0013716*scaled_Heatwaves_LP) +
                 offset(-0.0065655*scaled_HeatwavesCC23) +
                 scaled_TempCC23 + scaled_SPEIminCC23 + scaled_HeatwavesCC23,
               family = Gamma(link = "log"), 
               data = TempNorth)
summary(Model3b)
varImp(Model3b)







plot(ClimaSPSW_new$ratesAB32~ClimaSPSW_new$TempCC12)


####BOREAL 0.5(-Prcp, -Temp)  0.6(-Temp) 0.65(Heatwaves)
Boreal <- as.data.frame(Boreal)

Boreal$ratesAB12 <- Boreal$ratesAB12 +1 -min(Boreal$ratesAB12)
Boreal$ratesAB23 <- Boreal$ratesAB23 +1 -min(Boreal$ratesAB23)

Model4 <- glm(formula = ratesAB12 ~ 
                scaled_BA1 + 
                scaled_Tmean_LP +
                scaled_TempCC12 +
                scaled_Prcp_LP + 
                scaled_PrcpCC12 +
                scaled_SPEImin_LP +
                scaled_SPEIminCC12 + 
                scaled_Heatwaves_LP +
                scaled_HeatwavesCC12,
              family = Gamma(link = "log"),
              data = Boreal)
summary(Model4)
varImp(Model4)



Model4b <- glm(formula = ratesAB23 ~ 
                 offset(-0.0027311*scaled_BA2) +
                 offset(0.0193855*scaled_Tmean_LP) +
                 offset(0.0037215*scaled_TempCC23) +
                 offset(-0.0072344*scaled_Prcp_LP) +
                 offset(0.0042054*scaled_PrcpCC23) +
                 offset(0.0017291*scaled_SPEImin_LP) +
                 offset(-0.0023532*scaled_SPEIminCC23) +
                 offset(0.0021949*scaled_Heatwaves_LP) +
                 offset(0.0001654*scaled_HeatwavesCC23) +
                 scaled_TempCC23 + scaled_PrcpCC23 + 
                 scaled_SPEIminCC23 + scaled_HeatwavesCC23,
               family = Gamma(link = "log"), 
               data = Boreal)
summary(Model4b)
varImp(Model4b)




################################
library(broom)
library(broom.mixed)
tidy_model <- function(model){
  tidy(model) |>
    filter(term != "(Intercept)") |>
    mutate(lci = estimate - 1.96*std.error,
           uci = estimate + 1.96*std.error) |>
    dplyr::select(var = term, coef = estimate, lci, uci)
}
models <- list(
  Model1,Model1b,
  Model2,Model2b,
  Model3,Model3b,
  Model4,Model4b
  
)
names(models) <- c("Mediterranean12", "Mediterranean23",
                   "TempSouth12", "TempSouth23",
                   "TempNorth12", "TempNorth23",
                   "Boreal12", "Boreal23")


models_tidy <- map(models, ~ tidy_model(.x))
arg2_dat_name <- list(dat = models_tidy, data_name = names(models))
add_name <- function(dat, data_name){
  dat |>
    mutate(
      data = data_name
    )
}
models_tidy_name <- arg2_dat_name |>
  pmap_df(add_name)

models_tidy_name$var <- gsub('CC', '_', models_tidy_name$var)
models_tidy_name$var <- gsub('A', 'A_', models_tidy_name$var)

models_tidy_name_s <- models_tidy_name |>
  separate(col = var, into = c('variable', 'period'), sep = '_') %>% 
  separate(col = data, into = c("region", NA), sep = "_") %>%
  mutate(
    variable = paste0(variable, "_", period)
  )
write.csv(models_tidy_name, "C:\\Users\\julia\\Desktop\\models_FINAL_III_RatesNoScales.csv")

library(tidyverse)
Variables <- models_FINAL_III_RatesNoScales |>
  filter(
    var == "TempCC12" |
      var == "PrcpCC12" |
      var == "SPEIminCC12" |
      var == "HeatwavesCC12" |
      var == "TempCC23" |
      var == "PrcpCC23" |
      var == "SPEIminCC23" |
      var == "HeatwavesCC23"|
      var == "Temp_overall"|
      var == "Prcp_overall"|
      var == "SPEImin_overall"|
      var == "Heatwaves_overall"
  ) |>
  mutate(
    var = recode_factor(
      var,
      "TempCC12" = "Temperature 12",
      "PrcpCC12" = "Precipitation 12",
      "SPEIminCC12" = "SPEImin 12",
      "HeatwavesCC12" = "Heatwaves 12",
      "TempCC23" = "Temperature 23",
      "PrcpCC23" = "Precipitation 23",
      "SPEIminCC23" = "SPEImin 23",
      "HeatwavesCC23" = "Heatwaves 23",
      "Temp_overall" = "Temperature overall",
      "Prcp_overall" = "Precipitation overall",
      "SPEImin_overall" = "SPEI overall",
      "Heatwaves_overall" = "Heatwaves overall"
    )) |> 
      mutate( 
        #region = str_replace(region, "TempNorth", "Temperate North"),
        #region = str_replace(region, "TempSouth", "Temperate South"),
        #Clim_Event = str_replace(Clim_Event, "Heatwave", "Heatwaves"),
        Clim_Event = str_replace(Clim_Event, "Precipitation", "Precipitation anomaly"),
        Clim_Event = str_replace(Clim_Event, "Temperatures", "Temperature anomaly"),
        Clim_Event = str_replace(Clim_Event, "Droughts", "Droughts"),
        Clim_Event = str_replace(Clim_Event, "Heatwaves", "Heatwaves")
      ) %>% 
      mutate(
        region = as.factor(region),
        period = as.factor(period),
        Clim_Event = as.factor(Clim_Event)) %>%
      mutate(region = fct_relevel(region,
                                  "Mediterranean", 
                                  "Temp_south",
                                  "Temp_north",
                                  "Boreal")) %>%
      mutate(Clim_Event = fct_relevel(Clim_Event, 
                                      "Temperature anomaly",
                                      "Precipitation anomaly",
                                      "Droughts", 
                                      "Heatwaves" 
      )) %>%
      mutate(
        coef = as.numeric(coef),
        period = as.factor(period)) %>% 
      mutate(period = fct_relevel(period,
                                  "23",
                                  "12",
                                  "Overall"
      )) %>%
      ggplot(aes(y = region, x = coef_II, xmin = lci_II, xmax = uci_II,color = region:period)) + ##Para extraer los intervalos de confianza para el periodo overall he seguido esta reseña: https://stats.stackexchange.com/questions/223924/how-to-add-up-partial-confidence-intervals-to-create-a-total-confidence-interval
      geom_pointrange(aes (fill = region, alpha = factor(period)), position = position_dodge2(.4),
                      size = 1) +
      #geom_point(position = position_dodge2(.0), shape = 1, size = 3.5, colour = "black") +
      scale_alpha_manual(values = c(1,1,1)) + 
      geom_vline(xintercept = 0, linetype = "dashed") + ylab("") +
      xlab(expression("Coefficients")) +
      scale_color_manual(values = c("#FDD835","#FFF59D","#212121",
                                    "#7CB342","#C5E1A5","#212121",
                                    "#2E7D32","#A5D6A7","#212121",
                                    "#2196F3","#BBDEFB","#212121"),
                         guide = guide_legend(reverse = T)) +
      theme_minimal() +
      theme(
        panel.spacing = unit(1.5, "cm"),
        axis.line.y.left = element_line(size = 1, color = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(size = 1, color = "black"),
        axis.line.x.bottom = element_line(size = 1, color = "black"),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 15, face = "bold", color = "black"),
        legend.title = element_text(),
        legend.position = "none",
        strip.text = element_text(size=15))
    
    Variables
    
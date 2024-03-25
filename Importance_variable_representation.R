####IMPORTANCE VARIABLE REPRESENTATION

library(forcats)
VI_MedP1 <- VarImp %>% 
  filter(Biome == "Mediterranean") %>%
  filter(P1 > 0) %>% 
  arrange(P1) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=P1)) +
  geom_bar(stat = "identity", color = "black", fill =  "#FFEB3B", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Mediterranean)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()
VI_MedP1

VI_MedP2 <- VarImp %>% 
  filter(Biome == "Mediterranean") %>%
  filter(P2 > 0) %>% 
  arrange(P2) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=P2)) +
  geom_bar(stat = "identity", color = "black", fill =  "#FFEB3B", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Mediterranean)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()
VI_MedP2

VI_TSP1 <- VarImp %>% 
  filter(Biome == "Temp_South") %>%
  filter(P1 > 0) %>% 
  arrange(P1) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=P1)) +
  geom_bar(stat = "identity", color = "black", fill =  "#7CB342", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Temp_South)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()



VI_TSP2 <- VarImp %>% 
  filter(Biome == "Temp_South") %>%
  filter(P2 > 0) %>% 
  arrange(P2) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=P2)) +
  geom_bar(stat = "identity", color = "black", fill =  "#7CB342", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Temp_South)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()

VI_TNP1 <- VarImp %>% 
  filter(Biome == "Temp_North") %>%
  filter(P1 > 0) %>% 
  arrange(P1) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=P1)) +
  geom_bar(stat = "identity", color = "black", fill =  "#2E7D32", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Temp_North)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()

VI_TNP2 <- VarImp %>% 
  filter(Biome == "Temp_North") %>%
  filter(P2 > 0) %>% 
  arrange(P2) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=P2)) +
  geom_bar(stat = "identity", color = "black", fill =  "#2E7D32", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Temp_North)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()

VI_BorP1 <- VarImp %>% 
  filter(Biome == "Boreal") %>%
  filter(P1 > 0) %>% 
  arrange(P1) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=P1)) +
  geom_bar(stat = "identity", color = "black", fill =  "#2196F3", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Boreal)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()

VI_BorP2 <- VarImp %>% 
  filter(Biome == "Boreal") %>%
  filter(P2 > 0) %>% 
  arrange(P2) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=P2)) +
  geom_bar(stat = "identity", color = "black", fill =  "#2196F3", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Boreal)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()

#############################################
#################################
#############################################
VI_MedX_P1 <- VarImp %>% 
  filter(Biome == "Mediterranean") %>%
  filter(X_P1 > 0) %>% 
  arrange(X_P1) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=X_P1)) +
  geom_bar(stat = "identity", color = "black", fill =  "#FFEB3B", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Mediterranean)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()

VI_MedX_P2 <- VarImp %>% 
  filter(Biome == "Mediterranean") %>%
  filter(X_P2 > 0) %>% 
  arrange(X_P2) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=X_P2)) +
  geom_bar(stat = "identity", color = "black", fill =  "#FFEB3B", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Mediterranean)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()


VI_TSX_P1 <- VarImp %>% 
  filter(Biome == "Temp_South") %>%
  filter(X_P1 > 0) %>% 
  arrange(X_P1) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=X_P1)) +
  geom_bar(stat = "identity", color = "black", fill =  "#7CB342", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Temp_South)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()

VI_TSX_P2 <- VarImp %>% 
  filter(Biome == "Temp_South") %>%
  filter(X_P2 > 0) %>% 
  arrange(X_P2) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=X_P2)) +
  geom_bar(stat = "identity", color = "black", fill =  "#7CB342", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Temp_South)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()

VI_TNX_P1 <- VarImp %>% 
  filter(Biome == "Temp_North") %>%
  filter(X_P1 > 0) %>% 
  arrange(X_P1) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=X_P1)) +
  geom_bar(stat = "identity", color = "black", fill =  "#2E7D32", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Temp_North)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()

VI_TNX_P2 <- VarImp %>% 
  filter(Biome == "Temp_North") %>%
  filter(X_P2 > 0) %>% 
  arrange(X_P2) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=X_P2)) +
  geom_bar(stat = "identity", color = "black", fill =  "#2E7D32", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Temp_North)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()

VI_BoX_P1 <- VarImp %>% 
  filter(Biome == "Boreal") %>%
  filter(X_P1 > 0) %>% 
  arrange(X_P1) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=X_P1)) +
  geom_bar(stat = "identity", color = "black", fill =  "#2196F3", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Boreal)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()

VI_BorX_P2 <- VarImp %>% 
  filter(Biome == "Boreal") %>%
  filter(X_P2 > 0) %>% 
  arrange(X_P2) %>%                     
  mutate(Variable=factor(Variable, levels=Variable)) %>% 
  ggplot( aes(x=Variable, y=X_P2)) +
  geom_bar(stat = "identity", color = "black", fill =  "#2196F3", alpha = 0.7) +
  #geom_text(aes(label = round(Overall, digits = 2)), hjust=3, color="black", size=3.5, position =  position_dodge(width = 1))+
  xlab('Variable (Boreal)')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()
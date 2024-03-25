####Model residual representation

### 
library(patchwork)

#create histogram of residuals
Mod_1res <- ggplot(Model1, aes(x = Model1$residuals)) +
  geom_histogram(aes(y = after_stat(density)),bins = 70, fill = 'steelblue', color = 'black') +
  #geom_density()+
  geom_vline(xintercept = 0, linetype = "dashed", size = 1.1) +
  coord_cartesian(xlim = c(-0.5,0.5))+
  stat_function(fun = dnorm, args = list(mean = mean(Model1$residuals), sd = sd(Model1$residuals)))+
  labs(title = 'Mediterranean', x = 'Residuals', y = 'Density') +
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    strip.text.x = element_text(size =17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none")

Mod_1res


#create histogram of residuals
Mod_2res <- ggplot(Model2, aes(x = Model2$residuals)) +
  geom_histogram(aes(y = after_stat(density)),bins = 70, fill = 'steelblue', color = 'black') +
  #geom_density()+
  geom_vline(xintercept = 0, linetype = "dashed", size = 1.1) +
  coord_cartesian(xlim = c(-0.5,0.5))+
  stat_function(fun = dnorm, args = list(mean = mean(Model2$residuals), sd = sd(Model2$residuals)))+
  labs(title = 'Mediterranean', x = 'Residuals', y = 'Density') +
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    strip.text.x = element_text(size =17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none")

Mod_2res


#create histogram of residuals
Mod_3res <- ggplot(Model3, aes(x = Model3$residuals)) +
  geom_histogram(aes(y = after_stat(density)),bins = 70, fill = 'steelblue', color = 'black') +
  #geom_density()+
  geom_vline(xintercept = 0, linetype = "dashed", size = 1.1) +
  coord_cartesian(xlim = c(-0.5,0.5))+
  stat_function(fun = dnorm, args = list(mean = mean(Model3$residuals), sd = sd(Model3$residuals)))+
  labs(title = 'Mediterranean', x = 'Residuals', y = 'Density') +
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    strip.text.x = element_text(size =17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none")

Mod_3res

#create histogram of residuals
Mod_4res <- ggplot(Model4, aes(x = Model4$residuals)) +
  geom_histogram(aes(y = after_stat(density)),bins = 70, fill = 'steelblue', color = 'black') +
  #geom_density()+
  geom_vline(xintercept = 0, linetype = "dashed", size = 1.1) +
  coord_cartesian(xlim = c(-0.5,0.5))+
  stat_function(fun = dnorm, args = list(mean = mean(Model4$residuals), sd = sd(Model4$residuals)))+
  labs(title = 'Mediterranean', x = 'Residuals', y = 'Density') +
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    strip.text.x = element_text(size =17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none")

Mod_4res



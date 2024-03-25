###VARIANCE INFLATION FACTOR (VIF)

# Tienes un modelo de regresión logística llamado 'Model1' 

vif_result <- car::vif(Model1)

# Muestra los resultados
print(vif_result)


# Tienes un modelo de regresión logística llamado 'Model1' 

vif_result <- car::vif(Model2)

# Muestra los resultados
print(vif_result)


# Tienes un modelo de regresión llamado 'Model1' 

vif_result <- car::vif(Model3)

# Muestra los resultados
print(vif_result)


# Tienes un modelo de regresión logística llamado 'Model1' 

vif_result <- car::vif(Model4)

# Muestra los resultados
print(vif_result)

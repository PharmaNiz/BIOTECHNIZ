
# Performance opérationnelle

### Taux de production
# Calcul du taux de production
production_rate <- get_mon_performance(VACZIN, type = "throughput")

# Plot du taux de production
plot(production_rate, type = "l", xlab = "Temps", ylab = "Taux de production", main = "Taux de production au fil du temps")

### Taux de service
# Calcul du taux de service
service_rate <- get_mon_performance(VACZIN, type = "service")

# Plot du taux de service
plot(service_rate, type = "l", xlab = "Temps", ylab = "Taux de service", main = "Taux de service au fil du temps")

### Utilisation des ressources
# Calcul de l'utilisation moyenne des ressources
mean_resource_utilization <- get_mon_resources(VACZIN, aggregate=TRUE)$data[, "utilization"]

# Plot de l'utilisation des ressources
plot(mean_resource_utilization, type = "l", xlab = "Temps", ylab = "Utilisation des ressources", main = "Utilisation des ressources au fil du temps")

### Taux de disponibilité des machines
# Calcul du taux de disponibilité des machines
availability_rate <- get_mon_resources(VACZIN, aggregate = TRUE)$data[, "availability"]

# Plot du taux de disponibilité des machines
plot(availability_rate, type = "l", xlab = "Temps", ylab = "Taux de disponibilité", main = "Taux de disponibilité des machines au fil du temps")

### Taux de rejet
# Calcul du taux de rejet
rejection_rate <- get_mon_attributes(VACZIN, "rejection_rate")

# Plot du taux de rejet
plot(rejection_rate, type = "l", xlab = "Temps", ylab = "Taux de rejet (%)", main = "Taux de rejet au fil du temps")

# Performance logistique

### Stocks
# Calcul du stock moyen de vaccins en cours de production
mean_in_production <- get_mon_resources(VACZIN, aggregate = TRUE)$data[, "queue_length_Lot Fini"]

# Plot du stock moyen de vaccins en cours de production
plot(mean_in_production, type = "l", xlab = "Temps", ylab = "Stock moyen en production", main = "Stock moyen en production au fil du temps")

### Rotation des stocks
# Calcul de la rotation des stocks
stock_rotation <- get_mon_attributes(VACZIN, "stock_rotation")

# Plot de la rotation des stocks
plot(stock_rotation, type = "l", xlab = "Temps", ylab = "Rotation des stocks", main = "Rotation des stocks au fil du temps")

### Temps de réapprovisionnement
# Calcul du temps de réapprovisionnement moyen
mean_restock_time <- get_mon_attributes(VACZIN, "mean_restock_time")

# Plot du temps de réapprovisionnement moyen
plot(mean_restock_time, type = "l", xlab = "Temps", ylab = "Temps de réapprovisionnement moyen", main = "Temps de réapprovisionnement moyen au fil du temps")

### Temps de traitement des commandes
# Calcul du temps moyen de traitement des commandes
mean_order_processing_time <- get_mon_attributes(VACZIN, "mean_order_processing_time")

# Plot du temps moyen de traitement des commandes
plot(mean_order_processing_time, type = "l", xlab = "Temps", ylab = "Temps de traitement des commandes moyen", main = "Temps de traitement des commandes moyen au fil du temps")

### Temps de transit
# Calcul du temps moyen de transit des produits
mean_transit_time <- get_mon_attributes(VACZIN, "mean_transit_time")

# Plot du temps moyen de transit des produits
plot(mean_transit_time, type = "l", xlab = "Temps", ylab = "Temps de transit moyen", main = "Temps de transit moyen au fil du temps")

# Performance client

### Délai d'attente moyen
# Calcul du délai d'attente moyen
mean_waiting_time <- get_mon_attributes(VACZIN, "mean_waiting_time")

# Plot du délai d'attente moyen
plot(mean_waiting_time, type = "l", xlab = "Temps", ylab = "Délai d'attente moyen", main = "Délai d'attente moyen au fil du temps")

### Taux de satisfaction client
# Calcul du taux de satisfaction client
customer_satisfaction_rate <- get_mon_attributes(VACZIN, "customer_satisfaction_rate")

# Plot du taux de satisfaction client
plot(customer_satisfaction_rate, type
     
     = "l", xlab = "Temps", ylab = "Taux de satisfaction client (%)", main = "Taux de satisfaction client au fil du temps")

### Délai de livraison
# Calcul du délai moyen de livraison
mean_delivery_time <- get_mon_attributes(VACZIN, "mean_delivery_time")

# Plot du délai moyen de livraison
plot(mean_delivery_time, type = "l", xlab = "Temps", ylab = "Délai moyen de livraison", main = "Délai moyen de livraison au fil du temps")

### Précision des commandes
# Calcul de la précision des commandes
order_accuracy_rate <- get_mon_attributes(VACZIN, "order_accuracy_rate")

# Plot de la précision des commandes
plot(order_accuracy_rate, type = "l", xlab = "Temps", ylab = "Précision des commandes (%)", main = "Précision des commandes au fil du temps")

### Taux de retour
# Calcul du taux de retour
return_rate <- get_mon_attributes(VACZIN, "return_rate")

# Plot du taux de retour
plot(return_rate, type = "l", xlab = "Temps", ylab = "Taux de retour (%)", main = "Taux de retour au fil du temps")
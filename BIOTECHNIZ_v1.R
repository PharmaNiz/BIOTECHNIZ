# Load Libraries
library(simmer)
library(simmer.bricks)
library(simmer.plot)
#######################################################################################
############ Définition des paramètres nécessaires à la Simulation#####################
set.seed(123)

DSM <- c(6)#En Mois (rajouter *30 à SimTime)
#DSJ <- 66 #En jours
# Modifier le SimTime pour simuler sur une période plus longue
SimTime <- c(DSM*  30* 24 * 60)  # Durée de la simulation (en minutes)


Incidence_Patients <-c()
for (i in 1:DSM) {Incidence_Patients[i]<-18000} #Incidence_Patients En milliers, facilite et allege la simulation
Incidence_Patients_Mois<-c(Incidence_Patients/12)


Prevalence_Patients <-c() #Augmente chaque mois car les Patients incidéents deviennent Prévalents
Prevalence_Patients[1] <-90000
for (i in 2:DSM){Prevalence_Patients[i] <- (Prevalence_Patients[i - 1] + Incidence_Patients)}



Cumul_Nizumab_Servi <- c()
Cumul_Nizumab_Servi[1] <- (Prevalence_Patients[1] + Incidence_Patients_Mois[1])

for (i in 2:DSM) 
  {Cumul_Nizumab_Servi[i] <-Prevalence_Patients[i]+Prevalence_Patients[i-1] +Incidence_Patients_Mois[i]}


# Définir la fonction pour calculer la demande quotidienne moyenne
n <- as.numeric(length(Cumul_Nizumab_Servi))  # Nombre de mois pour lesquels nous avons des données disponibles
DDM <- c(Cumul_Nizumab_Servi[n] / 30)  # Estimation de la demande quotidienne 
New_DDM <-c(round(mean(Incidence_Patients_Mois)/30,digits = 0))

NumBatchs <- c(10000)       # Inventaire initial de Batchs

RT <- c(2 *24 * 60)         # Temps de réapprovisionnement (en minutes) =2j

ST <- c(DDM * 3)         #On se donne 3jours de safety stock 

#similar to a reorder point, but it’s a surplus quantity to ensure you don’t run completely out of stock if there are delays


ROP <- c((DDM  *  RT/(24 * 60 )) + ST)             # ROP (Reorder Point)  #RT est en jours ici  puisque DDM aussi !! (ROQ également) !!!

ROQ <- c((DDM  *  RT/(24 * 60 ))) # ROQ (Reorder Quantity)


AQ_Time <-c(2 * 60)            #Durée de la demarche qualité en sortie de chaine avant liberation du lot (en minutes) (selon les référencs, environ 2h)



####### Fonction pour calculer les temps inter-arrivées des Patients 'en minutes)
#Pour simplifier les choses:
#On concidere qu'il y a 18 Centres de lutte contre le cancer
#Ils seront ceux qui recupéreront les lots pour les redistribuer aux patients
#Pour simplifier les choses:
#On considère qu'il y a un nombre égal de patients par region

NbrDeCentresLAC<-c(18)
#interarrival_time <- c(30 * 24 * 60 * NbrDeCentresLAC*4)#Minutes(4 fois par mois/1 par semaine) (a adapter en fonction de l'epidemio/demande)


CustArr <- rexp(1, DD) * 480
NewCustArr <- rexp(1, DD) * 480

#Les 18 centres se partagent les patients equitablement
#Donc les valeurs des seize ... = Prevalence/(Mois*18)

QPC_PP <- c(round((30*DDM)/(12*NbrDeCentresLAC),digits=0)+1)
QPC_PI <- c(round((30*New_DDM[1])/(12*NbrDeCentresLAC),digits=0)+1)

# On rajoute un +1 car arrondi à la valeur sup, et on ne veut pas qu'un patient manque une dose.
#Les 18 centres de lutte anti cancer se partagent les patients equitablement (pour simplifier la simulation)
#Pour plus de detail, coder 18 trajectoires pour les 18 centres avec des frequences d'arriver =/=, en fonction du nbr de cas par region
#Donc les valeurs des seize ... = Prevalence/(Mois*18)))



###### Fonction pour le forecast (à modifier selon la prevision de demainde)

MCNS<-mean(Cumul_Nizumab_Servi)  #MCNS = Mean Cumul Nizumab Servi

Forecast <- function() {MCNS}                     ##Mounthly Forecast
#####################################################################################
# Définir l'environnement, les Ressources et Trajectoires
BIOTECHNIZ <- simmer()

# Ajout des ressources
BIOTECHNIZ %>%
  add_global("Inventaire Batchs", NumBatchs) %>%
  add_global("Début de Prod", 0) %>%
  add_global("Rework", 0) %>%
  add_global("Fin de Prod", 0) %>%
  add_global("Carnet de Commande", 0) %>%
  add_resource("Batch", NumBatchs) %>%
  add_resource("Nizumab", 30)# Initial stock of finished lots
  

# Ajout des machines
machines <-
  c(
    "M1:Bioreacteur",
    "M2:Centrifugeuse",
    "M3:Colonne d'Affinité",
    "M4:Système de Filtration Tangentielle",
    "M5:SRA-BCI",
    "S1:AQ"
  )


# Capacités de chaque machine (en nombre de batch traité par cycle)
#pour l'instant se pasere sur la Daily Demande Moyenne
### Chercher dans la litterature de vrais chiffres si possible (scale up et autres articles )

Capacites <- c(
  M1 = (round(DDM * 0.33/100, digits = 0))
  +1,
  M2 = (round(DDM * 0.42/100, digits = 0))
  +1,
  M3 = (round(DDM * 0.55/100, digits = 0))
  +1,
  M4 = (round(DDM * 0.90/100, digits = 0))
  +1,             
  M5 = (round(DDM *1.1/100, digits = 0))
  +1,
  SAQ = (round(DDM *1.5/100, digits = 0))
  +1    #Nombre de lot validable par le service AQ
)

# Capacités de ligne d'attente pour chaque machine (nombre d'unités en attente)
# A affiner avec la litterature
Capacites_Ligne_Attente <- Capacites*2

# Temps de cycle de chaque machine (en minutes par batch)
Temps_Cycle <- c(
  M1 = 10,
  M2 = 15,
  M3 = 20,
  M4 = 25,
  M5 = 30,
  SAQ= AQ_Time
)

# Taux de rework pour chaque machine (en pourcentage)  
# Produits nécessitant une reprise

Rework <- c(
  M1 = 1,
  M2 = 2,
  M3 = 1,
  M4 = 2,
  M5 = 1,
  SAQ =1.2
)

# Temps moyen de réparation (MTTR) pour chaque machine (en minutes)
# A affiner avec les données de la littérature
MTTR <- c(
  M1 = 110,
  M2 = 110,
  M3 = 105,
  M4 = 105,
  M5 = 90,
  SAQ = 0) #S1 correspond aux employés, techniquement si l'usine tourne H/24, le temps est 0si 0 envie de dormir alors 0 (ou alors un bon roulement d'equipe)


# Temps moyen entre les pannes (MTBF) pour chaque machine (en heures*60 minutes)
#A Affiner également
MTBF <- c(
  M1 = 300,
  M2 = 250,
  M3 = 220,
  M4 = 180,
  M5 = 300,
  SAQ = 0 )*60     
#!! convertir les unités en minutes, S1 les gens de la qualité, 
#si 0 envie de dormir alors 0 (ou alors un bon roulement d'equipe)
# Mettre un temps tres long pour le SAQ? 2/3 fois par semestre pour simuler des jours feriés et dimanches?



for (i in seq_along(machines)) {
  BIOTECHNIZ %>% 
    add_resource(machines[i], 
                 capacity = Capacites[i],
                 queue_size = Capacites_Ligne_Attente[i],
                 mon = TRUE)}



###############################################################
# Trajectoire Patients

#En theorie il est possible de mettre 18 trajectoire pour les 18 centres
#Ou une alternative plus precise
#pour la v1, simple et efficace: 
#Les 18 centres régionnaux se partagent les patients français equitablement
#Donc les valeurs des seize ... = Prevalence/(Mois*18)

t_Patients_Prevalents <- trajectory() %>%
  seize("Nizumab", QPC_PP) %>%
  timeout(5) %>%   #Arrive, discute un peu, prend le medicament, repart (en minutes)
  release("Nizumab", QPC_PP) %>%
  set_capacity("Nizumab", -QPC_PP,mod = "+")# Reduit le Stock de Nizumab de 1
#Chiffre 1 pas adapté, on peut diviser cela en centre regionaux de cancer ou nombre de CHU

t_Patients_Incidents <- trajectory() %>%
  seize("Nizumab", QPC_PI) %>%
  timeout(25)%>%    #Un time out un peu plus élevé pour l'ETP
  release("Nizumab", QPC_PI) %>%
  set_capacity("Nizumab", -QPC_PI, mod = "+")


#ETP si Biotechniz recoit les patients un par un, en principe cela est compliqué

#Cependant on peut ajouter un temps plus long pour:
#representer le fait que pour les traitements des nouveaux patients,
#on a pris le temps de mettre dans la commande
#de la documentation ETP sur le Nizumab, sur la pathologie, un pillulier, = Kit complet pour le patient ...

#Patients prevalents ont un traitement habituel, simple a recuperer, charger et transporter
#Patients prevalents: traitement + Kit ETP et bonne observance, ce qui demande plus de temps a charger dans les camions...


#ps: ici on parle en chargement de camions et non de visite à l'officine, donc les valeurs de 5 minutes et 25 minutes sont bien sous-estimés
#affiner ces valeurs avec de la litterature et données réelles
#pour la v1, prendre des chiffres moyens officinaux

######## Trajectoire de la Ligne de Porduction###########

t_Ligne_de_Prod <- trajectory() %>%
  set_global("Début de Prod", 1, mod = "+") %>%
  seize("Batch", Capacites["M1"]) %>%
  
  visit("M1:Bioreacteur", Temps_Cycle["M1"], Capacites["M1"]) %>%
  rollback(
    3,
    check = function()
      runif(1) <= Rework["M1"],
    tag = "Rollback"
  ) %>%
  set_attribute("Rework",Capacites["M1"], mod = "+") %>%
  
  visit("M2:Centrifugeuse", Temps_Cycle["M2"], Capacites["M2"]) %>%
  rollback(
    3,
    check = function()
      runif(1) <= Rework["M2"],
    tag = "Rollback"
  ) %>%
  set_attribute("Rework", Capacites["M2"], mod = "+") %>%
  
  visit("M3:Colonne d'Affinité", Temps_Cycle["M3"], Capacites["M3"]) %>%
  rollback(
    3,
    check = function()
      runif(1) <= Rework["M3"],
    tag = "Rollback"
  ) %>%
  set_attribute("Rework", Capacites["M3"], mod = "+") %>%
  
  visit("M4:Système de Filtration Tangentielle", Temps_Cycle["M4"], Capacites["M4"]) %>%
  rollback(
    3,
    check = function()
      runif(1) <= Rework["M4"],
    tag = "Rollback"
  ) %>%
  set_attribute("Rework", Capacites["M4"], mod = "+") %>%
  
  visit("M5:SRA-BCI", Temps_Cycle["M5"], Capacites["M5"]) %>%
  rollback(
    3,
    check = function()
      runif(1) <= Rework["M5"],
    tag = "Rollback"
  ) %>%
  set_attribute("Rework", Capacites["M5"], mod = "+") %>%
  
  set_capacity("Batch", -Capacites["M5"], mod = "+") %>%
  set_global("Inventaire Batchs", -Capacites["M5"], mod = "+") %>%
  
  visit("S1:AQ", AQ_Time, Capacites["SAQ"]) %>%    #Passage par le Service AQ pour tests et autres (entre 2 et 4h) #Controle qualité +liberation du lot
  rollback(
    3,
    check = function()
      runif(1) <= Rework["SAQ"],
    tag = "Rollback"
  ) %>%
  set_attribute("Rework", Capacites["SAQ"], mod = "+") %>%
  set_global("Fin de prod", 1, mod = "+") %>%
  set_capacity("Nizumab", Capacites["SAQ"], mod = "+", tag = "Nizumab prêt")%>%
  release("Batch", Capacites["SAQ"])
  




######### Trajectoire pour le contrôle des stocks de Nizumab en sortie de chaine ################

t_Stock_Nizumab <- trajectory() %>%
  seize("Nizumab", 1) %>%
  timeout(0) %>% #instantané, un coup de scanner dans l'entrepot suffit (20 min si personne)
  release("Nizumab", 1)

################## Trajectoire pour les Pannes ################
# Trajectoire pour la panne de la machine M1
t_Panne_M1 <- trajectory() %>%
  seize("M1:Bioreacteur", 1) %>%
  timeout(MTTR["M1"]) %>%
  release("M1:Bioreacteur", 1)

# Trajectoire pour la panne de la machine M2
t_Panne_M2 <- trajectory() %>%
  seize("M2:Centrifugeuse", 1) %>%
  timeout(MTTR["M2"]) %>%
  release("M2:Centrifugeuse", 1)

# Trajectoire pour la panne de la machine M3
t_Panne_M3 <- trajectory() %>%
  seize("M3:Colonne d'Affinité", 1) %>%
  timeout(MTTR["M3"]) %>%
  release("M3:Colonne d'Affinité", 1)

# Trajectoire pour la panne de la machine M4
t_Panne_M4 <- trajectory() %>%
  seize("M4:Système de Filtration Tangentielle", 1) %>%
  timeout(MTTR["M4"]) %>%
  release("M4:Système de Filtration Tangentielle", 1)

# Trajectoire pour la panne de la machine M5
t_Panne_M5 <- trajectory() %>%
  seize("M5:SRA-BCI", 1) %>%
  timeout(MTTR["M5"]) %>%
  release("M5:SRA-BCI", 1)


############ Trajectoire pour le contrôle de stock (Batch)###########

t_Restock_Batch <- trajectory() %>%
  branch(
    function()
      get_global(BIOTECHNIZ, "Inventaire Batchs") <= ROP,
    continue = TRUE,
    trajectory() %>%
      set_global("Carnet de Commande", 1, mod = "+") %>%
      set_global("Inventaire Batchs", ROQ, mod = "+") %>%
      timeout(RT) %>%
      set_capacity("Batch", ROQ, mod = "+")
  ) %>%
  timeout(0)


# Ajout des générateurs avec une fréquence d'observation réduite

BIOTECHNIZ %>%
  add_generator("Ligne de Production", t_Ligne_de_Prod, Forecast ) %>%
  add_generator("CTRL des Stocks de Batch", t_Restock_Batch, function() {500}) %>%
  add_generator("Patients Prévalents", t_Patients_Prevalents, CustArr ) %>%
  add_generator("Patients Incidents", t_Patients_Incidents, NewCustArr ) %>%
  add_generator("Stocks de Nizumab", t_Stock_Nizumab, function() {300})%>%
  add_generator("Pannes_M1", t_Panne_M1, function() {rexp(1, 1 / MTBF["M1"])}, priority = 1 ) %>%
  add_generator("Pannes_M2", t_Panne_M2, function() {rexp(1, 1 / MTBF["M2"])}, priority = 1 ) %>%
  add_generator("Pannes_M3", t_Panne_M3, function() {rexp(1, 1 / MTBF["M3"])}, priority = 1 ) %>%
  add_generator("Pannes_M4", t_Panne_M4, function() {rexp(1, 1 / MTBF["M4"])}, priority = 1 ) %>%
  add_generator("Pannes_M5", t_Panne_M5, function() {rexp(1, 1 / MTBF["M5"])}, priority = 1 )

# Lancer la Simulation

BIOTECHNIZ %>% run(until = SimTime)

# Stocker les Résultats en agrégeant les données
Arrivals <- get_mon_arrivals(BIOTECHNIZ)
Ressources <- get_mon_resources(BIOTECHNIZ)
Attributes <- get_mon_attributes(BIOTECHNIZ)

###############################################
###############METRICS PATIENTS##############

# Calculer la différence entre le temps de fin et le temps d'arrivée pour chaque patient
Activité_Totale_Patient = subset(Arrivals,
                                 substr(name, 1, 4) == "Pati")

Waiting_Time_Per_Patient <-
  (Activité_Totale_Patient$end_time - Activité_Totale_Patient$start_time - 5)

plot(seq(length(Waiting_Time_Per_Patient)), Waiting_Time_Per_Patient, type = "l")


Stock_Levels <- 

# Tracer les niveaux de stock de Batch & NIZUMAB
ggplot(data = stock_levels, aes(x = time)) +
  geom_line(aes(y = batch_stock, color = "Stock de Batch")) +
  geom_line(aes(y = nizumab_stock, color = "Stock de NIZUMAB")) +
  labs(title = "Niveaux de Stock de Batch & NIZUMAB au Fil du Temps",
       x = "Temps",
       y = "Niveau de Stock",
       color = "Type de Stock") +
  theme_minimal()

# Tracer l'utilisation des ressources
ggplot(data = resource_utilization, aes(x = time, y = utilization, color = resource)) +
  geom_line() +
  labs(title = "Utilisation des Ressources au Fil du Temps",
       x = "Temps",
       y = "Utilisation",
       color = "Ressource") +
  theme_minimal()

# Tracer les temps d'écoulement des clients
ggplot(data = customer_flow, aes(x = customer_id, y = flow_time)) +
  geom_bar(stat = "identity") +
  labs(title = "Temps d'Écoulement des Clients",
       x = "Identifiant du Client",
       y = "Temps d'Écoulement",
       color = "Ressource") +
  theme_minimal()

# Trouver les instances où Inventaire Batch <= ROP
instances_below_rop <- stock_levels[stock_levels$batch_stock <= ROP, ]

# Trouver les instances où des commandes ont été passées
orders_placed <- orders[orders$type == "placed", ]

# Trouver les instances où il y avait rework
rework_instances <- production_events[production_events$type == "rework", ]

# Tracer les temps d'écoulement des ordres de travail dans l'usine
ggplot(data = work_orders_flow_times, aes(x = work_order_id, y = flow_time)) +
  geom_bar(stat = "identity") +
  labs(title = "Temps d'Écoulement des Ordres de Travail dans l'Usine",
       x = "Identifiant de l'Ordre de Travail",
       y = "Temps d'Écoulement") +
  theme_minimal()

# Nombre d'arrivées générées
arrivals_generated_count <- nrow(arrivals)

# Nombre total de clients servis
customers_served_count <- nrow(customers_served)

# Nombre de clients retardés (> 15 mins)
delayed_customers_count <- nrow(delayed_customers)

# Performance du service (> 15 jours) - Non calculée ici

# Calcul de la moyenne du stock de Batch, du stock de NIZUMAB, du débit moyen et de l'efficacité du cycle de processus
moyenne_stock_batch <- mean(stock_levels$batch_stock)
moyenne_stock_nizumab <- mean(stock_levels$nizumab_stock)
debit_moyen <- sum(production_events$output) / nrow(production_events)
efficacite_cycle_processus <- (sum(production_events$processing_time))
  

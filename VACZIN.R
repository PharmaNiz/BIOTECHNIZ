# MANUFACTURING PROCESS SIMULATION: VACZIN

# Load Libraries
library(simmer)
library(simmer.bricks)
library(simmer.plot)

# Definition des paramêtres nécessaires à la Simulation
set.seed(3141)  

SimTime <- 15 * 480   # Simulation Duration (mins)
DD <- 10              # Daily Demand (units/day)
RT <- 3 * 480         # Replenishment Time (mins)
ROP <- 45             # units
ROQ <- 25             # units
NumVaccins <- 50      # units
NumSeringues <- 50    # units
MTTR <- 60            # Mean Time To Repair (mins)
MTBF <- 7 * 480       # Mean Time Between Failures (mins)
Rework <- 0.02        # Products needing rework

# Inter arrival times (Customers, Work Oders)
CustArr <- function() rexp(1, DD) * 480   # mins
Forecast <- function() {50}                # mins

# Définir les Ressources et Trajectoires
VACZIN <- simmer()

# Ajout des ressources
VACZIN %>%
  add_global("Inventaire Vaccins", NumVaccins) %>%
  add_global("Inventaire Seringues", NumSeringues) %>%
  add_resource("Vaccin", NumVaccins) %>%
  add_resource("Seringue", NumSeringues) %>%
  add_resource("Lot Fini", 10) #on commence à 10 histoire d'avoir du stock au tout début pour les premiers clients

# Ajout des machines
machines <- c("M1 : Dissolution", "M2 : Filtration Clarifiante", "M3 : Repartition", "M4 : Setrilisation Terminale", "M5 : Sérialisation ")
for (machine in machines) {
  VACZIN %>% add_resource(machine, 1)
}

# Trajectoire Clients
t_Client <- trajectory() %>%
  seize("Lot Fini", 1) %>%
  timeout(5) %>%
  release("Lot Fini", 1) %>%
  set_capacity("Lot Fini", -1, mod = "+")  # Reduit le Stock de Lot Fini de 1

# Trajectoire des Work Orders
t_Ligne_de_Prod <- trajectory() %>%
  seize("Vaccin", 1) %>%
  visit("M1 : Dissolution", 40, 1) %>%
  visit("M2 : Filtration Clarifiante", 40, 1) %>%
  release("Vaccin", 1) %>%
  seize("Seringue", 1) %>%
  visit("M3 : Repartition", 40, 1) %>%
  visit("M4 : Setrilisation Terminale", 40, 1) %>%
  rollback(6, check = function() runif(1) <= Rework) %>%
  set_capacity("Vaccin", -1, mod = "+") %>%
  set_global("Inventaire Vaccins", -1, mod = "+") %>%
  set_capacity("Seringue", -1, mod = "+") %>%
  set_global("Inventaire Seringues", -1, mod = "+") %>%
  set_capacity("Lot Fini", 1, mod = "+") %>%
  release("Seringue", 1)  # Libérer les ressources acquises

plot(t_Ligne_de_Prod)

# Trajectoire pour le controle de stock
t_Restock_Vaccin <- trajectory() %>%
  branch(function() get_global(VACZIN, "Inventaire Vaccins") <= ROP,
         continue = TRUE,
         trajectory() %>%
           set_global("Inventaire Vaccins", ROQ, mod = "+") %>%
           timeout(RT) %>%
           set_capacity("Vaccin", ROQ, mod = "+")
  ) %>%
  timeout(0)

# Trajectoire pour la panne de la M2: filtration
t_Panne <- trajectory() %>%
  visit("M2 : Filtration Clarifiante", MTTR, 1)

# Ajout des générateurs
VACZIN %>%
  add_generator("Clients", t_Client, CustArr) %>%
  add_generator("Carnet de Commande", t_Ligne_de_Prod, Forecast) %>%
  add_generator("Scan Vaccin", t_Restock_Vaccin, function() {30}) %>%
  add_generator("Pannes", t_Panne,
                function() {rexp(1, 1/MTBF)},
                priority = 1)  

# Lancer La Simulation
VACZIN %>% run(until = SimTime)

# Stocker les Résultats
Arrivals <- get_mon_arrivals(VACZIN)
Ressources <- get_mon_resources(VACZIN)
Journal_de_Bord <- get_mon_attributes(VACZIN)

head(Arrivals)
head(Ressources)
head(Journal_de_Bord)
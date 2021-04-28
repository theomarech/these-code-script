source("/home/theo/Bureau/scriptR/librairies.R")

########################### CONNEXION BDD PLAN D'EAU  ############################

drv <-
  dbDriver("PostgreSQL") #interface avec système de gestion de base de données

con <- dbConnect(
  drv,
  dbname = "bd_plando",
  host = "axserveurbd.aix.irstea.priv",
  port = 5434,
  user = "theo.marechal",
  password = theo
)

dbDisconnect(con)
dbUnloadDriver(drv)

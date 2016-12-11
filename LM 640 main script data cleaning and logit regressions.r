# 9 diciembre, en INEGI
# analysis de victimizacion de las empresas

library(foreign)
library(plyr)
library(tidyr)
library(dplyr)

# TODO: what do we need from vctimization module
# victim = read.dbf("~/Procesamiento/Insumos/ENVE/ENVE_2014/tmv_sol_enve_2014.dbf")

# main survey data
main = read.dbf("~/Procesamiento/Insumos/ENVE/ENVE_2014/tpv_sol_enve_2014.dbf")

# this does not create a unique id var - if we request remote processing, need to add back distinct()
# # creat an id var - TODO: request actual id var - cve_unica
# main$id = as.character(paste(main$ID_ESTRATO, main$SECTOR, main$SECTOR_FIN, main$E23, main$TIPO, main$CVE_ENT, sep = " "))

# tabular & summary statistics - for the analyst in Sydney to refine the model
# TODO: cleaner output?
summary = summary(main)
write.csv(summary, "~/Resultados/LM 640 Summary statistics of raw data.csv", row.names = F)


# necesitamos un funcion a cambiar No sabe / no responde / NA a 0
clean = function(x) {
  x = ifelse(x %in% c(2,9,3, NA), 0, x)
  return(x)
}

# outcome variable: P24 - TODO: request P24
# # Pregunat 24. En lo que va de 2014, ¿el establecimiento ha sufrido alguna de las situaciones nombradas en la tarjeta?
# # filter No sabe / no responde
# main = main %>%
#   filter(P_24 != 9)

# create outcome vars from P_25


# por cada variable, cambia los respuestas No sabe / no responde / NA a 0
main$P25_1 = as.numeric(clean(main$P25_1))
main$P25_2 = as.numeric(clean(main$P25_2))
main$P25_3 = as.numeric(clean(main$P25_3))
main$P25_4 = as.numeric(clean(main$P25_4))
main$P25_5 = as.numeric(clean(main$P25_5))
main$P25_6 = as.numeric(clean(main$P25_6))
main$P25_7 = as.numeric(clean(main$P25_7))
main$P25_8 = as.numeric(clean(main$P25_8))
main$P25_9 = as.numeric(clean(main$P25_9))
main$P25_10 = as.numeric(clean(main$P25_10))
main$P25_11 = as.numeric(clean(main$P25_11))
main$P25_12 = as.numeric(clean(main$P25_12))
main$P25_13 = as.numeric(clean(main$P25_13))
main$P25_14 = as.numeric(clean(main$P25_14))


# victim 1 gives the degree of victimization from 0 to 14

main$victim1 = rowSums(main[, c("P25_1",  "P25_2",
                                "P25_3",  "P25_4",
                                "P25_5",  "P25_6",
                                "P25_7",  "P25_8",
                                "P25_9",  "P25_10",
                                "P25_11",  "P25_12",
                                "P25_13",  "P25_14")])

# victim 2 is binary

main$victim2 = ifelse(main$victim1 > 0, 1, 0)

# frequency tables
victim1 = table(main$victim1)
write.csv(victim1, "~/Resultados/LM 640 frequency table of victimization - continuous variable.csv", row.names = F)

victim2 = table(main$victim2)
write.csv(victim2, "~/Resultados/LM 640 frequency table of victimization - logit variable.csv", row.names = F)

##### #1. Simple logit model on binary victimization variable #####
# TODO: export logit and linear results to compare

# State - CVE_ENT - no NAs
# Sector/industry - Sector_final - no NAs
# Business size (Range of employees) - P4  - no NAs - TODO: why did we loose P4_1?

simple = main %>%
  select(victim2, CVE_ENT, P4, SECTOR_FIN) 

model = glm(victim2 ~ ., family = binomial(link = "logit"), data = simple)

model.an = anova(model)
model.sum =  summary(model)

results = summary.glm(model)$coefficients

write.csv(results, "~/Resultados/LM 640 preliminary logit regression results.csv")
write.csv(model.an, "~/Resultados/LM 640 preliminary logit regression anova.csv")

##### #2. Simple model, add Tipo - e23, Urban/rural - Tipo #####
  #- no NAs

simple2 = main %>%
  select(victim2, CVE_ENT, P4, SECTOR_FIN, TIPO, E23)

model = glm(victim2 ~ ., family = binomial(link = "logit"), data = simple2)

model.an = anova(model)
model.sum =  summary(model)

results = summary.glm(model)$coefficients

write.csv(results, "~/Resultados/LM 640 preliminary logit regression results 2.csv")
write.csv(model.an, "~/Resultados/LM 640 preliminary logit regression anova 2.csv")

##### #3. Run simple1 without state effects #####

simple3 = main %>%
  select(victim2, P4, SECTOR_FIN) 

model = glm(victim2 ~ ., family = binomial(link = "logit"), data = simple3)

model.an = anova(model)
model.sum =  summary(model)

results = summary.glm(model)$coefficients

write.csv(results, "~/Resultados/LM 640 preliminary logit regression results 3.csv")
write.csv(model.an, "~/Resultados/LM 640 preliminary logit regression anova 3.csv")

##### #4. Run simple2 without state effects #####

simple4 = main %>%
  select(victim2, P4, SECTOR_FIN, TIPO, E23) 

model = glm(victim2 ~ ., family = binomial(link = "logit"), data = simple4)

model.an = anova(model)
model.sum =  summary(model)

results = summary.glm(model)$coefficients

write.csv(results, "~/Resultados/LM 640 preliminary logit regression results 4.csv")
write.csv(model.an, "~/Resultados/LM 640 preliminary logit regression anova 4.csv")

# TODO: what are the sector codes?
# TODO: iterations w/o E23 - I think it is single store vs franchise - if small shops most affected, may as well remove and test significance of other vars

###### Clean and prepare additional independent variales #####

# business size is significant, but right tailed - 62% are microenterprises
# TODO: visualize distribution, compare to Mexico's overall distribution of business size

size = table(main$P4)
write.csv(size, "~/Resultados/LM 640 frequency table of business size by range of employees.csv", row.names = F)

# create a binary variable for microenterprises
main$micro = ifelse(main$P4 == 6, 1, 0)

# group business sizes for a categorical variable
  # 1 = 1 - 10 employees (code 6)
  # 2 = 11 - 50 (codes 4,5)
  # 3 = 51 + (codes 1,2,3)
main$size = ifelse(main$P4 == 6, 1,
                   ifelse(main$P4 %in% c(4,5), 2,
                          ifelse(main$P4 %in% c(1,2,3), 3, NA)))

size.groups = table(main$size)
write.csv(size.groups, "~/Resultados/LM 640 frequency table of business size by range of employees.csv", row.names = F)


# TODO: correlation matrix for each prevention question, binary and continuous victimization
  # if anything, scatterplots - summarise by state and export for visualization

# por cada variable, cambia los respuestas No sabe / no responde / NA a 0
main$P12_1 = as.numeric(clean(main$P12_1))
main$P12_2 = as.numeric(clean(main$P12_2))
main$P12_3 = as.numeric(clean(main$P12_3))
main$P12_4 = as.numeric(clean(main$P12_4))
main$P12_5 = as.numeric(clean(main$P12_5))
main$P12_6 = as.numeric(clean(main$P12_6))
main$P12_7 = as.numeric(clean(main$P12_7))
main$P12_8 = as.numeric(clean(main$P12_8))
main$P12_9 = as.numeric(clean(main$P12_9))
main$P12_10 = as.numeric(clean(main$P12_10))
main$P12_11 = as.numeric(clean(main$P12_11))
main$P12_12 = as.numeric(clean(main$P12_12))
main$P12_13 = as.numeric(clean(main$P12_13))
                         
# continuous variable of protection - number of measures
main$protect = rowSums(main[, c("P12_1",  "P12_2",
                                "P12_3",  "P12_4",
                                "P12_5",  "P12_6",
                                "P12_7",  "P12_8",
                                "P12_9",  "P12_10",
                                "P12_11", "P12_12",
                                "P12_13")])

# binary variable of protection measures -- about 50% of business have some protection
main$protect2 = ifelse(main$protect > 0, 1, 0)

# frequency tables
protect1 = table(main$protect)
write.csv(protect1, "~/Resultados/LM 640 frequency table of degree of protection - continuous variable.csv", row.names = F)

protect2 = table(main$protect2)
write.csv(protect2, "~/Resultados/LM 640 frequency table of binary protection varibale.csv", row.names = F)

# TODO: simplier categorical var for # of protection measures - is it about more protection or the right kind?

# group protection measures in 3 categories:
  # this method drops insurance, as it is not preventative, and ´other´ as it cannot be categorized

    # 1.	Install security equipment = 0/1
#   -	Change doors or windows - 1. cambiar puertas o ventanas?	P12_1
# -	Changing or installing locks 2. cambiar o colocar cerraduras y/o candados?	P12_2
# -	Install fences or 'grills' (gates?) 3. colocar rejas o bardas?	P12_3
# -	Buy 'boxes' or security rooms 4. comprar cajas o cuartos de seguridad?	P12_4
# -	Install alarms or cameras 5. instalar alarmas y/o videocámaras de vigilancia?	P12_5
# -	Install GPS 6. instalar dispositivos de localización: GPS?	P12_6
# -	Install cybersecurity systems "7. instalar sistemas de protección contra ataques
# cibernéticos?"	P12_7

# continuous variable of equipment - number of measures
main$equip = rowSums(main[, c("P12_1",  "P12_2",
                                "P12_3",  "P12_4",
                                "P12_5",  "P12_6",
                                "P12_7")])

# binary variable of equipment -- about 40% of business have some equipment, plurality: 1 measure
main$equip2 = ifelse(main$equip > 0, 1, 0)

# frequency tables
equip1 = table(main$equip)
write.csv(equip1, "~/Resultados/LM 640 frequency table of security equipment - continuous variable.csv", row.names = F)

equip2 = table(main$equip2)
write.csv(equip2, "~/Resultados/LM 640 frequency table of binary equipment varibale.csv", row.names = F)


# 2.	Hired security (security dept, security guard or security dog) = 0/1
  # # -	Hire private security 8. contratar o mantener vigilancia o seguridad privada?	P12_8
  # # -	Create a security department "9. crear un área dentro del establecimiento responsable
    # de la seguridad?"	P12_9
  # -	Hire a guard dog 11. comprar un perro guardián?	P12_11

# continuous variable of security personel, etc - number of measures
main$person = rowSums(main[, c("P12_11",  "P12_9",
                              "P12_8")])
# binary variable of personel -- less than 20% have staff, rarely more than one measure
main$person2 = ifelse(main$person > 0, 1, 0)

# frequency tables
person1 = table(main$person)
write.csv(person1, "~/Resultados/LM 640 frequency table of security staff, etc - continuous variable.csv", row.names = F)

person2 = table(main$person2)
write.csv(person2, "~/Resultados/LM 640 frequency table of binary security staff varibale.csv", row.names = F)


# 3.	Changed location  12. cambiar la ubicación del establecimiento?	P12_12

# simple variable for location change
main$loc = main$P12_12

# frequency table - very few (169) have changed location - unlikely to be significant
location = table(main$loc)
write.csv(location, "~/Resultados/LM 640 frequency table of binary location change varibale.csv", row.names = F)

                         
# Government actions:

# por cada variable, cambia los respuestas No sabe / no responde / NA a 0
# this assumes that "No sabe" and "No responde" are equivalent to "No se realizo"
# TODO: try this as categorical rather tahn binary
main$P15_1 = as.numeric(clean(main$P15_1))
main$P15_2 = as.numeric(clean(main$P15_2))
main$P15_3 = as.numeric(clean(main$P15_3))
main$P15_4 = as.numeric(clean(main$P15_4))
main$P15_5 = as.numeric(clean(main$P15_5))
main$P15_6 = as.numeric(clean(main$P15_6))
main$P15_7 = as.numeric(clean(main$P15_7))
main$P15_8 = as.numeric(clean(main$P15_8))
main$P15_9 = as.numeric(clean(main$P15_9))

# continuous variable of government action - number of measures
main$gov = rowSums(main[, c("P15_1",  "P15_2",
                                "P15_3",  "P15_4",
                                "P15_5",  "P15_6",
                                "P15_7",  "P15_8",
                                "P15_9")])

# binary variable of government action -- nearly 80% know of some gov action
main$gov2 = ifelse(main$gov > 0, 1, 0)

# frequency tables
gov1 = table(main$gov)
write.csv(gov1, "~/Resultados/LM 640 frequency table of degree of government action - continuous variable.csv", row.names = F)

gov2 = table(main$gov2)
write.csv(gov2, "~/Resultados/LM 640 frequency table of binary government action varibale.csv", row.names = F)

# group government action in 3 categories:
  # this method 
  
  #1. Public works - "Well-functioning government (WFG)"
#   -	Maintained/cleaned the surrounding area 1. dar mantenimiento y limpieza de los alrededores del establecimiento?	P15_1
# -	Improve lighting 2. mejorar el alumbrado de los alrededores del establecimiento?	P15_2

# continuous variable of wfg - number of measures
main$wfg = rowSums(main[, c("P15_1",  "P15_2")])

# binary variable of wfg 
main$wfg2 = ifelse(main$wfg > 0, 1, 0)

# frequency tables - maybe 60%
wfg1 = table(main$wfg)
write.csv(wfg1, "~/Resultados/LM 640 frequency table of WFG measure - continuous variable.csv", row.names = F)

wfg2 = table(main$wfg2)
write.csv(wfg2, "~/Resultados/LM 640 frequency table of binary WFG varibale.csv", row.names = F)


  #2. Improve or increase policing
# -	Potral the area 4. mayor patrullaje y vigilancia policiaca?	P15_4
# -	Install cameras (CCTV is everywhere) 5. la instalación de videocámaras de vigilancia?	P15_5
# -	Combat narcotrafficking (whatever that means.) 6. combatir el narcotráfico ?	P15_6
# -	Program to increase reporting of crime 7. programas de sensibilización para que los establecimientos denuncien?	P15_7
# -	Operations against crime 8. operativos contra la delincuencia?	P15_8

# continuous variable of policing - number of measures
main$police = rowSums(main[, c("P15_4", "P15_5", "P15_6", "P15_7", "P15_8")])

# binary variable of polcing 
main$police2 = ifelse(main$police > 0, 1, 0)

# frequency tables - maybe 75% - will need to use with others
police1 = table(main$police)
write.csv(police1, "~/Resultados/LM 640 frequency table of policing measure - continuous variable.csv", row.names = F)

police2 = table(main$police2)
write.csv(police2, "~/Resultados/LM 640 frequency table of binary policing varibale.csv", row.names = F)

  #3. Improve the business environment - "Sound Business Environment (SBE)"
# -	Programs to attract investment3. programas gubernamentales para la atracción de inversiones?	P15_3
# -	Programs to improve employment 9. programas de fomento al empleo?	P15_9

# continuous variable of sbe - number of measures
main$sbe = rowSums(main[, c("P15_3",  "P15_9")])

# binary variable of wfg 
main$sbe2 = ifelse(main$sbe > 0, 1, 0)

# frequency tables - maybe 40%?
sbe1 = table(main$sbe)
write.csv(sbe1, "~/Resultados/LM 640 frequency table of SBE measure - continuous variable.csv", row.names = F)

sbe2 = table(main$sbe2)
write.csv(sbe2, "~/Resultados/LM 640 frequency table of binary SBE varibale.csv", row.names = F)

# TODO: try investment and employment seperately
# TODO: try some of the policing vars seperately

# TODO: Principal products of the business - probably too messy to work with but sort of interesting 
  # Abarrotes??

# TODO: Age of the business (start year) - P3 - probably not useful




##### #5. Model w added binary protect var #####

m.protect = main %>%
  select(victim2, P4, SECTOR_FIN, TIPO, E23, protect2) 

model = glm(victim2 ~ ., family = binomial(link = "logit"), data = m.protect)

model.an = anova(model)
model.sum =  summary(model)
results = summary.glm(model)$coefficients
write.csv(results, "~/Resultados/LM 640 protection logit regression results 5.csv")
write.csv(model.an, "~/Resultados/LM 640 protection logit regression anova 5.csv")

##### #6. Model w added binary protect var - categorical size var #####

##### #7. Model w added binary protect var - microenterprises #####

##### #8. Model w categorical protect vars #####

m.protect = main %>%
  select(victim2, P4, SECTOR_FIN, TIPO, E23, equip2, person2, loc) 

model = glm(victim2 ~ ., family = binomial(link = "logit"), data = m.protect)

model.an = anova(model)
model.sum =  summary(model)
results = summary.glm(model)$coefficients
write.csv(results, "~/Resultados/LM 640 protection logit regression results 8.csv")
write.csv(model.an, "~/Resultados/LM 640 protection logit regression anova 8.csv")

# equip2 and person2 are significant and positive - most likely protection followed victimization

# TODO: test w lagged victimization var

##### #9. Model w added binary gov var #####

m.gov = main %>%
  select(victim2, P4, SECTOR_FIN, TIPO, E23, gov2) 

model = glm(victim2 ~ ., family = binomial(link = "logit"), data = m.gov)

model.an = anova(model)
model.sum =  summary(model)
results = summary.glm(model)$coefficients
write.csv(results, "~/Resultados/LM 640 government action logit regression results 9.csv")
write.csv(model.an, "~/Resultados/LM 640 government action logit regression anova 9.csv")

# government action overall is insignificant, what about 3 categories?

##### #10. Model w categorical gov vars #####

m.gov = main %>%
  select(victim2, P4, SECTOR_FIN, TIPO, E23, wfg2, police2, sbe2) 

model = glm(victim2 ~ ., family = binomial(link = "logit"), data = m.gov)

model.an = anova(model)
model.sum =  summary(model)
results = summary.glm(model)$coefficients
write.csv(results, "~/Resultados/LM 640 government action logit regression results 10.csv")
write.csv(model.an, "~/Resultados/LM 640 government action logit regression anova 10.csv")

# wfg2 is significant, with an inverse relationship
# we can take wfg2 as a proxy for service delivery, one of the key components of positive peace

# sbe2 has a positive relationship - likely because crime is economic 

# police is insignificant, which supports our narrative that police are ineffective

# # refine using business size vars
# 
# # categorical size var
# m.gov = main %>%
#   select(victim2, size, SECTOR_FIN, TIPO, E23, wfg2, police2, sbe2) 
# 
# model = glm(victim2 ~ ., family = binomial(link = "logit"), data = m.gov)
# 
# model.an = anova(model)
# model.sum =  summary(model)
# results = summary.glm(model)$coefficients
# write.csv(results, "~/Resultados/LM 640 government action logit regression results 11.csv")
# write.csv(model.an, "~/Resultados/LM 640 government action logit regression results 11.csv")
# 
# # microenterprise binary var
# m.gov = main %>%
#   select(victim2, micro, SECTOR_FIN, TIPO, E23, wfg2, police2, sbe2) 
# 
# model = glm(victim2 ~ ., family = binomial(link = "logit"), data = m.gov)
# 
# model.an = anova(model)
# model.sum =  summary(model)
# results = summary.glm(model)$coefficients
# write.csv(results, "~/Resultados/LM 640 government action logit regression results 12.csv")
# write.csv(model.an, "~/Resultados/LM 640 government action logit regression results 12.csv")

##### #13. Mixed Model ####

m.mixed = main %>%
  select(victim2, size, SECTOR_FIN, TIPO, E23, wfg2, sbe2, protect2) 

model = glm(victim2 ~ ., family = binomial(link = "logit"), data = m.mixed)

model.an = anova(model)
model.sum =  summary(model)
results = summary.glm(model)$coefficients
write.csv(results, "~/Resultados/LM 640 mixed var logit regression results 13.csv")
write.csv(model.an, "~/Resultados/LM 640 mixed var logit regression anova 13.csv")

# sbe is still positive, but the coefficient is reduced
# E23 is no longer significant

m.mixed = main %>%
  select(victim2, size, SECTOR_FIN, TIPO, wfg2, sbe2, protect2) 

model = glm(victim2 ~ ., family = binomial(link = "logit"), data = m.mixed)

model.an = anova(model)
model.sum =  summary(model)
results = summary.glm(model)$coefficients
write.csv(results, "~/Resultados/LM 640 mixed var logit regression results 14.csv")
write.csv(model.an, "~/Resultados/LM 640 mixed var logit regression anova 14.csv")

# every variable is significant in v14


##### #15. Extortion #####

m.mixed = main %>%
  select(P25_10, size, SECTOR_FIN, TIPO, wfg2, sbe2, protect2) 

model = glm(P25_10 ~ ., family = binomial(link = "logit"), data = m.mixed)

model.an = anova(model)
model.sum =  summary(model)
results = summary.glm(model)$coefficients
write.csv(results, "~/Resultados/LM 640 mixed var logit regression results 15.csv")
write.csv(model.an, "~/Resultados/LM 640 mixed var logit regression anova 15.csv")

# results are very similar for extortion as dependent var
# TODO: write up a couple iterations
  # we have the number of extortions as well

##### Summary Statistics #####

states = main %>%
  select(CVE_ENT, P25_1:P26_14) %>%
  group_by(CVE_ENT) %>%
  mutate_each_(tally(), P25_1:P26_14)

##### # TODO: #####
  #ggplot2
  #aod
  #Rcpp
  #wald.test()
  #confint() - dont need a package
  #chisq.test - overall utility of each
  #code to write results to a worksheet instead of a csv?
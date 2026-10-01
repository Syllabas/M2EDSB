###############################################################################
#  TP : Analyse de survie sur le logiciel R  (Kaplan-Meier, actuariel,
#       Nelson-Aalen, test du Log-Rank)
#  Script associe aux diapos "TP_R_LOGRANK_2026"
#
#  Donnees : baseline.csv (750 sujets, 12 variables)
#  -> Il suffit de modifier le chemin du fichier a la section 2 puis de lancer
#     tout le script (Ctrl + Shift + Entree dans RStudio).
###############################################################################


# =============================================================================
# 1. INSTALLATION / CHARGEMENT DES PACKAGES
# =============================================================================
# Liste des packages utilises dans le TP :
#   dplyr     : requetes et manipulation de donnees (select, filter, mutate...)
#   lubridate : gestion des dates
#   stringr   : gestion des variables caracteres
#   janitor   : clean_names() pour nettoyer les noms de variables
#               (utilise dans les diapos mais absent de la liste d'installation)
#   gtsummary : tableaux descriptifs (tbl_summary)
#   ggplot2   : graphiques
#   ggsurvfit : graphiques de survie (alternative a survminer)
#   survival  : Surv(), survfit(), survdiff()  -> coeur de l'analyse de survie
#   survminer : ggsurvplot(), surv_pvalue()
#   biostat3  : lifetab2() -> table de survie actuarielle
packages <- c("dplyr", "lubridate", "stringr", "janitor", "gtsummary",
              "ggplot2", "ggsurvfit", "survival", "survminer", "biostat3")

# On n'installe que les packages qui ne sont pas deja presents
a_installer <- packages[!packages %in% rownames(installed.packages())]
if (length(a_installer) > 0) {
  install.packages(a_installer, dependencies = TRUE)
}

library(dplyr)
library(lubridate)
library(stringr)
library(janitor)
library(gtsummary)
library(ggplot2)
library(ggsurvfit)
library(survival)
library(survminer)
library(biostat3)

# Pour l'aide sur dplyr :
# ?dplyr
# browseVignettes(package = "dplyr")


# =============================================================================
# 2. IMPORT DE LA BASE DE DONNEES
# =============================================================================
# Selon le format du fichier :
#   - CSV       : read.csv()  (ou read.csv2() si le separateur est ";")
#   - SAS       : haven::read_sas()
#   - XLS/XLSX  : readxl::read_excel()

# >>> A MODIFIER : chemin vers le fichier (utiliser des "/" et pas des "\") <<<
chemin_fichier <- "chemin/baseline.csv"

baseline <- read.csv(file = chemin_fichier, stringsAsFactors = FALSE)

# Securite : si le CSV a ete enregistre avec Excel en francais, le separateur
# est ";" -> read.csv ne lit alors qu'une seule colonne. On relit avec read.csv2.
if (ncol(baseline) == 1) {
  baseline <- read.csv2(file = chemin_fichier, stringsAsFactors = FALSE)
}


# =============================================================================
# 3. REGARDER LE CONTENU DE LA BASE
# =============================================================================
str(baseline)     # type de chaque variable
head(baseline)    # premieres lignes
# View(baseline)  # visualisation dans RStudio

# R est sensible a la casse ! (DATINC != datinc)
# clean_names() met tous les noms en minuscules "snake_case" :
#   DATENAIS -> datenais, DATINC -> datinc, DATDC -> datdc,
#   DatContact -> dat_contact, Age0c3 -> age0c3 ...
baseline <- clean_names(baseline)
names(baseline)

# --- Conversion des dates -----------------------------------------------------
# Avec read.csv, les dates sont lues comme du TEXTE (chr) et non comme des Date.
# Il faut donc les convertir, sinon les calculs de delais seront impossibles.
# parse_date_time() (lubridate) accepte plusieurs formats :
#   "2001-01-03" (annee-mois-jour) ou "03/01/2001" (jour/mois/annee).
# Les chaines vides "" deviennent NA.
convertir_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- na_if(str_trim(as.character(x)), "")
  as.Date(parse_date_time(x, orders = c("Ymd", "dmY", "dmy"), quiet = TRUE))
}

baseline <- baseline %>%
  mutate(across(c(datenais, datinc, datdc, dat_contact), convertir_date))

str(baseline)   # les 4 variables de dates doivent maintenant etre de type Date


# =============================================================================
# 4. LES 3 FONCTIONS A CONNAITRE DE DPLYR : select / filter / mutate
# =============================================================================
# - Travailler sur une COPIE      : table_new <- baseline %>% ...
# - Travailler sur la MEME table  : baseline  <- baseline %>% ...

# ---- 4.1 select() : selectionner des variables (colonnes) --------------------
# Nouvelle table "baseline_reduit" contenant uniquement position, sexe,
# poids0 et taille0
baseline_reduit <- baseline %>% select(position, sexe, poids0, taille0)

# Meme chose + toutes les variables qui commencent par "med"
# (s'il n'y en a pas dans la base, starts_with() ne renvoie simplement rien)
baseline_reduit <- baseline %>%
  select(position, sexe, poids0, taille0, starts_with("med"))

# ---- 4.2 filter() : selectionner des observations (lignes) -------------------
# Exemple : uniquement les sujets de 75 ans ou plus a l'inclusion
baseline_75plus <- baseline %>% filter(age0 >= 75)

# ---- 4.3 mutate() : creer / modifier des variables ---------------------------
# Nouvelle variable continue : IMC = poids (kg) / taille (m)^2
baseline <- baseline %>% mutate(bmi0 = poids0 / (taille0 / 100)^2)

# Gestion des donnees manquantes : on ne calcule l'IMC que si poids0 ET
# taille0 sont renseignes, sinon IMC = NA
baseline <- baseline %>%
  mutate(bmi0_NA = if_else(!is.na(poids0) & !is.na(taille0),
                           poids0 / (taille0 / 100)^2,
                           NA_real_))

# Decouper une variable continue en classes avec case_when() :
#   si IMC manquant -> NA ; sinon si < 25 -> 1 ; sinon si < 30 -> 2 ; sinon 3
baseline <- baseline %>%
  mutate(bmi_cl = case_when(is.na(bmi0_NA) ~ NA_real_,
                            bmi0_NA < 25   ~ 1,
                            bmi0_NA < 30   ~ 2,
                            TRUE           ~ 3))

# Et tout dans la meme instruction ! (les classes sont ici codees 0 / 1 / 2)
baseline <- baseline %>%
  mutate(bmi0_NA  = if_else(!is.na(poids0) & !is.na(taille0),
                            poids0 / (taille0 / 100)^2, NA_real_),
         bmi0     = round(bmi0_NA, 2),
         bmi_cl   = case_when(is.na(bmi0_NA) ~ NA_real_,
                              bmi0_NA < 25   ~ 0,
                              bmi0_NA < 30   ~ 1,
                              TRUE           ~ 2),
         bmi_cl_f = factor(bmi_cl, levels = c(0, 1, 2),
                           labels = c("Normal", "Surpoids", "Obese")))

# On supprime la variable intermediaire
baseline <- baseline %>% select(-bmi0_NA)

head(baseline)


# =============================================================================
# 5. PARENTHESE : FUSION DES TABLES (exemples sur de petites tables fictives)
# =============================================================================
# ---- 5.1 Fusion horizontale (on ajoute des COLONNES via un identifiant) ------
table_a <- data.frame(ident = c("A", "B", "C"), var1 = c(1, 2, 3))
table_b <- data.frame(ident = c("A", "B", "D"), var2 = c(10, 20, 40))

inner_join(table_a, table_b, by = c("ident" = "ident"))  # ids communs aux 2
full_join (table_a, table_b, by = c("ident" = "ident"))  # tous les ids
left_join (table_a, table_b, by = c("ident" = "ident"))  # ids de la table gauche
right_join(table_a, table_b, by = c("ident" = "ident"))  # ids de la table droite
# Avec les vraies tables : Table_h <- left_join(baseline, deces, by = c("ident" = "ident"))

# ---- 5.2 Fusion verticale (on empile des LIGNES) ----------------------------
tab_base   <- data.frame(id_baseline = c("A", "B", "C"),
                         poids0 = c(70, 80, 65), pas0 = c(5000, 7000, 3000))
tab_suivi1 <- data.frame(id_suivi1 = c("A", "B", "C"),
                         poids1 = c(68, 82, 66), pas1 = c(5500, 6000, 3500))

table_v <- bind_rows(
  tab_base %>%
    select(id = id_baseline, poids = poids0, pas = pas0) %>%
    mutate(visite = 0),
  tab_suivi1 %>%
    select(id = id_suivi1, poids = poids1, pas = pas1) %>%
    mutate(visite = 1)
) %>%
  arrange(id, visite)
table_v


# =============================================================================
# 6. VERIFIER LE FORMAT DES VARIABLES (variables qualitatives -> factor)
# =============================================================================
baseline <- baseline %>%
  mutate(age0c3  = factor(age0c3, levels = c(0, 1, 2),
                          labels = c("<75", "[75-85[", ">=85")),
         nivetud = as.factor(nivetud),
         centre  = as.factor(centre),
         bmi_cl  = as.factor(bmi_cl),
         sexe    = factor(sexe, levels = c(1, 2),
                          labels = c("homme", "femme")))

str(baseline)
table(baseline$sexe, useNA = "ifany")   # verification du recodage


# =============================================================================
# 7. DESCRIPTION DE LA POPULATION : tbl_summary (gtsummary)
# =============================================================================
# Description globale
baseline %>%
  select(age0, age0c3, sexe, nivetud, centre, bmi_cl_f) %>%
  tbl_summary(statistic = list(all_continuous()  ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} / {N} ({p}%)"),
              missing = "no")

# Description par sexe + p-value des tests de comparaison
baseline %>%
  select(age0, age0c3, sexe, nivetud, centre, bmi_cl_f) %>%
  tbl_summary(by = sexe,
              statistic = list(all_continuous()  ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} / {N} ({p}%)"),
              missing = "no") %>%
  add_p()


###############################################################################
#                         ANALYSE DE SURVIE
###############################################################################

# =============================================================================
# OBJECTIF 1 : VARIABLES POUR L'ANALYSE DE SURVIE
# =============================================================================
# Il faut 3 variables :
#   - l'evenement (0/1)                     -> statut
#   - la date de dernieres nouvelles (DDN)  -> datdn
#   - le delai jusqu'a l'evenement          -> deldninc (deces OU censure)
# Attention : ici on n'utilise PAS de "date de point" pour construire le delai.

# Evenement : 1 = decede (date de deces renseignee), 0 = censure
baseline <- baseline %>%
  mutate(statut = as.numeric(if_else(!is.na(datdc), 1, 0)))

# Date de dernieres nouvelles :
#   - date de deces si le sujet est decede
#   - sinon date du dernier contact
# (les variables etant deja au format Date, pas besoin de preciser de format)
baseline <- baseline %>%
  mutate(datdn = if_else(!is.na(datdc), datdc, dat_contact))

# Delai entre l'inclusion et les dernieres nouvelles, en ANNEES
baseline <- baseline %>%
  mutate(deldninc = as.numeric(datdn - datinc) / 365.25)

# /!\ Verifications indispensables avant toute analyse de survie /!\
table(baseline$statut, useNA = "ifany")    # nombre de deces / censures
summary(baseline$deldninc)                 # pas de NA ni de valeurs aberrantes ?
sum(baseline$deldninc < 0, na.rm = TRUE)   # delais negatifs = erreur de dates
sum(is.na(baseline$deldninc))              # delais manquants

# On exclut d'eventuels delais manquants ou negatifs (normalement 0 ligne)
baseline <- baseline %>% filter(!is.na(deldninc), deldninc >= 0)


# =============================================================================
# OBJECTIF 2 : CONSTRUCTION DE COURBES DE SURVIE (equivalent PROC LIFETEST)
# =============================================================================

# -----------------------------------------------------------------------------
# 2.1 Estimateur de KAPLAN-MEIER
# -----------------------------------------------------------------------------
# Creation de l'objet Surv : un "+" apres un temps indique une censure
surv_object <- Surv(time = baseline$deldninc, event = baseline$statut)
summary(surv_object)

# Ajustement de la courbe de Kaplan-Meier (~ 1 = pas de groupe)
fit <- survfit(Surv(deldninc, statut) ~ 1, data = baseline)
# (equivalent : fit <- survfit(surv_object ~ 1, data = baseline))
str(fit)   # time, n.risk, n.event, n.censor, surv, std.err, lower, upper...

# Trace simple
ggsurvplot(fit, data = baseline)

# C'est plus joli et complet comme ca !
ggsurvplot(fit,
           data             = baseline,
           risk.table       = TRUE,           # tableau des effectifs a risque
           conf.int         = TRUE,           # intervalle de confiance a 95 %
           ggtheme          = theme_minimal(),
           surv.median.line = "hv",           # ligne de la mediane de survie
           xlab             = "Temps (annees)",
           ylab             = "Probabilite de survie",
           title            = "Courbe de survie de Kaplan-Meier")

# Alternative avec le package ggsurvfit
survfit2(Surv(deldninc, statut) ~ 1, data = baseline) %>%
  ggsurvfit() +
  add_confidence_interval() +
  add_risktable() +
  labs(x = "Temps (annees)", y = "Probabilite de survie")

# ---- Combien de deces recense-t-on ? ----------------------------------------
print(fit)              # colonne "events"
sum(fit$n.event)        # nombre de deces    (diapo : 282)
sum(fit$n.censor)       # nombre de censures (diapo : 468)

# ---- Temps median de survie -------------------------------------------------
# Temps auquel la probabilite de survie passe sous 50 % (diapo : 11,83 ans)
print(fit)
quantile(fit, probs = 0.5)$quantile

# ---- Bonus : temps de suivi median (methode de Kaplan-Meier inverse) --------
# Le "vrai" suivi median s'obtient en inversant le role deces / censure
fit_suivi <- survfit(Surv(deldninc, 1 - statut) ~ 1, data = baseline)
quantile(fit_suivi, probs = 0.5)$quantile

# Probabilites de survie a 3, 6 et 9 ans
summary(fit, times = c(3, 6, 9))


# -----------------------------------------------------------------------------
# 2.2 Estimateur ACTUARIEL (survie par intervalles, pas de fonction en escalier)
# -----------------------------------------------------------------------------
# Intervalles de 1 an, de 0 a 12 ans
result_table <- lifetab2(Surv(deldninc, statut) ~ 1, data = baseline,
                         breaks = 0:12)
result_table
# Colonnes : tstart, tstop, nsubs (sujets en debut d'intervalle), nlost
# (censures), nrisk (effectif a risque corrige = nsubs - nlost/2), nevent,
# surv (survie en DEBUT d'intervalle), pdf, hazard, se.surv, se.pdf, se.hazard

# CPF (Conditional Probability of Failure) = Nb evenements / Nb a risque
#  = probabilite de deceder dans l'intervalle sachant qu'on a survecu jusque la
result_table$cpf <- result_table$nevent / result_table$nrisk
#  PDF    = Survie * CPF
#  Hazard = (2 * CPF) / (1 + (1 - CPF))   (deja calcules par lifetab2)

# Milieu de chaque intervalle (pour tracer le risque instantane)
result_table$temps <- (result_table$tstart + result_table$tstop) / 2

# Courbe de survie actuarielle
# NB : "surv" est la survie au DEBUT de l'intervalle -> on la place en tstart
# (la derniere ligne "12-Inf" donne la survie a 12 ans)
ggplot(result_table, aes(x = tstart, y = surv)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = surv - se.surv, ymax = surv + se.surv),
              fill = "blue", alpha = 0.2) +
  labs(title = "Courbe de survie actuarielle",
       x = "Temps (annees)", y = "Probabilite de survie") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Fonction de risque (hazard) estimee
# (on retire la derniere ligne "12-Inf" dont le hazard vaut NA)
# Correction par rapport a la diapo : ymax = hazard + se.hazard (et pas surv + ...)
ggplot(filter(result_table, is.finite(hazard)), aes(x = temps, y = hazard)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = hazard - se.hazard, ymax = hazard + se.hazard),
              fill = "blue", alpha = 0.2) +
  labs(title = "Fonction de risque estimee (actuariel)",
       x = "Temps (annees)", y = "Risque instantane") +
  theme_minimal() +
  theme(legend.title = element_blank())


# -----------------------------------------------------------------------------
# 2.3 Estimateur de NELSON-AALEN (risque cumule)
# -----------------------------------------------------------------------------
fit_NA <- survfit(Surv(deldninc, statut) ~ 1, data = baseline)

# Risque cumule de Nelson-Aalen : H(t) = somme des (d_i / n_i)
naest <- cumsum(fit_NA$n.event / fit_NA$n.risk)
# (identique a fit_NA$cumhaz calcule automatiquement par survfit)
all.equal(naest, fit_NA$cumhaz)

# Creation d'un data.frame pour ggplot
data_plot <- data.frame(time              = fit_NA$time,
                        survie            = fit_NA$surv,
                        cumulative_hazard = naest)

# Trace de la courbe
ggplot(data = data_plot, aes(x = time, y = cumulative_hazard)) +
  geom_step() +
  labs(title = "Risque cumule de Nelson-Aalen",
       x = "Temps (annees)", y = "Estimateur de Nelson-Aalen") +
  theme_minimal()


# =============================================================================
# OBJECTIF 3 : PLUSIEURS COURBES DE SURVIE ET COMPARAISON (Kaplan-Meier)
# =============================================================================

# -----------------------------------------------------------------------------
# 3.1 Deux courbes de survie : variable qualitative a 2 modalites (sexe)
# -----------------------------------------------------------------------------
# ---- Ecriture du modele (attention : Surv avec un S majuscule !) ------------
fit_sexe <- survfit(Surv(deldninc, statut) ~ sexe, data = baseline)
fit_sexe   # n, events, mediane de survie et IC 95 % pour chaque groupe

# ---- Verification de l'hypothese des risques proportionnels -----------------
# Graphe log(-log(S(t))) en fonction de log(t) : les courbes doivent etre
# a peu pres PARALLELES (et ne pas se croiser) pour que le log-rank soit valide.

# On extrait les donnees dans un data.frame
surv_data <- data.frame(
  time   = fit_sexe$time,
  surv   = fit_sexe$surv,
  # fit_sexe$strata contient le nombre de lignes de chaque groupe :
  # on repete le nom de chaque groupe autant de fois que necessaire
  strata = rep(names(fit_sexe$strata), fit_sexe$strata)
)

# Calcul des coordonnees x et y
surv_data$log_neg_log_survival <- log(-log(surv_data$surv))
surv_data$log_time             <- log(surv_data$time)

# On retire les valeurs infinies (S(t) = 1 ou S(t) = 0)
surv_data <- surv_data %>% filter(is.finite(log_neg_log_survival),
                                  is.finite(log_time))

ggplot(surv_data, aes(x = log_time, y = log_neg_log_survival, color = strata)) +
  geom_line() +
  geom_point() +
  labs(title = "Log(-Log(Survie)) selon le sexe",
       x = "log(Temps)", y = "Log(-Log(S(t)))") +
  theme_minimal() +
  theme(legend.title = element_blank())
# -> Si les courbes sont paralleles, on peut se fier au test du Log-Rank

# ---- Test du Log-Rank -------------------------------------------------------
result_logrank_sexe <- survdiff(Surv(deldninc, statut) ~ sexe, data = baseline)
result_logrank_sexe
# Observed = deces observes, Expected = deces attendus sous H0
# Chisq a 1 ddl et p-value (diapo : p = 4e-06 -> courbes significativement
# differentes)

# Pour recuperer la p-value seule :
1 - pchisq(result_logrank_sexe$chisq, df = length(result_logrank_sexe$n) - 1)

# ---- Trace des courbes de survie --------------------------------------------
ggsurvplot(fit_sexe,
           data       = baseline,
           risk.table = TRUE,
           conf.int   = TRUE,
           pval       = TRUE,           # p-value du log-rank sur le graphe
           legend.labs = levels(baseline$sexe),
           xlab       = "Temps (annees)",
           ylab       = "Probabilite de survie")
# - Ecart VERTICAL entre les courbes = difference de probabilite de survie a un
#   instant donne
# - Ecart HORIZONTAL au niveau de 0.5 = difference des temps medians de survie
# -> La survie chez les femmes est plus elevee que chez les hommes


# -----------------------------------------------------------------------------
# 3.2 Plus de 2 courbes : variable qualitative a 3 modalites (nivetud)
# -----------------------------------------------------------------------------
fit_nivetud <- survfit(Surv(deldninc, statut) ~ nivetud, data = baseline)
fit_nivetud

# ---- Verification de l'hypothese des risques proportionnels -----------------
surv_data_etud <- data.frame(
  time   = fit_nivetud$time,
  surv   = fit_nivetud$surv,
  strata = rep(names(fit_nivetud$strata), fit_nivetud$strata)
)
surv_data_etud$log_neg_log_survival <- log(-log(surv_data_etud$surv))
surv_data_etud$log_time             <- log(surv_data_etud$time)
surv_data_etud <- surv_data_etud %>% filter(is.finite(log_neg_log_survival),
                                            is.finite(log_time))

ggplot(surv_data_etud,
       aes(x = log_time, y = log_neg_log_survival, color = strata)) +
  geom_line() +
  geom_point() +
  labs(title = "Log(-Log(Survie)) selon le niveau d'etudes",
       x = "log(Temps)", y = "Log(-Log(S(t)))") +
  theme_minimal() +
  theme(legend.title = element_blank())
# -> Si les courbes se croisent, l'hypothese n'est pas verifiee et le log-rank
#    est "pas pertinent" (cf. diapo)

# ---- Test du Log-Rank -------------------------------------------------------
result_logrank_nivetud <- survdiff(Surv(deldninc, statut) ~ nivetud,
                                   data = baseline)
result_logrank_nivetud
# Chisq a 2 ddl (3 groupes - 1) ; diapo : courbes non significativement
# differentes

# ---- Trace des courbes de survie --------------------------------------------
ggsurvplot(fit_nivetud,
           data       = baseline,
           risk.table = TRUE,
           conf.int   = TRUE,
           pval       = TRUE)


# -----------------------------------------------------------------------------
# 3.3 Test du Log-Rank : plusieurs ponderations
# -----------------------------------------------------------------------------
# "survdiff"   : log-rank classique (poids = 1)
# "n"          : Gehan-Breslow (Wilcoxon generalise), poids = nb a risque
#                -> donne plus de poids aux evenements PRECOCES
# "sqrtN"      : Tarone-Ware, poids = racine du nb a risque
# "S1"         : Peto-Peto, poids = estimation de la survie
# "FH_p=1_q=1" : Fleming-Harrington (p = 1, q = 1) -> evenements intermediaires
surv_pvalue(fit_sexe, data = baseline, method = "survdiff")
surv_pvalue(fit_sexe, data = baseline, method = "n")
surv_pvalue(fit_sexe, data = baseline, method = "sqrtN")
surv_pvalue(fit_sexe, data = baseline, method = "S1")
surv_pvalue(fit_sexe, data = baseline, method = "FH_p=1_q=1")

# Recapitulatif dans un seul tableau
methodes <- c("survdiff", "n", "sqrtN", "S1", "FH_p=1_q=1")
bind_rows(lapply(methodes, function(m)
  surv_pvalue(fit_sexe, data = baseline, method = m)))

# On peut aussi afficher un test pondere directement sur le graphe :
# ggsurvplot(fit_sexe, data = baseline, pval = TRUE, log.rank.weights = "n")


# =============================================================================
# OBJECTIF 4 : LOG-RANK STRATIFIE
# =============================================================================
# Permet de tester l'effet de la variable d'interet (ici le sexe) en
# controlant l'effet d'une autre variable (ici le centre) qui pourrait masquer
# ou fausser cet effet.
# Question : y a-t-il une difference de survie entre hommes et femmes en
#            s'affranchissant de l'effet centre ?
result_logrank_sexe_centre <- survdiff(Surv(deldninc, statut) ~ sexe + strata(centre),
                                       data = baseline)
print(result_logrank_sexe_centre)
# Si p < 0.05 : difference significative de survie entre les sexes, meme en
# corrigeant l'effet "centre"


# =============================================================================
# ET SI... on veut tester l'effet d'une variable QUANTITATIVE ou de PLUSIEURS
# variables sur la survie ?  -> Modele de Cox (prochain TP) :
#   coxph(Surv(deldninc, statut) ~ age0 + sexe + centre, data = baseline)
# =============================================================================

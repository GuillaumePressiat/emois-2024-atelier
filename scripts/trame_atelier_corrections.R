


# Chargement des bibliothèques -----

library(pmeasyr)
library(tidyverse)


# Import des données -----

## Noyau de paramètres -----
#noyau_skeleton()
noyau_pmeasyr(
  finess = '290000017',
  annee  = 2023L,
  mois   = 12L,
  path   = 'data',
  tolower_names = TRUE
) -> p


## Dézippage -----
adezip(p, type = "out")

rsa <- irsa(p, typi = 1)

rsa <- irsa(p, typi = 4)

## Visualisation ----

View(rsa$rsa)

## Fichier tra -----
tra <- itra(p)

## Fichier ano -----

iano_mco(p) -> ano

# e-PMSI avec R -----

## Tableau [1.D.2.RTP] ----

rsa$rsa %>% 
  # group_by(moissor) %>%
  summarise(`Nb de RSA transmis` = n(),
            `Nb de RSA en CMD 90 (*)` = n_distinct(cle_rsa[rsacmd == "90"]),
            `Nb de RSA prestation inter-établissement` = n_distinct(cle_rsa[typesej == "B"]),
            `Nb de RSA en GHS 9999` = n_distinct(cle_rsa[noghs == "9999" & rsacmd != "90"]),
            `Nb de RSA séances` = n_distinct(cle_rsa[rsacmd == "28"]),
            `Nb de séances` = sum(nbseance[rsacmd == "28"]),
            `Nb de RSA DS=0` = n_distinct(cle_rsa[duree == 0 & rsacmd != "28" & rsacmd != "90"]),
            ` dont Nb de J ou T0` = n_distinct(cle_rsa[duree == 0 & rsacmd != "28" & rsacmd != "90" & rsacompx %in% c('J', 'T')]), # wrong
            `Nb de RSA hors séjour sans nuitée` = n_distinct(cle_rsa[duree > 0& rsacmd != "90"]), 
            `Nb de journées hors séjour sans nuitée` = sum(duree[rsacmd != "90"]),
            `Nb de RSA en UHCD réaffecté` = sum(uhcd == 1)
  ) %>% 
  tidyr::pivot_longer(everything(), #!moissor,
                      names_to = c("Rubrique"), 
                      values_to = "Année n")

# RSA hors périodes ? => définir les bonnes périodes
periodes_ok <- paste0("2023", stringr::str_pad(1:12, 2, "left", "0"))


## Tableau [1.D.2.ME/S] ----

# modes entrées / sorties
rsa$rsa %>% 
  filter(rsacmd != "90") %>% 
  mutate(mep = paste0(echpmsi, prov)) %>% 
  count(mep)

rsa$rsa %>% 
  filter(rsacmd != "90") %>% 
  mutate(msd = paste0(schpmsi, dest)) %>% 
  count(msd)

rsa$rsa %>% 
  filter(rsacmd != "90") %>% 
  mutate(mep = paste0(echpmsi, prov)) %>% 
  count(mep, cdpu) %>% 
  mutate(p = n / sum(n))

## Tableau [1.D.2.CMGS] ----
rsa$rsa %>% 
  group_by(anseqta, ghm) %>% 
  summarize(n = n(),
            dms = mean(duree[duree > 0]),
            n0 = sum(duree == 0L)) %>% 
  arrange(desc(n))

## Tableau [1.D.2.SUM] ----
rsa$rsa_um %>% 
  filter(substr(typaut1,1,2) %in% c('01', '02', '03', '04', '06', '13', '14', '16', '18')) %>% 
  count(typaut = substr(typaut1,1,2), natsupp1, wt = nbsupp1)

## Tableau [1.Q.1.EG] ----

eg <- ileg_mco(p, reshape = TRUE)

group_by(eg, eg) %>% 
  summarise(`Nb erreurs` = n(),
            `Nb séjours` = n_distinct(cle_rsa))

## Tableau [1.V.1.SV] ----

library(nomensland)
vrsa <- vvr_rsa(p)
vano <- vvr_ano_mco(p)

tarifs_ghs <- dplyr::distinct(get_table('tarifs_mco_ghs'), ghs, anseqta, .keep_all = TRUE)
tarifs_supp <- get_table('tarifs_mco_supplements') %>% mutate_if(is.numeric, tidyr::replace_na, 0) %>% 
  select(-cgeo)

rsa_valo <- vvr_mco(
  vvr_ghs_supp(rsa = vrsa, 
               tarifs = tarifs_ghs, 
               supplements =  tarifs_supp, 
               ano = vano, 
               full = FALSE,
               cgeo = 1L, 
               prudent = 1,
               bee = FALSE),
  vvr_mco_sv(vrsa, vano)
)

epmsi_mco_sv(rsa_valo)

## Tableau [1.V.1.RAV] ----
epmsi_mco_rav(rsa_valo)

## Tableau [1.V.2.VMED] ----
imed_mco(p) %>% filter(indication == 'IGLOUBI')

imed_mco(p) %>% filter(indication == 'IBOULGA')


# Module requêteur de pmeasyr -----

## Référentiels et libellés ----
library(nomensland)
View(rsa$actes)

### CCAM ----

ccam_actes <- get_table('ccam_actes')
rsa$actes %>% 
  count(cdccam, sort = TRUE) %>% 
  left_join(ccam_actes, by = c('cdccam' = 'code'))

### GHM ----
rgp_ghm <- get_table('ghm_ghm_regroupement')
rsa$rsa %>% 
  count(ghm, anseqta) %>% 
  left_join(rgp_ghm, by = c('ghm', 'anseqta'))


## Requêtes ----

dico <- get_dictionnaire_listes()

rsa <- irsa(p, typi = 6, tolower_names = TRUE)
rsa <- prepare_rsa(rsa)

### Requête : Recours exceptionnel ----

lancer_requete(rsa,
               get_all_listes("Recours Exceptionnel"),
               vars = c('duree', 'ghm', 'actes')
)

### Requête : Chirurgie bariatrique ----

lancer_requete(rsa,
               get_all_listes("Chirurgie bariatrique"),
               vars = c('duree', 'ghm', 'actes')
)

### Requête : 55 gestes marqueurs ----

lancer_requete(rsa,
               get_all_listes("Chir ambu : 55 GM"),
               vars = c('duree', 'ghm', 'actes')
)


### Requête : ad hoc ----

requete_complexe = list(diags = "E66", 
                        positions_diags = 5, 
                        dureemax = 0,
                        ghm = '28Z',
                        agemin = 59,
                        agemax = 89,
                        diags_exclus = "C")

requete(rsa, requete_complexe, vars = c('ghm', 'agean'))

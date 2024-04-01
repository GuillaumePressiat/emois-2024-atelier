

# https://toudim.netlify.app/methodes/ipa/ipa

# Tableau [1.D.2.IPA] ----

library(pmeasyr)
library(tidyverse)

#noyau_skeleton()
noyau_pmeasyr(
  finess = '290000017',
  annee  = 2023L,
  mois   = 12L,
  path   = 'data',
  tolower_names = TRUE
) -> p


adezip(p, type = "out")

rsa <- irsa(p, typi = 6)

g0_i <- rsa$rsa %>% 
filter(rsacmd == '90' |
         typesej == 'B' |
         ghm %in% c('15Z10E', '14Z08Z', '09Z02A', '09Z02B', '23Z03Z')  | # | ,, 
         (ghm == '22Z03Z' & !(dp %in% c('L5120', 'T3131', 'T3141', 'T3151', 'T316', 'T317', 'T318', 'T319', 'T323', 'T324', 'T325', 'T326', 'T327', 'T328', 'T329')))
) %>% 
  select(cle_rsa)

g0_ii <- rsa$rsa_um %>% 
  filter(substr(typaut1, 1,2) == '07') %>% 
  select(cle_rsa) %>% 
  semi_join(rsa$rsa %>% 
              filter(nbrum == 1), by = join_by(cle_rsa))

g0 <- union_all(g0_i, g0_ii)

classes_ipa <- tibble::tribble(
  ~ var, ~ libelle,
  "groupe_a_1",   "Diagnostic prénatal",
  "groupe_a_2",   "Obstétrique",
  "groupe_a_3",   "Néonatologie et Réanimation Néonatale",
  "groupe_a_4",   "Activités cliniques d’assistance médicale à la procréation",
  "groupe_b_1",   "Greffes d’organes et de cellules hématopoïétiques",
  "groupe_b_2",   "Neurochirurgie",
  "groupe_b_3",   "Chirurgie cardiaque",
  "groupe_b_4",   "Grands brûlés",
  "groupe_b_5",   "Neuroradiologie interventionnelle",
  "groupe_c_71",  "Traitement du cancer : Chirurgie (séjours)",
  "groupe_c_72",  "Traitement du cancer : Chimiothérapie (séjours)",
  "groupe_c_73",  "Traitement du cancer : Chimiothérapie (séances)",
  "groupe_c_74",  "Traitement du cancer : Radiothérapie (séjours) ",
  "groupe_c_75",  "Traitement du cancer : Radiothérapie (séances)",
  "groupe_c_76",  "Cardiologie interventionnelle",
  "groupe_c_81",  "Dialyses : Activités dans le champ IPA",
  "groupe_c_82",  "Dialyses : Activités hors champ IPA",
  "groupe_d_091", "Activité interventionnelle (séjours)",
  "groupe_d_092", "Médecine hospitalisation partielle (séances)",
  "groupe_d_093", "Médecine hospitalisation partielle (séjours)",
  "groupe_d_094", "Médecine hospitalisation complète (séjours)",
  "groupe_d_101", "Chirurgie ambulatoire hors transferts, mutations ou décès",
  "groupe_d_102", "Chirurgie  en  hospitalisation  complète ou  sans  nuitée  avec  transfert,  mutation  ou décès"
)

# Groupe A : Activités autorisées dans le champ obstétrique / néonatologie


gA <- rsa$rsa %>% 
  select(cle_rsa, dp, rsacmd, nbsupnn1, nbsupnn2, nbsupnn3) %>% 
  mutate(
    groupe_a_1 = (substr(dp, 1,3) == 'Z36'),
    groupe_a_2 = (rsacmd == '14'),
    groupe_a_3 = (rsacmd == '15' | (nbsupnn1 + nbsupnn2 + nbsupnn3 > 0)),
    groupe_a_4 = (substr(dp, 1,3) == 'Z31' | dp == 'Z52801')
  ) %>% 
  filter((groupe_a_1 + groupe_a_2 + groupe_a_3 + groupe_a_4) > 0) %>% 
  select(cle_rsa, starts_with('groupe_a'))

gA %>% 
  anti_join(g0, by = "cle_rsa") %>% 
  tidyr::gather(var, val, - cle_rsa) %>% 
  filter(val > 0) %>% 
  arrange(cle_rsa, var) %>% 
  distinct(.keep_all = T) %>% 
  left_join(classes_ipa, by = 'var') %>% 
  count(var, libelle)


# Groupe B : Activités autorisées SIOS
gB_i <- rsa$rsa %>% 
  select(cle_rsa, rsacmd, ghm, dp) %>% 
  mutate(
    groupe_b_1 = (rsacmd == '27'),
    groupe_b_3 = (substr(ghm,1,5) %in% c('05C02', '05C03', '05C04', '05C05', '05C06', '05C07', '05C08', '05C09')), 
    groupe_b_4 = (substr(ghm,1,5) == '22Z02'  | (ghm == '22Z03Z' &
                                                   dp %in% c('L5120', 'T3131', 'T3141', 'T3151', 'T316', 'T317', 'T318', 'T319', 'T323', 'T324', 'T325', 'T326', 'T327', 'T328', 'T329')))
  ) %>% 
  filter((groupe_b_1 + groupe_b_3 + groupe_b_4) > 0) %>% 
  select(cle_rsa, starts_with('groupe_b'))

a033 <- readr::read_csv2('tools/a_033.csv', col_types = readr::cols(
  acte = readr::col_character(),
  phase = readr::col_character()
))

temp <- rsa$actes %>% 
  semi_join(a033, by = c('cdccam' = 'acte'), copy = TRUE) %>% # , 'phase' = 'phase'
  distinct(cle_rsa)

escape_06 <- gB_i %>% filter(groupe_b_3 == 1) %>% 
  inner_join(temp, by = 'cle_rsa')

gB_i <- gB_i %>% 
  anti_join(escape_06, by = c('cle_rsa', 'groupe_b_3'))

annexes <- 22:23 %>% purrr::map(function(an){
  readr::read_delim(paste0("tools/annexes_ipa_20", an, ".csv"), col_types = readr::cols(
    liste = readr::col_character(),
    titre = readr::col_character()), delim = ";", locale = readr::locale(encoding = "latin1")) %>% 
    mutate(anseqta = as.character(2000 + an))
}) %>% 
  bind_rows()

nri_a <- annexes %>% 
  filter(grepl('radiologie', titre)) %>% 
  distinct(liste, anseqta)

nri <- rsa$actes %>% 
  inner_join(rsa$rsa %>% 
               select(cle_rsa, anseqta), by = 'cle_rsa') %>% 
  inner_join(nri_a, by = c('cdccam' = 'liste', 'anseqta' = 'anseqta'), copy = TRUE) %>% 
  distinct(cle_rsa) %>% 
  mutate(groupe_b_5 = 1 )

nch_a <- annexes %>% 
  filter(grepl('chirurgie', titre)) %>% 
  distinct(liste, anseqta)


nch <- rsa$actes %>% 
  inner_join(rsa$rsa %>% 
               select(cle_rsa, anseqta), by = 'cle_rsa') %>% 
  inner_join(nch_a, by = c('cdccam' = 'liste', 'anseqta' = 'anseqta')) %>% 
  distinct(cle_rsa) %>% 
  mutate(groupe_b_2 = 1 )


gB <- bind_rows(gB_i, nri, nch)

gB[is.na(gB)] <- FALSE
gB %>%
  anti_join(bind_rows(gA, g0)) %>% 
  tidyr::gather(var, val, - cle_rsa) %>% 
  filter(val > 0) %>% 
  arrange(cle_rsa, var) %>% 
  distinct(.keep_all = T)  %>% 
  left_join(classes_ipa, by = 'var') %>% 
  count(var, libelle)  %>% 
  knitr::kable()


car_a <- annexes %>% 
  filter(grepl('cardio', titre)) %>% 
  distinct(liste, anseqta)

car <- rsa$actes %>% 
  inner_join(rsa$rsa %>% 
               select(cle_rsa, anseqta), by = 'cle_rsa') %>% 
  inner_join(car_a, by = c('cdccam' = 'liste', 'anseqta' = 'anseqta')) %>% 
  distinct(cle_rsa) %>% 
  mutate(groupe_c_76 = 1)

gC_i <- rsa$rsa %>% 
  select(cle_rsa, rsacmd,rsatype, ghm,noghs, dp, dr) %>% 
  mutate(groupe_c_71 = (rsatype == 'C' & (('C00' <= dp & dp < 'C98') | ('D00' <= dp & dp < 'D10') | ('D37' <= dp & dp < 'D49'))),
         groupe_c_72 = (substr(ghm,1,5) %in% c('17M05', '17M06')),
         groupe_c_73 = (substr(ghm,1,5) == '28Z07'),
         groupe_c_74 = (substr(ghm,1,5) %in% c('17K04', '17K05', '17K08', '17K09') & dp %in% c('Z5100', 'Z5101') & (('C00' <= dr & dr < 'C98') | ('D00' <= dr & dr < 'D10') | ('D37' <= dr & dr < 'D49'))),
         groupe_c_75 = (substr(ghm,1,5) %in% c('28Z10', '28Z11', '28Z18', '28Z19', '28Z20', '28Z21', '28Z22', '28Z23', '28Z24', '28Z25') & dp %in% c('Z5100', 'Z5101') & (('C00' <= dr & dr < 'C98') | ('D00' <= dr & dr < 'D10') | ('D37' <= dr & dr < 'D49'))),
         
         groupe_c_81 = (ghm == '28Z04Z' & noghs == '9605'),
         groupe_c_82 = ((ghm == '28Z04Z' & noghs %in% c('9617', '9999')) | (substr(ghm,1,5) %in% c('28Z01', '28Z02', '28Z03')))) %>% 
  filter(groupe_c_71 + groupe_c_72 + groupe_c_73 + groupe_c_74 + groupe_c_75 + groupe_c_81 + groupe_c_82 > 0) %>% 
  select(cle_rsa, starts_with('groupe_c'))


gC <- bind_rows(gC_i, car)

gC %>% 
  anti_join(bind_rows(gA)) %>% #gB g0, 
  tidyr::gather(var, val, - cle_rsa) %>% 
  filter(val > 0) %>% 
  arrange(cle_rsa, var) %>% 
  distinct(.keep_all = T)  %>% 
  left_join(classes_ipa, by = 'var') %>% 
  count(var, libelle)  %>% 
  knitr::kable()

# ...


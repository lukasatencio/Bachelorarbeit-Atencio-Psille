#Bachelorarbeit
#Daten matchen
#Autor: Atencio Psille, Lukas
#Datum: 13.07.2026
#=============================================================================================
#packages
library(pacman)
pacman::p_load(readxl, tidyverse, dplyr, gtools, MatchIt, dreamerr, fixest, writexl)
#Liste leeren
rm(list = ls())
#Gebiete definieren
RR <- c("05162", "05358", "05362", "05334", "05370", "05366", "05116")
LR <- c("12062", "12066", "12061", "12071", "12052", "14625", "14626")
MR <- c("14713", "14729", "14730", "15084", "15088", "15002", "15087", "15082")
kap2 <- c("03154", "03405", "05978", "05916", "05915", "05112", "05513", "13003",
          "13072", "10044", "10041", "16077")
#=============================================================================================
#Datensatz einlesen
df <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_unmatched.xlsx")
#Zuerst matching nur mit Kap. 1 InvKG
#jeweils mit Mahalanobis-Distanz für jede zielvariable
#Dafür Kap. 2 herausfiltern
matching_data_kap1 <- df %>%
  as.data.frame() %>%
  filter(!kz %in% kap2) %>%
  #Matching auf Basis von 2019
  #Letztes Jahr vor Einsetzen des Treatments
  filter(year == 2019) %>%
  drop_na(treat, east, herfindahl, z1a_GewerbePro10000, z2a_Sozialhilfe_Anteil,
          z3a_WanderungRelativ, c1_Bevölkerung, c2_BIP_pro_Kopf, c3_BWS, 
          c4_SVB, c5_ALO, c6_ALQ, c7_LangzeitALO, c7a_LangzeitALQ, 
          c8_Fläche, c9_jugend, c9a_alten, c11a_öffDienst_Q, c12_Erwerbstätige)
#Matchen
#Zuerst z1a
matchtest_kap1_z1a <- matchit(treat ~ GRW + herfindahl + z1a_GewerbePro10000 +
                            z2a_Sozialhilfe_Anteil + z3a_WanderungRelativ + log(c1_Bevölkerung) +
                            c2_BIP_pro_Kopf + c6_ALQ + c7a_LangzeitALQ + log(c8_Fläche) +
                            c9_jugend + c9a_alten + c11a_öffDienst_Q,
                          data = matching_data_kap1,
                          #nearest neighbor matching
                          method = "nearest",
                          distance = "glm",
                          link = "linear.logit",
                          #max SD des PS von 0.2
                          caliper = 0.2,
                          #east als exaktes Matching-Kriterium
                          exact = ~east,
                          #Pro Treatment-Kreis 3 Matching Partner
                          ratio = 3,
                          #Mit Replacement 
                          #(Ein Kontrolllandkreis kann mit mehreren Treatment-Kreisen gematcht werden)
                          replace = TRUE,
                          mahvars = ~ z1a_GewerbePro10000)

summary(matchtest_kap1_z1a)
#z2a, herfindahl, c2_BIP_pro_Kopf, c9a außerhalb Kriterien
#Auf 20 Treatment-Kreise kommen 41 gematchte Kontroll-Kreise
#Zwei Treatment-Kreise bleiben unmatched

matched_df_kap1_z1a <- match.data(matchtest_kap1_z1a) %>%
  #normalen PS extrahieren
  mutate(propensity_score = plogis(distance)) %>%
  relocate(kz, name, year, treat, east, propensity_score) %>%
  select(kz, name, year, propensity_score, weights, distance)

#in den normalen df übertragen
df_matched_kap1_z1a <- df %>%
  as.data.frame() %>%
  left_join(matched_df_kap1_z1a, by = c("kz", "name", "year")) %>%
  #DiD Variable erstellen
  mutate(post = if_else(year >= 2020, 1, 0),
         did_interact = post * treat,
         herfindahl = herfindahl * 100) %>%
  relocate(kz, name, year, post, treat, did_interact, east, propensity_score) %>%
  group_by(kz, name) %>%
  fill(weights, .direction = "updown") %>%
  filter(!is.na(weights)) %>%
  relocate(z3a_WanderungRelativ, .after = z3_Wanderung)
#==============================================================================
#jetzt mit z2a
#Kap 1
matchtest_kap1_z2a <- matchit(treat ~ GRW + herfindahl + z1a_GewerbePro10000 +
                                z2a_Sozialhilfe_Anteil + z3a_WanderungRelativ + log(c1_Bevölkerung) +
                                c2_BIP_pro_Kopf + c6_ALQ + c7a_LangzeitALQ + log(c8_Fläche) +
                                c9_jugend + c9a_alten + c11a_öffDienst_Q,
                              data = matching_data_kap1,
                              #nearest neighbor matching
                              method = "nearest",
                              distance = "glm",
                              link = "linear.logit",
                              #max SD des PS von 0.2
                              caliper = 0.2,
                              #east als exaktes Matching-Kriterium
                              exact = ~east,
                              #Pro Treatment-Kreis 3 Matching Partner
                              ratio = 3,
                              #Mit Replacement 
                              #(Ein Kontrolllandkreis kann mit mehreren Treatment-Kreisen gematcht werden)
                              replace = TRUE,
                              mahvars = ~ z2a_Sozialhilfe_Anteil)

summary(matchtest_kap1_z2a)
#GRW, c1, c2, c7a, c8, c9a (alle knapp) und herfindahl außerhalb Kriterien
#Auf 20 Treatment-Kreise kommen 43 gematchte Kontroll-Kreise
#Zwei Treatment-Kreis bleibt unmatched

matched_df_kap1_z2a <- match.data(matchtest_kap1_z2a) %>%
  #normalen PS extrahieren
  mutate(propensity_score = plogis(distance)) %>%
  relocate(kz, name, year, treat, east, propensity_score) %>%
  select(kz, name, year, propensity_score, weights, distance)

#in den normalen df übertragen
df_matched_kap1_z2a <- df %>%
  as.data.frame() %>%
  left_join(matched_df_kap1_z2a, by = c("kz", "name", "year")) %>%
  #DiD Variable erstellen
  mutate(post = if_else(year >= 2020, 1, 0),
         did_interact = post * treat,
         herfindahl = herfindahl * 100) %>%
  relocate(kz, name, year, post, treat, did_interact, east, propensity_score) %>%
  group_by(kz, name) %>%
  fill(weights, .direction = "updown") %>%
  filter(!is.na(weights)) %>%
  relocate(z3a_WanderungRelativ, .after = z3_Wanderung)
#==============================================================================
#jetzt mit z3a
#Kap 1
matchtest_kap1_z3a <- matchit(treat ~ GRW + herfindahl + z1a_GewerbePro10000 +
                                z2a_Sozialhilfe_Anteil + z3a_WanderungRelativ + log(c1_Bevölkerung) +
                                c2_BIP_pro_Kopf + c6_ALQ + c7a_LangzeitALQ + log(c8_Fläche) +
                                c9_jugend + c9a_alten + c11a_öffDienst_Q,
                              data = matching_data_kap1,
                              #nearest neighbor matching
                              method = "nearest",
                              distance = "glm",
                              link = "linear.logit",
                              #max SD des PS von 0.2
                              caliper = 0.2,
                              #east als exaktes Matching-Kriterium
                              exact = ~east,
                              #Pro Treatment-Kreis 3 Matching Partner
                              ratio = 3,
                              #Mit Replacement 
                              #(Ein Kontrolllandkreis kann mit mehreren Treatment-Kreisen gematcht werden)
                              replace = TRUE,
                              mahvars = ~ z3a_WanderungRelativ)

summary(matchtest_kap1_z3a)
#GRW, z2a, c1, c2, c6, c7a, c9a und herfindahl außerhalb Kriterien
#Auf 20 Treatment-Kreise kommen 43 gematchte Kontroll-Kreise
#Zwei Treatment-Kreise bleiben unmatched

matched_df_kap1_z3a <- match.data(matchtest_kap1_z3a) %>%
  #normalen PS extrahieren
  mutate(propensity_score = plogis(distance)) %>%
  relocate(kz, name, year, treat, east, propensity_score) %>%
  select(kz, name, year, propensity_score, weights, distance)

#in den normalen df übertragen
df_matched_kap1_z3a <- df %>%
  as.data.frame() %>%
  left_join(matched_df_kap1_z3a, by = c("kz", "name", "year")) %>%
  #DiD Variable erstellen
  mutate(post = if_else(year >= 2020, 1, 0),
         did_interact = post * treat,
         herfindahl = herfindahl * 100) %>%
  relocate(kz, name, year, post, treat, did_interact, east, propensity_score) %>%
  group_by(kz, name) %>%
  fill(weights, .direction = "updown") %>%
  filter(!is.na(weights)) %>%
  relocate(z3a_WanderungRelativ, .after = z3_Wanderung)
#===============================================================================
#===============================================================================
#Jetzt nur mit Kap. 2 Matchen
#nur mit kap2 matchen
#Ansonsten gleiches Prozedere
#erst mit z1a als mahvar
dfkap2 <- df %>%
  as.data.frame() %>%
  #Reviere herausfiltern
  filter(!kz %in% c(RR, LR, MR)) %>%
  filter(year == 2019) %>%
  drop_na(treat, east, herfindahl, z1a_GewerbePro10000, z2a_Sozialhilfe_Anteil,
          z3a_WanderungRelativ, c1_Bevölkerung, c2_BIP_pro_Kopf, c3_BWS, 
          c4_SVB, c5_ALO, c6_ALQ, c7_LangzeitALO, c7a_LangzeitALQ, 
          c8_Fläche, c9_jugend, c9a_alten, c11a_öffDienst_Q, c12_Erwerbstätige)

matchtest_kap2_z1a <- matchit(treat ~ GRW + herfindahl + z1a_GewerbePro10000 +
                            z2a_Sozialhilfe_Anteil + z3a_WanderungRelativ + log(c1_Bevölkerung) +
                            c2_BIP_pro_Kopf + c6_ALQ + c7a_LangzeitALQ + log(c8_Fläche) +
                            c9_jugend + c9a_alten + c11a_öffDienst_Q,
                          data = dfkap2,
                          #nearest neighbor matching
                          method = "nearest",
                          distance = "glm",
                          link = "linear.logit",
                          #max SD des PS von 0.2
                          caliper = 0.2,
                          #east als exaktes Matching-Kriterium
                          exact = ~east,
                          #Pro Treatment-Kreis 3 Matching Partner
                          ratio = 3,
                          #Mit Replacement 
                          #(Ein Kontrolllandkreis kann mit mehreren Treatment-Kreisen gematcht werden)
                          replace = TRUE,
                          mahvars = ~z1a_GewerbePro10000)

summary(matchtest_kap2_z1a)
#z2a, z3a, c2, c6, c7a, c8, c9a, c11a außerhalb der Kriterien
#Auf 12 Treatment-Kreise kommen 29 gematchte Kontrollkreise
#Kein Treatment Kreis bleibt unmatched

matched_df_kap2_z1a <- match.data(matchtest_kap2_z1a) %>%
  mutate(propensity_score = plogis(distance)) %>%
  relocate(kz, name, year, treat, east, propensity_score) %>%
  select(kz, name, year, propensity_score, weights, distance)

df_matched_kap2_z1a <- df %>%
  as.data.frame() %>%
  left_join(matched_df_kap2_z1a, by = c("kz", "name", "year")) %>%
  #DiD Variable erstellen
  mutate(post = if_else(year >= 2020, 1, 0),
         did_interact = post * treat,
         herfindahl = herfindahl * 100) %>%
  relocate(kz, name, year, post, treat, did_interact, east, propensity_score) %>%
  group_by(kz, name) %>%
  fill(weights, .direction = "updown") %>%
  filter(!is.na(weights)) %>%
  relocate(z3a_WanderungRelativ, .after = z3_Wanderung)
#===================================================================================
#jetzt mit z2a
matchtest_kap2_z2a <- matchit(treat ~ GRW + herfindahl + z1a_GewerbePro10000 +
                                z2a_Sozialhilfe_Anteil + z3a_WanderungRelativ + log(c1_Bevölkerung) +
                                c2_BIP_pro_Kopf + c6_ALQ + c7a_LangzeitALQ + log(c8_Fläche) +
                                c9_jugend + c9a_alten + c11a_öffDienst_Q,
                              data = dfkap2,
                              #nearest neighbor matching
                              method = "nearest",
                              distance = "glm",
                              link = "linear.logit",
                              #max SD des PS von 0.2
                              caliper = 0.2,
                              #east als exaktes Matching-Kriterium
                              exact = ~east,
                              #Pro Treatment-Kreis 3 Matching Partner
                              ratio = 3,
                              #Mit Replacement 
                              #(Ein Kontrolllandkreis kann mit mehreren Treatment-Kreisen gematcht werden)
                              replace = TRUE,
                              mahvars = ~z2a_Sozialhilfe_Anteil)

summary(matchtest_kap2_z2a)
#c11a außerhalb der Kriterien
#Auf 12 Treatment-Kreise kommen 27 gematchte Kontrollkreise
#Kein Treatment Kreis bleibt unmatched

matched_df_kap2_z2a <- match.data(matchtest_kap2_z2a) %>%
  mutate(propensity_score = plogis(distance)) %>%
  relocate(kz, name, year, treat, east, propensity_score) %>%
  select(kz, name, year, propensity_score, weights, distance)

df_matched_kap2_z2a <- df %>%
  as.data.frame() %>%
  left_join(matched_df_kap2_z2a, by = c("kz", "name", "year")) %>%
  #DiD Variable erstellen
  mutate(post = if_else(year >= 2020, 1, 0),
         did_interact = post * treat,
         herfindahl = herfindahl * 100) %>%
  relocate(kz, name, year, post, treat, did_interact, east, propensity_score) %>%
  group_by(kz, name) %>%
  fill(weights, .direction = "updown") %>%
  filter(!is.na(weights)) %>%
  relocate(z3a_WanderungRelativ, .after = z3_Wanderung)
#===================================================================================
#jetzt mit z3a
matchtest_kap2_z3a <- matchit(treat ~ GRW + herfindahl + z1a_GewerbePro10000 +
                                z2a_Sozialhilfe_Anteil + z3a_WanderungRelativ + log(c1_Bevölkerung) +
                                c2_BIP_pro_Kopf + c6_ALQ + c7a_LangzeitALQ + log(c8_Fläche) +
                                c9_jugend + c9a_alten + c11a_öffDienst_Q,
                              data = dfkap2,
                              #nearest neighbor matching
                              method = "nearest",
                              distance = "glm",
                              link = "linear.logit",
                              #max SD des PS von 0.2
                              caliper = 0.2,
                              #east als exaktes Matching-Kriterium
                              exact = ~east,
                              #Pro Treatment-Kreis 3 Matching Partner
                              ratio = 3,
                              #Mit Replacement 
                              #(Ein Kontrolllandkreis kann mit mehreren Treatment-Kreisen gematcht werden)
                              replace = TRUE,
                              mahvars = ~z3a_WanderungRelativ)

summary(matchtest_kap2_z3a)
#c1, c7a und c11a außerhalb der Kriterien
#Auf 12 Treatment-Kreise kommen 27 gematchte Kontrollkreise
#Kein Treatment Kreis bleibt unmatched

matched_df_kap2_z3a <- match.data(matchtest_kap2_z3a) %>%
  mutate(propensity_score = plogis(distance)) %>%
  relocate(kz, name, year, treat, east, propensity_score) %>%
  select(kz, name, year, propensity_score, weights, distance)

df_matched_kap2_z3a <- df %>%
  as.data.frame() %>%
  left_join(matched_df_kap2_z3a, by = c("kz", "name", "year")) %>%
  #DiD Variable erstellen
  mutate(post = if_else(year >= 2020, 1, 0),
         did_interact = post * treat,
         herfindahl = herfindahl * 100) %>%
  relocate(kz, name, year, post, treat, did_interact, east, propensity_score) %>%
  group_by(kz, name) %>%
  fill(weights, .direction = "updown") %>%
  filter(!is.na(weights)) %>%
  relocate(z3a_WanderungRelativ, .after = z3_Wanderung)
#===================================================================================
#====================================================================================
#Jetzt mit Kap.1 und Kap. 2 matchen
matching_data <- df %>% 
  filter(year == 2019) %>%
  drop_na(treat, east, herfindahl, z1a_GewerbePro10000, z2a_Sozialhilfe_Anteil,
          z3a_WanderungRelativ, c1_Bevölkerung, c2_BIP_pro_Kopf, c3_BWS, 
          c4_SVB, c5_ALO, c6_ALQ, c7_LangzeitALO, c7a_LangzeitALQ, 
          c8_Fläche, c9_jugend, c9a_alten, c11a_öffDienst_Q, c12_Erwerbstätige)
#matching
#zuerst z1a
matchtest_z1a <- matchit(treat ~ GRW + herfindahl + z1a_GewerbePro10000 +
                       z2a_Sozialhilfe_Anteil + z3a_WanderungRelativ + log(c1_Bevölkerung) +
                       c2_BIP_pro_Kopf + c6_ALQ + c7a_LangzeitALQ + log(c8_Fläche) +
                       c9_jugend + c9a_alten + c11a_öffDienst_Q,
                     data = matching_data,
                     #nearest neighbor matching
                     method = "nearest",
                     distance = "glm",
                     link = "linear.logit",
                     #max SD des PS von 0.2
                     caliper = 0.2,
                     #east als exaktes Matching-Kriterium
                     exact = ~east,
                     #Pro Treatment-Kreis 3 Matching Partner
                     ratio = 3,
                     #Mit Replacement 
                     #(Ein Kontrolllandkreis kann mit mehreren Treatment-Kreisen gematcht werden)
                     replace = TRUE,
                     mahvars = ~z1a_GewerbePro10000)
summary(matchtest_z1a)
#Alle Variablen innerhalb der Kriterien
#Auf 33 Treatment-Kreise kommen 61 gematchte Kontroll-Kreise
#Ein Treatment Kreis bleibt unmatched

matched_df_z1a <- match.data(matchtest_z1a) %>%
  mutate(propensity_score = plogis(distance)) %>%
  relocate(kz, name, year, treat, east, propensity_score) %>%
  select(kz, name, year, propensity_score, weights, distance)
#in den normalen df übertragen
df_matched_z1a <- df %>%
  as.data.frame() %>%
  left_join(matched_df_z1a, by = c("kz", "name", "year")) %>%
  mutate(post = if_else(year >= 2020, 1, 0),
         did_interact = post * treat,
         herfindahl = herfindahl * 100) %>%
  relocate(kz, name, year, post, treat, did_interact, east, propensity_score) %>%
  group_by(kz, name) %>%
  fill(weights, .direction = "updown") %>%
  filter(!is.na(weights)) %>%
  relocate(z3a_WanderungRelativ, .after = z3_Wanderung) %>%
  dplyr::select(-starts_with("geometry"))
#=========================================================================================
#jetzt mit z2a
matchtest_z2a <- matchit(treat ~ GRW + herfindahl + z1a_GewerbePro10000 +
                           z2a_Sozialhilfe_Anteil + z3a_WanderungRelativ + log(c1_Bevölkerung) +
                           c2_BIP_pro_Kopf + c6_ALQ + c7a_LangzeitALQ + log(c8_Fläche) +
                           c9_jugend + c9a_alten + c11a_öffDienst_Q,
                         data = matching_data,
                         #nearest neighbor matching
                         method = "nearest",
                         distance = "glm",
                         link = "linear.logit",
                         #max SD des PS von 0.2
                         caliper = 0.2,
                         #east als exaktes Matching-Kriterium
                         exact = ~east,
                         #Pro Treatment-Kreis 3 Matching Partner
                         ratio = 3,
                         #Mit Replacement 
                         #(Ein Kontrolllandkreis kann mit mehreren Treatment-Kreisen gematcht werden)
                         replace = TRUE,
                         mahvars = ~z2a_Sozialhilfe_Anteil)
summary(matchtest_z2a)
#c1 und c8 Variablen außerhalb der Kriterien
#Auf 33 Treatment-Kreise kommen 64 gematchte Kontroll-Kreise
#Ein Treatment Kreis bleibt unmatched

matched_df_z2a <- match.data(matchtest_z2a) %>%
  mutate(propensity_score = plogis(distance)) %>%
  relocate(kz, name, year, treat, east, propensity_score) %>%
  select(kz, name, year, propensity_score, weights, distance)
#in den normalen df übertragen
df_matched_z2a <- df %>%
  as.data.frame() %>%
  left_join(matched_df_z2a, by = c("kz", "name", "year")) %>%
  mutate(post = if_else(year >= 2020, 1, 0),
         did_interact = post * treat,
         herfindahl = herfindahl * 100) %>%
  relocate(kz, name, year, post, treat, did_interact, east, propensity_score) %>%
  group_by(kz, name) %>%
  fill(weights, .direction = "updown") %>%
  filter(!is.na(weights)) %>%
  relocate(z3a_WanderungRelativ, .after = z3_Wanderung) %>%
  dplyr::select(-starts_with("geometry"))
#=========================================================================================
#jetzt mit z3a
matchtest_z3a <- matchit(treat ~ GRW + herfindahl + z1a_GewerbePro10000 +
                           z2a_Sozialhilfe_Anteil + z3a_WanderungRelativ + log(c1_Bevölkerung) +
                           c2_BIP_pro_Kopf + c6_ALQ + c7a_LangzeitALQ + log(c8_Fläche) +
                           c9_jugend + c9a_alten + c11a_öffDienst_Q,
                         data = matching_data,
                         #nearest neighbor matching
                         method = "nearest",
                         distance = "glm",
                         link = "linear.logit",
                         #max SD des PS von 0.2
                         caliper = 0.2,
                         #east als exaktes Matching-Kriterium
                         exact = ~east,
                         #Pro Treatment-Kreis 3 Matching Partner
                         ratio = 3,
                         #Mit Replacement 
                         #(Ein Kontrolllandkreis kann mit mehreren Treatment-Kreisen gematcht werden)
                         replace = TRUE,
                         mahvars = ~z3a_WanderungRelativ)
summary(matchtest_z3a)
#GRW außerhalb Kriterien
#Auf 33 Treatment-Kreise kommen 63 gematchte Kontroll-Kreise
#Ein Treatment Kreis bleibt unmatched

matched_df_z3a <- match.data(matchtest_z3a) %>%
  mutate(propensity_score = plogis(distance)) %>%
  relocate(kz, name, year, treat, east, propensity_score) %>%
  select(kz, name, year, propensity_score, weights, distance)
#in den normalen df übertragen
df_matched_z3a <- df %>%
  as.data.frame() %>%
  left_join(matched_df_z3a, by = c("kz", "name", "year")) %>%
  mutate(post = if_else(year >= 2020, 1, 0),
         did_interact = post * treat,
         herfindahl = herfindahl * 100) %>%
  relocate(kz, name, year, post, treat, did_interact, east, propensity_score) %>%
  group_by(kz, name) %>%
  fill(weights, .direction = "updown") %>%
  filter(!is.na(weights)) %>%
  relocate(z3a_WanderungRelativ, .after = z3_Wanderung) %>%
  dplyr::select(-starts_with("geometry"))
#==========================================================================================
#Export
write_xlsx(df_matched_kap1_z1a, "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap1_z1a.xlsx")
write_xlsx(df_matched_kap1_z2a, "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap1_z2a.xlsx")
write_xlsx(df_matched_kap1_z3a, "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap1_z3a.xlsx")

write_xlsx(df_matched_kap2_z1a, "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap2_z1a.xlsx")
write_xlsx(df_matched_kap2_z2a, "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap2_z2a.xlsx")
write_xlsx(df_matched_kap2_z3a, "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap2_z3a.xlsx")

write_xlsx(df_matched_z1a, "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_z1a.xlsx")
write_xlsx(df_matched_z2a, "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_z2a.xlsx")
write_xlsx(df_matched_z3a, "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_z3a.xlsx")



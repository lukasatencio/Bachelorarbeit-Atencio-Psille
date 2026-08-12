#Bachelorarbeit
#Autor: Atencio Psille, Lukas
#Datum: 

rm(list=ls())
library(pacman)
install.packages("BiocManager")
library(BiocManager)
library(remotes)
remotes::install_version("perturb", version = "2.1-0")
library(perturb)
options(scipen = 999)
pacman::p_load(dplyr, tidyverse, haven, readxl, writexl, remotes, gtools, powerjoin, fixest,
               HonestDiD, stargazer, car, performance, patchwork, purrr, tidyr, mctest, multiColl)
#Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
#remotes::install_github("asheshrambachan/HonestDiD")
#=============================================================================================
#Daten einlesen
df_matched_kap1_z1a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap1_z1a.xlsx")
df_matched_kap1_z2a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap1_z2a.xlsx")
df_matched_kap1_z3a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap1_z3a.xlsx")

df_matched_kap2_z1a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap2_z1a.xlsx")
df_matched_kap2_z2a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap2_z2a.xlsx")
df_matched_kap2_z3a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap2_z3a.xlsx")

df_matched_z1a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_z1a.xlsx")
df_matched_z2a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_z2a.xlsx")
df_matched_z3a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_z3a.xlsx")

#===============================================================================
#twfe modelle
dfs <- list(df_matched_kap1_z1a = df_matched_kap1_z1a, df_matched_kap1_z2a = df_matched_kap1_z2a, df_matched_kap1_z3a = df_matched_kap1_z3a,
            df_matched_kap2_z1a = df_matched_kap2_z1a, df_matched_kap2_z2a = df_matched_kap2_z2a, df_matched_kap2_z3a = df_matched_kap2_z3a,
            df_matched_z1a = df_matched_z1a, df_matched_z2a = df_matched_z2a, df_matched_z3a = df_matched_z3a)
#kontrollvariablen aufs jahr 2019 fixieren
#c1_bevölkerung, c2_BIP_pro_Kopf, c7a_LangzeitALQ, c9a_alten, c6a_ALQ, c8_Fläche, c11_öffDienstQ
#das sind die kontrollvariablen die nicht das matchingkriterium erfüllen
kontroll <- c("c1_Bevölkerung", "c2_BIP_pro_Kopf", "c7a_LangzeitALQ", "c9a_alten", "c6_ALQ", "c8_Fläche", 
              "c11a_öffDienst_Q", "z2a_Sozialhilfe_Anteil", "z3a_WanderungRelativ")
dfs_2019 <- list()
for(df in seq_along(dfs)){
  df_temp <- dfs[[df]]
  
  for(i in kontroll){
    df_temp <- df_temp %>%
      group_by(kz) %>%
      mutate(across(all_of(kontroll), ~ .x[year == 2019][1], .names = "{.col}_2019")) %>%
      ungroup()
  }
  dfs_2019[[df]] <- df_temp
}
names(dfs_2019) <- names(dfs)
list2env(dfs_2019, envir = .GlobalEnv)


#zuerst ohne controlls, dann neighbor + GRW, dann alle controlls (die beim matching außerhalb der Kriterien waren)
#kap1
kap1_z1a <- feols(z1a_GewerbePro10000 ~ i(year, treat, ref = 2019) | kz + year,
                  data = df_matched_kap1_z1a,
                  weights = ~weights,
                  cluster = ~kz)
summary(kap1_z1a)

kap1_z2a <- feols(z2a_Sozialhilfe_Anteil ~ i(year, treat, ref = 2019) | kz + year,
                  data = df_matched_kap1_z2a,
                  weights = ~weights,
                  cluster = ~kz)
summary(kap1_z2a)

kap1_z3a <- feols(z3a_WanderungRelativ ~ i(year, treat, ref = 2019) | kz + year,
                  data = df_matched_kap1_z3a,
                  weights = ~weights,
                  cluster = ~kz)
summary(kap1_z3a)
#===============================================================================
#kap2
kap2_z1a <- feols(z1a_GewerbePro10000 ~ i(year, treat, ref = 2019) | kz + year,
                  data = df_matched_kap2_z1a,
                  weights = ~weights,
                  cluster = ~kz)
summary(kap2_z1a)

kap2_z2a <- feols(z2a_Sozialhilfe_Anteil ~ i(year, treat, ref = 2019) | kz + year,
                  data = df_matched_kap2_z2a,
                  weights = ~weights,
                  cluster = ~kz)
summary(kap2_z2a)

kap2_z3a <- feols(z3a_WanderungRelativ ~ i(year, treat, ref = 2019) | kz + year,
                  data = df_matched_kap2_z3a,
                  weights = ~weights,
                  cluster = ~kz)
summary(kap2_z3a)
#================================================================================
#kap 1 & kap 2
insg_z1a <- feols(z1a_GewerbePro10000 ~ i(year, treat, ref = 2019) | kz + year,
                  data = df_matched_z1a,
                  weights = ~weights,
                  cluster = ~kz)
summary(insg_z1a)

insg_z2a <- feols(z2a_Sozialhilfe_Anteil ~ i(year, treat, ref = 2019) | kz + year,
                  data = df_matched_z2a,
                  weights = ~weights,
                  cluster = ~kz)
summary(insg_z2a)

insg_z3a <- feols(z3a_WanderungRelativ ~ i(year, treat, ref = 2019)| kz + year,
                  data = df_matched_z3a,
                  weights = ~weights,
                  cluster = ~kz)
summary(insg_z3a)
#===============================================================================
#zuerst ohne controlls (bzw. nur mit neighbor effekt)
#kap1
kap1_z1a_neighbor <- feols(z1a_GewerbePro10000 ~ i(year, treat, ref = 2019) +
                     + i(year, neighbor, ref = 2019) | kz + year,
                   data = df_matched_kap1_z1a,
                   weights = ~weights,
                   cluster = ~kz)
summary(kap1_z1a_neighbor)

kap1_z2a_neighbor <- feols(z2a_Sozialhilfe_Anteil ~ i(year, treat, ref = 2019) +
                    + i(year, neighbor, ref = 2019) | kz + year,
                  data = df_matched_kap1_z2a,
                  weights = ~weights,
                  cluster = ~kz)
summary(kap1_z2a_neighbor)

kap1_z3a_neighbor <- feols(z3a_WanderungRelativ ~ i(year, treat, ref = 2019) +
                    + i(year, neighbor, ref = 2019) | kz + year,
                  data = df_matched_kap1_z3a,
                  weights = ~weights,
                  cluster = ~kz)
summary(kap1_z3a_neighbor)
#===============================================================================
#kap2
kap2_z1a_neighbor <- feols(z1a_GewerbePro10000 ~ i(year, treat, ref = 2019) +
                    + i(year, neighbor, ref = 2019) | kz + year,
                  data = df_matched_kap2_z1a,
                  weights = ~weights,
                  cluster = ~kz)
summary(kap2_z1a_neighbor)

kap2_z2a_neighbor <- feols(z2a_Sozialhilfe_Anteil ~ i(year, treat, ref = 2019) +
                    + i(year, neighbor, ref = 2019) | kz + year,
                  data = df_matched_kap2_z2a,
                  weights = ~weights,
                  cluster = ~kz)
summary(kap2_z2a_neighbor)

kap2_z3a_neighbor <- feols(z3a_WanderungRelativ ~ i(year, treat, ref = 2019) +
                    + i(year, neighbor, ref = 2019) | kz + year,
                  data = df_matched_kap2_z3a,
                  weights = ~weights,
                  cluster = ~kz)
summary(kap2_z3a_neighbor)
#================================================================================
#kap 1 & kap 2
insg_z1a_neighbor <- feols(z1a_GewerbePro10000 ~ i(year, treat, ref = 2019) +
                    + i(year, neighbor, ref = 2019) | kz + year,
                  data = df_matched_z1a,
                  weights = ~weights,
                  cluster = ~kz)
summary(insg_z1a_neighbor)

insg_z2a_neighbor <- feols(z2a_Sozialhilfe_Anteil ~ i(year, treat, ref = 2019) +
                    + i(year, neighbor, ref = 2019) | kz + year,
                  data = df_matched_z2a,
                  weights = ~weights,
                  cluster = ~kz)
summary(insg_z2a_neighbor)

insg_z3a_neighbor <- feols(z3a_WanderungRelativ ~ i(year, treat, ref = 2019) +
                    + i(year, neighbor, ref = 2019) | kz + year,
                  data = df_matched_z3a,
                  weights = ~weights,
                  cluster = ~kz)
summary(insg_z3a_neighbor)
#===============================================================================
#===============================================================================
#Jetzt mit mit allen controls
#jeweils nur die jenigen die beim matching außerhalb der kriterien sind
#(SMD bis max 0.25 und variance ratio zwischen 0.5 bis 2)
kap1_z1a_con <- feols(z1a_GewerbePro10000 ~ i(year, treat, ref = 2019) +
                        i(year, neighbor, ref = 2019) + GRW + herfindahl + i(year, z2a_Sozialhilfe_Anteil_2019, keep = 2020:2023) +
                        i(year, c2_BIP_pro_Kopf_2019, keep = 2020:2023) + i(year, c9a_alten_2019, keep = 2020:2023)| kz + year,
                      data = df_matched_kap1_z1a %>% filter(year >= 2011 & year <= 2023),
                      weights = ~weights,
                      cluster = ~kz)
summary(kap1_z1a_con)
kap1_z2a_con <- feols(z2a_Sozialhilfe_Anteil ~ i(year, treat, ref = 2019) +
                        + i(year, neighbor, ref = 2019) + GRW + herfindahl +
                        i(year, log(c1_Bevölkerung_2019), keep = 2020:2023) + i(year, c2_BIP_pro_Kopf_2019, keep = 2020:2023)
                      + i(year, c7a_LangzeitALQ_2019, keep = 2020:2023) +
                        i(year, c9a_alten_2019, keep = 2020:2023) + log(c8_Fläche) 
                      | kz + year,
                      data = df_matched_kap1_z2a %>%
                        filter(year >= 2011 & year <= 2023),
                      weights = ~weights,
                      cluster = ~kz)
summary(kap1_z2a_con)
kap1_z3a_con <- feols(z3a_WanderungRelativ ~ i(year, treat, ref = 2019) +
                        + i(year, neighbor, ref = 2019) + GRW + herfindahl + i(year, z2a_Sozialhilfe_Anteil_2019, keep = 2020:2023) +
                        i(year, log(c1_Bevölkerung_2019), keep = 2020:2023) + i(year, c2_BIP_pro_Kopf_2019, keep = 2020:2023)
                      + i(year, c6_ALQ_2019, keep = 2020:2023) + i(year, c7a_LangzeitALQ_2019, keep = 2020:2023)  +
                        i(year, c9a_alten_2019, keep = 2020:2023) | kz + year,
                      data = df_matched_kap1_z3a %>%
                        filter(year >= 2011 & year <= 2023),
                      weights = ~weights,
                      cluster = ~kz)
summary(kap1_z3a_con)
#jetzt kap2
kap2_z1a_con <- feols(z1a_GewerbePro10000 ~ i(year, treat, ref = 2019) +
                        + i(year, neighbor, ref = 2019) + GRW + herfindahl + i(year, z2a_Sozialhilfe_Anteil_2019, keep = 2020:2023) + 
                        i(year, z3a_WanderungRelativ_2019, keep = 2020:2023) +
                       i(year, c6_ALQ_2019, keep = 2020:2023) + i(year, c7a_LangzeitALQ_2019, keep = 2020:2023)
                      + log(c8_Fläche) + i(year, c9a_alten_2019, keep = 2020:2023) +
                        i(year, c11a_öffDienst_Q_2019, keep = 2020:2023) | kz + year,
                      data = df_matched_kap2_z1a %>%
                        filter(year >= 2011 & year <= 2023),
                      weights = ~weights,
                      cluster = ~kz)
summary(kap2_z1a_con)
kap2_z2a_con <- feols(z2a_Sozialhilfe_Anteil ~ i(year, treat, ref = 2019) +
                        + i(year, neighbor, ref = 2019) + GRW + herfindahl +
                        i(year, c11a_öffDienst_Q_2019, keep = 2020:2023) | kz + year,
                      data = df_matched_kap2_z2a %>%
                        filter(year >= 2011 & year <= 2023),
                      weights = ~weights,
                      cluster = ~kz)
summary(kap2_z2a_con)
kap2_z3a_con <- feols(z3a_WanderungRelativ ~ i(year, treat, ref = 2019) +
                        + i(year, neighbor, ref = 2019) + GRW + herfindahl +
                        i(year, log(c1_Bevölkerung_2019), keep = 2020:2023) + i(year, c7a_LangzeitALQ_2019, keep = 2020:2023)
                      + i(year, c11a_öffDienst_Q_2019, keep = 2020:2023) | kz + year,
                      data = df_matched_kap2_z3a %>%
                        filter(year >= 2011 & year <= 2023),
                      weights = ~weights,
                      cluster = ~kz)
summary(kap2_z3a_con)
#jetzt kap1 und kap2
insg_z1a_con <- feols(z1a_GewerbePro10000 ~ i(year, treat, ref = 2019) +
                        + i(year, neighbor, ref = 2019) + GRW + herfindahl| kz + year,
                      data = df_matched_z1a %>%
                        filter(year >= 2011 & year <= 2023),
                      weights = ~weights,
                      cluster = ~kz)
summary(insg_z1a_con)
insg_z2a_con <- feols(z2a_Sozialhilfe_Anteil ~ i(year, treat, ref = 2019) +
                        + i(year, neighbor, ref = 2019) + GRW + herfindahl +
                        i(year, log(c1_Bevölkerung_2019), keep = 2020:2023) + log(c8_Fläche) | kz + year,
                      data = df_matched_z2a %>%
                        filter(year >= 2011 & year <= 2023),
                      weights = ~weights,
                      cluster = ~kz)
summary(insg_z2a_con)
insg_z3a_con <- feols(z3a_WanderungRelativ ~ i(year, treat, ref = 2019) +
                        + i(year, neighbor, ref = 2019) + GRW + herfindahl | kz + year,
                      data = df_matched_z3a %>%
                        filter(year >= 2011 & year <= 2023),
                      weights = ~weights,
                      cluster = ~kz)
summary(insg_z3a_con)

#===================================================================================
#iplots vorbereiten
kap1_z1a_iplot <- function(){
  iplot(list("Kap 1 (ohne Controlls)" = kap1_z1a, 
           "Kap 1 (mit Nachbareffekten)" = kap1_z1a_neighbor,
           "Kap 1 (Nachbar + Controlls)" = kap1_z1a_con))
  legend(
  "bottomleft", 
  legend = c("Kap 1 (ohne Controls)", "Kap 1 (mit Nachbareffekten)", "Kap 1 (Nachbar + Controls)"),
  col    = 1:3,     # Verwendet automatisch die ersten 3 Standardfarben von R (Schwarz, Rot, Grün)
  pch    = c(19, 17, 20),      # Zeichnet Punkte in die Legende
  lty    = 1,       # Zeichnet Linien in die Legende
  bty    = "n"      # Entfernt den störenden Kasten um die Legende
  )}
kap1_z1a_iplot()
kap2_z1a_iplot <-function(){iplot(list("Kap 2 (ohne Controlls)" = kap2_z1a, 
           "Kap 2 (mit Nachbareffekten)" = kap2_z1a_neighbor,
           "Kap 2 (Nachbar + Controlls)" = kap2_z1a_con))
  legend(
  "bottomleft", 
  legend = c("Kap 2 (ohne Controls)", "Kap 2 (mit Nachbareffekten)", "Kap 2 (Nachbar + Controls)"),
  col    = 1:3,     # Verwendet automatisch die ersten 3 Standardfarben von R (Schwarz, Rot, Grün)
  pch    = c(19, 17, 20),      # Zeichnet Punkte in die Legende
  lty    = 1,       # Zeichnet Linien in die Legende
  bty    = "n"      # Entfernt den störenden Kasten um die Legende
)}
kap2_z1a_iplot()
insg_z1a_iplot <- function(){iplot(list("Kap 1 & 2 (ohne Controlls)" = insg_z1a, 
           "Kap 1 & 2 (mit Nachbareffekten)" = insg_z1a_neighbor,
           "Kap 1 & 2 (Nachbar + Controlls)" = insg_z1a_con))
  legend(
  "bottomleft", 
  legend = c("Kap 1 & 2 (ohne Controls)", "Kap 1 & 2 (mit Nachbareffekten)", "Kap 1 & 2 (Nachbar + Controls)"),
  col    = 1:3,     # Verwendet automatisch die ersten 3 Standardfarben von R (Schwarz, Rot, Grün)
  pch    = c(19, 17, 20),      # Zeichnet Punkte in die Legende
  lty    = 1,       # Zeichnet Linien in die Legende
  bty    = "n"      # Entfernt den störenden Kasten um die Legende
)}

kap1_z2a_iplot <- function(){iplot(list("Kap 1 (ohne Controlls)" = kap1_z2a, 
           "Kap 1 (mit Nachbareffekten)" = kap1_z2a_neighbor,
           "Kap 1 (Nachbar + Controlls)" = kap1_z2a_con))
  legend(
  "bottomleft", 
  legend = c("Kap 1 (ohne Controls)", "Kap 1 (mit Nachbareffekten)", "Kap 1 (Nachbar + Controls)"),
  col    = 1:3,     # Verwendet automatisch die ersten 3 Standardfarben von R (Schwarz, Rot, Grün)
  pch    = c(19, 17, 20),      # Zeichnet Punkte in die Legende
  lty    = 1,       # Zeichnet Linien in die Legende
  bty    = "n"      # Entfernt den störenden Kasten um die Legende
)}

kap2_z2a_iplot <- function(){iplot(list("Kap 2 (ohne Controlls)" = kap2_z2a, 
                                        "Kap 2 (mit Nachbareffekten)" = kap2_z2a_neighbor,
                                        "Kap 2 (Nachbar + Controlls)" = kap2_z2a_con))
  legend(
    "bottomleft", 
    legend = c("Kap 2 (ohne Controls)", "Kap 2 (mit Nachbareffekten)", "Kap 2 (Nachbar + Controls)"),
    col    = 1:3,     # Verwendet automatisch die ersten 3 Standardfarben von R (Schwarz, Rot, Grün)
    pch    = c(19, 17, 20),      # Zeichnet Punkte in die Legende
    lty    = 1,       # Zeichnet Linien in die Legende
    bty    = "n"      # Entfernt den störenden Kasten um die Legende
  )}

insg_z2a_iplot <- function(){iplot(list("Kap 1 & 2 (ohne Controlls)" = insg_z2a, 
                                        "Kap 1 & 2 (mit Nachbareffekten)" = insg_z2a_neighbor,
                                        "Kap 1 & 2(Nachbar + Controlls)" = insg_z2a_con))
  legend(
    "bottomleft", 
    legend = c("Kap 1 & 2 (ohne Controls)", "Kap 1 & 2 (mit Nachbareffekten)", "Kap 1 & 2(Nachbar + Controls)"),
    col    = 1:3,     # Verwendet automatisch die ersten 3 Standardfarben von R (Schwarz, Rot, Grün)
    pch    = c(19, 17, 20),      # Zeichnet Punkte in die Legende
    lty    = 1,       # Zeichnet Linien in die Legende
    bty    = "n"      # Entfernt den störenden Kasten um die Legende
  )}

kap1_z3a_iplot <- function(){iplot(list("Kap 1 (ohne Controlls)" = kap1_z3a, 
                                        "Kap 1 (mit Nachbareffekten)" = kap1_z3a_neighbor,
                                        "Kap 1 (Nachbar + Controlls)" = kap1_z3a_con))
  legend(
    "bottomleft", 
    legend = c("Kap 1 (ohne Controls)", "Kap 1 (mit Nachbareffekten)", "Kap 1 (Nachbar + Controls)"),
    col    = 1:3,     # Verwendet automatisch die ersten 3 Standardfarben von R (Schwarz, Rot, Grün)
    pch    = c(19, 17, 20),      # Zeichnet Punkte in die Legende
    lty    = 1,       # Zeichnet Linien in die Legende
    bty    = "n"      # Entfernt den störenden Kasten um die Legende
  )}

kap2_z3a_iplot <- function(){iplot(list("Kap 2 (ohne Controlls)" = kap2_z3a, 
                                        "Kap 2 (mit Nachbareffekten)" = kap2_z3a_neighbor,
                                        "Kap 2 (Nachbar + Controlls)" = kap2_z3a_con))
  legend(
    "bottomleft", 
    legend = c("Kap 2 (ohne Controls)", "Kap 2 (mit Nachbareffekten)", "Kap 2 (Nachbar + Controls)"),
    col    = 1:3,     # Verwendet automatisch die ersten 3 Standardfarben von R (Schwarz, Rot, Grün)
    pch    = c(19, 17, 20),      # Zeichnet Punkte in die Legende
    lty    = 1,       # Zeichnet Linien in die Legende
    bty    = "n"      # Entfernt den störenden Kasten um die Legende
  )}

insg_z3a_iplot <- function(){iplot(list("Kap 1 & 2 (ohne Controlls)" = insg_z3a, 
                                        "Kap 1 & 2(mit Nachbareffekten)" = insg_z3a_neighbor,
                                        "Kap 1 & 2(Nachbar + Controlls)" = insg_z3a_con))
  legend(
    "bottomleft", 
    legend = c("Kap 1 & 2 (ohne Controls)", "Kap 1 & 2 (mit Nachbareffekten)", "Kap 1 & 2 (Nachbar + Controls)"),
    col    = 1:3,     # Verwendet automatisch die ersten 3 Standardfarben von R (Schwarz, Rot, Grün)
    pch    = c(19, 17, 20),      # Zeichnet Punkte in die Legende
    lty    = 1,       # Zeichnet Linien in die Legende
    bty    = "n"      # Entfernt den störenden Kasten um die Legende
  )}

#alle in eine pdf
pdf("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Bachelorarbeit/3x3_eventstudyplot.pdf", width = 14, height = 12)
par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))
kap1_z1a_iplot()
kap1_z2a_iplot()
kap1_z3a_iplot()
kap2_z1a_iplot()
kap2_z2a_iplot()
kap2_z3a_iplot()
insg_z1a_iplot()
insg_z2a_iplot()
insg_z3a_iplot()
dev.off()
#====================================================================================
#tabellen vorbereiten für LaTeX
#tabelle z1a (ohne controls)
?etable
etable(
  list(kap1_z1a, kap2_z1a, insg_z1a),
  tex = TRUE,
  title = "Event-Study Ergebnisse: Gewerbeanmeldungen pro 10.000 Einwohner:innen (Ohne Controls)",
  notes = c("Robust Standardfehler in Klammern. * p < 0.1, ** p < 0.05, *** p < 0.01.",
            "Quelle: Eigene Darstellung"),  
  
  se.below = FALSE, 
  
  # Falls deine Version das unterstützt, packt das die SEs daneben:
  # (Sollte das einen Fehler werfen, lass "semicolon = TRUE" einfach weg)
  style.tex = style.tex("aer"), 
  
  # Deine Kontrollen ausblenden:
  drop = c("GRW", "c1_Bevölkerung", "c7a_LangzeitALQ", "c8_Fläche", "c9_jugend", "c9a_alten", "c11_öffDienst"),
  
  # Kontroll-Zeile unten einfügen:
  extralines = list(
    "^Region-level Controls" = c("No", "No", "No")
  ),
  
  headers = c("Kap. 1 InvKG", "Kap. 2 InvKG", "Kap. 1 & 2 InvKG"),
  depvar = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))
#tabelle z1a (mit neighbor)
?etable
etable(
  list(kap1_z1a_neighbor, kap2_z1a_neighbor, insg_z1a_neighbor),
  tex = TRUE,
  title = "Event-Study Ergebnisse: Gewerbeanmeldungen pro 10.000 Einwohner:innen (Mit Nachbareffekten)",
  notes = c("Robust Standardfehler in Klammern. * p < 0.1, ** p < 0.05, *** p < 0.01.",
            "Quelle: Eigene Darstellung"),  

  se.below = FALSE, 
  
  # Falls deine Version das unterstützt, packt das die SEs daneben:
  # (Sollte das einen Fehler werfen, lass "semicolon = TRUE" einfach weg)
  style.tex = style.tex("aer"), 
  
  # Deine Kontrollen ausblenden:
  drop = c("GRW", "c1_Bevölkerung", "c7a_LangzeitALQ", "c8_Fläche", "c9_jugend", "c9a_alten", "c11_öffDienst"),
  
  # Kontroll-Zeile unten einfügen:
  extralines = list(
    "^Region-level Controls" = c("No", "No", "No")
  ),
  
  headers = c("Kap. 1 InvKG", "Kap. 2 InvKG", "Kap. 1 & 2 InvKG"),
  depvar = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))
#tabelle z1a (neighbor + controls)
etable(
  list(kap1_z1a_con, kap2_z1a_con, insg_z1a_con),
  tex = TRUE,
  title = "Event-Study Ergebnisse: Gewerbeanmeldungen pro 10.000 Einwohner:innen (Mit Nachbareffekten + Controls)",
  notes = c("Robust Standardfehler in Klammern. * p < 0.1, ** p < 0.05, *** p < 0.01.",
            "Quelle: Eigene Darstellung"), 
  se.below = FALSE, 
  
  style.tex = style.tex("aer"), 
  # Kontroll-Zeile unten einfügen:
  extralines = list(
    "^Region-level Controls" = c("Yes", "Yes", "Yes")
  ),
  
  headers = c("Kap. 1 InvKG", "Kap. 2 InvKG", "Kap. 1 & 2 InvKG"),
  depvar = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

#z2a (ohne controls)
etable(
  list(kap1_z2a, kap2_z2a, insg_z2a),
  tex = TRUE,
  title = "TWFE-Ergebnisse: Anteil der Sozialhilfeempfänger:innen (%) (Ohne Controls)",
  notes = c("\\textit{Anmerkung: }Robust Standardfehler in Klammern. * p < 0.1, ** p < 0.05, *** p < 0.01.",
            "\\textit{Quelle: }Eigene Darstellung"), 
  se.below = FALSE, 
  
  style.tex = style.tex("aer"), 
  
  drop = c("c1_Bevölkerung", "c2_BIP", "c7a_LangzeitALQ", "c8_Fläche", "c9_jugend", "c9a_alten", "c11a_öffDienst_Q"),
  
  # Kontroll-Zeile unten einfügen:
  extralines = list(
    "^Region-level Controls" = c("Yes", "Yes", "Yes")
  ),
  
  headers = c("Kap. 1 InvKG", "Kap. 2 InvKG", "Kap. 1 & 2 InvKG"),
  depvar = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

#z2a mit neighbor
etable(
  list(kap1_z2a_neighbor, kap2_z2a_neighbor, insg_z2a_neighbor),
  tex = TRUE,
  title = "TWFE-Ergebnisse: Anteil der Sozialhilfeempfänger:innen (%) (Mit Nachbareffekten)",
  notes = c("\\textit{Anmerkung: }Robust Standardfehler in Klammern. * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01.",
            "\\textit{Quelle: }Eigene Darstellung"), 
  se.below = FALSE, 
  
  style.tex = style.tex("aer"), 
  
  drop = c("c1_Bevölkerung", "c2_BIP", "c7a_LangzeitALQ", "c8_Fläche", "c9_jugend", "c9a_alten", "c11a_öffDienst_Q"),
  
  # Kontroll-Zeile unten einfügen:
  extralines = list(
    "^Region-level Controls" = c("Yes", "Yes", "Yes")
  ),
  
  headers = c("Kap. 1 InvKG", "Kap. 2 InvKG", "Kap. 1 & 2 InvKG"),
  depvar = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

#z2a controls
etable(
  list(kap1_z2a_con, kap2_z2a_con, insg_z2a_con),
  tex = TRUE,
  title = "Event-Study Ergebnisse: Gewerbeanmeldungen pro 10.000 Einwohner:innen (Mit Nachbareffekten + Controls)",
  notes = c("\\textit{Anmerkung:} Robust Standardfehler in Klammern. * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01.",
            "\\textit{Quelle:} Quelle: Eigene Darstellung"), 
  se.below = FALSE, 

  style.tex = style.tex("aer"), 
  
  headers = c("Kap. 1 InvKG", "Kap. 2 InvKG", "Kap. 1 & 2 InvKG"),
  depvar = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))
#z3a ohne controls
etable(
  list(kap1_z3a, kap2_z3a, insg_z3a),
  tex = TRUE,
  title = "TWFE-Ergebnisse: Zu/-Abwanderungssaldo pro 100 Einwohner:innen (Ohne Controls)",
  notes = c("\\textit{Anmerkung: }Robust Standardfehler in Klammern. * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01.",
            "\\textit{Quelle: }Eigene Darstellung"), 
  se.below = FALSE, 
  style.tex = style.tex("aer"), 
  drop = c("c1_Bevölkerung", "c7a_LangzeitALQ", "c8_Fläche", "c9_jugend", "c9a_alten", "c11_öffDienst"),
  extralines = list(
    "^Region-level Controls" = c("Yes", "Yes", "Yes")
  ),
  headers = c("Kap. 1 InvKG", "Kap. 2 InvKG", "Kap. 1 & 2 InvKG"),
  depvar = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))
#z3a mit neighbor
etable(
  list(kap1_z3a_neighbor, kap2_z3a_neighbor, insg_z3a_neighbor),
  tex = TRUE,
  title = "TWFE-Ergebnisse: Zu/-Abwanderungssaldo pro 100 Einwohner:innen (Mit Nachbareffekten)",
  notes = c("\\textit{Anmerkung: }Robust Standardfehler in Klammern. * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01.",
            "\\textit{Quelle: }Eigene Darstellung"), 
  se.below = FALSE, 
  style.tex = style.tex("aer"), 
  drop = c("c1_Bevölkerung", "c7a_LangzeitALQ", "c8_Fläche", "c9_jugend", "c9a_alten", "c11_öffDienst"),
  extralines = list(
    "^Region-level Controls" = c("Yes", "Yes", "Yes")
  ),
  headers = c("Kap. 1 InvKG", "Kap. 2 InvKG", "Kap. 1 & 2 InvKG"),
  depvar = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

#z3a mit controls
etable(
  list(kap1_z3a_con, kap2_z3a_con, insg_z3a_con),
  tex = TRUE,
  title = "TWFE-Ergebnisse: Zu/-Abwanderungssaldo pro 100 Einwohner:innen (Mit Nachbareffekten + Controls)",
  notes = c("\\textit{Anmerkung: }Robust Standardfehler in Klammern. * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01.",
            "\\textit{Quelle: }Eigene Darstellung"), 
  se.below = FALSE, 
  style.tex = style.tex("aer"), 
  headers = c("Kap. 1 InvKG", "Kap. 2 InvKG", "Kap. 1 & 2 InvKG"),
  depvar = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))


modelle <- list(kap1_z1a = kap1_z1a, kap1_z2a = kap1_z2a, kap1_z3a = kap1_z3a,
             kap1_z1a_neighbor = kap1_z1a_neighbor, kap1_z2a_neighbor = kap1_z2a_neighbor, kap1_z3a_neighbor = kap1_z3a_neighbor,
             kap1_z1a_con = kap1_z1a_con, kap1_z2a_con = kap1_z2a_con, kap1_z3a_con = kap1_z3a_con,
             kap2_z1a = kap2_z1a, kap2_z2a = kap2_z2a, kap2_z3a = kap2_z3a,
             kap2_z1a_neighbor = kap2_z1a_neighbor, kap2_z2a_neighbor = kap2_z2a_neighbor, kap2_z3a_neighbor = kap2_z3a_neighbor,
             kap2_z1a_con = kap2_z1a_con, kap2_z2a_con = kap2_z2a_con, kap2_z3a_con = kap2_z3a_con,
             insg_z1a = insg_z1a, insg_z2a = insg_z2a, insg_z3a = insg_z3a,
             insg_z1a_neighbor = insg_z1a_neighbor, insg_z2a_neighbor = insg_z2a_neighbor, insg_z3a_neighbor = insg_z3a_neighbor,
             insg_z1a_con = insg_z1a_con, insg_z2a_con = insg_z2a_con, insg_z3a_con = insg_z3a_con)

withinr <- sapply(modelle, function(x) fitstat(x, type = "wr2")[[1]])
max(withinr)
quantile(withinr)
mean(withinr)
median(withinr)
print(sum(sapply(modelle, function(mod) length(coef(mod)))))
print(sum(sapply(modelle, function(mod) {
  ct <- coeftable(mod)
  sum(ct[, 4] <= 0.05)
})))


#condition index

CN(model.matrix(kap1_z1a_con))
CN(model.matrix(kap1_z2a_con))
CN(model.matrix(kap1_z3a_con))
CN(model.matrix(kap2_z1a_con))
CN(model.matrix(kap2_z2a_con))
CN(model.matrix(kap2_z3a_con))
CN(model.matrix(insg_z1a_con))
CN(model.matrix(insg_z2a_con))
CN(model.matrix(insg_z3a_con))

mean(c(CN(model.matrix(kap1_z1a_con)),
          CN(model.matrix(kap1_z2a_con)),
          CN(model.matrix(kap1_z3a_con)),
          CN(model.matrix(kap2_z1a_con)),
          CN(model.matrix(kap2_z2a_con)),
          CN(model.matrix(kap2_z3a_con)),
          CN(model.matrix(insg_z1a_con)),
          CN(model.matrix(insg_z2a_con)),
          CN(model.matrix(insg_z3a_con))))

shapiro_list <- list()
qqplot_list <- list()
modelle1 <- modelle[1:9]
modelle2 <- modelle[10:18]
modelle3 <- modelle[19:27]

pdf("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Bachelorarbeit/qqPlot_Insg.pdf", 
    width = 8.5,   # ca. Textbreite einer DIN-A4 Seite bei 1-Zoll-Rändern
    height = 10,   # Bietet genug Platz für 3 Zeilen
    paper = "special")

par(
  mfrow = c(3, 3), 
  mar = c(3.5, 3.5, 2, 1), # Ränder kompakt halten
  mgp = c(2, 0.7, 0),      # Schiebt Achsenbeschriftung näher an die Achse
  cex = 0.85               # Skaliert Schriften leicht runter, damit nichts überlappt
)

for(i in names(modelle3)){
  res_i <- residuals(modelle[[i]])
  shapiro_i <- shapiro.test(res_i)
  
  shapiro_list[[i]] <- shapiro_i
  
  qqnorm(res_i, main = paste("Q-Q Plot: ", i))
  qqline(res_i, col = "red", lwd = 2)
}

dev.off()

ergebnis_tabelle <- imap_dfr(modelle, function(mod, mod_name) {
  res <- residuals(mod)
  shap <- shapiro_list[[mod_name]]
  
  tibble(
    Modell = mod_name,
    W_Statistik = shap$statistic,
    p_Wert = shap$p.value,
    Min = min(res),
    Q25 = quantile(res, 0.25),
    Median = median(res),
    Q75 = quantile(res, 0.75),
    Max = max(res)
  )
})
quantile(ergebnis_tabelle$W_Statistik)
mean(ergebnis_tabelle$W_Statistik)

#============================================================================
#sensitivity analyse für alle modelle
sensitivitylist <- list()
names(modelle)

for (m_name in names(modelle)) {
  
  # Aktuelles Modell-Objekt aus der Liste holen
  mod_obj <- modelle[[m_name]]
  
  # Koeffizienten und Kovarianzmatrix extrahieren
  all_coefs_i <- coef(mod_obj)
  all_varcov_i <- as.matrix(vcov(mod_obj))
  es_idx <- grep(":treat$", names(all_coefs_i))
  coefs_i <- all_coefs_i[es_idx]
  varcov_i <- all_varcov_i[es_idx, es_idx]
  # Unterscheidung nach Modelltyp anhand des Namens
  if (str_detect(m_name, "con")) {
    
    # Modell mit "con" im Namen (8 Pre, 4 Post)
    delta_rm_results_i <- HonestDiD::createSensitivityResults_relativeMagnitudes(
      betahat = coefs_i,
      sigma = varcov_i,
      numPrePeriods = 8,
      numPostPeriods = 4,
      Mbarvec = seq(0, 1, by = 0.1)
    )
    
    originalresults_i <- HonestDiD::constructOriginalCS(
      betahat = coefs_i,
      sigma = varcov_i,
      numPrePeriods = 8,
      numPostPeriods = 4
    )
    
  } else {
    
    # Modell ohne "con" im Namen (9 Pre, 5 Post)
    delta_rm_results_i <- HonestDiD::createSensitivityResults_relativeMagnitudes(
      betahat = coefs_i,
      sigma = varcov_i,
      numPrePeriods = 9,
      numPostPeriods = 5,
      Mbarvec = seq(0, 1, by = 0.1)
    )
    
    originalresults_i <- HonestDiD::constructOriginalCS(
      betahat = coefs_i,
      sigma = varcov_i,
      numPrePeriods = 9,
      numPostPeriods = 5
    )
  }
  
  # Plot erstellen und unter dem Modellnamen in der Liste speichern
  sensitivityplot_i <- HonestDiD::createSensitivityPlot_relativeMagnitudes(
    delta_rm_results_i, 
    originalresults_i
  )
  
  sensitivitylist[[m_name]] <- sensitivityplot_i
  
  message("fertig; ", m_name)
}

year_coefs <- grep("year::", names(coef(kap1_z1a_con)))
print(year_coefs)
names(coef(modelle[[8]]))[grep(":treat", names(coef(modelle[[8]])))]
for(i in seq_along(modelle)){
  print(names(coef(modelle[[i]]))[grep(":treat", names(coef(modelle[[i]])))])
}
plots_title_kap1 <- imap(sensitivitylist[1:9], ~ .x + ggtitle(.y))
plots_title_kap2 <- imap(sensitivitylist[10:18], ~ .x + ggtitle(.y))
plots_title_insg <- imap(sensitivitylist[19:27], ~ .x + ggtitle(.y))

combined_sensitivity_kap1 <- wrap_plots(plots_title_kap1, ncol = 3, nrow = 3) +
  plot_layout(
    guides = "collect"
  ) &
  plot_annotation(
    title = "Sensitivity Analyse Kap. 1 InvKG",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  ) &
  theme(
    legend.position = "right",
    # Schriftgrößen leicht anpassen, damit auf der 9er-Seite nichts überlappt
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 6, hjust = 0)
  )
print(combined_sensitivity_kap1)
ggsave(
  filename = "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/sensitivityAnalyseKap1.pdf",
  plot = combined_sensitivity_kap1,
  width = 8.5,   # Breite in Zoll (passt perfekt zu DIN A4 Textbreite)
  height = 11,   # Höhe in Zoll
  units = "in")


combined_sensitivity_kap2 <- wrap_plots(plots_title_kap2, ncol = 3, nrow = 3) +
  plot_layout(
    guides = "collect"
  ) &
  plot_annotation(
    title = "Sensitivity Analyse Kap. 2 InvKG",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  ) &
  theme(
    legend.position = "right",
    # Schriftgrößen leicht anpassen, damit auf der 9er-Seite nichts überlappt
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 6, hjust = 0)
  )
print(combined_sensitivity_kap2)
ggsave(
  filename = "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/sensitivityAnalyseKap2.pdf",
  plot = combined_sensitivity_kap2,
  width = 8.5,   # Breite in Zoll (passt perfekt zu DIN A4 Textbreite)
  height = 11,   # Höhe in Zoll
  units = "in")

combined_sensitivity_insg <- wrap_plots(plots_title_insg, ncol = 3, nrow = 3) +
  plot_layout(
    guides = "collect"
  ) &
  plot_annotation(
    title = "Sensitivity Analyse Kap. 1 & 2 InvKG",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  ) &
  theme(
    legend.position = "right",
    # Schriftgrößen leicht anpassen, damit auf der 9er-Seite nichts überlappt
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 6, hjust = 0)
  )
print(combined_sensitivity_insg)
ggsave(
  filename = "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/sensitivityAnalyseKapInsg.pdf",
  plot = combined_sensitivity_insg,
  width = 8.5,   # Breite in Zoll (passt perfekt zu DIN A4 Textbreite)
  height = 11,   # Höhe in Zoll
  units = "in")

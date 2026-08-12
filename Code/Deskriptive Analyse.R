#Bachelorarbeit
#Deskrpitive Analyse
#Autor: Atencio Psille, Lukas
#Datum: 13.07.2026
#=========================================================================================
#Environment leeren
rm(list = ls())
#packages laden
options(scipen = 999)
library(pacman)
pacman::p_load(readxl, tidyverse, dplyr, gtools, MatchIt, dreamerr, fixest, writexl,
               sf, spdep, MatchIt, patchwork, rmapshaper, ggrastr, cowplot, qpdf, magick, 
               pdftools, knitr, kableExtra, Hmisc, correlation, car, powerjoin)
#install.packages("modelsummary", type = "binary")
library(modelsummary)
#=========================================================================================
#Reviere einlesen
RR <- c("05162", "05358", "05362", "05334", "05370", "05366", "05116")
LR <- c("12062", "12066", "12061", "12071", "12052", "14625", "14626")
MR <- c("14713", "14729", "14730", "15084", "15088", "15002", "15087", "15082")
kap2 <- c("03154", "03405", "05978", "05916", "05915", "05112", "05513", "13003",
          "13072", "10044", "10041", "16077")
reviere <- c("05162", "05358", "05362", "05334", "05370", "05366", "05116",
             "12062", "12066", "12061", "12071", "12052", "14625", "14626",
             "14713", "14729", "14730", "15084", "15088", "15002", "15087", "15082",
             "03154", "03405", "05978", "05916", "05915", "05112", "05513", "13003",
             "13072", "10044", "10041", "16077")
kap1 <- c("05162", "05358", "05362", "05334", "05370", "05366", "05116",
          "12062", "12066", "12061", "12071", "12052", "14625", "14626",
          "14713", "14729", "14730", "15084", "15088", "15002", "15087", "15082")
#=========================================================================================
#Daten einlesen
#Geodaten für Karten
shapes <- read_sf("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/Shapefiles/2024/vg250-ew_ebenen_1231/VG250_KRS.shp") %>%
  dplyr::select(AGS, geometry) %>%
  #pro kreis ganzes gebiet in ein geometry mergen (inkl. exklaven usw.)
  group_by(AGS) %>% 
  summarise(geometry = st_union(geometry)) %>%
  ungroup()
shapes_länder <- read_sf("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/Shapefiles/2024/vg250-ew_ebenen_1231/VG250_LAN.shp") %>%
  dplyr::select(AGS, geometry) %>%
  ms_simplify(keep = 0.05)
df_unmatched <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_unmatched.xlsx") %>%
  left_join(shapes, by = c("kz" = "AGS")) %>%
  st_as_sf()

df_matched_kap1_z1a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap1_z1a.xlsx") %>%
  left_join(shapes, by = c("kz" = "AGS")) %>%
  st_as_sf()
df_matched_kap1_z2a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap1_z2a.xlsx") %>%
  left_join(shapes, by = c("kz" = "AGS")) %>%
  st_as_sf()
df_matched_kap1_z3a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap1_z3a.xlsx") %>%
  left_join(shapes, by = c("kz" = "AGS")) %>%
  st_as_sf()

df_matched_kap2_z1a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap2_z1a.xlsx") %>%
  left_join(shapes, by = c("kz" = "AGS")) %>%
  st_as_sf()
df_matched_kap2_z2a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap2_z2a.xlsx") %>%
  left_join(shapes, by = c("kz" = "AGS")) %>%
  st_as_sf()
df_matched_kap2_z3a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_kap2_z3a.xlsx") %>%
  left_join(shapes, by = c("kz" = "AGS")) %>%
  st_as_sf()

df_matched_z1a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_z1a.xlsx") %>%
  left_join(shapes, by = c("kz" = "AGS")) %>%
  st_as_sf()
df_matched_z2a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_z2a.xlsx") %>%
  left_join(shapes, by = c("kz" = "AGS")) %>%
  st_as_sf()
df_matched_z3a <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_matched_z3a.xlsx") %>%
  left_join(shapes, by = c("kz" = "AGS")) %>%
  st_as_sf()

dfs <- list("Model 1a" = df_matched_kap1_z1a, 
            "Model 1b" =  df_matched_kap1_z2a, 
            "Model 1c" = df_matched_kap1_z3a,
            "Model 2a" = df_matched_kap2_z1a, 
            "Model 2b" =  df_matched_kap2_z2a, 
            "Model 2c" = df_matched_kap2_z3a,
            "Model 3a" =  df_matched_z1a, 
            "Model 3b" = df_matched_z2a, 
            "Model 3c" = df_matched_z3a)
#==========================================================================================
#Verteilung des Propensity Scores in allen gematchten DFs überprüfen
#Jeweils von Treatment und Kontrollgruppe
pdf("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Bachelorarbeit/Propensity_Score_Density_Alle_Modelle2.pdf", 
    width = 8.5,   # ca. Textbreite einer DIN-A4 Seite bei 1-Zoll-Rändern
    height = 10,   # Bietet genug Platz für 3 Zeilen
    paper = "special")

par(
  mfrow = c(3, 3), 
  mar = c(3.5, 3.5, 2, 1), # Ränder kompakt halten
  mgp = c(2, 0.7, 0),      # Schiebt Achsenbeschriftung näher an die Achse
  cex = 0.85               # Skaliert Schriften leicht runter, damit nichts überlappt
)

#farben festlegen
col_treated <- rgb(0.8, 0.2, 0.2, alpha = 0.25)
col_control <- rgb(0.2, 0.4, 0.8, alpha = 0.25)


for (i in seq_along(dfs)) {
  df <- dfs[[i]] %>%
    filter(year == 2019)
  model_name <- names(dfs)[i]
  
  ps <- plogis(df$distance)
  ps_treated <- ps[df$treat == 1]
  ps_control <- ps[df$treat == 0]
  
  #Dichten berechnen
  d_tr <- density(ps_treated, from = 0, to = 1)
  d_ctrl <- density(ps_control, from = 0, to = 1)
  
  #maximale höhe bestimmen
  max_y <- max(c(d_tr$y, d_ctrl$y)) * 1.25
  
  #Plotten
  plot(1, type = "n", 
       xlim = c(0, 1), 
       ylim = c(0, max_y), 
       main = model_name,
       xlab = "Propensity Score", 
       ylab = "Dichte")
  
  #Treated Kurve + Fläche
  polygon(d_tr, col = col_treated, border = "red", lwd = 1.5)
  
  #Control Kurve + Fläche
  polygon(d_ctrl, col = col_control, border = "blue", lwd = 1.5)
  
  legend("topright", 
         legend = c("Treated (1)", "Control (0)"), 
         col = c("red", "blue"), 
         lwd = 2,
         bty = "n", 
         cex = 0.8)
}


dev.off()

#=========================================================================================================
#Karten erstellen
#Umrisse der Reviere erstellen
umriss_RR <- df_unmatched %>%
  filter(kz %in% RR) %>%
  st_union() %>%
  ms_simplify(keep = 0.05)
umriss_LR <- df_unmatched %>%
  filter(kz %in% LR) %>%
  st_union() %>%
  ms_simplify(keep = 0.05)
umriss_MR <- df_unmatched %>%
  filter(kz %in% MR) %>%
  st_union() %>%
  ms_simplify(keep = 0.05)
umriss_kap2 <- df_unmatched %>%
  filter(kz %in% kap2) %>%
  ms_simplify(keep = 0.05)
#namen extrahieren
namen_liste <- df_unmatched %>% 
  st_drop_geometry() %>% 
  select(kz, name) %>% 
  distinct()

#Karte für Treatment und GRW machen
invkg_map <- ggplot() +
  geom_sf(data = df_unmatched, fill = "white") +
  geom_sf(data = df_unmatched %>% filter(kz %in% RR), aes(fill = "Rheinisches Revier")) + # Markieren des Rheinischen Reviers in Rot
  geom_sf(data = df_unmatched %>% filter(kz %in% MR), aes(fill = "Mitteldeutsches Revier")) + # Markieren des Mitteldeutschen Reviers in Blau
  geom_sf(data = df_unmatched %>% filter(kz %in% LR), aes(fill = "Lausitzer Revier")) + # Markieren des Lausitzer Reviers in Grün
  geom_sf(data = df_unmatched %>% filter(kz %in% kap2), aes(fill = "Kap2 InvKG")) + # Markieren des Kap2InvKG Reviers in Orange
  geom_sf(data = shapes_länder, color = "black", fill = NA, lwd = 0.8) +
  scale_fill_manual(values = c("Rheinisches Revier" = "skyblue3",
                               "Mitteldeutsches Revier" = "lightblue2",
                               "Lausitzer Revier" = "skyblue",
                               "Kap2 InvKG" = "darkred"),
                    name = NULL) +
  theme_void() +
  labs(title = "InvKG Fördergebiete")
print(invkg_map)
#grw map erstellen
grw_map <- ggplot() +
  geom_sf(data = df_unmatched, fill = "white") +
  geom_sf(data = df_unmatched %>% filter(year == 2022, GRW == 1), aes(fill = "GRW")) +
  geom_sf(data = shapes_länder, color = "black", fill = NA, lwd = 0.8) +
  scale_fill_manual(values = c("GRW" = "lightgreen"),
                    name = NULL) +
  theme_void() +
  labs(title = "GRW Fördergebiete (Seit 2022)")
print(grw_map)
grw_inv_maps <- invkg_map + grw_map +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
print(grw_inv_maps)
ggsave("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Bachelorarbeit/grw_invkg_maps.pdf", 
       plot = grw_inv_maps, width = 6.8, height = 3.5, units = "in", dpi = 300)
#komprimieren
pfad_orig <- "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Bachelorarbeit/grw_invkg_maps.pdf"
pfad_neu  <- "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Bachelorarbeit/grw_invkg_maps_comp.pdf"
bild <- image_read_pdf(pfad_orig, density = 300)
image_write(bild, path = pfad_neu, format = "pdf")
#leere liste erstellen
maplist <- list()

dfs_simple <- lapply(dfs, function(x) ms_simplify(x, keep = 0.05))
#plotten

for (i in seq_along(dfs_simple)) {
  
  names_df <- names(dfs_simple)[i]
  
  # 1. Daten für das aktuelle Modell filtern (2019)
  df_matched <- dfs_simple[[i]] %>% 
    filter(year == 2019)
  
  # 2. Nicht-gematchte Reviere/Kreise ermitteln
  # 'all_reviere' sollte ein Vektor aller krs_id/kz aus den Reviere-Shapes sein
  # 'df_matched$kz' sind die tatsächlich gematchten Kreise
  if(str_detect(names_df, "Model 1")){
    unmatched_reviere <- (setdiff(kap1, df_matched$kz[df_matched$treat==1]))
    n_unmatched <- length(unmatched_reviere)
  } else if(str_detect(names_df, "Model 2")){
    unmatched_reviere <- (setdiff(kap2, df_matched$kz[df_matched$treat==1]))
    n_unmatched <- length(unmatched_reviere)
  } else if(str_detect(names_df, "Model 3")){
    unmatched_reviere <- (setdiff(reviere, df_matched$kz[df_matched$treat==1]))
    n_unmatched <- length(unmatched_reviere)
  }
  
  # 3. Dynamic Caption aufbauen
  if (n_unmatched == 0) {
    caption_text <- NULL
  } else {
    namen_vektor <- namen_liste %>% 
      filter(kz %in% unmatched_reviere) %>% 
      pull(name)
    if (length(namen_vektor) == 0) {
      namen_vektor <- unmatched_reviere
    }
    names_unmatched <- paste(namen_vektor, collapse = ", ")
    
    caption_text <- paste0(
      "Hinweis: Für folgende(n) Landkreis(e) gibt es\nkeinen gematchten Partner:\n", 
      names_unmatched
    )
  }
  
  # 4. Basis-Plot erstellen (für alle Modelle gleich)
  map_i <- ggplot() +
    # Hintergrund: Alle ungematchten Deutschland-Kreise in weiß/grau
    geom_sf(data = df_unmatched, fill = "white", color = "grey80", lwd = 0.2) +
    
    # Gematchte Kreise: Kontrollgruppe (0) & Treatmentgruppe (1)
    geom_sf(data = df_matched, aes(fill = factor(treat)), color = "black", lwd = 0.3) +
    
    # Bundesländergrenzen
    geom_sf(data = shapes_länder, fill = NA, aes(color = "Bundesländergrenzen"), lwd = 0.6)
  
  # 5. MODELL-SPEZIFISCHE UMRISE (if / else if)
  
  if (str_detect(names_df, "Model 1")) {
    # Model 1: Nur Umrisse der Reviere (Kap. 1 + Kap. 2)
    map_i <- map_i +
      geom_sf(data = umriss_RR,   fill = NA, aes(color = "Kap. 1 InvKG"), lwd = 0.7) +
      geom_sf(data = umriss_MR,   fill = NA, aes(color = "Kap. 1 InvKG"), lwd = 0.7) +
      geom_sf(data = umriss_LR,   fill = NA, aes(color = "Kap. 1 InvKG"), lwd = 0.7) +
      labs(subtitle = "Kap. 1 InvKG")
    
  } else if (str_detect(names_df, "Model 2")) {
    # Model 2: Nur Kap. 2 Reviere
    map_i <- map_i +
      geom_sf(data = umriss_kap2, fill = NA, aes(color = "Kap. 2 InvKG"), lwd = 0.7) +
      labs(subtitle = "Kap. 2 InvKG")
    
  } else if (str_detect(names_df, "Model 3")) {
    # Model 3: Alle Reviere (Kapitel 1 + Kapitel 2)
    map_i <- map_i +
      geom_sf(data = umriss_RR,   fill = NA, aes(color = "Kap. 1 InvKG"), lwd = 0.7) +
      geom_sf(data = umriss_MR,   fill = NA, aes(color = "Kap. 1 InvKG"), lwd = 0.7) +
      geom_sf(data = umriss_LR,   fill = NA, aes(color = "Kap. 1 InvKG"), lwd = 0.7) +
      geom_sf(data = umriss_kap2, fill = NA, aes(color = "Kap. 2 InvKG"), lwd = 0.7) +
      labs(subtitle = "Kap. 1 & Kap. 2 InvKG")
  }
  
  # 6. Finale Skalen, Layout und Farbschemata anhängen
  map_i <- map_i +
    labs(
      title = names_df,
      caption = caption_text
    ) +
    scale_fill_manual(
      name = NULL,
      values = c("0" = "lightblue", "1" = "darkblue"),
      labels = c("0" = "Kontrollgruppe", "1" = "Treatmentgruppe"),
      drop = FALSE
    ) +
    scale_color_manual(
      name = NULL,
      values = c(
        "Kap. 1 InvKG" = "red",
        "Kap. 2 InvKG" = "green",
        "Bundesländergrenzen" = "black"
      ), limits = c("Bundesländergrenzen", "Kap. 1 InvKG", "Kap. 2 InvKG"),
      drop = FALSE
    ) +
    guides(
      color = guide_legend(order = 1, override.aes = list(fill = NA, linetype = 1)),
      fill = guide_legend(order = 2, override.aes = list(color = NA))
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 8, hjust = 0.5),
      plot.subtitle = element_text(size = 7, hjust = 0.5),
      plot.caption = element_text(hjust = 0, face = "italic", size = 6),
      legend.position = "right",
      plot.margin   = margin(t = 5, r = 8, b = 5, l = 8)
    )
  
  maplist[[i]] <- map_i
}
print(maplist[[7]])

#maps kombinieren in einen plot
combined_maps <- wrap_plots(maplist, ncol = 3, nrow = 3) +
  plot_layout(
    guides = "collect" # Kombiniert doppelte Legenden sauber am Rand
  ) &
  plot_annotation(
    title = "Treatment- und gematchte Kontrollkreise",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  ) &
  theme(
    legend.position = "right",
    # Schriftgrößen leicht anpassen, damit auf der 9er-Seite nichts überlappt
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 6, hjust = 0)
  )
print(combined_maps)
ggsave(
  filename = "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/MatchingKarten.pdf",
  plot = combined_maps,
  width = 8.5,   # Breite in Zoll (passt perfekt zu DIN A4 Textbreite)
  height = 11,   # Höhe in Zoll
  units = "in")

#koprimieren
pfad_orig <- "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/MatchingKarten.pdf"
pfad_neu  <- "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/MatchingKarten_kompakt2.pdf"
bild <- image_read_pdf(pfad_orig, density = 300)
image_write(bild, path = pfad_neu, format = "pdf")
#=========================================================================================
#deskriptive analyse
#PRE MATCHING
#df unmatched nochmal ohne pgeography einlesen
df <- read_xlsx("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_unmatched.xlsx") %>%
  #Bevölkerung um 1000 runterskalieren
  mutate(c1_Bevölkerung = c1_Bevölkerung / 1000)
ergebnis <- df %>%
  #gebietsreformen werden so rausgefiltert
  filter(!if_all(c(starts_with("z"), starts_with("c")), is.na)) %>%
  group_by(year) %>%
  summarise(anzahl_unique_kz = n_distinct(kz))
print(ergebnis)

vars <- c(
  "GRW", 
  "herfindahl", 
  "z1a_GewerbePro10000",
  "z2a_Sozialhilfe_Anteil", 
  "z3a_WanderungRelativ", 
  "c1_Bevölkerung", 
  "c2_BIP_pro_Kopf", 
  "c6_ALQ", 
  "c7a_LangzeitALQ", 
  "c8_Fläche",
  "c9_jugend", 
  "c9a_alten", 
  "c11a_öffDienst_Q"
)

df_unmatched_filter <- df %>%
  filter(!if_all(c(starts_with("z"), starts_with("c")), is.na)) %>%
  mutate(Gruppe = case_when(
    kz %in% kap1 ~ "Kap1 Kreise",
    kz %in% kap2 ~ "Kap2 Kreise",
    TRUE ~ "Alle Kreise"
  ),
  herfindahl = ifelse(herfindahl == 0, NA, herfindahl))

alle_kreise_stats <- df_unmatched_filter  %>%
    pivot_longer(
      cols = all_of(vars),
      names_to = "variable",
      values_to = "wert"
    ) %>%
    group_by(variable) %>%
    summarise(
      N_alle    = sum(!is.na(wert)),
      Mean_alle = mean(wert, na.rm = TRUE),
      SD_alle   = sd(wert, na.rm = TRUE),
      Min_alle  = min(wert, na.rm = TRUE),
      Max_alle  = max(wert, na.rm = TRUE),
      .groups = "drop"
    )
kap1_stats <- df_unmatched_filter  %>%
  filter(kz %in% kap1) %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "variable",
    values_to = "wert"
  ) %>%
  group_by(variable) %>%
  summarise(
    N_kap1    = sum(!is.na(wert)),
    Mean_kap1 = mean(wert, na.rm = TRUE),
    SD_kap1   = sd(wert, na.rm = TRUE),
    Min_kap1  = min(wert, na.rm = TRUE),
    Max_kap1  = max(wert, na.rm = TRUE),
    .groups = "drop"
  )
kap2_stats <- df_unmatched_filter  %>%
  mutate(herfindahl = herfindahl *100) %>%
  filter(kz %in% kap2) %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "variable",
    values_to = "wert"
  ) %>%
  group_by(variable) %>%
  summarise(
    N_kap2    = sum(!is.na(wert)),
    Mean_kap2 = mean(wert, na.rm = TRUE),
    SD_kap2   = sd(wert, na.rm = TRUE),
    Min_kap2  = min(wert, na.rm = TRUE),
    Max_kap2  = max(wert, na.rm = TRUE),
    .groups = "drop"
  )
insg_stats <- alle_kreise_stats %>%
  left_join(kap1_stats, by = "variable") %>%
  left_join(kap2_stats, by = "variable") %>%
  dplyr::select(mixedsort(names(.))) %>%
  relocate(variable, starts_with("N"), starts_with("Mean"))

kable(insg_stats, format = "latex", booktabs = TRUE, digits = 2)
#===================================================================================
#===================================================================================
#deskriptive analyse
#POST MATCHING
#treated vs. controls
#control daten nochmal einlesen ohne geometry
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
#=====================================================================================
#zuerst kap. 1 und 2 (model 3) stats berechnen (vorher nur kap.1 und kap. 2 getrennt)
kap1_2_stats_matched <- df_matched_z1a  %>%
  #Bevölkerung um 1000 runterskalieren
  mutate(c1_Bevölkerung = c1_Bevölkerung / 1000) %>%
  filter(kz %in% c(kap1, kap2)) %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "variable",
    values_to = "wert"
  ) %>%
  group_by(variable) %>%
  summarise(
    N_insg    = sum(!is.na(wert)),
    Mean_insg = mean(wert, na.rm = TRUE),
    SD_insg   = sd(wert, na.rm = TRUE),
    Min_insg  = min(wert, na.rm = TRUE),
    Max_insg  = max(wert, na.rm = TRUE),
    .groups = "drop"
  )
kap1_stats_matched <- df_matched_kap1_z1a  %>%
  #Bevölkerung um 1000 runterskalieren
  mutate(c1_Bevölkerung = c1_Bevölkerung / 1000) %>%
  filter(kz %in% kap1) %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "variable",
    values_to = "wert"
  ) %>%
  group_by(variable) %>%
  summarise(
    N_kap1    = sum(!is.na(wert)),
    Mean_kap1 = mean(wert, na.rm = TRUE),
    SD_kap1   = sd(wert, na.rm = TRUE),
    Min_kap1  = min(wert, na.rm = TRUE),
    Max_kap1  = max(wert, na.rm = TRUE),
    .groups = "drop"
  )

controls <- list(df_matched_kap1_z1a, df_matched_kap1_z2a, df_matched_kap1_z3a,
                 df_matched_kap2_z1a, df_matched_kap2_z2a, df_matched_kap2_z3a,
                 df_matched_z1a, df_matched_z2a, df_matched_z3a)
controls_stat_list <- list()
for(i in seq_along(controls)){
  df_i <- controls[[i]]
  
  df_temp <- df_i %>%
    #nur für controls filtern
    filter(treat == 0) %>%
    #Bevölkerung um 1000 runterskalieren
    mutate(c1_Bevölkerung = c1_Bevölkerung / 1000) %>%
    pivot_longer(
      cols = all_of(vars),
      names_to = "variable",
      values_to = "wert"
    ) %>%
    group_by(variable) %>%
    summarise(
      N   = sum(!is.na(wert)),
      Mean = mean(wert, na.rm = TRUE),
      SD   = sd(wert, na.rm = TRUE),
      Min = min(wert, na.rm = TRUE),
      Max  = max(wert, na.rm = TRUE),
      .groups = "drop") %>%
    rename_with(~ paste0(.x, "_", i), .cols = 2:6)
  
  controls_stat_list[[i]] <- df_temp
}
#merge into three different datasets
kap1_stats_controls <- controls_stat_list[1:3] %>%
  reduce(left_join, by = "variable") %>%
  left_join(kap1_stats_matched, by = "variable")
kap2_stats_controls <- controls_stat_list[4:6] %>%
  reduce(left_join, by = "variable") %>%
  left_join(kap2_stats, by = "variable")
kap1_2_stats_controls <- controls_stat_list[7:9] %>%
  reduce(left_join, by = "variable") %>%
  left_join(kap1_2_stats_matched, by = "variable")

tabelle_sauber_kap1 <- kap1_stats_controls %>%
  transmute(
    Variable = variable,
    `Treatment (Kap 1)` = sprintf("%.2f (%.2f)", Mean_kap1, SD_kap1),
    `Control z1a`          = sprintf("%.2f (%.2f)", Mean_1, SD_1),
    `Control z2a`          = sprintf("%.2f (%.2f)", Mean_2, SD_2),
    `Control z3a`          = sprintf("%.2f (%.2f)", Mean_3, SD_3),
  ) %>%
  select(Variable, `Treatment (Kap 1)`, `Control z1a`, `Control z2a`, `Control z3a`) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "c", "c", "c", "c"), # Variable linksbündig, Werte zentriert
    caption = "Deskriptive Statistik der Kontroll- und Treatmentgruppen (Kap. 1 InvKG)",
    label = "tab:deskriptiv_stats"
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 9 # Entspricht ca. \small
  ) %>%
  footnote(
    general = "Angetragene Werte stellen jeweils den Mittelwert und in Klammern die Standardabweichung dar.\\\\\\\\BIP in Mio. Euro, Bevölkerung in Tsd.\\\\\\\\ \\\\textit{Quelle:} Eigene Berechnungen.",
    general_title = "Anmerkung: ",
    title_format = "italic",
    threeparttable = TRUE, # Perfekte Breite der Fußnote
    escape = FALSE         # Erlaubt LaTeX-Befehle wie \textit{}
  )
cat(tabelle_sauber_kap1)

tabelle_sauber2 <- kap2_stats_controls %>%
  transmute(
    Variable = variable,
    `Treatment (Kap 2)` = sprintf("%.2f (%.2f)", Mean_kap2, SD_kap2),
    `Control z1a`          = sprintf("%.2f (%.2f)", Mean_4, SD_4),
    `Control z2a`          = sprintf("%.2f (%.2f)", Mean_5, SD_5),
    `Control z3a`          = sprintf("%.2f (%.2f)", Mean_6, SD_6),
  ) %>%
  select(Variable, `Treatment (Kap 2)`, `Control z1a`, `Control z2a`, `Control z3a`) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "c", "c", "c", "c"), # Variable linksbündig, Werte zentriert
    caption = "Deskriptive Statistik der Kontroll- und Treatmentgruppen (Kap. 2 InvKG)",
    label = "tab:deskriptiv_stats"
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 9 # Entspricht ca. \small
  ) %>%
  footnote(
    general = "Angetragene Werte stellen jeweils den Mittelwert und in Klammern die Standardabweichung dar.\\\\\\\\BIP in Mio. Euro, Bevölkerung in Tsd.\\\\\\\\ \\\\textit{Quelle:} Eigene Berechnungen.",
    general_title = "Anmerkung: ",
    title_format = "italic",
    threeparttable = TRUE, # Perfekte Breite der Fußnote
    escape = FALSE         # Erlaubt LaTeX-Befehle wie \textit{}
  )

cat(tabelle_sauber2)

tabelle_sauber3 <- kap1_2_stats_controls %>%
  transmute(
    Variable = variable,
    `Treatment (Kap 1&2)` = sprintf("%.2f (%.2f)", Mean_insg, SD_insg),
    `Control z1a`          = sprintf("%.2f (%.2f)", Mean_7, SD_7),
    `Control z2a`          = sprintf("%.2f (%.2f)", Mean_8, SD_8),
    `Control z3a`          = sprintf("%.2f (%.2f)", Mean_9, SD_9),
  ) %>%
  select(Variable, `Treatment (Kap 1&2)`, `Control z1a`, `Control z2a`, `Control z3a`) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "c", "c", "c", "c"), # Variable linksbündig, Werte zentriert
    caption = "Deskriptive Statistik der Kontroll- und Treatmentgruppen (Kap. 1\\&2 InvKG)",
    label = "tab:deskriptiv_stats"
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 9 # Entspricht ca. \small
  ) %>%
  footnote(
    general = "Angetragene Werte stellen jeweils den Mittelwert und in Klammern die Standardabweichung dar.\\\\\\\\BIP in Mio. Euro, Bevölkerung in Tsd.\\\\\\\\ \\\\textit{Quelle:} Eigene Berechnungen.",
    general_title = "Anmerkung: ",
    title_format = "italic",
    threeparttable = TRUE, # Perfekte Breite der Fußnote
    escape = FALSE         # Erlaubt LaTeX-Befehle wie \textit{}
  )
cat(tabelle_sauber3)

#============================================================================================
#============================================================================================
#Bivariate Korrelationsmatrizen
#unmatched
quantile(df$herfindahl)
df_filter <- df %>%
  dplyr::select(treat, all_of(vars)) %>%
  mutate(herfindahl = herfindahl * 100)
res <- rcorr(as.matrix(df_filter), type = "pearson")
cor_matrix <- res$r
p_matrix   <- res$P
stars_matrix <- matrix("", nrow = nrow(p_matrix), ncol = ncol(p_matrix))
stars_matrix[p_matrix < 0.01] <- "***"
stars_matrix[p_matrix >= 0.01 & p_matrix < 0.05] <- "**"
stars_matrix[p_matrix >= 0.05 & p_matrix < 0.1]  <- "*"

formatted_matrix <- matrix(
  paste0(sprintf("%.2f", cor_matrix), stars_matrix),
  nrow = nrow(cor_matrix),
  dimnames = dimnames(cor_matrix)
)
formatted_matrix[upper.tri(formatted_matrix, diag = TRUE)] <- ""
formatted_matrix <- formatted_matrix[, -ncol(formatted_matrix)]
print(formatted_matrix)

model1 <- list(df_matched_kap1_z1a, df_matched_kap1_z2a, df_matched_kap1_z3a)
model2 <- list(df_matched_kap2_z1a, df_matched_kap2_z2a, df_matched_kap2_z3a)
model3 <- list(df_matched_z1a, df_matched_z2a, df_matched_z3a)

df_model1 <- model1 %>%
  reduce(power_left_join, by = c("kz", "year"), conflict = coalesce_xy) %>%
  dplyr::select(treat, all_of(vars))
df_model2 <- model2 %>%
  reduce(power_left_join, by = c("kz", "year"), conflict = coalesce_xy) %>%
  dplyr::select(treat, all_of(vars))
df_model3 <- model3 %>%
  reduce(power_left_join, by = c("kz", "year"), conflict = coalesce_xy) %>%
  dplyr::select(treat, all_of(vars))

print(rcorr(as.matrix(df_model1), type = "pearson"))
print(rcorr(as.matrix(df_model2), type = "pearson"))
print(rcorr(as.matrix(df_model3), type = "pearson"))

#gesamtmatrix erstellen
df_alle_modelle <- bind_rows(df_model1, df_model2, df_model3)
options(modelsummary_factory_default = "kableExtra")
res <- rcorr(as.matrix(df_alle_modelle))
r <- res$r
p <- res$P

stars <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
mat_formatted <- matrix(paste0(sprintf("%.2f", r), stars), nrow = nrow(r))
rownames(mat_formatted) <- colnames(df_alle_modelle)
colnames(mat_formatted) <- colnames(df_alle_modelle)

# 3. Oberes Dreieck leeren
mat_formatted[upper.tri(mat_formatted, diag = TRUE)] <- ""

# 4. Exportieren
mat_formatted %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Pearson-Korrelationsmatrix aller Variablen",
    label = "korrelation",
    align = "c"
  ) %>%
  kable_styling(
    latex_options = c("scale_down", "hold_position")
  ) %>%
  pack_rows("Panel A: Primäre Zielgrößen", 1, 7) %>%
  pack_rows("Panel B: Kontrollvariablen", 8, 14) %>%
  footnote(
    general = "* p < 0.05, ** p < 0.01, *** p < 0.001.",
    general_title = "Anmerkung: ",
    footnote_as_chunk = TRUE
  )

#einzelmatrizzen erstellen
modelle <- list(model1, model2, model3)
matrizzen <- list()
matrizzen_latex <- list()
for(m in seq_along(modelle)){
  df_temp <- modelle[[m]] %>%
  reduce(power_left_join, by = c("kz", "year"), conflict = coalesce_xy) %>%
  dplyr::select(treat, all_of(vars))
res_i <- rcorr(as.matrix(df_temp), type = "pearson")
cor_matrix_i <- res_i$r
p_matrix_i   <- res_i$P
stars_matrix_i <- matrix("", nrow = nrow(p_matrix_i), ncol = ncol(p_matrix_i))
stars_matrix_i[p_matrix_i < 0.01] <- "***"
stars_matrix_i[p_matrix_i >= 0.01 & p_matrix_i < 0.05] <- "**"
stars_matrix_i[p_matrix_i >= 0.05 & p_matrix_i < 0.1]  <- "*"
formatted_matrix_i <- matrix(
  paste0(sprintf("%.2f", cor_matrix_i), stars_matrix_i),
  nrow = nrow(cor_matrix_i),
  dimnames = dimnames(cor_matrix_i)
)
formatted_matrix_i[upper.tri(formatted_matrix_i, diag = TRUE)] <- ""
formatted_matrix_i <- formatted_matrix_i[, -ncol(formatted_matrix_i)]

latex_corr_i <- formatted_matrix_i %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    align = c("l", rep("c", ncol(formatted_matrix_i) - 1)),
    caption = "Bivariate Korrelationsmatrix (Gematchtes Sample)",
    label = "tab:corr_matrix"
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 9
  ) %>%
  footnote(
    general = "* p < 0.1, ** p < 0.05, *** p < 0.01. Pearson-Korrelationskoeffizienten.",
    general_title = "Anmerkung: ",
    title_format = "italic",
    threeparttable = TRUE
  )

matrizzen[[m]] <- formatted_matrix_i
matrizzen_latex[[m]] <- latex_corr_i
}
treatment_matrix_1 <- matrizzen[[1]][,1] %>% as.data.frame() %>% rownames_to_column(var = "Variable")
treatment_matrix_2 <- matrizzen[[2]][,1] %>% as.data.frame() %>% rownames_to_column(var = "Variable")
treatment_matrix_3 <- matrizzen[[3]][,1] %>% as.data.frame() %>% rownames_to_column(var = "Variable")

treatment_matrix <- list(treatment_matrix_1, treatment_matrix_2, treatment_matrix_3) %>%
  reduce(left_join, by = "Variable") %>%
  rename(`Treatment (Kap.1 InvKG)` = 2, `Treatment (Kap. 2 InvKG)` = 3, `Treatment (Kap. 1&2) InvKG` = 4) %>%
  slice(-1)
latex_corr <- treatment_matrix %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    align = c("l", rep("c", ncol(treatment_matrix) - 1)),
    caption = "Bivariate Korrelationsmatrix (Gematchtes Sample)",
    label = "tab:corr_matrix"
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 9
  ) %>%
  footnote(
    general = "* p < 0.1, ** p < 0.05, *** p < 0.01. Pearson-Korrelationskoeffizienten.",
    general_title = "Anmerkung: ",
    title_format = "italic",
    threeparttable = TRUE
  )
cat(latex_corr)

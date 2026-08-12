#Bachelorarbeit
#Daten aggregieren
#13.07.2026
#Autor: Atencio Psille
#==============================================================================
#liste leeren
rm(list = ls())
#packages laden
library(pacman)
pacman::p_load(readxl, tidyverse, dplyr, gtools, dreamerr, fixest, writexl,
               sf, spdep, powerjoin)
#==============================================================================
#gebiete vordefinieren
RR <- c("05162", "05358", "05362", "05334", "05370", "05366", "05116")
LR <- c("12062", "12066", "12061", "12071", "12052", "14625", "14626")
MR <- c("14713", "14729", "14730", "15084", "15088", "15002", "15087", "15082")
kap2 <- c("03154", "03405", "05978", "05916", "05915", "05112", "05513", "13003",
          "13072", "10044", "10041", "16077")
#GRW definieren
GRW_bis2013 <- c(#SH
  "01001", "01002", "01003", "01004", "01051", "01053", "01054", "01055", "01057", 
  "01058", "01059", "01061",
  #NDS
  "03101", "03102", "03151", "03152", "03153", "03154", "03155", "03156", "03158", "03159",
  "03252", "03255", "03256", "03257", "03351", "03352", "03354", "03355", "03358", "03360",
  "03402", "03403", "03405", "03451", "03452", "03453", "03455", "03456", "03457", "03458",
  "03461", "03462", 
  #BREMEN
  "04011", "04011", 
  #NRW
  "05112", "05116", "05370", "05512", "05513", "05562", "05762", "05766", "05913",
  "05914", "05915", "05916", "05978", 
  #HESSEN
  "06611", "06631", "06632", "06633", "06634", "06635", "06636", 
  #RLP
  "07134", "07312", "07317", "07320", "07333", "07335", "07336", "07340", 
  #BY
  "09672", "09673", "09674", "09463", "09473", "09478", "09476", "09477",
  "09464", "09475", "09462", "09472", "09479", "09377", "09374", "09363",
  "09376", "09372", "09276", "09272", "09262", "09275",
  #SL
  "10041", "10044", "10043",
  #BL
  "11000")
GRW_bis2021 <- c(#SH
  "01001", "01002", "01003", "01004", "01051", "01053", "01054", "01055", "01057", 
  "01058", "01059", "01061",
  #NDS
  "03152", "03153", "03154", "03155", "03156", "03159",
  "03252", "03254", "03255", "03257", "03351", "03352", "03354", "03355", "03358", "03360",
  "03401", "03402", "03403", "03405", "03452", "03455", "03456", "03457", "03458",
  "03462", 
  #BREMEN
  "04011", "04011", 
  #NRW
  "05112", "05116", "05334", "05370", "05512", "05513", "05562", "05762", "05766", "05913",
  "05914", "05915", "05916", "05978", "05122", "05120", "05124", "05114", "05170", "05119",
  "05119", "05113", "05911", "05711", "05758",
  #HESSEN
  "06632", "06635", "06636", "06535", "06531", 
  #RLP
  "07134", "07312", "07317", "07320", "07333", "07335", "07340", "07133", "07135",
  #BY
  "09464", "09475", "09479", "09377", "09374", "09363",
  "09376", "09372", "09276", "09272",
  #SL
  "10041", "10044", "10043",
  #BL
  "11000")
GRW_seit2022 <- c(#SH
  "01001", "01002", "01003", "01004", "01051", "01054", "01055", "01057", 
  "01058", "01059", "01061",
  #NDS
  "03152", "03153", "03154", "03155", "03156", "03159",
  "03252", "03255", "03257", "03352", "03354", "03357", "03358", "03360",
  "03401", "03402", "03403", "03405", "03452", "03455", "03456", "03457", "03458",
  "03462", "03461", "03451", "03453", "03251", "03459", "03404", 
  #BREMEN
  "04011", "04011", 
  #NRW
  "05112", "05116", "05334", "05370", "05512", "05513", "05562", "05762", "05766", "05913",
  "05914", "05915", "05916", "05978", "05122", "05120", "05124", "05170", "05119",
  "05119", "05113", "05911", "05711", "05758", "05358", "05366", "05154", "05954", "05962",
  "05374", "05958", "05774", 
  #HESSEN
  "06635", "06636", "06535", "06437",
  #RLP
  "07134", "07312", "07317", "07320", "07233", "07333", "07335", "07340", "07133", "07135",
  "07131", "07132", "07231", "07211", "07140", "07336", "07319",
  #BY
  "09464", "09475", "09479", "09377", "09374", "09363", "09476",
  "09376", "09372", "09276", "09272",
  #SL
  "10041", "10044", "10043", "10045", "10042", "10046",
  #BL
  "11000")           
#=====================================================================================================
#rohdaten einlesen
#Zu- und Fortzugsdaten (für jedes Jahr eine excel tabelle)
files <- c(
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/zu und fortzüge 2010 bis 2024/12711-04-02-4 (1).xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/zu und fortzüge 2010 bis 2024/12711-04-02-4 (2).xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/zu und fortzüge 2010 bis 2024/12711-04-02-4 (3).xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/zu und fortzüge 2010 bis 2024/12711-04-02-4 (4).xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/zu und fortzüge 2010 bis 2024/12711-04-02-4 (5).xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/zu und fortzüge 2010 bis 2024/12711-04-02-4 (6).xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/zu und fortzüge 2010 bis 2024/12711-04-02-4 (7).xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/zu und fortzüge 2010 bis 2024/12711-04-02-4 (8).xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/zu und fortzüge 2010 bis 2024/12711-04-02-4 (9).xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/zu und fortzüge 2010 bis 2024/12711-04-02-4 (10).xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/zu und fortzüge 2010 bis 2024/12711-04-02-4 (11).xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/zu und fortzüge 2010 bis 2024/12711-04-02-4 (12).xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/zu und fortzüge 2010 bis 2024/12711-04-02-4 (13).xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/zu und fortzüge 2010 bis 2024/12711-04-02-4 (14).xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/zu und fortzüge 2010 bis 2024/12711-04-02-4.xlsx"
)
#liste mit 15 datensätzen erstellen (jeweils im gleichen Format)
df_list <- lapply(files, read_xlsx)

df_list2 <- lapply(df_list, function(df){
  yearval <- df[2,1]                                          #Jahr extrahieren
  df <- df[7:3772, c(1:4,7)]                                  #DF zuschneiden
  names(df) <- c("kz", "name", "alter", "zuzüge", "fortzüge") #Spalten umbenennen
  
  df <- df %>%
    mutate(year = yearval) %>%
    rename(year = 6) %>%
    mutate(year = str_extract(year, "\\d{4}")) %>%            #nur jahreszahl extrahieren
    mutate(across(4:6, as.numeric)) %>%                       #Als numerische variable speichern
    mutate(across(c(kz, name), ~na_if(trimws(.x), ""))) %>%   #Leerzeichen entfernen
    relocate(kz, name, year) %>%
    fill(name, kz, .direction = "down") %>%
    mutate(kz = case_when(                                    #Kreiskennziffer ins richtige Format bringen
      kz == "DG" ~ "1",
      kz == "02" ~ "02000",
      kz == "11" ~ "11000",
      TRUE ~ kz
    )) %>%
    mutate(saldo = zuzüge - fortzüge) %>%                     #Saldo errechnen
    filter(alter == "Insgesamt") %>%
    select(-alter)
})
Bevölkerungssaldo_df <- bind_rows(df_list2) %>%               #Alle einzelnen Dfs in einen Df mergen
  arrange(kz, name, year) %>%
  rename(z3_Wanderung = 6) %>%
  select(kz, year, z3_Wanderung)

#=======================================================================================================
#Restlichen daten hinzufügen
rawdata_files <- c(
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/Bevölkerung bis 2024.xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/BIP bis 2023.xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/bürgergeld bis 2024.xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/erwerbstaetige bis 2024.xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/gebietsfläche bis 2024.xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/gewerbedemografie bis 2024.xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/jugend- & altersquotient bis 2024.xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/oeffDienst.xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/svb bis 2025.xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/ALO.xlsx",
  "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/raw data/bws.xlsx"
)
rawdata <- lapply(rawdata_files, read_xlsx)

#alle dfs die im gleichen format sind:
rawdata_wide <- rawdata[2:11]
 #Leere Liste erstellen für processed Data
processeddata <- list()    

#processed Data erstellen
for (i in seq_along(rawdata_wide)){
  df_name <- rawdata_wide[[i]]
  #Namen aus den jeweiligen Spalten extrahieren
  neue_namen <- sapply(df_name, function(spalte) {
    if (!is.na(spalte[2])) {
      return(as.character(spalte[2]))
    } else if (!is.na(spalte[3])) {
      return(as.character(spalte[3]))
    } else if (!is.na(spalte[4])) {
      return(as.character(spalte[4]))
    } else {
      return(NA) #NA, falls alle drei Zeilen NA sind
    }
  })
  #Duplikate erkennen und ".x" hinzufügen
  for (j in seq_along(neue_namen)) {
    while (neue_namen[j] %in% neue_namen[c(0:(j-1))]) {
      neue_namen[j] <- paste0(neue_namen[j], ".x")
    }
  }  
  #Bereinigte Namen zuweisen
  names(df_name) <- neue_namen
  #alle ins gleiche Format bringen
  temp_df <- df_name %>%
    rename(kz = 1, name = 2) %>%
    fill(kz, .direction = "down") %>%
    mutate(kz = case_when(
      kz == "DG" ~ "1",
      kz == "02" ~ "02000",
      kz == "11" ~ "11000",
      TRUE ~ kz
    )) %>%
    mutate(year = case_when(
      #nach Datum suchen
      str_detect(kz, "\\b\\d{2}\\.\\d{2}\\.\\d{4}$") ~ str_extract(kz, "\\d{4}$"),
      #nach Jahreszahl suchen
      str_detect(kz, "^\\d{4}$") ~ kz,
      TRUE ~ NA
    )) %>%
    fill(year, .direction = "down")
  #Jeweiligen df in Liste abspeichern
  processeddata[[i]] <- temp_df
}

#In einen DF mergen
testdf <- processeddata %>%
  reduce(full_join, by = c("kz", "year")) %>%
  #namen manuell ändern
  rename(name = 2, c2_BIP_pro_Kopf = 5, z2a_SozialhilfeGesamt = 8,
         z2b_Asyl = 14, c12_Erwerbstätige = 16, 
         c12a_Erwerbstätige_Produzierend_BE = 18,
         c12b_Erwerbstätige_Verarbeitende_C = 19,
         c8_Fläche = 25, z1_Gewerbeanmeldungen = 27,
         c9_jugend = 38, c9a_alten = 41, öffDienstKategorie = 45,
         c11_öffDienst = 46, c4_SVB = 54, c5_ALO = 61, c7_LangzeitALO = 67,
         c6ALQ = 69, c3_BWS = 75, c3_BWS_A = 76,
         c3_BWS_BE = 77, c3_BWS_C = 78, c3_BWS_F = 79,
         c3_BWS_GJ = 80, c3_BWS_KN = 81, c3_BWS_OT = 82
  ) %>%
  filter(öffDienstKategorie == "Insgesamt") %>%
  rename(name = 2) %>%
  select(-ends_with(".x"), -ends_with(".y")) %>%
  filter(str_length(kz) == 5 | kz == "1") %>%
  arrange(kz, name, year) %>%
  fill(name, .direction = "down") %>%
  select(kz, name, year, starts_with("z"), starts_with("c"), -Zuzüge) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(Bevölkerungssaldo_df, by = c("kz", "year")) %>%
  .[mixedsort(names(.))] %>%
  relocate(kz, name, year, starts_with("z"))

#Bevölkerungsdaten aufbereiten
#Einzeln, da Bevölkerungsrohdaten in anderem Format sind
Bevölkerung <- rawdata[[1]]
#Nur die Spalten mit insgesamter bev. auswählen (nicht männlich/weiblich)
Bevölkerung <- Bevölkerung[,c(1,2,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45)]
Bevölkerung <- Bevölkerung %>%
  set_names({
    #Jahreszahl aus der der jeweils vierten Zeile extrahieren
    neue_namen <- as.character(.[4, ])
    neue_namen[is.na(neue_namen)] <- paste0("Spalte_", which(is.na(neue_namen)))
    neue_namen <- make.unique(neue_namen, sep = ".")
    neue_namen
  }) %>%
  rename(kz = 1, name = 2) %>%
  #ins long format bringen
  pivot_longer(cols = 3:17,
               names_to = "year",
               values_to = "c1_Bevölkerung") %>%
  filter(!is.na(name)) %>%
  #nur jahreszahl extrahieren
  mutate(year = str_extract(year, "\\d{4}$")) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(c1_Bevölkerung = as.numeric(c1_Bevölkerung)) %>%
  #kz von DE, HH, BER korrigieren
  mutate(kz = case_when(
    kz == "DG" ~ "1",
    kz == "02" ~ "02000",
    kz == "11" ~ "11000",
    TRUE ~ kz
  )) %>%
  select(kz, year, c1_Bevölkerung)

#alles in einen df joinen
final_df <- testdf %>%
  left_join(Bevölkerung, by = c("kz", "year")) %>%
  mutate(across(4:28, as.numeric)) %>%
  rename(c6_ALQ = c6ALQ) %>%
  mutate(z1a_GewerbePro10000 = z1_Gewerbeanmeldungen / c1_Bevölkerung * 10000,
         z2_Sozialhilfe = z2a_SozialhilfeGesamt - z2b_Asyl,
         z2a_Sozialhilfe_Anteil = z2_Sozialhilfe / c1_Bevölkerung * 100,
         c7a_LangzeitALQ = c7_LangzeitALO / c5_ALO * 100,
         c3_BWS_BDE = c3_BWS_BE - c3_BWS_C,
         c3a_BWS_A_proc = c3_BWS_A / c3_BWS,
         c3a_BWS_BDE_proc = c3_BWS_BDE / c3_BWS,
         c3a_BWS_C_proc = c3_BWS_C / c3_BWS,
         c3a_BWS_F_proc = c3_BWS_F / c3_BWS,
         c3a_BWS_GJ_proc = c3_BWS_GJ / c3_BWS,
         c3a_BWS_KN_proc = c3_BWS_KN / c3_BWS,
         c3a_BWS_OT_proc = c3_BWS_OT / c3_BWS,
         herfindahl = rowSums(
           cbind(
             c3a_BWS_A_proc^2, 
             c3a_BWS_BDE_proc^2, 
             c3a_BWS_C_proc^2, 
             c3a_BWS_F_proc^2, 
             c3a_BWS_GJ_proc^2, 
             c3a_BWS_KN_proc^2, 
             c3a_BWS_OT_proc^2
           ), na.rm = TRUE),
         east = case_when(
           str_detect(kz, "^(12|13|14|15|16)") ~ 1,
           TRUE ~ 0
         ),
         treat = case_when(
           kz %in% c(LR, MR, RR, kap2) ~ 1,
           TRUE ~ 0
         )) %>%
  filter(!kz == "1") %>%
  mutate(z3a_WanderungRelativ = z3_Wanderung / c1_Bevölkerung * 100) %>%
  mutate(c12_Erwerbstätige = c12_Erwerbstätige * 1000) %>%
  mutate(c11a_öffDienst_Q = c11_öffDienst / c12_Erwerbstätige * 100) %>%
  .[mixedsort(names(.))] %>%
  relocate(kz, name, year, treat, east, herfindahl, starts_with("z")) 
#========================================================================================
#W-Matrix erstellen
#W-Matrix wird auf Basis von Geodaten aus 2024 erstellt
shape_kreise <- read_sf("C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/Shapefiles/2024/vg250-ew_ebenen_1231/VG250_KRS.shp") %>%
  dplyr::select(AGS, geometry) %>%
  #pro kreis ganzes gebiet in ein geometry mergen (inkl. exklaven usw.)
  group_by(AGS) %>% 
  summarise(geometry = st_union(geometry)) %>%
  ungroup()
#shapedaten in hauptdf mergen
df_shaped <- final_df %>%
  left_join(shape_kreise, by = c("kz" = "AGS")) %>%
  #df in ein SF-Objekt verwandeln
  st_as_sf() %>%
  arrange(kz)
#Geobasis (2024) erstellen
geobasis <- df_shaped %>%
  filter(year == 2024) %>%
  filter(!st_is_empty(geometry))
#Liste der Nachbarn erstellen
#queen Kriterium (mind. 1 eckpunkt reicht)
neighbor_list <- poly2nb(geobasis, queen = TRUE)
#zeilennormierte Gewichtung 
#Kreise ohne Nachbarn werden abgefangen (z.B. Rügen)
neighborweight_list <- nb2listw(neighbor_list, style = "W", zero.policy = TRUE)
master_kz <- geobasis$kz

#Leere Spalte erstellen für Nachbar Treatment
df_shaped$neighbor <- NA
#liste der Jahre erstellen
jahre <- c(as.numeric(2010:2024))
#Schleife erstellen
for(j in jahre){
  #für jedes Jahr ein datensatz erstellen
  df_jahr <- df_shaped %>%
    filter(year == j) %>%
    arrange(kz)
  
  df_jahr_matched <- data.frame(kz = master_kz) %>%
    left_join(df_jahr, by = "kz")
  
  #Räumlichen Lag errechnen
  lag_jahr <- lag.listw(neighborweight_list, 
                        df_jahr_matched$treat, zero.policy = TRUE)
  
  #in hauptdf einfügen
  df_jahr_matched$calculated_lag <- lag_jahr
  
  df_shaped <- df_shaped %>%
    left_join(df_jahr_matched %>% 
                filter(year == j) %>% 
                select(kz, calculated_lag), by = "kz") %>%
    mutate(neighbor = ifelse(year == j, calculated_lag, neighbor)) %>%
    select(-calculated_lag)
}
#GRW Dummy hinzufügen
df_shaped_grw <- df_shaped %>%
  mutate(GRW = case_when(
    (kz %in% GRW_bis2013 | str_detect(kz, "^(12|13|14|15|16)")) 
    & year %in% c(2010:2013) ~ 1,
    (kz %in% GRW_bis2021 | str_detect(kz, "^(12|13|14|15|16)")) 
    & year %in% c(2014:2021) ~ 1, 
    (kz %in% GRW_seit2022 | str_detect(kz, "^(12|13|14|15|16)")) 
    & year %in% c(2022:2024) ~ 1, 
    TRUE ~ 0
  )) %>%
  relocate(kz, name, year, GRW)

#Für Export vorbereiten
df_grw <- df_shaped_grw %>%
  as.data.frame() %>%
  dplyr::select(-geometry)
write_xlsx(df_grw, "C:/Users/lukas/OneDrive/Studium/Bachelorarbeit/Data/processed data/df_unmatched.xlsx")

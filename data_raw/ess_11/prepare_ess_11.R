# ESS 11 – Datenaufbereitung
# Seminarsitzung: 22.01.2026
# ==============================================================================

# Hinweise zum Datenimport ------------------------------------------------

# Speicherort der Rohdaten: siehe data_raw/ess_11/ess_11_citation.md

# Hinweis: Zur besseren Nachvollziehbarkeit der untenstehenden
#          Datenaufbereitung sollten Fragebogen und Codebuch
#          des ESS konsultiert werden.


# Pakete laden ------------------------------------------------------------

# install.packages("dplyr")
# install.packages("readr")

library(dplyr)  # Datenmanipulation
library(readr)  # Datenimport (CSV)


# Daten einlesen und auf Deutschland beschränken --------------------------

ess_11_raw <- read_csv("data_raw/ess_11/ESS11e04_1.csv") |>
  filter(cntry == "DE")  # Auswahl der Befragten aus Deutschland

# Auswahl und Rekodierung der Variablen -----------------------------------

ess_11 <- ess_11_raw |>
  mutate(
    # Hinweis: Die folgende mutate()-Kette enthält viele Rekodierungen.
    # Ziel ist es, aus den ESS-Originalvariablen analysereife,
    # deutschsprachige Variablen mit klaren Kategorien zu erzeugen.
    
    
    # (A) POLITISCHE PARTIZIPATION ----------------------------------------
    
    # Teilnahme an der letzten nationalen Wahl ----------------------------
    wahlbet = case_when(
      vote == 1 ~ "Ja",
      vote == 2 ~ "Nein",
      vote == 3 ~ "Nicht wahlberechtigt"
    ),
    wahlbet = factor(
      wahlbet,
      levels = c("Nein", "Ja", "Nicht wahlberechtigt")
    ),
    
    # Kontakt mit Politiker:in oder Regierungsvertreter:in (letzte 12 Monate)
    kontakt = case_when(
      contplt == 1 ~ "Ja",
      contplt == 2 ~ "Nein"
    ),
    kontakt = factor(kontakt, levels = c("Nein", "Ja")),
    
    # Parteiaktivität (Spende oder Mitarbeit) ------------------------------
    parteiaktiv = case_when(
      donprty == 1 ~ "Ja",
      donprty == 2 ~ "Nein"
    ),
    parteiaktiv = factor(parteiaktiv, levels = c("Nein", "Ja")),
    
    # Petition unterschrieben (letzte 12 Monate) ---------------------------
    petition = case_when(
      sgnptit == 1 ~ "Ja",
      sgnptit == 2 ~ "Nein"
    ),
    petition = factor(petition, levels = c("Nein", "Ja")),
    
    # Teilnahme an Demonstration (letzte 12 Monate) ------------------------
    demo = case_when(
      pbldmna == 1 ~ "Ja",
      pbldmna == 2 ~ "Nein"
    ),
    demo = factor(demo, levels = c("Nein", "Ja")),
    
    # Boykott von Produkten (letzte 12 Monate) -----------------------------
    boykott = case_when(
      bctprd == 1 ~ "Ja",
      bctprd == 2 ~ "Nein"
    ),
    boykott = factor(boykott, levels = c("Nein", "Ja")),

    
    # (B) POLITISCHE EINSTELLUNGEN ----------------------------------------
    
    # Links-rechts-Selbsteinstufung (0–10) ---------------------------------
    links_rechts = if_else(lrscale %in% 0:10, lrscale, NA_real_),
    
    # Umverteilungspräferenz ----------------------------------------------
    umverteilung = case_when(
      gincdif %in% 1:2 ~ "Ja",
      gincdif %in% 3:5 ~ "Nein"
    ),
    umverteilung = factor(umverteilung, levels = c("Nein", "Ja")),
    
    
    # (C) SOZIODEMOGRAFIE -------------------------------------------------
    
    # Gender ----------------------------------------------------------
    gender = case_when(
      gndr == 1 ~ "Männlich",
      gndr == 2 ~ "Weiblich"
    ),
    gender = factor(gender, levels = c("Männlich", "Weiblich")),
    
    # Alter ---------------------------------------------------------------
    alter = case_when(
      agea == 999 ~ NA_real_,  # ESS-Missing-Code 999 wird als NA behandelt
      TRUE ~ agea
    ),
    
    # Haushaltseinkommen --------------------------------------------------
    # Einkommensquintile, basierend auf ESS-Dokumentation (Deutschland)
    einkommen = case_when(
      hinctnta %in% 1:2  ~ "0 – 1.730",
      hinctnta %in% 3:4  ~ "1.731 – 2.600",
      hinctnta %in% 5:6  ~ "2.601 – 3.620",
      hinctnta %in% 7:8  ~ "3.621 – 5.050",
      hinctnta %in% 9:10 ~ "5.051 oder mehr"
    ),
    einkommen = factor(einkommen),
    
    # Bildung -------------------------------------------------------------
    # Rekodierung auf Basis der ISCED-Klassifikation
    bildung = case_when(
      eisced %in% 1:2 ~ "Niedrig",
      eisced %in% 3:5 ~ "Mittel",
      eisced %in% 6:7 ~ "Hoch"
    ),
    bildung = factor(bildung, levels = c("Niedrig", "Mittel", "Hoch")),
    
    # Gewerkschaftsmitgliedschaft -----------------------------------------
    gewerkschaft = case_when(
      mbtru == 1 ~ "Ja",
      mbtru %in% 2:3 ~ "Nein"
    ),
    gewerkschaft = factor(gewerkschaft, levels = c("Nein", "Ja")),
    
    # Migrationshintergrund -----------------------------------------------
    # Mindestens ein Elternteil oder die befragte Person im Ausland geboren
    mighint = case_when(
      brncntr == 2 | facntr == 2 | mocntr == 2 ~ "Ja",
      brncntr == 1 & facntr == 1 & mocntr == 1 ~ "Nein"
    ),
    mighint = factor(mighint, levels = c("Nein", "Ja")),
    
    # Wohnorttyp ----------------------------------------------------------
    wohnort = case_when(
      domicil %in% 1:2 ~ "Großstadt oder Vororte",
      domicil %in% 3:5 ~ "Kleinstadt oder ländlicher Raum"
    ),
    wohnort = factor(
      wohnort,
      levels = c("Kleinstadt oder ländlicher Raum", "Großstadt oder Vororte")
    ),
    
    # Region (Ost-/Westdeutschland) ---------------------------------------
    region = if_else(
      region %in% c("DE3", "DE4", "DE8", "DED", "DEE", "DEG"),
      "Ostdeutschland",
      "Westdeutschland"
    ),
    region = factor(region, levels = c("Westdeutschland", "Ostdeutschland"))
  ) |>
  select(
    wahlbet,
    kontakt,
    parteiaktiv,
    petition,
    demo,
    boykott,
    links_rechts,
    umverteilung,
    gender,
    alter,
    einkommen,
    bildung,
    gewerkschaft,
    mighint,
    wohnort,
    region
  )

# Variablenlabels ---------------------------------------------------------

attr(ess_11$wahlbet, "label") <- "Teilnahme an der letzten Bundestagswahl"
attr(ess_11$kontakt, "label") <- "Kontakt mit Politiker*in oder Regierungsvertreter*in in den letzten 12 Monaten"
attr(ess_11$parteiaktiv, "label") <- "Spende oder Mitarbeit in einer politischen Partei in den letzten 12 Monaten"
attr(ess_11$petition, "label") <- "Petition unterschrieben in den letzten 12 Monaten"
attr(ess_11$demo, "label") <- "Teilnahme an einer Demonstration in den letzten 12 Monaten"
attr(ess_11$boykott, "label") <- "Boykott von Produkten in den letzten 12 Monaten"
attr(ess_11$links_rechts, "label") <- "Links-rechts-Selbsteinstufung"
attr(ess_11$umverteilung, "label") <- "Unterstützung wirtschaftlicher Umverteilung"
attr(ess_11$gender, "label") <- "Gender"
attr(ess_11$alter, "label") <- "Alter"
attr(ess_11$einkommen, "label") <- "Haushaltsnettoeinkommen"
attr(ess_11$bildung, "label") <- "Höchster Bildungsabschluss (ISCED)"
attr(ess_11$gewerkschaft, "label") <- "Derzeitiges Gewerkschaftsmitglied"
attr(ess_11$mighint, "label") <- "Selbst oder Eltern im Ausland geboren"
attr(ess_11$wohnort, "label") <- "Wohnorttyp (Selbsteinschätzung)"
attr(ess_11$region, "label") <- "Region (Ost- / Westdeutschland)"


# Save dataset ------------------------------------------------------------

saveRDS(ess_11, file = "data/ess_11.rds")

# Bundeswahlleiterin
# Seminar session: 2026-01-29
# ==============================================================================

# Source: https://www.bundeswahlleiterin.de/bundestagswahlen/2025/ergebnisse/opendata.html

library(dplyr)
library(readr)

# Import results from Bundeswahlleiter ------------------------------------

bwl_results_raw <- read_csv2("data_raw/bwl_btw_2025/kerg.csv", 
                             skip = 8,
                             col_names = FALSE)

bwl_results <- bwl_results_raw  |> 
  select(wkr_num = X1,
         wkr_name = X2,
         bundesland = X3,
         n_wahlberechtigte = X5,
         n_waehlende = X11) |>
  mutate(across(c(wkr_num, bundesland), as.numeric)) |> 
  filter(!is.na(bundesland) & bundesland != 99) |>
  mutate(wahlbet = (n_waehlende / n_wahlberechtigte) * 100,
         bundesland = case_when(bundesland == 1 ~ "SH",
                                bundesland == 2 ~ "HH",
                                bundesland == 3 ~ "NI",
                                bundesland == 4 ~ "HB",
                                bundesland == 5 ~ "NW",
                                bundesland == 6 ~ "HE",
                                bundesland == 7 ~ "RP",
                                bundesland == 8 ~ "BW",
                                bundesland == 9 ~ "BY",
                                bundesland == 10 ~ "SL",
                                bundesland == 11 ~ "BE",
                                bundesland == 12 ~ "BB",
                                bundesland == 13 ~ "MV",
                                bundesland == 14 ~ "SN",
                                bundesland == 15 ~ "ST",
                                bundesland == 16 ~ "TH"),
         bundesland = factor(bundesland),
         region = case_when(bundesland == "BE" ~ "Berlin",
                            bundesland %in% c("BB", "MV", "SN", "ST", "TH") ~ "Ost",
                            TRUE ~ "West"),
         region = factor(region, levels = c("West", "Ost", "Berlin"))) |>
  select(bundesland,
         region,
         wkr_num,
         wkr_name,
         wahlbet)


# Import structure data ---------------------------------------------------

bwl_str_raw <- read_csv2("data_raw/bwl_btw_2025/btw2025_strukturdaten.csv", 
                             skip = 9)

bwl_str <- bwl_str_raw |>
  select(wkr_num = `Wahlkreis-Nr.`,
         bip = `Bruttoinlandsprodukt 2021 (EUR je EW)`,
         einkommen = `Verfügbares Einkommen der privaten Haushalte 2021 (EUR je EW)`,
         arbeitslos = `Arbeitslosenquote November 2024 - insgesamt`) |>
  mutate(wkr_num = as.numeric(wkr_num),
         bip = bip / 1000,
         einkommen = einkommen / 1000)
  

# Merge data --------------------------------------------------------------

bwl <- bwl_results |>
  left_join(bwl_str, by = join_by(wkr_num))


# Add variable labels -----------------------------------------------------

attr(bwl$bundesland, "label") <- "Bundesland"
attr(bwl$region, "label") <- "Region"
attr(bwl$wkr_num, "label") <- "Wahlkreis-Nr."
attr(bwl$wkr_name, "label") <- "Wahlkreis-Name"
attr(bwl$wahlbet, "label") <- "Wahlbeteiligung, BTW 2025"
attr(bwl$bip, "label") <- "Bruttoinlandsprodukt 2021 (x 1.000 EUR, je EW)"
attr(bwl$einkommen, "label") <- "Verfügbares Einkommen der privaten Haushalte 2021 (x 1.000 EUR, je EW)"
attr(bwl$arbeitslos, "label") <- "Arbeitslosenquote November 2024 - insgesamt"

# Save data ---------------------------------------------------------------

saveRDS(bwl, file = "data/bwl_btw_2025.rds")

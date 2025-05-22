# 1. Carica le librerie e pulisce l'ambiente di lavoro
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library (quantmod)
library(tidyr) 

rm(list = ls())

# 2. Specifica il percorso del file
percorso_file <- "PncPubbl.xlsx"

# 3. Leggi il file Excel
dati_excel <- read_excel(percorso_file, sheet = " Storiche - Historic ", skip = 1)

# 4. Visualizza le prime righe per controllo
print(head(dati_excel))

# 5. Puliamo i nomi delle colonne e il aggiorniamo la tipologia delle variabili
label_colonne <- c("Position_Holder", "Position_Holder_LEI", "Company", "Company_LEI", "Company_ISIN", "PNC", "Date" ) 
colnames(dati_excel) <- label_colonne

dati_excel <- dati_excel %>%
  mutate(
    PNC = as.numeric(PNC),
    Date = as.Date(Date, origin = "1899-12-30"))


# 6. Creiamo i dati aggregati:
# Raggruppiamo per società e per data e calcoliamo la somma delle PNC
dati_excel_pivot <- dati_excel %>%
  group_by(Company, Date) %>%
  summarise(
    Somma_PNC = sum(PNC, na.rm = TRUE),
    .groups = 'drop')


n_distinct(dati_excel_pivot$Company) #181 società
unique (dati_excel_pivot$Company) # quali?


# 7. Iniziamo l'analisi
dati_excel_pivot %>% 
  ggplot () +
  geom_line(aes (x = Date, y = Somma_PNC))+
  facet_wrap(~Company)


nome_azienda <- "AMPLIFON SPA"
ticker_azienda <- "AMP"

dati_excel_pivot %>% 
  filter (Company == nome_azienda) %>% 
  ggplot () +
  geom_line(aes (x = Date, y = Somma_PNC), color = "#0072B2", linewidth = 1) +
  scale_x_date(
    date_labels = "%d %b %Y", # Formato data: es. 21 Mag 2025
    date_breaks = "3 month" # Intervalli sull'asse x (aggiusta se necessario, es. "2 weeks", "3 months")
  ) +
  labs(
    title = paste("Andamento Posizione Netta Corta per", nome_azienda),
    subtitle = "Evoluzione giornaliera della somma delle posizioni nette corte dichiarate",
    x = "Data della Posizione",
    y = "Somma Posizione Netta Corta (%)",
    caption = paste("Fonte: Consob - Dati elaborati il", format(Sys.Date(), "%d %B %Y"))
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # ruota il testo dell'asse X di 45 gradi
  )







############################
# Definiamo il ticker e le date di interesse

lista_aziende <- c("AMPLIFON SPA", "Biesse", "Seco", "SAIPEM SPA", "Tamburi", "Stellantis", "Enel", "Eni")
lista_ticker <- c("AMP.MI", "BSS.MI", "IOT.MI","SPM.MI","TIP.MI","STLAM.MI","ENEL.MI","ENI.MI")


nome_azienda <- "SAIPEM SPA"
ticker_azienda <- lista_ticker[match(nome_azienda, lista_aziende)]
ticker_azienda

data_inizio <- dati_excel_pivot %>%
  filter(Company == nome_azienda) %>%
  pull(Date) %>% # Estrae la colonna Date come vettore
  min(na.rm = TRUE)
  
data_fine <- Sys.Date()     # Fino alla data odierna


# Scarichiamo i dati storici da Yahoo Finance usa auto.assing = FALSE e assegna il risultato, es: price_data <- getSymbols(...)
price_data <- getSymbols(ticker_azienda, 
           src = "yahoo", 
           from = data_inizio, 
           to = data_fine, 
           auto.assign = FALSE)

# dato che getSymbols non restituisce la data come Colonna, la importiamo in modo da poterla usare
price_data <- tibble::rownames_to_column (as.data.frame(price_data), var = "Date")
head (price_data)

# trasformiamo la Data da carattere a Data
price_data <- price_data %>% 
  mutate (Date = as.Date(Date, origin = "1899-12-30"))

# left join per avere tutti i dati di Prezzo ed i giorni in cui vi è una posizione short aperta, e di quanto 
consolidated_data <- left_join(price_data, dati_excel_pivot %>% 
                                 filter (Company == nome_azienda), by = "Date")

# manteniamo solo i dati di Azienda, Prezzo e Somma PNC
consolidated_data <- consolidated_data [,c(1,5,9)]

# togliamo gli NA che sono di fatto 0
consolidated_data <- consolidated_data %>%
  mutate(Somma_PNC = replace_na(Somma_PNC, 0))

# qui aggiungere DELTA PREZZO!!!



# standardizziamo i nomi Colonna
colnames (consolidated_data) <- c("Date", "Price", "Somma_PNC") 



consolidated_data %>% 
  ggplot () +
  geom_line(aes (x = Date, y = Somma_PNC), color = "#0072B2", linewidth = 1) +
  geom_line(aes (x = Date, y = Price/max(Price)*100), color = "#FF0000", linewidth = 1) +
  scale_x_date(
    date_labels = "%d %b %Y", # Formato data: es. 21 Mag 2025
    date_breaks = "3 month" # Intervalli sull'asse x (aggiusta se necessario, es. "2 weeks", "3 months")
  ) +
  labs(
    title = paste("Andamento Prezzo azionario (fatto 100 il massimo) vs Posizione Netta Corta per", nome_azienda),
    subtitle = "Evoluzione giornaliera dei prezzi vs posizioni nette corte dichiarate",
    x = "Data della Posizione",
    y = "Prezzo, Somma Posizione Netta Corta (%)",
    caption = paste("Fonti: Yahoo, Consob - Dati elaborati il", format(Sys.Date(), "%d %B %Y"))
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # ruota il testo dell'asse X di 45 gradi
  )

ggsave (filename = paste(nome_azienda, ".png", sep= ""), scale = 2, dpi = 600 )









################################################################################
consolidated_data %>% 
  ggplot () +
  geom_point(aes (x = Somma_PNC, y = Price/max(Price)*100), color = "#00FF00") 
  
consolidated_data <- consolidated_data %>%
  arrange(Date) %>% # Assicura l'ordinamento per data (fondamentale per lag())
  mutate(
    Delta_Price = (Price - lag(Price))/lag(Price),
    Delta_Somma_PNC = (Somma_PNC - lag(Somma_PNC))/lag(Somma_PNC)
  ) %>% 
  filter(!is.na(Delta_Price))  %>% 
  filter(!is.na(Delta_Somma_PNC)) %>% 
  filter(!is.infinite (Delta_Somma_PNC))

cor (consolidated_data$Price, consolidated_data$Somma_PNC)
cor (consolidated_data$Delta_Price, consolidated_data$Somma_PNC)
cor (consolidated_data$Delta_Price, consolidated_data$Delta_Somma_PNC)

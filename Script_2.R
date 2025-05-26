# ==============================================================================
# SCRIPT RIVISTO - MINIMAL CHANGES
# Analisi Correlazione Delta Prezzo vs Delta PNC
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP: CARICAMENTO LIBRERIE E PULIZIA AMBIENTE
# ------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(quantmod)
library(tidyr)
library(tibble) # Aggiunta per rownames_to_column

# Pulisce l'ambiente (come nel tuo codice)
rm(list = ls())

# ------------------------------------------------------------------------------
# 2. CARICAMENTO E PULIZIA DATI PNC (DAL TUO CODICE)
# ------------------------------------------------------------------------------
# Specifica il percorso del file (come nel tuo codice)
percorso_file <- "PncPubbl.xlsx"

# Leggi il file Excel (come nel tuo codice)
dati_excel <- read_excel(percorso_file, sheet = " Storiche - Historic ", skip = 1)

# Visualizza le prime righe per controllo (come nel tuo codice)
print(head(dati_excel))

# Pulisce i nomi e aggiorna i tipi (come nel tuo codice)
label_colonne <- c("Position_Holder", "Position_Holder_LEI", "Company", "Company_LEI", "Company_ISIN", "PNC", "Date" ) 
colnames(dati_excel) <- label_colonne

dati_excel <- dati_excel %>%
  mutate(
    PNC = as.numeric(PNC),
    # Usiamo direttamente as.Date, spesso funziona bene con Excel. 
    # Se riscontri problemi, torna a as.Date(Date, origin = "1899-12-30")
    Date = as.Date(Date) 
  )

# ------------------------------------------------------------------------------
# 3. AGGREGAZIONE DATI PNC (DAL TUO CODICE)
# ------------------------------------------------------------------------------
# Raggruppiamo per società e per data e calcoliamo la somma delle PNC
dati_excel_pivot <- dati_excel %>%
  group_by(Company, Date) %>%
  summarise(
    Somma_PNC = sum(PNC, na.rm = TRUE),
    .groups = 'drop'
  )

# ------------------------------------------------------------------------------
# 4. DEFINIZIONE AZIENDE E CICLO FOR (DAL TUO CODICE, CON PICCOLE MODIFICHE)
# ------------------------------------------------------------------------------
# Definiamo il ticker e le date di interesse (come nel tuo codice)
lista_aziende <- c("AMPLIFON SPA", "BIESSE SPA", "SECO SPA", "SAIPEM SPA", "TAMBURI INVESTMENT PARTNERS SPA", "STELLANTIS NV", "ENEL SPA", "ENI SPA")
lista_ticker <- c("AMP.MI", "BSS.MI", "IOT.MI","SPM.MI","TIP.MI","STLAM.MI","ENEL.MI","ENI.MI")

# Inizializziamo la lista (come nel tuo codice, ma rinominata `consolidated_data_list`)
consolidated_data_list <- list()

for (azienda in lista_aziende) {
  
  nome_azienda <- azienda
  ticker_azienda <- lista_ticker[match(nome_azienda, lista_aziende)]
  print(paste("Sto processando:", nome_azienda, "(", ticker_azienda, ")"))
  
  # ** NOTA: Manteniamo la data fissa come nel tuo codice, sovrascrivendo il calcolo. **
  # data_inizio_calc <- dati_excel_pivot %>%
  #   filter(Company == nome_azienda) %>%
  #   pull(Date) %>%   #Estrae la colonna Date come vettore
  #   min(na.rm = TRUE)
  data_inizio <- "2020-01-01"
  data_fine <- Sys.Date()
  
  # Scarichiamo i dati storici (aggiunto tryCatch per robustezza minima)
  price_data <- tryCatch({
    getSymbols(ticker_azienda, 
               src = "yahoo", 
               from = data_inizio, 
               to = data_fine, 
               auto.assign = FALSE)
  }, error = function(e) {
    warning("Errore download per ", ticker_azienda, ": ", e$message)
    return(NULL)
  })
  
  # Se c'è stato un errore, salta al prossimo ticker
  if (is.null(price_data)) { next }
  
  # Trasformiamo e puliamo (come nel tuo codice)
  price_data <- tibble::rownames_to_column(as.data.frame(price_data), var = "Date")
  price_data <- price_data %>% 
    mutate(Date = as.Date(Date)) # Tolto 'origin' per semplicità, se dà problemi, rimettilo.
  
  # Left join (come nel tuo codice)
  company_data <- left_join(price_data, 
                            dati_excel_pivot %>% filter(Company == nome_azienda), 
                            by = "Date")
  
  # ** NOTA: Manteniamo la selezione per indice c(1, 5, 9) come nel tuo codice, **
  # ** anche se è fragile. Assicurati che 5 sia il prezzo e 9 la PNC. **
  # ** Molto probabilmente la colonna 5 è 'Close', non 'Adjusted'. **
  company_data <- company_data[, c(1, 5, 9)]
  
  # ** NOTA: Manteniamo replace_na(0) come nel tuo codice, anche se 'fill' sarebbe meglio. **
  company_data <- company_data %>%
    mutate(Somma_PNC = replace_na(Somma_PNC, 0))
  
  # Rinominiamo le colonne (come nel tuo codice)
  colnames(company_data) <- c("Date", "Price", "Somma_PNC") 
  
  # Aggiungiamo l'azienda e salviamo nella lista (come nel tuo codice)
  company_data$Company <- nome_azienda
  consolidated_data_list[[azienda]] <- company_data
}

# ------------------------------------------------------------------------------
# 5. COMBINAZIONE DATI E CALCOLO DELTA (MODIFICATO PER CHIAREZZA)
# ------------------------------------------------------------------------------
# Combiniamo i dati (come nel tuo codice)
consolidated_data_final <- dplyr::bind_rows(consolidated_data_list)

# Ordiniamo i dati (come nel tuo codice)
consolidated_data_final <- consolidated_data_final %>%
  select(Company, Date, Price, Somma_PNC) %>%
  arrange(Company, Date)

# ** MODIFICA CHIAVE: Calcoliamo i Delta **
consolidated_data_final <- consolidated_data_final %>%
  arrange(Company, Date) %>% 
  group_by(Company) %>%
  mutate(
    # Calcoliamo il Delta Prezzo come variazione percentuale
    Delta_Price = (Price - lag(Price)) / lag(Price),
    
    # ** Calcoliamo il Delta PNC come variazione percentuale (come nel tuo codice). **
    # ** ATTENZIONE: Questo può generare 'Inf' (infinito) se lag(Somma_PNC) è 0. **
    # ** Li filtreremo via prima del plot. **
    Delta_Somma_PNC = (Somma_PNC - lag(Somma_PNC)) / lag(Somma_PNC)
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 6. PREPARAZIONE DATI E PLOTTING (MODIFICATO PER OBIETTIVO)
# ------------------------------------------------------------------------------
# Prepariamo i dati per il grafico, focalizzandoci su DELTA vs DELTA
plot_data <- consolidated_data_final %>%
  # Filtriamo via NA (da lag) e Inf (da divisione per 0 in Delta_Somma_PNC)
  filter(!is.na(Delta_Price) & !is.na(Delta_Somma_PNC)) %>%
  filter(!is.infinite(Delta_Price) & !is.infinite(Delta_Somma_PNC)) %>%
  
  # Raggruppiamo per calcolare la correlazione PER OGNI AZIENDA.
  group_by(Company) %>%
  
  # Calcoliamo la correlazione TRA I DELTA.
  mutate(
    # Aggiungiamo un controllo per evitare errori se ci sono pochi dati.
    correlation = if(n() > 2) cor(Delta_Somma_PNC, Delta_Price, use = "complete.obs") else NA_real_,
    
    # ** MODIFICA CHIAVE: Usiamo ifelse() invece di if() **
    cor_label = paste0(Company, "\nCor: ", 
                       ifelse(is.na(correlation), 
                              "N/A", 
                              round(correlation, 2))
    )
  ) %>%
  ungroup()

# Se plot_data è vuoto dopo i filtri, avvisa l'utente.
if(nrow(plot_data) == 0) {
  stop("ATTENZIONE: Nessun dato rimanente dopo aver filtrato NA e Inf. Controlla i calcoli dei Delta e i dati di input.")
}

# Creiamo il 'labeller' (come nel tuo codice)
facet_labels <- setNames(plot_data$cor_label, plot_data$Company)

# ** MODIFICA CHIAVE: Creiamo il grafico DELTA vs DELTA **
mio_grafico <- ggplot(plot_data, aes(x = Delta_Somma_PNC, y = Delta_Price)) +
  geom_point(color = "#0072B2", alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~Company, 
             scales = "free", 
             labeller = labeller(Company = facet_labels)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) + 
  labs(
    title = "Correlazione tra Variazione PNC (%) e Variazione Prezzo (%)",
    subtitle = "Analisi giornaliera per singola azienda",
    x = "Delta giornaliero Posizione Netta Corta (%)", 
    y = "Delta giornaliero Prezzo (%)", 
    caption = "Fonte: Elaborazione dati Consob e Yahoo Finance"
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8)
  )

# Visualizza il grafico a schermo (opzionale, ma utile per controllo)
print(mio_grafico)

# ==============================================================================
# 11. SALVATAGGIO DEL GRAFICO IN A3 ALTA QUALITÀ
# ==============================================================================

# --- OPZIONE 1: Salva come PDF (Consigliato per la stampa) ---
ggsave(
  filename = "Grafico_Correlazione_A3.pdf", # Nome del file che verrà creato
  plot = mio_grafico,                       # Il grafico da salvare
  width = 420,                              # Larghezza in mm (A3 Landscape)
  height = 297,                             # Altezza in mm (A3 Landscape)
  units = "mm",                             # Unità di misura
  dpi = 600,                                # Risoluzione alta (Dots Per Inch)
  device = cairo_pdf                        # Usa 'cairo_pdf' per una migliore gestione di font e trasparenze (se hai Cairo installato)
  # Se dà errore, prova semplicemente device = "pdf"
)

# --- OPZIONE 2: Salva come PNG (Alternativa raster ad alta qualità) ---
# ggsave(
#  filename = "Grafico_Correlazione_A3.png", # Nome del file che verrà creato
#  plot = mio_grafico,                       # Il grafico da salvare
#  width = 420,                              # Larghezza in mm (A3 Landscape)
#  height = 297,                             # Altezza in mm (A3 Landscape)
#  units = "mm",                             # Unità di misura
#  dpi = 600                                 # Risoluzione alta (Dots Per Inch)
# )

# Messaggio di conferma
print("Grafico salvato con successo come 'Grafico_Correlazione_A3.pdf'")
# ==============================================================================
# FINE SCRIPT
# ==============================================================================
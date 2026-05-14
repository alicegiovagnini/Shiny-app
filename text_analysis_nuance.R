# ============================================================================
#  SCI - Strategic & Competitive Intelligence
#  Lab: Text Analysis (Bibliometric & Co-word Analysis)
#  Project: Nuance Audio (EssilorLuxottica) — Hearing Glasses Landscape
#  Author: Alice (Master DSBI, A.Y. 2024/2025)
#  Method: bibliometrix + tidyverse + tidytext (per il prof. Spada)
# ============================================================================
#
# OBIETTIVO
# ---------
# Analizzare il landscape scientifico delle smart glasses con focus sul
# sottocampo audio/hearing per posizionare Nuance Audio in un quadro
# tecnologico-competitivo basato su evidenze bibliometriche.
#
# OUTPUT
# ------
# - Oggetti .rds salvati in ./output/  (per riuso nella Shiny app)
# - Plot .png in ./output/figures/    (per il report e l'app)
# - Tabelle .csv di sintesi in ./output/tables/
#
# COME ESEGUIRE
# -------------
# 1. Aggiorna la variabile FILE_PATH qui sotto col path locale del tuo CSV Scopus.
# 2. Esegui tutto lo script in RStudio (Source) oppure sezione per sezione (Run).
# 3. La sezione 4 (corpus completo) e 5 (subset audio) sono indipendenti.
# 4. Tempo stimato: ~2-4 minuti su dataset 879 paper.
#
# ============================================================================


# 0. SETUP --------------------------------------------------------------------

# Pacchetti necessari (decommentare la prima volta per installare)
# install.packages(c("bibliometrix", "tidyverse", "tidytext", "igraph",
#                    "ggraph", "widyr", "scales", "patchwork", "viridis"))

suppressPackageStartupMessages({
  library(bibliometrix)
  library(tidyverse)
  library(tidytext)
  library(igraph)
  library(ggraph)
  library(widyr)
  library(scales)
  library(patchwork)
  library(viridis)
})


# >>> AGGIORNA QUESTO PATH <<<
FILE_PATH <- "scopus_glasses.csv"   # path al CSV scaricato da Scopus

# Cartelle di output
OUT_DIR    <- "output"
FIG_DIR    <- file.path(OUT_DIR, "figures")
TAB_DIR    <- file.path(OUT_DIR, "tables")
RDS_DIR    <- file.path(OUT_DIR, "rds")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RDS_DIR, recursive = TRUE, showWarnings = FALSE)

# Tema ggplot coerente con la Shiny app (dark theme + accenti emerald)
theme_nuance <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.background  = element_rect(fill = "#0e1117", color = NA),
      panel.background = element_rect(fill = "#0e1117", color = NA),
      panel.grid.major = element_line(color = "#1f2937", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      text             = element_text(color = "#e5e7eb"),
      axis.text        = element_text(color = "#9ca3af"),
      plot.title       = element_text(color = "#10b981", face = "bold", size = base_size + 3),
      plot.subtitle    = element_text(color = "#9ca3af", size = base_size - 1),
      plot.caption     = element_text(color = "#6b7280", size = base_size - 3, hjust = 1),
      legend.background = element_rect(fill = "#0e1117", color = NA),
      legend.key       = element_rect(fill = "#0e1117", color = NA),
      strip.background = element_rect(fill = "#1f2937", color = NA),
      strip.text       = element_text(color = "#10b981", face = "bold")
    )
}

# Palette emerald-multicolor (allineata alla app)
PAL <- c("#10b981", "#3b82f6", "#f59e0b", "#ef4444", "#8b5cf6",
         "#ec4899", "#14b8a6", "#f97316", "#06b6d4", "#a855f7")


# 1. IMPORT DATI --------------------------------------------------------------
cat("=== [1] Importing Scopus collection...\n")

# convert2df parsifica il CSV Scopus in un dataframe bibliografico
M <- convert2df(file = FILE_PATH, dbsource = "scopus", format = "csv")

cat(sprintf("  -> %d documents imported\n", nrow(M)))
cat(sprintf("  -> %d columns (bibliometric fields)\n", ncol(M)))

# Salvo il dataframe grezzo per riuso
saveRDS(M, file.path(RDS_DIR, "M_full.rds"))


# 2. ANALISI BIBLIOMETRICA GENERALE -------------------------------------------
cat("\n=== [2] Bibliometric analysis on FULL corpus...\n")

# === [2] Bibliometric analysis on FULL corpus...

# Forza l'estrazione dei paesi dalle affiliazioni (risolve il crash)
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")

results <- biblioAnalysis(M, sep = ";")
S <- summary(object = results, k = 20, pause = FALSE, verbose = FALSE)

# Salva per Shiny app
saveRDS(results, file.path(RDS_DIR, "results_biblio_full.rds"))
saveRDS(S,       file.path(RDS_DIR, "summary_biblio_full.rds"))

# --- 2a. Documenti per anno --------------------------------------------------
docs_per_year <- as_tibble(table(M$PY), .name_repair = "unique") |>
  setNames(c("Year", "Documents")) |>
  mutate(Year = as.integer(as.character(Year)))

write_csv(docs_per_year, file.path(TAB_DIR, "docs_per_year.csv"))

p_year <- docs_per_year |>
  ggplot(aes(x = Year, y = Documents)) +
  geom_col(fill = PAL[1], alpha = 0.85) +
  geom_text(aes(label = Documents), vjust = -0.5, color = "#e5e7eb", size = 3.2) +
  scale_x_continuous(breaks = unique(docs_per_year$Year)) +
  labs(
    title    = "Annual Scientific Production — Smart Glasses",
    subtitle = sprintf("Scopus collection, %d documents (2018–2026)", nrow(M)),
    x = NULL, y = "Number of documents",
    caption  = "Source: Scopus (query: smart glasses)"
  ) +
  theme_nuance()

ggsave(file.path(FIG_DIR, "01_docs_per_year.png"), p_year, width = 10, height = 5.5, dpi = 150, bg = "#0e1117")


# --- 2b. Top 15 author keywords ---------------------------------------------
top_DE <- as_tibble(results$DE, .name_repair = "unique") |>
  count(Tab, sort = TRUE) |>
  rename(Keyword = Tab, Frequency = n) |>
  slice_head(n = 15)

write_csv(top_DE, file.path(TAB_DIR, "top15_author_keywords_full.csv"))

p_DE <- top_DE |>
  mutate(Keyword = fct_reorder(Keyword, Frequency)) |>
  ggplot(aes(x = Frequency, y = Keyword)) +
  geom_col(fill = PAL[1], alpha = 0.85) +
  geom_text(aes(label = Frequency), hjust = -0.2, color = "#e5e7eb", size = 3.4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Top 15 Author Keywords — Smart Glasses Field",
    subtitle = "Most frequent terms used by authors to describe their work",
    x = "Occurrences", y = NULL
  ) +
  theme_nuance()

ggsave(file.path(FIG_DIR, "02_top15_author_keywords.png"), p_DE, width = 10, height = 6, dpi = 150, bg = "#0e1117")


# --- 2c. Top 15 sources (riviste/conferenze) --------------------------------
top_sources <- as_tibble(results$Sources, .name_repair = "unique") |>
  setNames(c("Source", "Documents")) |>
  slice_head(n = 15) |>
  mutate(Source = str_trunc(as.character(Source), 55))

write_csv(top_sources, file.path(TAB_DIR, "top15_sources.csv"))

p_src <- top_sources |>
  mutate(Source = fct_reorder(Source, Documents)) |>
  ggplot(aes(x = Documents, y = Source)) +
  geom_col(fill = PAL[2], alpha = 0.85) +
  geom_text(aes(label = Documents), hjust = -0.2, color = "#e5e7eb", size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Top 15 Sources",
    subtitle = "Journals and conferences with the highest output",
    x = "Documents", y = NULL
  ) +
  theme_nuance()

ggsave(file.path(FIG_DIR, "03_top15_sources.png"), p_src, width = 11, height = 6.5, dpi = 150, bg = "#0e1117")


# --- 2d. Top 15 paesi (corresponding author) --------------------------------
# --- 2d. Top 15 paesi (corresponding author) --------------------------------
# Controlliamo che i dati geografici esistano prima di plottarli
if (!is.null(results$CountriesCollaboration) && nrow(results$CountriesCollaboration) > 0) {
  
  df_countries <- as.data.frame(results$CountriesCollaboration)
  colnames(df_countries)[1] <- "Country" # Diamo un nome forzato alla colonna vuota
  
  top_countries <- as_tibble(df_countries) |>
    arrange(desc(Articles)) |>
    slice_head(n = 15)
  
  write_csv(top_countries, file.path(TAB_DIR, "top15_countries.csv"))
  
  p_ctry <- top_countries |>
    mutate(Country = fct_reorder(Country, Articles)) |>
    pivot_longer(c(SCP, MCP), names_to = "Type", values_to = "n") |>
    ggplot(aes(x = n, y = Country, fill = Type)) +
    geom_col(alpha = 0.9) +
    scale_fill_manual(
      values = c(SCP = PAL[1], MCP = PAL[3]),
      labels = c(SCP = "Single-country (SCP)", MCP = "Multi-country (MCP)")
    ) +
    labs(
      title    = "Top 15 Countries by Scientific Production",
      subtitle = "SCP = single-country publications · MCP = multi-country (international collaboration)",
      x = "Articles", y = NULL, fill = NULL
    ) +
    theme_nuance() +
    theme(legend.position = "top")
  
  ggsave(file.path(FIG_DIR, "04_top15_countries.png"), p_ctry, width = 10, height = 6, dpi = 150, bg = "#0e1117")
  
} else {
  cat("  -> SKIP Grafico Paesi: Dati geografici non sufficienti nel CSV.\n")
}


# 3. CO-WORD ANALYSIS — FULL CORPUS -------------------------------------------
cat("\n=== [3] Co-word analysis (conceptual structure, MDS)...\n")

# Determino minDegree dinamicamente: prendo la frequenza del 30esimo keyword
freq_DE <- as_tibble(results$DE) |> count(Tab, sort = TRUE)
min_deg <- max(5, freq_DE$n[min(30, nrow(freq_DE))])
cat(sprintf("  -> minDegree adattivo: %d (taglio sui top-30 keyword)\n", min_deg))

CS <- conceptualStructure(
  M, field = "DE", method = "MDS",
  minDegree = min_deg, clust = 5,
  stemming = TRUE, labelsize = 8,
  documents = 10, graph = FALSE
)

saveRDS(CS, file.path(RDS_DIR, "conceptual_structure_full.rds"))

# Salvo il plot bibliometrix (ggplot2 object)
ggsave(file.path(FIG_DIR, "05_conceptual_structure_MDS.png"),
       CS$graph_terms,
       width = 11, height = 8, dpi = 150, bg = "white")


# 4. KEYWORD CO-OCCURRENCE NETWORK --------------------------------------------
cat("\n=== [4] Keyword co-occurrence network (bibliometrix biblioNetwork)...\n")

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences",
                           network = "keywords", sep = ";")

saveRDS(NetMatrix, file.path(RDS_DIR, "netmatrix_keywords_full.rds"))

# Plot del network — uso networkPlot di bibliometrix
png(file.path(FIG_DIR, "06_keyword_cooccurrence_network.png"),
    width = 1800, height = 1400, res = 150)
net <- networkPlot(
  NetMatrix, n = 40, Title = "Keyword Co-occurrence Network — Smart Glasses",
  type = "fruchterman", size.cex = TRUE, size = 10,
  remove.multiple = TRUE, labelsize = 1.0,
  edgesize = 4, label.cex = TRUE, label.n = 40,
  cluster = "louvain"
)
dev.off()

# Estraggo le statistiche del network e le centrality measures (utili in app)
net_stats <- net$networkStats
saveRDS(net,       file.path(RDS_DIR, "net_keywords_full.rds"))
saveRDS(net_stats, file.path(RDS_DIR, "net_stats_keywords_full.rds"))


# 5. FOCUS NUANCE AUDIO — SUBSET HEARING/AUDIO --------------------------------
cat("\n=== [5] FOCUS: subset audio/hearing for Nuance Audio positioning...\n")

# Filtro: paper che menzionano hearing/audio in titolo, abstract o keyword
hearing_pattern <- regex(
  "\\b(hearing|audio|audiolog|hearing\\s*aid|hearing\\s*loss|sound|speech\\s*enhancement|speech\\s*recognition|beamforming|cocktail\\s*party|deaf|hard[\\s-]of[\\s-]hearing)\\b",
  ignore_case = TRUE
)

is_hearing <- str_detect(
  paste(
    replace_na(M$TI, ""),
    replace_na(M$AB, ""),
    replace_na(M$DE, ""),
    replace_na(M$ID, "")
  ),
  hearing_pattern
)

M_audio <- M[is_hearing, ]
cat(sprintf("  -> Subset audio/hearing: %d documents (%.1f%% del corpus)\n",
            nrow(M_audio), 100*nrow(M_audio)/nrow(M)))

saveRDS(M_audio, file.path(RDS_DIR, "M_audio.rds"))

# --- 5a. Bibliometric analysis sul subset -----------------------------------
results_audio <- biblioAnalysis(M_audio, sep = ";")
saveRDS(results_audio, file.path(RDS_DIR, "results_biblio_audio.rds"))

top_DE_audio <- as_tibble(results_audio$DE) |>
  count(Tab, sort = TRUE) |>
  rename(Keyword = Tab, Frequency = n) |>
  slice_head(n = 20)

write_csv(top_DE_audio, file.path(TAB_DIR, "top20_keywords_audio.csv"))

p_DE_audio <- top_DE_audio |>
  mutate(Keyword = fct_reorder(Keyword, Frequency)) |>
  ggplot(aes(x = Frequency, y = Keyword)) +
  geom_col(fill = PAL[1], alpha = 0.85) +
  geom_text(aes(label = Frequency), hjust = -0.2, color = "#e5e7eb", size = 3.4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Top 20 Keywords — Audio/Hearing Smart Glasses Subfield",
    subtitle = sprintf("Subset: %d documents on hearing-related smart eyewear", nrow(M_audio)),
    x = "Occurrences", y = NULL,
    caption  = "Subfield aligned with Nuance Audio's positioning"
  ) +
  theme_nuance()

ggsave(file.path(FIG_DIR, "07_top20_keywords_AUDIO.png"), p_DE_audio,
       width = 10, height = 7, dpi = 150, bg = "#0e1117")


# --- 5b. Co-word network sul subset audio (più rilevante per Nuance) -------
cat("  -> Building co-occurrence network on audio subset...\n")

# minDegree più basso perché il subset è più piccolo
freq_DE_audio <- as_tibble(results_audio$DE) |> count(Tab, sort = TRUE)
min_deg_audio <- max(3, freq_DE_audio$n[min(25, nrow(freq_DE_audio))])

NetMatrix_audio <- biblioNetwork(M_audio, analysis = "co-occurrences",
                                  network = "keywords", sep = ";")
saveRDS(NetMatrix_audio, file.path(RDS_DIR, "netmatrix_keywords_audio.rds"))

png(file.path(FIG_DIR, "08_keyword_network_AUDIO.png"),
    width = 1800, height = 1400, res = 150)
net_audio <- networkPlot(
  NetMatrix_audio, n = 30,
  Title = "Keyword Co-occurrence Network — Hearing/Audio Subfield",
  type = "fruchterman", size.cex = TRUE, size = 12,
  remove.multiple = TRUE, labelsize = 1.0,
  edgesize = 4, label.cex = TRUE, label.n = 30,
  cluster = "louvain"
)
dev.off()
saveRDS(net_audio, file.path(RDS_DIR, "net_audio.rds"))


# --- 5c. Conceptual structure sul subset audio ------------------------------
if (nrow(M_audio) >= 30) {
  CS_audio <- conceptualStructure(
    M_audio, field = "DE", method = "MDS",
    minDegree = min_deg_audio, clust = 4,
    stemming = TRUE, labelsize = 8,
    documents = 10, graph = FALSE
  )
  saveRDS(CS_audio, file.path(RDS_DIR, "conceptual_structure_audio.rds"))

  ggsave(file.path(FIG_DIR, "09_conceptual_structure_AUDIO.png"),
         CS_audio$graph_terms,
         width = 11, height = 8, dpi = 150, bg = "white")
}


# 6. TIDYTEXT — N-GRAM ANALYSIS SU ABSTRACT (subset audio) -------------------
cat("\n=== [6] Tidytext: bigrams from abstracts (audio subset)...\n")

# Stopwords aggiuntive specifiche del dominio (rumore tipico negli abstract)
domain_stop <- tibble(word = c(
  "study", "results", "paper", "approach", "method", "based", "proposed",
  "system", "systems", "data", "model", "models", "analysis", "research",
  "show", "shows", "showed", "found", "using", "used", "use", "uses",
  "also", "two", "three", "one", "first", "second", "however", "moreover",
  "due", "achieved", "obtained", "different", "various", "high", "low",
  "well", "many", "much", "yet", "i.e", "e.g", "et", "al",
  "smart", "glass", "glasses"   # tolgo i termini ovvi del dominio
))

abstracts_tbl <- tibble(
  doc_id = seq_len(nrow(M_audio)),
  text   = tolower(replace_na(M_audio$AB, ""))
) |> filter(text != "")

# Bigrammi
bigrams <- abstracts_tbl |>
  unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
  separate(bigram, c("w1", "w2"), sep = " ", remove = FALSE) |>
  filter(!w1 %in% stop_words$word,
         !w2 %in% stop_words$word,
         !w1 %in% domain_stop$word,
         !w2 %in% domain_stop$word,
         !str_detect(w1, "\\d"),
         !str_detect(w2, "\\d"),
         nchar(w1) > 2, nchar(w2) > 2)

top_bigrams <- bigrams |>
  count(bigram, sort = TRUE) |>
  slice_head(n = 25)

write_csv(top_bigrams, file.path(TAB_DIR, "top25_bigrams_audio.csv"))

p_bigrams <- top_bigrams |>
  mutate(bigram = fct_reorder(bigram, n)) |>
  ggplot(aes(x = n, y = bigram)) +
  geom_col(fill = PAL[5], alpha = 0.85) +
  geom_text(aes(label = n), hjust = -0.2, color = "#e5e7eb", size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Top 25 Bigrams from Abstracts — Audio/Hearing Smart Glasses",
    subtitle = "Domain stopwords (smart, glass, system, ...) removed",
    x = "Occurrences", y = NULL
  ) +
  theme_nuance()

ggsave(file.path(FIG_DIR, "10_top25_bigrams_AUDIO.png"), p_bigrams,
       width = 10, height = 8, dpi = 150, bg = "#0e1117")

saveRDS(top_bigrams, file.path(RDS_DIR, "top_bigrams_audio.rds"))


# 7. PAIRWISE CORRELATION (widyr) — ABSTRACT TERMS ---------------------------
cat("\n=== [7] Pairwise word co-occurrence (widyr) on audio abstracts...\n")

word_pairs <- abstracts_tbl |>
  unnest_tokens(word, text) |>
  filter(!word %in% stop_words$word,
         !word %in% domain_stop$word,
         !str_detect(word, "\\d"),
         nchar(word) > 3) |>
  add_count(word) |>
  filter(n >= 8) |>     # parole che compaiono in almeno 8 documenti
  pairwise_count(word, doc_id, sort = TRUE)

write_csv(slice_head(word_pairs, n = 50), file.path(TAB_DIR, "top50_pairwise_words_audio.csv"))
saveRDS(word_pairs, file.path(RDS_DIR, "word_pairs_audio.rds"))

# Network ggraph dei pair top-50
top_pairs_g <- word_pairs |>
  slice_head(n = 60) |>
  graph_from_data_frame()

set.seed(42)
p_net <- ggraph(top_pairs_g, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n),
                 edge_colour = PAL[1], show.legend = FALSE) +
  geom_node_point(color = PAL[3], size = 4, alpha = 0.9) +
  geom_node_text(aes(label = name), repel = TRUE, color = "#e5e7eb",
                 size = 3.6, family = "sans") +
  scale_edge_width(range = c(0.4, 2.5)) +
  scale_edge_alpha(range = c(0.2, 0.85)) +
  labs(
    title    = "Co-occurrence Network — Top Word Pairs in Abstracts",
    subtitle = "Audio/Hearing Smart Glasses subfield · widyr::pairwise_count"
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.background  = element_rect(fill = "#0e1117", color = NA),
    text             = element_text(color = "#e5e7eb"),
    plot.title       = element_text(color = "#10b981", face = "bold", size = 15),
    plot.subtitle    = element_text(color = "#9ca3af", size = 11)
  )

ggsave(file.path(FIG_DIR, "11_pairwise_network_AUDIO.png"), p_net,
       width = 12, height = 9, dpi = 150, bg = "#0e1117")


# 8. THEMATIC EVOLUTION (opzionale ma molto strategico) -----------------------
cat("\n=== [8] Thematic evolution over time (full corpus)...\n")

# Suddivido in due time slices: pre-2022 vs 2022-2026 (era post-pandemia / boom AI)
years_cuts <- c(min(M$PY, na.rm = TRUE), 2021, max(M$PY, na.rm = TRUE))

tryCatch({
  thematic_evo <- thematicEvolution(M, field = "DE", years = 2022,
                                     n = 250, minFreq = 3,
                                     stemming = TRUE)
  saveRDS(thematic_evo, file.path(RDS_DIR, "thematic_evolution.rds"))

  # Salva solo se ha generato qualcosa
  if (!is.null(thematic_evo$Sankey)) {
    cat("  -> Thematic evolution computed\n")
  }
}, error = function(e) {
  cat("  -> Skip thematicEvolution (errore o dati insufficienti):", conditionMessage(e), "\n")
})


# 9. RIEPILOGO FINALE ---------------------------------------------------------
cat("\n========================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================================\n")
cat(sprintf("Full corpus:    %d documents\n", nrow(M)))
cat(sprintf("Audio subset:   %d documents (%.1f%%)\n",
            nrow(M_audio), 100*nrow(M_audio)/nrow(M)))
cat(sprintf("Year range:     %d - %d\n", min(M$PY, na.rm=TRUE), max(M$PY, na.rm=TRUE)))
cat(sprintf("Top keyword:    %s (%d occurrences)\n",
            top_DE$Keyword[1], top_DE$Frequency[1]))
cat(sprintf("Top audio kw:   %s (%d occurrences)\n",
            top_DE_audio$Keyword[1], top_DE_audio$Frequency[1]))
cat("\nOutput salvati in:\n")
cat(sprintf("  - %s/  (oggetti .rds per Shiny app)\n", RDS_DIR))
cat(sprintf("  - %s/  (plot .png)\n", FIG_DIR))
cat(sprintf("  - %s/  (tabelle .csv)\n", TAB_DIR))
cat("========================================================\n")


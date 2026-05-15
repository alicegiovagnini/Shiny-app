# ── Install required packages (run once) ──
# install.packages(c("shiny", "bslib", "plotly", "echarts4r", "DT", "htmltools", "shinyWidgets", "dplyr"))
# --- AGGIUNGI QUESTA LIBRERIA IN ALTO NEL FILE global.R ---
library(dplyr)
library(shiny)
library(bslib)
library(plotly)
library(echarts4r)
library(DT)
library(htmltools)
library(shinyWidgets)

# ═══════════════════════════════════════════════════════════════════════════════
# COLOUR PALETTE & THEME
# ═══════════════════════════════════════════════════════════════════════════════
col <- list(
  bg      = "#080c14",
  card    = "#0f1724",
  card2   = "#151e30",
  border  = "#1c2840",
  accent  = "#00e5a0",
  accent2 = "#00c98b",
  orange  = "#ff9f43",
  red     = "#ff5757",
  blue    = "#4facfe",
  purple  = "#a78bfa",
  cyan    = "#22d3ee",
  pink    = "#f472b6",
  text    = "#e0e7ef",
  dim     = "#8494a7",
  muted   = "#556378"
)

# ═══════════════════════════════════════════════════════════════════════════════
# CUSTOM CSS
# ═══════════════════════════════════════════════════════════════════════════════
app_css <- paste0("
@import url('https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600;700;800;900&family=JetBrains+Mono:wght@400;600;700&display=swap');
@import url('https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css');

:root {
  --bg: ", col$bg, ";
  --card: ", col$card, ";
  --card2: ", col$card2, ";
  --border: ", col$border, ";
  --accent: ", col$accent, ";
  --text: ", col$text, ";
  --dim: ", col$dim, ";
  --muted: ", col$muted, ";
}

* { box-sizing: border-box; }

body, .bslib-page-fill {
  background: var(--bg) !important;
  color: var(--text) !important;
  font-family: 'Outfit', sans-serif !important;
}

/* ── Navigation ── */
.nav-pills .nav-link {
  color: var(--dim) !important;
  font-weight: 500;
  font-size: 13px;
  padding: 10px 18px;
  border-radius: 10px;
  transition: all 0.25s ease;
  border: 1px solid transparent;
  letter-spacing: 0.2px;
  margin-bottom: 2px;
}
.nav-pills .nav-link:hover {
  background: rgba(0,229,160,0.06) !important;
  color: var(--accent) !important;
}
.nav-pills .nav-link.active {
  background: rgba(0,229,160,0.12) !important;
  color: var(--accent) !important;
  font-weight: 700;
  border: 1px solid rgba(0,229,160,0.25);
  box-shadow: 0 0 20px rgba(0,229,160,0.06);
}

/* ── Cards ── */
.sci-card {
  background: var(--card);
  border: 1px solid var(--border);
  border-radius: 14px;
  padding: 24px;
  margin-bottom: 18px;
  transition: transform 0.2s, box-shadow 0.2s;
}
.sci-card:hover {
  transform: translateY(-1px);
  box-shadow: 0 8px 30px rgba(0,0,0,0.25);
}

/* ── Stat Blocks ── */
.stat-block {
  background: var(--card);
  border: 1px solid var(--border);
  border-radius: 14px;
  padding: 22px 26px;
  text-align: center;
  position: relative;
  overflow: hidden;
}
.stat-block::before {
  content: '';
  position: absolute;
  top: 0; left: 0; right: 0;
  height: 3px;
  border-radius: 14px 14px 0 0;
}
.stat-block .stat-value {
  font-family: 'JetBrains Mono', monospace;
  font-size: 30px;
  font-weight: 800;
  line-height: 1.1;
}
.stat-block .stat-label {
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: 1.8px;
  color: var(--muted);
  margin-bottom: 8px;
  font-weight: 600;
}
.stat-block .stat-sub {
  font-size: 12px;
  color: var(--dim);
  margin-top: 6px;
}

/* ── Section Titles ── */
.section-title {
  font-size: 22px;
  font-weight: 800;
  color: white;
  margin-bottom: 4px;
  letter-spacing: -0.5px;
}
.section-sub {
  font-size: 13px;
  color: var(--dim);
  margin-bottom: 24px;
  line-height: 1.5;
}

/* ── Badge ── */
.sci-badge {
  display: inline-block;
  padding: 4px 12px;
  border-radius: 20px;
  font-size: 11px;
  font-weight: 700;
  letter-spacing: 0.5px;
}

/* ── Hero Banner ── */
.hero-banner {
  background: linear-gradient(135deg, rgba(0,229,160,0.08) 0%, var(--card) 60%, rgba(79,172,254,0.05) 100%);
  border: 1px solid rgba(0,229,160,0.15);
  border-radius: 18px;
  padding: 36px;
  margin-bottom: 24px;
  position: relative;
  overflow: hidden;
}
.hero-banner::after {
  content: '';
  position: absolute;
  top: -50%; right: -20%;
  width: 400px; height: 400px;
  background: radial-gradient(circle, rgba(0,229,160,0.04) 0%, transparent 70%);
  pointer-events: none;
}

/* ── Insight Box ── */
.insight-box {
  border-radius: 14px;
  padding: 24px;
  border-left: 4px solid;
  margin-bottom: 18px;
}

/* ── Code Block ── */
.query-code {
  background: #060a12;
  border: 1px solid var(--border);
  border-radius: 10px;
  padding: 16px 20px;
  font-family: 'JetBrains Mono', monospace;
  font-size: 12px;
  color: var(--accent);
  line-height: 1.7;
  overflow-x: auto;
  white-space: pre-wrap;
}

/* ── DataTable ── */
.dataTables_wrapper { color: var(--dim) !important; }
table.dataTable { color: var(--text) !important; border-collapse: collapse !important; }
table.dataTable thead th {
  background: var(--card2) !important;
  color: var(--muted) !important;
  font-size: 11px !important;
  text-transform: uppercase;
  letter-spacing: 1px;
  font-weight: 600;
  border-bottom: 1px solid var(--border) !important;
  padding: 12px 14px !important;
}
table.dataTable tbody td {
  border-bottom: 1px solid var(--border) !important;
  padding: 11px 14px !important;
  font-size: 13px;
}
table.dataTable tbody tr { background: transparent !important; }
table.dataTable tbody tr:hover { background: var(--card2) !important; }
.dataTables_info, .dataTables_length, .dataTables_filter { color: var(--muted) !important; font-size: 12px; }
.dataTables_filter input {
  background: var(--card) !important;
  border: 1px solid var(--border) !important;
  color: var(--text) !important;
  border-radius: 8px;
  padding: 4px 10px;
}
.dataTables_length select {
  background: var(--card) !important;
  border: 1px solid var(--border) !important;
  color: var(--text) !important;
  border-radius: 6px;
}
.paginate_button { color: var(--dim) !important; }
.paginate_button.current { color: var(--accent) !important; font-weight: 700; }

/* ── VUCA / KIT interactive buttons ── */
.vuca-btn, .kit-btn {
  border: 2px solid var(--border);
  border-radius: 12px;
  padding: 16px 20px;
  cursor: pointer;
  transition: all 0.25s;
  text-align: center;
  background: var(--card);
}
.vuca-btn:hover, .kit-btn:hover { transform: translateY(-2px); }
.vuca-btn.active { border-color: var(--accent); background: rgba(0,229,160,0.06); }
.kit-btn.active { border-color: var(--accent); background: rgba(0,229,160,0.06); }

/* ── Scrollbar ── */
::-webkit-scrollbar { width: 6px; }
::-webkit-scrollbar-track { background: var(--bg); }
::-webkit-scrollbar-thumb { background: var(--border); border-radius: 3px; }

/* ── plotly bg ── */
.js-plotly-plot .plotly .main-svg { background: transparent !important; }

/* ── Tab Content Padding ── */
.tab-content > .tab-pane { padding: 8px 0; }

/* ── Responsive columns ── */
.row-stats { display: flex; gap: 14px; flex-wrap: wrap; margin-bottom: 22px; }
.row-stats > div { flex: 1; min-width: 160px; }

/* Stile per il Diagramma di Porter */
.porter-grid {
  display: grid;
  /* Sostituito 1fr con minmax(0, 1fr) per forzare le 3 colonne ad essere di larghezza identica */
  grid-template-columns: minmax(0, 1fr) 55px minmax(0, 1fr) 55px minmax(0, 1fr);
  grid-template-rows: 210px 50px 210px 50px 210px;
  gap: 10px;
  margin: 20px 0;
}

.porter-box {
  background: transparent;
  border: 2px solid var(--border);
  border-radius: 10px;
  padding: 16px;
  text-align: center;
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: column;
  justify-content: center;
  box-sizing: border-box;
  box-shadow: 0 4px 16px rgba(0,0,0,0.3);
}

.porter-box h5 {
  font-size: 14px;
  font-weight: 800;
  color: #080c14; 
  margin-bottom: 8px;
  text-decoration: none; 
}

.porter-box p {
  font-size: 12px;
  color: #334155; 
  line-height: 1.4;
  margin: 0;
}

.status-u {
  text-decoration: underline;
}

.porter-arrow {
  font-size: 38px;
  color: #ffffff !important;
  font-weight: bold;
  display: flex;
  align-items: center;
  justify-content: center;
}
.porter-arrow i,
.porter-arrow svg {
  color: #ffffff !important;
  fill: #ffffff !important;
}

/* Contenitore per i loghi dei rivali nel box centrale */
.logos-container {
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  align-items: center;
  gap: 12px;
  margin-top: 14px;
  padding-top: 14px;
  border-top: 1px solid rgba(0,0,0,0.1); /* Linea scura per sfondo chiaro */
}

/* Stile per i singoli loghi */
.logos-container img {
  height: 28px; 
  width: auto;
  object-fit: contain;
  mix-blend-mode: multiply; 
}

/* Stile per le immagini tematiche degli altri 4 box */
.thematic-img {
  height: 45px;
  width: auto;
  margin-top: 12px;
  object-fit: contain;
  opacity: 0.9;
}
")

# ═══════════════════════════════════════════════════════════════════════════════
# HELPER FUNCTIONS
# ═══════════════════════════════════════════════════════════════════════════════

# Stat block HTML
stat_block <- function(label, value, sub = NULL, color = col$accent) {
  tags$div(
    class = "stat-block",
    style = paste0("border-top: 3px solid ", color, ";"),
    tags$div(class = "stat-label", label),
    tags$div(class = "stat-value", style = paste0("color:", color, ";"), value),
    if (!is.null(sub)) tags$div(class = "stat-sub", sub)
  )
}

# Section header
section_hdr <- function(title, sub = NULL) {
  tagList(
    tags$div(class = "section-title", title),
    if (!is.null(sub)) tags$div(class = "section-sub", sub)
  )
}

# Badge
sci_badge <- function(text, color = col$accent) {
  tags$span(
    class = "sci-badge",
    style = paste0("background:", color, "18; color:", color, "; border:1px solid ", color, "35;"),
    text
  )
}

# Insight box  (icon = nome FA, es. "lightbulb", "bolt", "check-circle")
insight_box <- function(title, text, color = col$accent, icon = "lightbulb") {
  tags$div(
    class = "insight-box",
    style = paste0("background: linear-gradient(135deg, ", color, "0d, ", col$card, "); border-color:", color, ";"),
    tags$h4(
      style = paste0("font-size:15px; font-weight:700; color:", color, "; margin:0 0 10px;"),
      tags$i(class = paste0("fas fa-", icon), `aria-hidden` = "true"),
      " ",
      title
    ),
    tags$p(style = "font-size:13px; color:var(--dim); line-height:1.7; margin:0;", HTML(text))
  )
}

# Info card with bullet points
info_card <- function(title, items, color = col$accent) {
  tags$div(
    class = "sci-card",
    style = paste0("border-left: 4px solid ", color, ";"),
    tags$h4(style = "font-size:15px; font-weight:700; color:white; margin:0 0 14px;", title),
    tags$div(
      lapply(items, function(item) {
        tags$div(
          style = "display:flex; gap:10px; align-items:flex-start; margin-bottom:10px;",
          tags$span(style = paste0("display:inline-block; width:7px; height:7px; border-radius:50%; background:", color, "; margin-top:6px; flex-shrink:0; box-shadow:0 0 6px ", color, "60;")),
          tags$span(style = "font-size:13px; color:var(--dim); line-height:1.55;", HTML(item))
        )
      })
    )
  )
}

# Common plotly layout settings
plotly_layout_dark <- function(p, ...) {
  p %>% layout(
    paper_bgcolor = "transparent",
    plot_bgcolor  = "transparent",
    font = list(family = "Outfit", color = col$dim, size = 12),
    xaxis = list(gridcolor = col$border, zerolinecolor = col$border, tickfont = list(size = 11)),
    yaxis = list(gridcolor = col$border, zerolinecolor = col$border, tickfont = list(size = 11)),
    margin = list(l = 50, r = 20, t = 40, b = 40),
    legend = list(font = list(size = 11, color = col$dim)),
    ...
  ) %>% config(displayModeBar = FALSE)
}

# ═══════════════════════════════════════════════════════════════════════════════
# DATA
# ═══════════════════════════════════════════════════════════════════════════════

# Market growth
df_market <- data.frame(
  Year     = c(2024, 2025, 2026, 2028, 2030),
  Actual   = c(2.0,  2.5,  5.6,  NA,   NA),
  Forecast = c(NA,   2.5,  5.6,  11.0, 29.0)
)

# Shipments
df_ship <- data.frame(
  Year  = c("2024","2025","2026","2028","2030"),
  Units = c(3, 6, 20, 45, 75)
)

# Segments
df_seg <- data.frame(
  Segment = c("Audio","AR/Immersive","Camera/Social","Health/Medical","Other"),
  Share   = c(28, 24, 22, 15, 11)
)

# Regions
df_reg <- data.frame(
  Region = c("North America","Europe","Asia-Pacific","Rest of World"),
  Share  = c(36, 32, 24, 8)
)

# ASP
df_asp <- data.frame(
  Year = c(2024, 2025, 2026, 2027, 2028),
  ASP  = c(450, 380, 320, 285, 250)
)

# Competitor comparison table
df_comp <- data.frame(
  Product     = c("Nuance Audio","Ray-Ban Meta","Xiaomi Mijia","Huawei GM II","Amazon Echo Frames","Rokid Max"),
  Focus       = c("Hearing / Medical","Lifestyle / AI","Audio","Audio / Smart","Audio / AI","AR / Entertainment"),
  Technology  = c("Beamforming, open-ear, SaMD","LLM, camera, audio","Touch, voice assistant","Vibration, pressure","Alexa, touch","AR display 120Hz"),
  Price       = c("$1,100","~$299","$80–150","$250–350","~$269","$400–500"),
  Target      = c("Medical + Consumer","Consumer","Consumer","Consumer","Consumer","Consumer / Gaming"),
  Strength    = c("Only FDA-cleared; 18K stores","85% market share; Meta ecosystem","Aggressive pricing","Luxury design; health monitoring","Alexa / smart home","High-res AR display"),
  MarketShare = c(3, 85, 4, 3, 2, 3.9),
  stringsAsFactors = FALSE
)

# Substitutes
df_subs <- data.frame(
  Substitute  = c("Traditional hearing aids","AirPods Pro 2","Discreet OTC aids","Captioning glasses","Audio smart glasses"),
  Threat      = c("HIGH","HIGH","MEDIUM","MEDIUM","LOW-MED"),
  PriceRange  = c("$1K–$6K","~$250","$200–$3K","$250–$600","$300–$500"),
  Performance = c("Superior audiological","FDA-cleared + music","Invisible CIC/IIC","Speech-to-text","Audio + AI, no medical"),
  stringsAsFactors = FALSE
)

# Porter scores
df_porter <- data.frame(
  Force     = c("New Entrants","Substitutes","Buyer Power","Supplier Power","Rivalry"),
  Level     = c("LOW/MEDIUM", "HIGH", "LOW/MEDIUM", "MEDIUM/HIGH", "MEDIUM"),
  Score     = c(40, 90, 40, 75, 50)
)

# Rivalry map
df_rivalry <- data.frame(
  Competitor = c("Sonova (Phonak)","Demant (Oticon)","Apple (AirPods Pro 2)","Meta (Ray-Ban Meta)","Cearvol (Lyra)","WS Audiology"),
  Type       = c("Direct","Direct","Convergent","Adjacent","Direct","Direct"),
  Description = c(
    "Market leader in hearing aids. AI-powered, Bluetooth, teleaudiology. Strong clinical channel.",
    "Deep neural network sound processing. Strong in EU. Clinical distribution.",
    "FDA-cleared OTC hearing aid. Massive user base. $250. Cultural acceptance.",
    "Same parent retailer (EssilorLuxottica). AI glasses with audio. Could add hearing features.",
    "Smart hearing glasses with NeuroFlow AI 2.0. Emerging Chinese competitor at CES 2026.",
    "Premium hearing aids with sound customization. Strong brand in EU audiology."
  ),
  stringsAsFactors = FALSE
)

# GenAI evaluation
df_eval <- data.frame(
  Criterion = c(
    "Creativity / Relevance / Depth of Thought",
    "Accuracy / Objectivity / Comprehensive Analysis",
    "Clarity / Linguistic Fluency / Cultural Sensitivity",
    "Insightfulness / Coherence"
  ),
  Score = c(4, 4, 5, 4),
  Comment = c(
    "The generated content is relevant and thorough for the Nuance Audio case. The depth of competitive analysis is good, but could benefit from more specific financial data on EssilorLuxottica.",
    "Market data is consistent with authoritative sources. However, estimates vary significantly across sources (CAGR 12–24%), requiring critical attention. Objectivity is good, with no evident bias.",
    "The output is clear, well-structured and linguistically fluent. Technical terminology is used appropriately.",
    "Strategic implications are coherent with the data presented. The identification of Nuance Audio’s unique positioning in the medical segment is a relevant insight."
  ),
  stringsAsFactors = FALSE
)

# ── Annual scientific production (Lab 5 — bibliometrix) ─────────────────────
# DATI REALI generati da text_analysis_nuance.R
df_lab5_annual <- read.csv("output/tables/docs_per_year.csv") %>%
  rename(year = Year, articles = Documents)

# ── Top countries (most productive) ─────────────────────────────────────────
# DATI REALI generati da text_analysis_nuance.R
#df_lab5_countries <- read.csv("output/tables/top15_countries.csv") %>%
 # rename(country = Country, articles = Articles)

# ── Top sources ─────────────────────────────────────────────────────────────
# DATI REALI generati da text_analysis_nuance.R
df_lab5_sources <- read.csv("output/tables/top15_sources.csv") %>%
  rename(source = Source, articles = Documents)

# ── Top author keywords ─────────────────────────────────────────────────────
# DATI REALI generati da text_analysis_nuance.R
df_lab5_keywords <- read.csv("output/tables/top15_author_keywords_full.csv") %>%
  rename(keyword = Keyword, n = Frequency)

# ── Co-occurrence pairs (top bigrams) ───────────────────────────────────────
# DATI REALI generati da text_analysis_nuance.R
df_lab5_bigrams <- read.csv("output/tables/top25_bigrams_audio.csv") %>%
  rename(pair = bigram, n = n)

# ── BERTopic — Topic info ───────────────────────────────────────────────────
# (Dati mantenuti statici in quanto BERTopic viene eseguito su Python/Colab)
df_lab5_topics <- data.frame(
  topic = 0:6,
  count = c(96, 71, 58, 49, 42, 31, 24),
  label = c(
    "Open-ear audio devices & speech enhancement",
    "AR smart glasses for AI-assisted listening",
    "Hearing-aid integration & beamforming",
    "Wearable acceptance, stigma & elderly users",
    "Deep learning for speech intelligibility",
    "Bone-conduction & assistive listening",
    "Smart-eyewear UX, comfort & design"
  ),
  top_words = c(
    "speech, audio, open-ear, glasses, listener, signal",
    "ar, glasses, ai, assistant, captioning, conversation",
    "hearing, aid, beamforming, microphone, directional",
    "wearable, acceptance, elderly, stigma, adoption",
    "deep, learning, neural, speech, enhancement",
    "bone, conduction, assistive, listening, vibration",
    "design, comfort, eyewear, ux, fashion"
  ),
  KIT_link = c("KIT2", "KIT2 / KIT3", "KIT2", "KIT1", "KIT2",
               "KIT2", "KIT1"),
  stringsAsFactors = FALSE
)

# ── BERTopic — topics over time ─────────────────────────────────────────────
df_lab5_topics_time <- expand.grid(
  year  = 2018:2025,
  topic = 0:6
)
df_lab5_topics_time$count <- c(
  4, 6, 9, 12, 15, 17, 16, 17,        # T0 Open-ear
  1, 2, 3, 4, 6, 12, 19, 24,          # T1 AR + AI captioning (boom)
  3, 4, 6, 7, 9, 10, 9, 10,           # T2 Beamforming
  1, 2, 3, 5, 7, 9, 10, 12,           # T3 Acceptance/stigma
  2, 3, 5, 7, 8, 7, 6, 4,             # T4 Deep learning speech
  4, 5, 5, 4, 4, 3, 3, 3,             # T5 Bone conduction
  1, 2, 2, 4, 4, 4, 4, 3              # T6 Smart eyewear UX
)
df_lab5_topics_time$topic_label <- df_lab5_topics$label[df_lab5_topics_time$topic + 1]

# ── Most cited papers ───────────────────────────────────────────────────────
df_lab5_top_cited <- data.frame(
  title    = c(
    "Augmented reality smart glasses use and acceptance: a literature review",
    "Consumers' adoption of wearable technologies",
    "Smart glasses based on hearing aid technology",
    "Beamforming in head-mounted devices for hearing assistance",
    "User acceptance of head-mounted displays for elderly",
    "Open-ear hearing devices: a systematic review",
    "Deep learning for speech enhancement on wearables",
    "From OTC hearing aids to hearing eyewear: a regulatory map",
    "Stigma and adoption barriers in modern hearing devices",
    "Real-time conversational captioning on AR smart glasses"
  ),
  year     = c(2023, 2021, 2022, 2023, 2022, 2024, 2023, 2024, 2022, 2024),
  cites    = c(187, 156, 132, 121, 108, 96, 94, 81, 76, 64),
  KIT      = c("KIT3","KIT3","KIT2","KIT2","KIT1","KIT2","KIT2","KIT1","KIT1","KIT2"),
  stringsAsFactors = FALSE
)


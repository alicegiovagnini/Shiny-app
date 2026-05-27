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
library(openxlsx)
library(scales)
library(wordcloud2)
library(htmlwidgets)

# ═══════════════════════════════════════════════════════════════════════════════
# PRELOAD HTMLWIDGET JS DEPENDENCIES
# ═══════════════════════════════════════════════════════════════════════════════
# Le schede sono generate dinamicamente via renderUI. Quando una scheda con molti
# grafici/tabelle veniva aperta, le librerie JS dei widget (plotly.js, DataTables,
# ECharts, wordcloud2) venivano iniettate in modo asincrono: i widget in cima al
# DOM venivano "bound" prima che la libreria fosse caricata e restavano vuoti
# (solo gli ultimi nel DOM si disegnavano). Precaricando qui le dipendenze
# nell'<head> all'avvio, le librerie sono già disponibili quando la scheda viene
# mostrata e TUTTI i widget si disegnano in modo affidabile.
widget_deps <- local({
  safe_deps <- function(expr) tryCatch(
    htmltools::findDependencies(expr),
    error = function(e) list()
  )
  suppressWarnings(suppressMessages(
    htmltools::resolveDependencies(c(
      safe_deps(plotly::plot_ly()),
      safe_deps(DT::datatable(data.frame(x = 1))),
      safe_deps(wordcloud2::wordcloud2(data.frame(word = "a", freq = 1))),
      safe_deps(echarts4r::e_charts(data.frame(x = 1, y = 2), x))
    ))
  ))
})

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

html {
  background: var(--bg) !important;
  min-height: 100%;
}

body, .bslib-page-fill {
  background: var(--bg) !important;
  color: var(--text) !important;
  font-family: 'Outfit', sans-serif !important;
  min-width: 1200px !important;
  min-height: 100vh !important;
  overflow-x: auto !important;
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
  transition: transform 0.22s, box-shadow 0.22s, border-color 0.22s, background 0.22s;
}
.sci-card:hover {
  transform: translateY(-2px);
  border-color: rgba(0,229,160,0.22);
  box-shadow: 0 0 0 1px rgba(0,229,160,0.12), 0 10px 32px rgba(0,0,0,0.28);
}

/* ── Light theme per le card contenenti grafici o tabelle ── */
.sci-card:has(.plotly),
.sci-card:has(.echarts4r),
.sci-card:has(.echarts4r-component),
.sci-card:has(.dataTables_wrapper) {
  background: #f1f5f9;
  border-color: #cbd5e1;
}
.sci-card:has(.plotly):hover,
.sci-card:has(.echarts4r):hover,
.sci-card:has(.echarts4r-component):hover,
.sci-card:has(.dataTables_wrapper):hover {
  transform: translateY(-4px);
  border-color: #94a3b8;
  box-shadow: 0 0 0 1px rgba(0,0,0,0.04), 0 14px 30px rgba(0,0,0,0.18);
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
  transition: transform 0.22s, box-shadow 0.22s, border-color 0.22s;
}
.stat-block:hover {
  transform: translateY(-2px);
  border-color: rgba(0,229,160,0.22);
  box-shadow: 0 0 0 1px rgba(0,229,160,0.12), 0 8px 24px rgba(0,0,0,0.25);
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
/* ── Sidebar bloccata ── */
.bslib-sidebar-toggle,
[data-bslib-sidebar-toggle] {
  display: none !important;
}

.bslib-sidebar-resize-handle {
  display: none !important;
  pointer-events: none !important;
}

/* ── Folder Nav ── */
.nav-custom-link {
  display: flex;
  align-items: center;
  gap: 9px;
  padding: 7px 14px;
  border-radius: 9px;
  font-size: 13px;
  font-weight: 500;
  color: var(--dim);
  cursor: pointer;
  transition: all 0.2s;
  border: 1px solid transparent;
  margin-bottom: 2px;
  user-select: none;
}
.nav-custom-link:hover { background: rgba(0,229,160,0.06); color: var(--accent); }
.nav-custom-link.active {
  background: rgba(0,229,160,0.12);
  color: var(--accent);
  font-weight: 700;
  border-color: rgba(0,229,160,0.25);
  box-shadow: 0 0 14px rgba(0,229,160,0.07);
}

.nav-folder-hdr {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 5px 12px;
  border-radius: 8px;
  font-size: 10px;
  font-weight: 800;
  letter-spacing: 1.2px;
  text-transform: uppercase;
  cursor: pointer;
  transition: background 0.18s;
  margin: 4px 0 2px;
  user-select: none;
}
.nav-folder-hdr:hover { background: rgba(255,255,255,0.06); }

.nav-folder-group { position: relative; }
.nav-folder-children {
  max-height: 0;
  overflow: hidden;
  transition: max-height 0.28s ease;
}
.nav-folder-group.has-active:not(.is-suppressed) .nav-folder-children,
.nav-folder-group.is-open .nav-folder-children {
  max-height: 400px;
}
.nav-folder-chevron {
  font-size: 9px;
  opacity: 0.45;
  margin-left: auto;
  transition: transform 0.22s ease, opacity 0.2s;
  flex-shrink: 0;
}
.nav-folder-group.has-active:not(.is-suppressed) .nav-folder-chevron,
.nav-folder-group.is-open .nav-folder-chevron {
  transform: rotate(90deg);
  opacity: 0.75;
}

.nav-btn-item {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 6px 10px 6px 12px;
  border-radius: 8px;
  font-size: 12px;
  font-weight: 500;
  color: var(--dim);
  cursor: pointer;
  transition: all 0.18s;
  margin: 1px 0 1px 6px;
  border: 1px solid var(--border);
  background: rgba(255,255,255,0.015);
  user-select: none;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
.nav-btn-item:hover {
  background: rgba(255,255,255,0.045);
  color: var(--text);
  transform: translateX(2px);
}
.nav-btn-item.active {
  font-weight: 600;
  color: white;
  background: rgba(255,255,255,0.06);
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
  transition: transform 0.22s, box-shadow 0.22s;
}
.insight-box:hover {
  transform: translateY(-2px);
  box-shadow: 0 8px 28px rgba(0,0,0,0.25);
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

/* ── Hover universale su tutti i contenitori card-like ── */
.query-code-light,
.rumsfeld-cell,
.porter-box {
  cursor: default;
}
.query-code-light:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 18px rgba(0,0,0,0.1);
  transition: transform 0.22s, box-shadow 0.22s;
}

/* code tags inside pastel insight-boxes */
.insight-box code,
.methodology-banner code {
  color: inherit;
  font-weight: 700;
  background: transparent;
  font-size: 0.9em;
}

.query-code-light {
  background: #ffffff;
  border: 1px solid #e2e8f0;
  border-radius: 10px;
  padding: 16px 20px;
  font-family: 'JetBrains Mono', monospace;
  font-size: 12px;
  color: #1e293b;
  line-height: 1.7;
  overflow-x: auto;
  white-space: pre-wrap;
}

/* ── DataTable (light theme) ── */
.dataTables_wrapper { color: #475569; }
table.dataTable { color: #1e293b; border-collapse: collapse; }
table.dataTable thead th {
  background: #e2e8f0;
  color: #475569;
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: 1px;
  font-weight: 600;
  border-bottom: 1px solid #cbd5e1;
  padding: 12px 14px;
}
table.dataTable tbody td {
  border-bottom: 1px solid #e2e8f0;
  padding: 11px 14px;
  font-size: 13px;
  color: #1e293b;
}
/* Sfondo righe (senza !important → formatStyle inline può sovrascrivere) */
table.dataTable tbody tr,
table.dataTable tbody tr > td { background-color: #ffffff; }
table.dataTable.stripe tbody tr:nth-child(odd) > td,
table.dataTable.display tbody tr:nth-child(odd) > td { background-color: #f8fafc; }
table.dataTable.hover tbody tr:hover > td,
table.dataTable.display tbody tr:hover > td { background-color: #e2e8f0; }

.dataTables_info, .dataTables_length, .dataTables_filter { color: #475569; font-size: 12px; }
.dataTables_filter input {
  background: #ffffff;
  border: 1px solid #cbd5e1;
  color: #1e293b;
  border-radius: 8px;
  padding: 4px 10px;
}
.dataTables_length select {
  background: #ffffff;
  border: 1px solid #cbd5e1;
  color: #1e293b;
  border-radius: 6px;
}
.dataTables_wrapper .dataTables_paginate { padding-top: 6px; }

/* Bottone esterno (li wrapper Bootstrap o a classico) */
.dataTables_wrapper .dataTables_paginate .paginate_button {
  background: #f1f5f9 !important;
  background-color: #f1f5f9 !important;
  border: 1px solid #e2e8f0 !important;
  border-radius: 6px !important;
  margin: 0 2px !important;
  box-shadow: none !important;
  transition: all 0.15s !important;
}
/* Elemento interno (a o .page-link): nessun bordo proprio */
.dataTables_wrapper .dataTables_paginate .paginate_button a,
.dataTables_wrapper .dataTables_paginate .paginate_button .page-link {
  color: #475569 !important;
  background: transparent !important;
  background-color: transparent !important;
  border: none !important;
  box-shadow: none !important;
  outline: none !important;
  padding: 4px 12px !important;
  display: block !important;
  border-radius: 5px !important;
}
/* Hover */
.dataTables_wrapper .dataTables_paginate .paginate_button:hover {
  background: #dcfce7 !important;
  background-color: #dcfce7 !important;
  border-color: #86efac !important;
}
.dataTables_wrapper .dataTables_paginate .paginate_button:hover a,
.dataTables_wrapper .dataTables_paginate .paginate_button:hover .page-link {
  color: #00b37e !important;
  background: transparent !important;
  border: none !important;
}
/* Pagina attiva */
.dataTables_wrapper .dataTables_paginate .paginate_button.current,
.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
  background: #6ee7b7 !important;
  background-color: #6ee7b7 !important;
  border-color: #34d399 !important;
}
.dataTables_wrapper .dataTables_paginate .paginate_button.current a,
.dataTables_wrapper .dataTables_paginate .paginate_button.current .page-link,
.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover a {
  color: #065f46 !important;
  font-weight: 700 !important;
  background: transparent !important;
  border: none !important;
}
/* Disabilitato */
.dataTables_wrapper .dataTables_paginate .paginate_button.disabled,
.dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover {
  background: #f8fafc !important;
  background-color: #f8fafc !important;
  border-color: #e2e8f0 !important;
  opacity: 0.6 !important;
  cursor: default !important;
}
.dataTables_wrapper .dataTables_paginate .paginate_button.disabled a,
.dataTables_wrapper .dataTables_paginate .paginate_button.disabled .page-link {
  color: #94a3b8 !important;
  background: transparent !important;
  border: none !important;
  cursor: default !important;
}

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
.vuca-btn:hover, .kit-btn:hover {
  transform: translateY(-2px);
  border-color: rgba(0,229,160,0.35);
  box-shadow: 0 0 0 1px rgba(0,229,160,0.15), 0 8px 24px rgba(0,0,0,0.2);
}
.vuca-btn.active { border-color: var(--accent); background: rgba(0,229,160,0.06); }
.kit-btn.active { border-color: var(--accent); background: rgba(0,229,160,0.06); }

/* ── Scrollbar ── */
::-webkit-scrollbar { width: 6px; }
::-webkit-scrollbar-track { background: var(--bg); }
::-webkit-scrollbar-thumb { background: var(--border); border-radius: 3px; }

/* ── plotly bg ── */
.js-plotly-plot .plotly .main-svg { background: transparent !important; }

/* ── Tab Content Padding ── */
.tab-content { background: var(--bg); min-height: 100vh; }
.tab-content > .tab-pane { padding: 0; background: var(--bg); min-height: 100vh; }

/* ── Responsive columns ── */
.row-stats { display: flex; gap: 14px; flex-wrap: wrap; margin-bottom: 22px; }
.row-stats > div { flex: 1; min-width: 160px; }

/* Stile per il Diagramma di Porter */
.porter-grid {
  display: grid;
  grid-template-columns: minmax(0, 1fr) 44px minmax(0, 1fr) 44px minmax(0, 1fr);
  grid-template-rows: 148px 28px 148px 28px 148px;
  gap: 8px;
  margin: 10px 0 0;
}

.porter-box {
  background: #ffffff;
  border: 1px solid #e2e8f0;
  border-radius: 10px;
  padding: 12px 10px;
  text-align: center;
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: column;
  justify-content: center;
  box-sizing: border-box;
  transition: transform 0.22s, box-shadow 0.22s, border-color 0.22s;
}
.porter-box:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 20px rgba(0,0,0,0.12);
}

.porter-box h5 {
  font-size: 12px;
  font-weight: 800;
  color: #0f1724;
  margin: 0 0 4px;
  line-height: 1.25;
}

.porter-box p {
  font-size: 10.5px;
  color: #475569;
  line-height: 1.4;
  margin: 0;
}

.porter-icon {
  font-size: 20px;
  margin-bottom: 6px;
}

.porter-level {
  display: inline-block;
  padding: 1px 8px;
  border-radius: 20px;
  font-size: 9px;
  font-weight: 700;
  letter-spacing: 0.4px;
  margin-bottom: 5px;
}

.porter-level {
  display: inline-block;
  padding: 2px 9px;
  border-radius: 20px;
  font-size: 10px;
  font-weight: 700;
  letter-spacing: 0.4px;
  margin-bottom: 8px;
}

.porter-actors {
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  gap: 4px;
  margin-top: 9px;
}

.porter-actor {
  font-size: 9.5px;
  font-weight: 600;
  padding: 2px 7px;
  border-radius: 20px;
  letter-spacing: 0.2px;
}

.status-u { text-decoration: underline; }

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

/* ── Rumsfeld Matrix ── */
.rumsfeld-cell {
  background: #fdfdfd;
  border: 2px dashed #94a3b8;
  border-radius: 8px;
  overflow: hidden;
  transition: border-color 0.22s, box-shadow 0.22s, transform 0.22s;
  cursor: default;
}
.rumsfeld-cell:hover {
  border-color: #475569;
  box-shadow: 0 8px 22px rgba(0,0,0,0.12);
  transform: translateY(-4px);
}

/* ── Hover-lift su riquadri Scope & KITs/KIQs ── */
.scope-statement-box,
.kit-card,
.kiq-card {
  transition: transform 0.22s ease, box-shadow 0.22s ease, border-color 0.22s ease;
}
.scope-statement-box:hover,
.kit-card:hover,
.kiq-card:hover {
  transform: translateY(-4px);
  box-shadow: 0 8px 22px rgba(0,0,0,0.12);
}
.rumsfeld-cell-header {
  padding: 15px;
  display: flex;
  align-items: flex-start;
  gap: 10px;
}
.rumsfeld-cell-icon {
  font-size: 17px;
  color: #556378;
  margin-top: 2px;
  flex-shrink: 0;
}
.rumsfeld-drop {
  max-height: 0;
  overflow: hidden;
  transition: max-height 0.4s ease, padding 0.3s ease;
  padding: 0 15px;
}
.rumsfeld-cell:hover .rumsfeld-drop {
  max-height: 500px;
  padding: 0 15px 16px;
}

/* ── Sidebar bloccata ── */
.bslib-sidebar-toggle,
[data-bslib-sidebar-toggle],
.collapse-toggle,
button.sidebar-toggle { display: none !important; }

.sidebar-resize-handle,
.bslib-sidebar .resize-handle { display: none !important; pointer-events: none !important; }
")

# ═══════════════════════════════════════════════════════════════════════════════
# HELPER FUNCTIONS
# ═══════════════════════════════════════════════════════════════════════════════

# ── Pastel palette lookup ──────────────────────────────────────────────────────
.pastel <- list(
  "#00e5a0" = list(bg = "#d1fae5", text = "#065f46"),
  "#4facfe" = list(bg = "#dbeafe", text = "#1e3a8a"),
  "#ff9f43" = list(bg = "#ffedd5", text = "#9a3412"),
  "#a78bfa" = list(bg = "#ede9fe", text = "#4c1d95"),
  "#ff5757" = list(bg = "#fee2e2", text = "#7f1d1d"),
  "#22d3ee" = list(bg = "#cffafe", text = "#164e63"),
  "#f472b6" = list(bg = "#fce7f3", text = "#831843"),
  "#eab308" = list(bg = "#fef9c3", text = "#713f12")
)
pastel_bg   <- function(color) if (color %in% names(.pastel)) .pastel[[color]]$bg   else "#f8fafc"
pastel_text <- function(color) if (color %in% names(.pastel)) .pastel[[color]]$text else "#334155"

# Stat block HTML
stat_block <- function(label, value, sub = NULL, color = col$accent) {
  bg  <- pastel_bg(color)
  txt <- pastel_text(color)
  tags$div(
    class = "stat-block",
    style = paste0("border-top:3px solid ", color, "; background:", bg, ";"),
    tags$div(class = "stat-label", style = paste0("color:", txt, "99;"), label),
    tags$div(class = "stat-value", style = paste0("color:", color, ";"), value),
    if (!is.null(sub)) tags$div(class = "stat-sub", style = paste0("color:", txt, ";"), sub)
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

# Insight box
insight_box <- function(title, text, color = col$accent, icon = "lightbulb") {
  bg  <- pastel_bg(color)
  txt <- pastel_text(color)
  tags$div(
    class = "insight-box",
    style = paste0("background:", bg, "; border-color:", color, ";"),
    tags$h4(
      style = paste0("font-size:15px; font-weight:700; color:", color, "; margin:0 0 10px;"),
      tags$i(class = paste0("fas fa-", icon), `aria-hidden` = "true"), " ", title
    ),
    tags$p(style = paste0("font-size:13px; color:", txt, "; line-height:1.7; margin:0;"), HTML(text))
  )
}

# Info card with bullet points
info_card <- function(title, items, color = col$accent) {
  bg  <- pastel_bg(color)
  txt <- pastel_text(color)
  tags$div(
    class = "sci-card",
    style = paste0("border-left:4px solid ", color, "; background:", bg, ";"),
    tags$h4(style = paste0("font-size:15px; font-weight:700; color:", txt, "; margin:0 0 14px;"), title),
    tags$div(
      lapply(items, function(item) {
        tags$div(
          style = "display:flex; gap:10px; align-items:flex-start; margin-bottom:10px;",
          tags$span(style = paste0("display:inline-block; width:7px; height:7px; border-radius:50%; background:", color, "; margin-top:6px; flex-shrink:0;")),
          tags$span(style = paste0("font-size:13px; color:", txt, "; line-height:1.55;"), HTML(item))
        )
      })
    )
  )
}

# Query syntax highlighter
hl_query <- function(txt) {
  txt <- gsub("&",  "&amp;",  txt, fixed = TRUE)
  txt <- gsub("<",  "&lt;",   txt, fixed = TRUE)
  txt <- gsub(">",  "&gt;",   txt, fixed = TRUE)
  txt <- gsub("AND NOT", "\x01ANDNOT\x01", txt, fixed = TRUE)
  txt <- gsub("\\bAND\\b",  "<span style='color:#d97706;font-weight:800;'>AND</span>",  txt, perl = TRUE)
  txt <- gsub("\\bOR\\b",   "<span style='color:#2563eb;font-weight:800;'>OR</span>",   txt, perl = TRUE)
  txt <- gsub("\x01ANDNOT\x01", "<span style='color:#dc2626;font-weight:800;'>AND NOT</span>", txt, fixed = TRUE)
  txt <- gsub("TITLE-ABS-KEY", "<span style='color:#7c3aed;font-weight:700;'>TITLE-ABS-KEY</span>", txt, fixed = TRUE)
  txt <- gsub("PUBYEAR", "<span style='color:#7c3aed;font-weight:700;'>PUBYEAR</span>", txt, fixed = TRUE)
  HTML(txt)
}

# Common plotly layout settings (light panel theme)
plotly_layout_dark <- function(p, ...) {
  p %>% layout(
    paper_bgcolor = "transparent",
    plot_bgcolor  = "transparent",
    font = list(family = "Outfit", color = "#1e293b", size = 12),
    xaxis = list(gridcolor = "#e2e8f0", zerolinecolor = "#cbd5e1",
                 tickfont = list(size = 11, color = "#1e293b"),
                 titlefont = list(color = "#1e293b")),
    yaxis = list(gridcolor = "#e2e8f0", zerolinecolor = "#cbd5e1",
                 tickfont = list(size = 11, color = "#1e293b"),
                 titlefont = list(color = "#1e293b")),
    margin = list(l = 50, r = 20, t = 40, b = 40),
    legend = list(font = list(size = 11, color = "#1e293b")),
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
  Level     = c("MEDIUM/HIGH", "HIGH", "LOW/MEDIUM", "MEDIUM", "MEDIUM"),
  Score     = c(72, 85, 32, 60, 60)
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
  rename(year = Year, articles = Documents) %>%
  filter(year <= 2025)

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

# ─── BERTopic — Derivati per Topic Map / Hierarchy / Heatmap / Top Words ─────
# Replica delle visualizzazioni "canoniche" di BERTopic
# (visualize_topics, visualize_hierarchy, visualize_heatmap, visualize_barchart)
# partendo dai dati aggregati già in `df_lab5_topics` (no embeddings disponibili
# in R: usiamo Jaccard sulle parole-chiave c-TF-IDF di ciascun topic).

.lab5_topic_words_list <- strsplit(df_lab5_topics$top_words, ",\\s*")
names(.lab5_topic_words_list) <- paste0("T", df_lab5_topics$topic)

.lab5_all_words <- unique(unlist(.lab5_topic_words_list))
.lab5_topic_word_mat <- sapply(.lab5_topic_words_list,
                               function(ws) as.integer(.lab5_all_words %in% ws))
rownames(.lab5_topic_word_mat) <- .lab5_all_words

# Matrice di similarità Jaccard topic × topic
.lab5_n_topics <- length(.lab5_topic_words_list)
lab5_topic_jaccard <- matrix(0, .lab5_n_topics, .lab5_n_topics,
                             dimnames = list(names(.lab5_topic_words_list),
                                             names(.lab5_topic_words_list)))
for (i in seq_len(.lab5_n_topics)) {
  for (j in seq_len(.lab5_n_topics)) {
    a <- .lab5_topic_word_mat[, i]; b <- .lab5_topic_word_mat[, j]
    uni <- sum(a | b)
    lab5_topic_jaccard[i, j] <- if (uni > 0) sum(a & b) / uni else 0
  }
}

# Distanze + coordinate MDS 2D (intertopic distance map)
.lab5_topic_dist <- as.dist(1 - lab5_topic_jaccard)
.lab5_topic_mds  <- cmdscale(.lab5_topic_dist, k = 2)
df_lab5_topics_map <- data.frame(
  topic  = paste0("T", df_lab5_topics$topic),
  label  = df_lab5_topics$label,
  count  = df_lab5_topics$count,
  kit    = df_lab5_topics$KIT_link,
  top_words = df_lab5_topics$top_words,
  x      = .lab5_topic_mds[, 1],
  y      = .lab5_topic_mds[, 2],
  stringsAsFactors = FALSE
)

# Long form della similarity per heatmap
df_lab5_topics_sim <- expand.grid(
  topic_a = rownames(lab5_topic_jaccard),
  topic_b = colnames(lab5_topic_jaccard),
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
)
df_lab5_topics_sim$similarity <- mapply(function(a, b) lab5_topic_jaccard[a, b],
                                        df_lab5_topics_sim$topic_a,
                                        df_lab5_topics_sim$topic_b)

# Top words per topic con pseudo-score c-TF-IDF (decrescente per rank)
df_lab5_topics_words <- do.call(rbind, lapply(seq_along(.lab5_topic_words_list),
  function(i) {
    ws <- .lab5_topic_words_list[[i]]
    data.frame(
      topic       = paste0("T", df_lab5_topics$topic[i]),
      topic_label = df_lab5_topics$label[i],
      word        = ws,
      rank        = seq_along(ws),
      # score = 1.0 → 0.4 decrescente lineare (pseudo c-TF-IDF)
      score       = round(1 - (seq_along(ws) - 1) * (0.6 / max(1, length(ws) - 1)), 3),
      stringsAsFactors = FALSE
    )
  }))

# Hierarchical clustering per dendrogramma
lab5_topic_hclust <- hclust(.lab5_topic_dist, method = "average")
lab5_topic_hclust$labels <- paste0("T", df_lab5_topics$topic, " — ",
                                   df_lab5_topics$label)

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

# ── Word Cloud — author keywords frequency (Scopus full corpus) ─────────────
df_wordcloud <- read.csv("output/tables/wordcloud_keywords.csv",
                         stringsAsFactors = FALSE)
set.seed(42)
n_wc <- nrow(df_wordcloud)
# Archimedes spiral placement: inner words are most frequent
theta_wc <- seq(0.4, 6.5 * pi, length.out = n_wc)
r_wc     <- seq(0.05, 1, length.out = n_wc)
df_wordcloud$x <- r_wc * cos(theta_wc) * (1 + runif(n_wc, -0.18, 0.18))
df_wordcloud$y <- r_wc * sin(theta_wc) * (0.7 + runif(n_wc, -0.18, 0.18))
df_wordcloud$size <- scales::rescale(sqrt(df_wordcloud$freq), to = c(9, 36))
df_wordcloud$tier <- cut(df_wordcloud$freq,
                         breaks = c(0, 5, 15, 40, Inf),
                         labels = c("low", "mid", "high", "top"))

# ── Patent keyword word cloud — from Espacenet patent titles (500 patents) ──
# The Espacenet export has no keyword/abstract column, so keywords are derived
# from the only free-text field available: the patent Title. We tokenize the
# 500 downloaded titles, drop grammatical words and patent boilerplate
# (method/apparatus/device/system…), count frequencies and keep the top 120.
df_patent_wc <- local({
  pat <- read.csv("Espacenet_search_result_20260520_1510.csv",
                  sep = ";", skip = 7, header = TRUE, quote = "\"",
                  fill = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
  titles <- pat$Title
  titles <- titles[!is.na(titles) & nzchar(titles)]
  toks   <- tolower(unlist(strsplit(paste(titles, collapse = " "), "[^a-zA-Z]+")))
  toks   <- toks[nchar(toks) >= 3]
  stop_pat <- c(
    "the","and","for","with","using","use","used","based","said","via","into","onto",
    "method","methods","apparatus","apparatuses","device","devices","system","systems",
    "means","unit","units","plurality","having","comprising","wherein","thereof","same",
    "one","two","three","first","second","third","from","that","this","these","those",
    "which","are","can","its","such","may","not","per","providing","provided","includes",
    "including","include","than","more","less","type","multi","program","data"
  )
  toks <- toks[!toks %in% stop_pat]
  freq <- sort(table(toks), decreasing = TRUE)
  d <- data.frame(word = names(freq), freq = as.integer(freq),
                  stringsAsFactors = FALSE)
  # Drop the most generic high-frequency terms (augmented, reality, glasses,
  # display, head, mounted …) so the cloud surfaces the more specific concepts.
  d <- d[d$freq < 200, ]
  head(d, 120)
})

# ── Topic cluster map — MDS from Jaccard similarity ─────────────────────────
.jac_mat <- matrix(0, nrow = 7, ncol = 7)
for (i in seq_len(nrow(df_lab5_topics_sim))) {
  a <- as.integer(sub("T", "", df_lab5_topics_sim$topic_a[i])) + 1
  b <- as.integer(sub("T", "", df_lab5_topics_sim$topic_b[i])) + 1
  .jac_mat[a, b] <- df_lab5_topics_sim$similarity[i]
  .jac_mat[b, a] <- df_lab5_topics_sim$similarity[i]
}
diag(.jac_mat) <- 1
.dist_mat <- as.dist(1 - .jac_mat)
.mds <- cmdscale(.dist_mat, k = 2)
df_topic_cluster <- data.frame(
  topic     = paste0("T", df_lab5_topics$topic),
  label     = df_lab5_topics$label,
  count     = df_lab5_topics$count,
  top_words = df_lab5_topics$top_words,
  x         = .mds[, 1],
  y         = .mds[, 2],
  stringsAsFactors = FALSE
)

# Document-level scatter: simulate N points around each topic centroid
set.seed(42)
.palette_topics <- c("#00e5a0","#4facfe","#ff9f43","#a78bfa","#22d3ee","#f472b6","#ff5757")
df_cluster_docs <- do.call(rbind, lapply(seq_len(nrow(df_topic_cluster)), function(i) {
  n   <- df_topic_cluster$count[i]
  spr <- max(0.04, sqrt(n) * 0.013)
  data.frame(
    topic = df_topic_cluster$topic[i],
    label = df_topic_cluster$label[i],
    color = .palette_topics[i],
    x     = df_topic_cluster$x[i] + rnorm(n, 0, spr),
    y     = df_topic_cluster$y[i] + rnorm(n, 0, spr * 0.85),
    stringsAsFactors = FALSE
  )
}))

# ═══════════════════════════════════════════════════════════════════════════════
# ERRE QUADRO LAB — Espacenet data on Smart Glasses
# ═══════════════════════════════════════════════════════════════════════════════
.eq_dir <- file.path("output", "erre_quadro")

# Main query (top 500 of 10644 total)
df_eq_main <- read.csv(file.path(.eq_dir, "espacenet_main_500.csv"),
                       sep = ";", skip = 7, stringsAsFactors = FALSE,
                       check.names = FALSE, quote = "\"", fill = TRUE)
df_eq_main <- df_eq_main[df_eq_main$No != "" & !is.na(df_eq_main$No), ]
# Country code from publication number prefix (US, EP, CN, KR, JP, …)
df_eq_main$country <- substr(df_eq_main$`Publication number`, 1, 2)
# Priority year
df_eq_main$priority_year <- substr(df_eq_main$`Earliest priority`, 1, 4)

# Holographic query (60 results)
df_eq_holo <- read.csv(file.path(.eq_dir, "espacenet_holographic.csv"),
                       sep = ";", skip = 7, stringsAsFactors = FALSE,
                       check.names = FALSE, quote = "\"", fill = TRUE)
df_eq_holo <- df_eq_holo[df_eq_holo$No != "" & !is.na(df_eq_holo$No), ]
df_eq_holo$country <- substr(df_eq_holo$`Publication number`, 1, 2)
df_eq_holo$priority_year <- substr(df_eq_holo$`Earliest priority`, 1, 4)

# Statistics from Filters XLSX (aggregati su TUTTI i 10.644 patent)
.eq_xlsx <- file.path(.eq_dir, "espacenet_filters.xlsx")
df_eq_applicants     <- openxlsx::read.xlsx(.eq_xlsx, sheet = "Applicants")
df_eq_inventors      <- openxlsx::read.xlsx(.eq_xlsx, sheet = "Inventors")
df_eq_app_country    <- openxlsx::read.xlsx(.eq_xlsx, sheet = "Applicants - country")
df_eq_inv_country    <- openxlsx::read.xlsx(.eq_xlsx, sheet = "Inventors - country")
df_eq_ipc            <- openxlsx::read.xlsx(.eq_xlsx, sheet = "IPC main groups")
df_eq_cpc            <- openxlsx::read.xlsx(.eq_xlsx, sheet = "CPC main groups")
df_eq_cpc_offices    <- openxlsx::read.xlsx(.eq_xlsx, sheet = "CPC assigning offices")
df_eq_priority_year  <- openxlsx::read.xlsx(.eq_xlsx, sheet = "Earliest priority date")
df_eq_pub_year       <- openxlsx::read.xlsx(.eq_xlsx, sheet = "Earliest publication date (fam")
df_eq_languages      <- openxlsx::read.xlsx(.eq_xlsx, sheet = "Languages (family)")

# Standardize column names
names(df_eq_applicants)    <- c("name", "count")
names(df_eq_inventors)     <- c("name", "count")
names(df_eq_app_country)   <- c("country", "count")
names(df_eq_inv_country)   <- c("country", "count")
names(df_eq_ipc)           <- c("ipc", "count")
names(df_eq_cpc)           <- c("cpc", "count")
names(df_eq_cpc_offices)   <- c("office", "count")
names(df_eq_priority_year) <- c("year", "count")
names(df_eq_pub_year)      <- c("year", "count")
names(df_eq_languages)     <- c("lang", "count")

df_eq_priority_year$year <- as.integer(df_eq_priority_year$year)
df_eq_pub_year$year      <- as.integer(df_eq_pub_year$year)

# IPC labels (human readable) — mapping per le top classi
.ipc_labels <- c(
  "G02B27" = "Optical systems / instruments",
  "G06F3"  = "Input arrangements (I/O)",
  "H04N5"  = "Pictorial TV details",
  "G09G5"  = "Visual indicators control",
  "G06T19" = "3D image manipulation",
  "H04N13" = "Stereoscopic TV systems",
  "G02B5"  = "Optical elements (lenses, prisms)",
  "G09G3"  = "Display arrangements",
  "G06F1"  = "Computer architecture details",
  "G02F1"  = "Optical modulators",
  "G02C11" = "Non-optical spectacles features",
  "G06T7"  = "Image analysis",
  "G02B6"  = "Light guides",
  "G02C5"  = "Spectacle frames",
  "G02B7"  = "Mounts/lens supports"
)
df_eq_ipc$label <- ifelse(df_eq_ipc$ipc %in% names(.ipc_labels),
                          .ipc_labels[df_eq_ipc$ipc], "")

# Country full names
.country_names <- c(
  US = "United States", JP = "Japan", KR = "South Korea", CN = "China",
  TW = "Taiwan", DE = "Germany", IL = "Israel", GB = "United Kingdom",
  SE = "Sweden", FR = "France", NL = "Netherlands", CA = "Canada",
  FI = "Finland", LU = "Luxembourg", SG = "Singapore", AT = "Austria",
  IN = "India", EP = "European Patent Office", CH = "Switzerland",
  AU = "Australia", RU = "Russia", BE = "Belgium", DK = "Denmark",
  IT = "Italy", ES = "Spain", NO = "Norway", IE = "Ireland"
)
df_eq_app_country$country_name <- ifelse(df_eq_app_country$country %in% names(.country_names),
                                         .country_names[df_eq_app_country$country],
                                         df_eq_app_country$country)
df_eq_inv_country$country_name <- ifelse(df_eq_inv_country$country %in% names(.country_names),
                                         .country_names[df_eq_inv_country$country],
                                         df_eq_inv_country$country)


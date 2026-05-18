# ═══════════════════════════════════════════════════════════════════════════════
# UI — AGGIORNATA CON DATA VIZ E BREVETTI REAL
# ═══════════════════════════════════════════════════════════════════════════════

ui <- page_fillable(
  theme = bs_theme(
    version  = 5,
    bg       = col$bg,
    fg       = col$text,
    primary  = col$accent,
    base_font = font_google("Outfit"),
    code_font = font_google("JetBrains Mono")
  ),
  tags$head(tags$style(HTML(app_css))),
  
  layout_sidebar(
    fillable = TRUE,
    
    # ── SIDEBAR ──
    sidebar = sidebar(
      width  = 240,
      bg     = col$card,
      open   = TRUE,
      tags$div(
        style = "padding: 8px 4px 20px;",
        tags$div(style = "font-size:11px; letter-spacing:3px; text-transform:uppercase; color:var(--muted); font-weight:600;", "SCI Project"),
        tags$img(
          src = "porter/loghi/logo_nuance.png", 
          style = "height: 32px; margin-top: 8px; margin-bottom: 8px; filter: invert(1) brightness(1.5);", 
          alt = "Nuance Audio Logo"
        ),
        tags$div(style = paste0("font-size:12px; color:", col$accent, "; font-weight:500; margin-top:2px;"), "Competitive Intelligence"),
        tags$hr(style = "border-color:var(--border); margin:18px 0 12px;")
      ),
      navset_pill_list(
        id = "main_nav",
        well = FALSE,
        widths = c(12, 12),
        nav_panel(title = "Home (README)",        value = "intro",       icon = icon("home")),
        nav_panel(title = "Porter's Five Forces", value = "porter",      icon = icon("shield-alt")),
        nav_panel(title = "VUCA Analysis",        value = "vuca",        icon = icon("compass")),
        nav_panel(title = "Scope & KITs/KIQs",    value = "scope",       icon = icon("crosshairs")),
        nav_panel(title = "Query Design",         value = "queries",     icon = icon("search")),
        nav_panel(title = "GenAI for CI",         value = "genai",       icon = icon("robot")),
        nav_panel(title = "Text Analysis",        value = "textanalysis", icon = icon("file-lines")),
        
        # AGGIUNTI PER GLI ULTIMI LABORATORI
        nav_panel(title = "Patent Analysis",      value = "patents",      icon = icon("certificate")),
        nav_panel(title = "Data Viz Lab",         value = "storyboard",   icon = icon("chart-pie"))
      ),
      tags$div(
        style = "position:absolute; bottom:16px; left:16px; right:16px;",
        tags$div(
          style = "display:flex; align-items:center; gap:12px; margin-bottom:12px;",
          tags$img(src = "cherubino_556378.jpg", style = "width:42px; height:auto; mix-blend-mode: lighten;", alt = "Unipi"),
          tags$div(
            style = "font-size:10px; color:var(--muted); line-height:1.6;",
            HTML("SCI — A.Y. 2025/2026<br>Prof. Irene Spada<br>University of Pisa")
          )
        ),
        tags$div(
          style = "font-size:10px; color:var(--dim); line-height:1.5; border-top:1px solid var(--border); padding-top:8px;",
          HTML("<strong>Project Team:</strong><br>Filippo Del Rosso,<br>Riccardo Diprima,<br>Alice Giovagnini")
        )
      )
    ),
    
    # ── MAIN CONTENT ──
    tags$div(style = "max-width:1100px; margin:0 auto; padding: 0 12px;",
             navset_hidden(
               id = "main_content",
               nav_panel(title = "", value = "intro",        uiOutput("tab_intro")),
               nav_panel(title = "", value = "porter",       uiOutput("tab_porter")),
               nav_panel(title = "", value = "vuca",         uiOutput("tab_vuca")),
               nav_panel(title = "", value = "scope",        uiOutput("tab_scope")),
               nav_panel(title = "", value = "queries",      uiOutput("tab_queries")),
               nav_panel(title = "", value = "genai",        uiOutput("tab_genai")),
               nav_panel(title = "", value = "textanalysis", uiOutput("tab_textanalysis")),
               
               # AGGIUNTI PER GLI ULTIMI LABORATORI
               nav_panel(title = "", value = "patents",      uiOutput("tab_patents")),
               nav_panel(title = "", value = "storyboard",   uiOutput("tab_storyboard"))
             )
    )
  )
)
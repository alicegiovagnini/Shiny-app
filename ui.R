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
  tags$head(
    # Precarica le librerie JS dei widget (plotly, DataTables, ECharts, wordcloud2)
    # così sono già disponibili quando le schede vengono generate via renderUI.
    widget_deps,
    tags$style(HTML(app_css)),
    tags$script(HTML("
      // Le schede sono pannelli di un navset_hidden: a differenza di un tabset
      // normale, cambiando pannello NON viene emesso l'evento 'shown.bs.tab' che
      // Shiny usa per ricalcolare la visibilità degli output. Senza quel segnale
      // molti output restavano 'sospesi' e non ricevevano mai il loro valore,
      // apparendo come riquadri vuoti. Qui, ad ogni cambio scheda, emettiamo noi
      // un evento 'shown' (che risale fino a document.body, dove Shiny ascolta):
      // Shiny riattiva gli output ora visibili -> il server invia i valori ->
      // i grafici/tabelle si disegnano. Poi forziamo il redraw alla dimensione
      // reale di plotly e degli altri htmlwidget.
      Shiny.addCustomMessageHandler('triggerPlotlyResize', function(msg) {
        // Riporta in cima la pagina ad ogni cambio scheda.
        // Azzera tutti i possibili container scrollabili: bslib può usare
        // .bslib-main, .bslib-page-fill, .tab-content o un elemento overflow:auto
        // a seconda della versione e del browser.
        function scrollAllToTop() {
          window.scrollTo(0, 0);
          document.documentElement.scrollTop = 0;
          document.body.scrollTop = 0;
          var selectors = [
            '.bslib-main', '.bslib-page-fill', '.tab-content',
            '.bslib-sidebar-layout > :last-child',
            '.bslib-gap-spacing', '.html-fill-container'
          ];
          selectors.forEach(function(sel) {
            document.querySelectorAll(sel).forEach(function(el) {
              el.scrollTop = 0;
            });
          });
          // Fallback: qualunque elemento con scrollTop > 0
          document.querySelectorAll('*').forEach(function(el) {
            if (el.scrollTop > 0) el.scrollTop = 0;
          });
        }
        scrollAllToTop();
        setTimeout(scrollAllToTop, 80);
        setTimeout(scrollAllToTop, 250);

        // Una volta sola: segnala al server che la scheda è renderizzata e i suoi
        // output sono ormai 'bound' lato client, così il server può ri-inviare i
        // valori dei widget. Senza questo, i valori calcolati troppo presto
        // (prima che i div fossero bound) venivano scartati e i grafici/tabelle
        // restavano vuoti.
        setTimeout(function() {
          if (window.Shiny) {
            Shiny.setInputValue('nav_redraw', Date.now(), {priority: 'event'});
          }
        }, 300);
        // Più volte: ridisegna gli htmlwidget visibili alla dimensione reale.
        [60, 350, 700, 1100].forEach(function(delay) {
          setTimeout(function() {
            if (window.jQuery) {
              jQuery('#main_content').trigger('shown');
              jQuery('.tab-pane.active, .tab-content > .active').trigger('shown');
            }
            window.dispatchEvent(new Event('resize'));
            if (typeof Plotly !== 'undefined') {
              document.querySelectorAll('.plotly.html-widget').forEach(function(el) {
                if (el.offsetParent !== null && el._fullLayout) {
                  Plotly.Plots.resize(el);
                }
              });
            }
          }, delay);
        });
      });
    "))
  ),
  
  layout_sidebar(
    fillable = TRUE,
    
    # ── SIDEBAR ──
    sidebar = sidebar(
      width  = 240,
      bg     = col$card,
      open   = "always",
      resizable = FALSE,
      # ── Header + Video in un unico contenitore (evita padding bslib) ──
      tags$div(
        style = "margin:0; padding:0;",
        tags$div(
          style = "padding:4px 4px 2px; text-align:center;",
          tags$div(style = "font-size:11px; letter-spacing:3px; text-transform:uppercase; color:var(--muted); font-weight:600;", "SCI Project"),
          tags$img(
            src = "porter/loghi/logo_nuance.png",
            style = "height:28px; margin-top:4px; margin-bottom:4px; filter:invert(1) brightness(1.5); display:block; margin-left:auto; margin-right:auto;",
            alt = "Nuance Audio Logo"
          ),
          tags$div(style = paste0("font-size:12px; color:", col$accent, "; font-weight:500; margin-bottom:1px;"), "Competitive Intelligence")
        ),
        tags$div(
          style = paste0("background:", col$card, "; overflow:hidden; height:80px;"),
          tags$video(
            src = "sidebar_video.mp4", type = "video/mp4",
            autoplay = NA, loop = NA, muted = NA, playsinline = NA,
            style = "width:100%; height:100%; object-fit:cover; object-position:center; display:block; filter:invert(1) brightness(0.85); mix-blend-mode:screen;"
          )
        ),
        tags$hr(style = "border-color:var(--border); margin:0 0 2px;"),
        # ── Nav nello stesso contenitore per eliminare il gap bslib ──
        tags$div(
          style = "overflow-y:visible; padding-right:2px;",
          uiOutput("custom_nav")
        )
      ),
      tags$div(style = "display:none;",
        navset_pill_list(
          id = "main_nav",
          well = FALSE,
          widths = c(12, 12),
          nav_panel(title = "Home (README)",        value = "intro",        icon = icon("home")),
          nav_panel(title = "Porter's Five Forces", value = "porter",       icon = icon("shield-alt")),
          nav_panel(title = "VUCA Analysis",        value = "vuca",         icon = icon("compass")),
          nav_panel(title = "Scope & KITs/KIQs",   value = "scope",        icon = icon("crosshairs")),
          nav_panel(title = "Query Design",         value = "queries",      icon = icon("search")),
          nav_panel(title = "GenAI for CI",         value = "genai",        icon = icon("robot")),
          nav_panel(title = "Text Analysis",        value = "textanalysis", icon = icon("file-lines")),
          nav_panel(title = "Co-word Networks",     value = "cowords",      icon = icon("diagram-project")),
          nav_panel(title = "BERTopic Maps",        value = "bertopicmaps", icon = icon("sitemap")),
          nav_panel(title = "Patent Analysis",      value = "patents",      icon = icon("certificate")),
          nav_panel(title = "KIQs Visualization",         value = "storyboard",   icon = icon("chart-pie")),
          nav_panel(title = "Executive Summary",          value = "executive",    icon = icon("flag"))
        )
      ),
      tags$div(
        style = "position:absolute; bottom:16px; left:16px; right:16px; text-align:center;",
        tags$div(
          style = "display:inline-flex; align-items:center; gap:10px; margin-bottom:10px;",
          tags$img(src = "cherubino_556378.jpg", style = "width:48px; height:auto; mix-blend-mode: lighten; filter: brightness(1.6);", alt = "Unipi"),
          tags$div(
            style = "font-size:10px; color:var(--dim); line-height:1.6; text-align:left;",
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
               nav_panel(title = "", value = "cowords",      uiOutput("tab_cowords")),
               nav_panel(title = "", value = "bertopicmaps", uiOutput("tab_bertopicmaps")),

               # AGGIUNTI PER GLI ULTIMI LABORATORI
               nav_panel(title = "", value = "patents",      uiOutput("tab_patents")),
               nav_panel(title = "", value = "storyboard",   uiOutput("tab_storyboard")),
               nav_panel(title = "", value = "executive",    uiOutput("tab_executive")),
               nav_panel(title = "", value = "guide",        uiOutput("tab_guide"))
             )
    )
  )
)
# ═══════════════════════════════════════════════════════════════════════════════
# SERVER
# ═══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  # ── Sincronizza la sidebar con l'area principale ──
  observeEvent(input$main_nav, {
    nav_select("main_content", selected = input$main_nav)
  })

  # ── Navigazione custom ad albero (fissa, sempre aperta) ──
  nav_folder_open <- reactiveVal(NULL)

  observeEvent(input$nav_folder_click, {
    clicked <- input$nav_folder_click
    current <- nav_folder_open()
    cur_tab <- if (is.null(input$main_nav)) "intro" else input$main_nav
    p_tabs  <- c("scope","queries","genai","textanalysis","cowords","bertopicmaps","patents","errequadro")
    r_tabs  <- c("porter","vuca","storyboard","executive")

    is_open <- switch(clicked,
      "process" = (cur_tab %in% p_tabs || identical(current, "process")) &&
                  !identical(current, "suppress-process") && !identical(current, "results"),
      "results" = (cur_tab %in% r_tabs || identical(current, "results")) &&
                  !identical(current, "suppress-results") && !identical(current, "process"),
      FALSE
    )
    nav_folder_open(if (is_open) paste0("suppress-", clicked) else clicked)
  })

  observeEvent(input$main_nav, { nav_folder_open(NULL) }, ignoreInit = TRUE)

  output$custom_nav <- renderUI({
    cur        <- if (is.null(input$main_nav)) "intro" else input$main_nav
    fol_open   <- nav_folder_open()

    btn_item <- function(value, label, fa_icon, color) {
      is_active <- cur == value
      tags$div(
        class = paste("nav-btn-item", if (is_active) "active" else ""),
        style = paste0("border-left:2px solid ", if (is_active) color else "var(--border)", ";"),
        onclick = paste0("Shiny.setInputValue('main_nav','", value, "',{priority:'event'});"),
        tags$i(
          class = paste0("fas fa-", fa_icon),
          style = paste0("font-size:11px; width:14px; flex-shrink:0; color:",
                         if (is_active) color else "var(--muted)", ";")
        ),
        tags$span(label)
      )
    }

    folder_hdr <- function(folder_id, color, fa_icon, label, is_active = FALSE) {
      tags$div(
        class = "nav-custom-link",
        style = paste0(
          "color:", color, "; border-color:", color, "40;",
          " background:", color, if (is_active) "30;" else "15;"
        ),
        onclick    = paste0("Shiny.setInputValue('nav_folder_click','", folder_id, "',{priority:'event'});"),
        onmouseover = paste0("this.style.background='", color, "28'; this.style.borderColor='", color, "80';"),
        onmouseout  = paste0("this.style.background='", color, if (is_active) "30'" else "15'", "; this.style.borderColor='", color, "40';"),
        tags$i(class = paste0("fas fa-", fa_icon), style = paste0("font-size:12px; width:15px; color:", color, ";")),
        tags$span(label, style = "flex:1;"),
        tags$i(class = "fas fa-chevron-right nav-folder-chevron")
      )
    }

    process_tabs <- c("scope", "queries", "genai", "textanalysis", "cowords", "bertopicmaps", "patents", "errequadro")
    results_tabs <- c("porter", "vuca", "storyboard", "executive")

    proc_active  <- cur %in% process_tabs
    res_active   <- cur %in% results_tabs

    proc_suppressed <- identical(fol_open, "results") || identical(fol_open, "suppress-process")
    res_suppressed  <- identical(fol_open, "process") || identical(fol_open, "suppress-results")

    proc_class <- trimws(paste(
      "nav-folder-group",
      if (proc_active) "has-active" else "",
      if (proc_suppressed) "is-suppressed" else "",
      if (identical(fol_open, "process")) "is-open" else ""
    ))
    res_class <- trimws(paste(
      "nav-folder-group",
      if (res_active) "has-active" else "",
      if (res_suppressed) "is-suppressed" else "",
      if (identical(fol_open, "results")) "is-open" else ""
    ))

    tagList(
      tags$div(
        class = paste("nav-custom-link", if (cur == "intro") "active" else ""),
        style = if (cur == "intro") "background:rgba(255,255,255,0.09); border-color:rgba(255,255,255,0.2); color:white;" else "color:var(--dim);",
        onclick = "Shiny.setInputValue('main_nav','intro',{priority:'event'});",
        onmouseover = if (cur == "intro") NULL else "this.style.background='rgba(255,255,255,0.07)'; this.style.color='white';",
        onmouseout  = if (cur == "intro") NULL else "this.style.background=''; this.style.color='var(--dim)';",
        tags$i(class = "fas fa-house", style = "font-size:12px; width:15px;"),
        "Home"
      ),

      tags$div(style = "height:1px; background:var(--border); margin:4px 2px;"),

      tags$div(
        class = proc_class,
        folder_hdr("process", col$blue, "route", "The Process", is_active = proc_active),
        tags$div(
          class = "nav-folder-children",
          style = "margin-bottom:4px;",
          btn_item("scope",       "Scope & KITs/KIQs", "crosshairs",       col$blue),
          btn_item("queries",     "Query Design",       "magnifying-glass", col$blue),
          btn_item("genai",       "GenAI for CI",       "robot",            col$blue),
          btn_item("textanalysis","Text Analysis",      "file-lines",       col$blue),
          btn_item("cowords",     "Co-word Networks",   "diagram-project",  col$blue),
          btn_item("bertopicmaps","BERTopic Maps",      "sitemap",          col$blue),
          btn_item("patents",     "Patent Analysis",    "certificate",      col$blue),
          btn_item("errequadro",  "Erre Quadro",        "flask",            col$blue)
        )
      ),

      tags$div(style = "height:1px; background:var(--border); margin:4px 2px;"),

      tags$div(
        class = res_class,
        folder_hdr("results", col$accent, "chart-line", "The Results", is_active = res_active),
        tags$div(
          class = "nav-folder-children",
          btn_item("porter",     "Porter's Five Forces", "shield-halved", col$accent),
          btn_item("vuca",       "VUCA Analysis",        "compass",       col$accent),
          btn_item("storyboard", "Data Visualization",   "chart-pie",     col$accent),
          btn_item("executive",  "Executive Summary",    "flag",          col$accent)
        )
      )
    )
  })

  # ──────────────────────────────────────────────
  # TAB 0: INTRODUCTION AND GUIDE (README)
  # ──────────────────────────────────────────────
  output$tab_intro <- renderUI({
    tagList(
      # 1. Welcome Hero Banner — split layout
      tags$div(class = "hero-banner",
        style = "padding:16px 22px; margin-bottom:10px; overflow:hidden; cursor:default; transition:transform 0.22s, box-shadow 0.22s;",
        onmouseover = "this.style.transform='translateY(-3px)'; this.style.boxShadow='0 12px 36px rgba(0,0,0,0.35)';",
        onmouseout  = "this.style.transform=''; this.style.boxShadow='';",
        tags$div(style = "display:flex; align-items:center; gap:24px;",

          # LEFT — branding + tagline (45%)
          tags$div(style = "flex:0 0 45%; min-width:0;",
            tags$div(
              style = "display:flex; align-items:center; gap:8px; margin-bottom:14px; flex-wrap:wrap;",
              tags$img(src = "porter/loghi/logo_nuance.png",
                       style = "height:24px; filter:invert(1) brightness(1.8);"),
              tags$span(style = "font-size:9px; color:var(--accent); font-weight:700; letter-spacing:2px; text-transform:uppercase;",
                        "EssilorLuxottica"),
              tags$span(style = "color:var(--border); font-size:12px;", "·"),
              tags$span(style = "font-size:9px; color:var(--muted); font-weight:500; letter-spacing:1.5px; text-transform:uppercase;",
                        "Strategic & Competitive Intelligence")
            ),
            tags$div(
              style = paste0("font-size:20px; font-weight:900; line-height:1.25; margin-bottom:8px;",
                             " background:linear-gradient(90deg, white 0%, ", col$accent, " 100%);",
                             " -webkit-background-clip:text; -webkit-text-fill-color:transparent;",
                             " background-clip:text;"),
              "The new invisible acoustic solution"
            ),
            tags$p(
              style = "font-size:11.5px; color:var(--dim); margin:0; line-height:1.55;",
              HTML("A Strategic & Competitive Intelligence project on EssilorLuxottica's Nuance Audio.<br>The first FDA-cleared hearing glasses.")
            )
          ),

          # RIGHT — due immagini uguali affiancate (55%)
          tags$div(
            style = "flex:0 0 55%; display:grid; grid-template-columns:1fr 1fr; gap:8px; height:115px;",
            tags$img(src = "hero_lifestyle.png",
                     style = paste0("width:100%; height:115px; object-fit:cover; object-position:top;",
                                    " border-radius:12px; border:1px solid var(--border);",
                                    " box-shadow:0 4px 20px rgba(0,0,0,0.4);")),
            tags$img(src = "hero_business.png",
                     style = paste0("width:100%; height:115px; object-fit:cover; object-position:top;",
                                    " border-radius:12px; border:1px solid ", col$accent, "40;",
                                    " box-shadow:0 4px 20px rgba(0,0,0,0.4);"))
          )
        )
      ),

      # domanda sopra i 4 card
      tags$div(
        style = "text-align:center; margin-bottom:10px; display:flex; align-items:center; gap:12px;",
        tags$div(style = paste0("flex:1; height:1px; background:linear-gradient(to right, transparent, ", col$accent, "60);")),
        tags$span(
          style = "font-size:12px; font-weight:700; color:var(--dim); letter-spacing:1px; text-transform:uppercase; white-space:nowrap;",
          "What sets Nuance Audio apart?"
        ),
        tags$div(style = paste0("flex:1; height:1px; background:linear-gradient(to left, transparent, ", col$accent, "60);"))
      ),

      # 2. Concept Section — CSS grid, 4 colonne uguale altezza
      local({
        hover_in  <- paste0(
          "this.style.transform='translateY(-3px)';",
          "this.style.borderColor='rgba(0,229,160,0.28)';",
          "this.style.boxShadow='0 0 0 1px rgba(0,229,160,0.12),0 14px 36px rgba(0,0,0,0.4)';"
        )
        hover_out <- paste0("this.style.transform='';this.style.borderColor='", col$border, "';this.style.boxShadow='';")


        fcard <- function(media, title, text, is_video = FALSE) {
          tags$div(
            style = paste0(
              "border-radius:14px; overflow:hidden;",
              "border:1px solid var(--border); background:var(--card);",
              "transition:transform 0.22s, box-shadow 0.22s, border-color 0.22s;",
              "display:flex; flex-direction:column;"
            ),
            onmouseover = hover_in, onmouseout = hover_out,
            tags$div(
              style = "height:175px; overflow:hidden; background:#0a1020; flex-shrink:0;",
              if (is_video)
                tags$video(src = media, type = "video/mp4",
                           autoplay = NA, loop = NA, muted = NA, playsinline = NA,
                           style = "width:100%; height:100%; object-fit:cover; display:block;")
              else
                tags$img(src = media,
                         style = "width:100%; height:100%; object-fit:cover; display:block;")
            ),
            tags$div(
              style = "padding:10px 14px 12px; flex:1;",
              tags$h3(style = "color:white; font-weight:800; font-size:13px; margin:0 0 4px; line-height:1.3;", title),
              tags$p(style = "color:var(--dim); font-size:11px; line-height:1.5; margin:0;", text)
            )
          )
        }

        tags$div(
          style = "display:grid; grid-template-columns:repeat(4,1fr); gap:14px; margin-bottom:14px;",
          fcard("intro_invisible.jpg",
            "Invisible and comfortable",
            "Lightweight eyewear with open-ear acoustic solutions integrated into the frames. No bulky earbuds, no visible technology."),
          fcard("intro_sight.jpg",
            "Sight and hearing in one solution",
            "Why choose between sight support and hearing support? Nuance Audio glasses do both, breaking down social stigma."),
          fcard("intro_app.png",
            "Easy to use",
            "Start using them easily with the Nuance Audio App and video tutorials that will guide you step by step. You won't need an expert."),
          fcard("intro_transitions.mp4",
            "Adaptable, for all-day wear",
            "Our non-prescription glasses feature Transitions® technology, with lenses that adapt to light to ensure clear and effortless vision.",
            is_video = TRUE)
        )
      }),
      
      # 3. Navigation Guide — two clickable tiles that open a dedicated guide tab
      local({
        nav_card <- function(icon_name, badge, title, desc, color, bg_rgba, border_rgba, section) {
          tags$div(
            class = "sci-card",
            style = paste0(
              "padding:20px 16px 18px; border-top:3px solid ", color,
              "; margin-bottom:0; background:", pastel_bg(color),
              "; cursor:pointer; transition:transform 0.18s, box-shadow 0.18s;"
            ),
            onclick = paste0("Shiny.setInputValue('guide_section','", section, "',{priority:'event'}); Shiny.setInputValue('nav_goto','guide',{priority:'event'});"),
            onmouseover = paste0("this.style.transform='translateY(-2px)'; this.style.boxShadow='0 8px 24px rgba(0,0,0,0.18)'; this.style.borderColor='", color, "';"),
            onmouseout  = paste0("this.style.transform=''; this.style.boxShadow=''; this.style.borderColor='", color, "';"),
            tags$div(style = "text-align:center;",
              tags$div(
                style = paste0("display:inline-flex; align-items:center; justify-content:center; width:40px; height:40px; border-radius:12px; background:", bg_rgba, "; border:1px solid ", border_rgba, "; margin-bottom:7px;"),
                tags$i(class = paste0("fas fa-", icon_name), style = paste0("font-size:18px; color:", color, ";"))
              ),
              tags$div(style = paste0("font-size:9px; letter-spacing:2px; text-transform:uppercase; color:", color, "; font-weight:700; margin-bottom:4px;"), badge),
              tags$h3(style = paste0("color:", pastel_text(color), "; font-weight:800; font-size:15px; margin-bottom:5px;"), title),
              tags$p(style = paste0("color:", pastel_text(color), "bb; font-size:11.5px; line-height:1.5; margin:0 0 12px;"), desc),
              tags$div(
                style = paste0("display:inline-flex; align-items:center; gap:5px; font-size:10px; letter-spacing:1px; text-transform:uppercase; font-weight:700; color:", color, "; border:1px solid ", border_rgba, "; border-radius:20px; padding:4px 12px;"),
                tags$i(class = "fas fa-arrow-right", style = "font-size:9px;"),
                "Open guide"
              )
            )
          )
        }

        tagList(
          section_hdr("Navigation Guide", "Click on a section to explore tabs, methods, and deliverables"),
          tags$div(
            style = "display:grid; grid-template-columns:1fr 1fr; gap:14px;",
            nav_card("route",      "Methodology", "The Process",
              "The methodological workflow of the labs: KIT/KIQ, Query Design, GenAI Prompting, and Bibliometric Text Analysis.",
              col$blue, "rgba(79,172,254,0.18)", "rgba(79,172,254,0.35)", "process"),
            nav_card("chart-line", "Outcome",     "The Results",
              "Strategic core deliverables: Porter's Five Forces, VUCA Analysis, Data Visualization, and Executive Summary.",
              col$accent, "rgba(0,229,160,0.18)", "rgba(0,229,160,0.35)", "results")
          )
        )
      })
    )
  })
  
  # Questo "ascolta" il click sui bottoni e cambia pagina
  observeEvent(input$nav_goto, {
    nav_select("main_content", selected = input$nav_goto)
  })

  # ──────────────────────────────────────────────
  # TAB: NAVIGATION GUIDE
  # ──────────────────────────────────────────────
  output$tab_guide <- renderUI({
    section    <- if (is.null(input$guide_section)) "process" else input$guide_section
    is_process <- identical(section, "process")

    col_main   <- if (is_process) col$blue   else col$accent
    col_other  <- if (is_process) col$accent else col$blue
    icon_main  <- if (is_process) "route"       else "chart-line"
    badge_main <- if (is_process) "Methodology" else "Outcome"
    title_main <- if (is_process) "The Process" else "The Results"
    other_sec  <- if (is_process) "results"     else "process"
    other_title<- if (is_process) "The Results" else "The Process"
    other_icon <- if (is_process) "chart-line"  else "route"

    arrow_link <- function(value, label, color) {
      tags$div(
        style = "display:flex; align-items:center; gap:7px; padding:2px 0; cursor:pointer; transition:opacity 0.15s;",
        onmouseover = "this.style.opacity='0.6';",
        onmouseout  = "this.style.opacity='1';",
        onclick = paste0("Shiny.setInputValue('nav_goto','", value, "',{priority:'event'});"),
        tags$div(style = paste0("width:3px; height:15px; background:", color, "; border-radius:2px; flex-shrink:0;")),
        tags$span(paste0("→ ", label), style = paste0("font-size:12px; font-weight:700; color:", color, ";"))
      )
    }
    gsec <- function(fa_icon, title, desc, color, ...) {
      tags$div(
        style = paste0("background:", pastel_bg(color), "; border:1px solid rgba(0,0,0,0.07); border-left:3px solid ", color, "; border-radius:8px; padding:8px 12px; margin-bottom:6px;"),
        tags$div(
          style = "display:flex; align-items:center; gap:7px; margin-bottom:3px;",
          tags$i(class = paste0("fas fa-", fa_icon), style = paste0("font-size:11px; color:", color, ";")),
          tags$span(title, style = paste0("font-size:12px; font-weight:800; color:", pastel_text(color), ";"))
        ),
        tags$p(style = paste0("font-size:10px; color:", pastel_text(color), "99; line-height:1.4; margin:0 0 5px;"), desc),
        ...
      )
    }

    content <- if (is_process) {
      tagList(
        gsec("crosshairs", "Scope & KITs/KIQs",
          "Strategic objectives and intelligence questions structured via the Rumsfeld Matrix and SMART framework.",
          col$blue, arrow_link("scope", "Scope & KITs/KIQs", col$blue)),
        gsec("magnifying-glass", "Query Design",
          "Systematic construction of Boolean search strings for Scopus and open-web sources.",
          col$blue, arrow_link("queries", "Query Design", col$blue)),
        gsec("robot", "GenAI for CI",
          "Evaluation of generative AI tools using the CREATE Framework with structured benchmarking.",
          col$blue, arrow_link("genai", "GenAI for CI", col$blue)),
        gsec("file-lines", "Advanced Text Analysis",
          "Bibliometric pipelines, NLP, BERTopic topic modelling, and patent landscape classification.",
          col$blue,
          arrow_link("textanalysis", "Text Analysis",    col$blue),
          arrow_link("cowords",      "Co-word Networks", col$blue),
          arrow_link("bertopicmaps", "BERTopic Maps",    col$blue),
          arrow_link("patents",      "Patent Analysis",  col$blue),
          arrow_link("errequadro",   "Erre Quadro Lab",  col$blue))
      )
    } else {
      tagList(
        gsec("shield-halved", "Strategic Frameworks",
          "Porter's Five Forces and VUCA Analysis: competitive dynamics, market entry barriers, and uncertainty drivers.",
          col$accent,
          arrow_link("porter", "Porter's Five Forces", col$accent),
          arrow_link("vuca",   "VUCA Analysis",        col$accent)),
        gsec("lightbulb", "Decision Synthesis",
          "The final intelligence storyboard connecting empirical indicators to business actions and investment signals.",
          col$accent, arrow_link("storyboard", "Data Visualization", col$accent)),
        gsec("flag", "Executive Summary",
          "Key findings, strategic recommendations, and actionable next steps for the Nuance Audio product line.",
          col$accent, arrow_link("executive", "Executive Summary", col$accent))
      )
    }

    stat_mini <- function(value, label) {
      tags$div(
        style = "display:flex; align-items:center; gap:10px; padding:7px 0; border-bottom:1px solid var(--border);",
        tags$span(value, style = paste0("font-size:13px; font-weight:900; color:", col_main, "; min-width:94px; flex-shrink:0;")),
        tags$span(label, style = "font-size:10.5px; color:var(--dim); line-height:1.4;")
      )
    }

    tagList(
      # Top bar: back + switch section
      tags$div(
        style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:18px;",
        tags$span(
          style = "display:inline-flex; align-items:center; gap:7px; cursor:pointer; font-size:12px; font-weight:700; color:var(--dim); opacity:0.8; transition:opacity 0.15s;",
          onmouseover = "this.style.opacity='1';",
          onmouseout  = "this.style.opacity='0.8';",
          onclick = "Shiny.setInputValue('nav_goto','intro',{priority:'event'});",
          tags$i(class = "fas fa-arrow-left", style = "font-size:11px;"),
          "Back to Home"
        ),
        tags$span(
          style = paste0("display:inline-flex; align-items:center; gap:7px; cursor:pointer; font-size:12px; font-weight:700; color:", col_other, "; opacity:0.75; transition:opacity 0.15s;"),
          onmouseover = "this.style.opacity='1';",
          onmouseout  = "this.style.opacity='0.75';",
          onclick = paste0("Shiny.setInputValue('guide_section','", other_sec, "',{priority:'event'});"),
          tags$i(class = paste0("fas fa-", other_icon), style = "font-size:11px;"),
          paste("Switch to", other_title),
          tags$i(class = "fas fa-arrow-right", style = "font-size:11px;")
        )
      ),

      # Two-column grid
      tags$div(
        style = "display:grid; grid-template-columns:58fr 42fr; gap:22px; align-items:start;",

        # ── LEFT: section content ──
        tags$div(
          tags$div(
            style = paste0("display:flex; align-items:center; gap:10px; padding:10px 14px; border-radius:10px; background:", pastel_bg(col_main), "; border-left:4px solid ", col_main, "; margin-bottom:10px;"),
            tags$div(
              style = paste0("display:inline-flex; align-items:center; justify-content:center; width:34px; height:34px; border-radius:10px; background:", col_main, "22; border:1px solid ", col_main, "55; flex-shrink:0;"),
              tags$i(class = paste0("fas fa-", icon_main), style = paste0("font-size:15px; color:", col_main, ";"))
            ),
            tags$div(
              tags$div(style = paste0("font-size:8px; letter-spacing:2px; text-transform:uppercase; color:", col_main, "; font-weight:700; margin-bottom:2px;"), badge_main),
              tags$h2(style = paste0("color:", pastel_text(col_main), "; font-weight:900; font-size:15px; margin:0;"), title_main)
            )
          ),
          content
        ),

        # ── RIGHT: product panel — fixed heights calibrated per section ──
        local({
          second_h <- if (is_process) "350px" else "168px"
          tags$div(
            style = "display:flex; flex-direction:column; gap:10px;",
            tags$img(
              src = "nuance_box.png",
              style = "width:100%; border-radius:12px; object-fit:cover; height:160px; display:block;"
            ),
            tags$img(
              src = "nuance_table.png",
              style = paste0("width:100%; border-radius:12px; object-fit:cover; height:", second_h, "; object-position:50% 60%; display:block;")
            )
          )
        })
      ),

      # ── BOTTOM: campaign photo gallery ──
      tags$div(
        style = "margin-top:28px; padding-top:22px; border-top:1px solid var(--border);",
        # Brand tagline header
        tags$div(
          style = "display:flex; align-items:center; justify-content:center; gap:14px; margin-bottom:16px;",
          tags$div(style = "flex:1; height:1px; background:var(--border);"),
          tags$span("Hear Better · See Better · Live Better",
                    style = "font-size:10px; letter-spacing:2px; text-transform:uppercase; color:var(--dim); font-weight:600; white-space:nowrap;"),
          tags$div(style = "flex:1; height:1px; background:var(--border);")
        ),
        # 4-image grid
        tags$div(
          style = "display:grid; grid-template-columns:repeat(4,1fr); gap:10px;",
          tags$div(style = "position:relative; overflow:hidden; border-radius:12px;",
            tags$img(src = "nuance_scan.png",    style = "width:100%; height:140px; object-fit:cover; object-position:50% 20%; display:block;"),
            tags$div(style = "position:absolute; bottom:0; left:0; right:0; padding:8px 10px; background:linear-gradient(to top,rgba(0,0,0,0.7),transparent); font-size:10px; color:white; font-weight:600;", "Technology")
          ),
          tags$div(style = "position:relative; overflow:hidden; border-radius:12px;",
            tags$img(src = "nuance_dinner.png",  style = "width:100%; height:140px; object-fit:cover; object-position:top; display:block;"),
            tags$div(style = "position:absolute; bottom:0; left:0; right:0; padding:8px 10px; background:linear-gradient(to top,rgba(0,0,0,0.7),transparent); font-size:10px; color:white; font-weight:600;", "Social Life")
          ),
          tags$div(style = "position:relative; overflow:hidden; border-radius:12px;",
            tags$img(src = "nuance_outdoor.png", style = "width:100%; height:140px; object-fit:cover; object-position:50% 45%; display:block;"),
            tags$div(style = "position:absolute; bottom:0; left:0; right:0; padding:8px 10px; background:linear-gradient(to top,rgba(0,0,0,0.7),transparent); font-size:10px; color:white; font-weight:600;", "Everyday Wear")
          ),
          tags$div(style = "position:relative; overflow:hidden; border-radius:12px;",
            tags$img(src = "nuance_social.png",  style = "width:100%; height:140px; object-fit:cover; object-position:center; display:block;"),
            tags$div(style = "position:absolute; bottom:0; left:0; right:0; padding:8px 10px; background:linear-gradient(to top,rgba(0,0,0,0.7),transparent); font-size:10px; color:white; font-weight:600;", "Connection")
          )
        )
      )
    )
  })
  
  # ──────────────────────────────────────────────
  # TAB 1: EXECUTIVE SUMMARY
  # ──────────────────────────────────────────────
  output$tab_overview <- renderUI({
    tagList(
      # Hero
      tags$div(class = "hero-banner",
               tags$div(style = "display:flex; align-items:center; gap:14px; margin-bottom:16px;",
                        tags$img(src = "occhiali_nobg.png", style = "height: 45px; object-fit: contain;", alt = "Occhiali"),
                        tags$div(
                          tags$h1(style = "font-size:28px; font-weight:900; color:white; margin:0; letter-spacing:-0.7px;", "Nuance Audio"),
                          tags$p(style = paste0("font-size:14px; color:", col$accent, "; margin:3px 0 0; font-weight:600;"), "AudioNova & EssilorLuxottica, Hearing Glasses")
                        )
               ),
               tags$p(style = "font-size:14px; color:var(--dim); line-height:1.75; margin:0; max-width:820px;", HTML(
                 "Nuance Audio sits at the intersection of <strong style='font-weight:800'>eyewear</strong>, <strong style='font-weight:800'>hearing aids</strong>, and <strong style='font-weight:800'>wearable technology</strong>. As the first FDA-cleared hearing glasses, it targets the <strong style='font-weight:800'>1.5 billion</strong> people worldwide affected by hearing loss, where the adoption rate remains at only 17%. This app synthesizes market intelligence, competitive analysis, and strategic frameworks to support decision-making."
               ))
      ),
      
      # Stats row
      tags$div(class = "row-stats",
               tags$div(stat_block("Market Size 2025", "$2.5B", "Smart glasses global", col$accent)),
               tags$div(stat_block("Projected 2030", "$29B", "AI smart glasses", col$blue)),
               tags$div(stat_block("Hearing Loss", "1.5B", "People affected", col$orange)),
               tags$div(stat_block("Adoption Rate", "17%", "Current HA usage", col$red)),
               tags$div(stat_block("Nuance Price", "$1,100", "\u00bc of traditional", col$purple))
      ),
      
      # Market growth chart
      section_hdr("Market Growth Trajectory", "Smart glasses market revenue projections (USD Billions), Sources: SAG, Grand View Research"),
      tags$div(class = "sci-card", plotlyOutput("plot_market_growth", height = "320px")),
      
      # Strengths / Risks
      fluidRow(
        column(6, info_card("Key Strengths", c(
          "First-mover in FDA-cleared hearing glasses (January 2025)",
          "EssilorLuxottica distribution: <strong style='font-weight:800'>18,000+ stores</strong> globally",
          "Price ~\u00bc of traditional hearing aids ($1,100 vs $4,000+)",
          "SaMD model enables OTA software updates",
          "Invisible design directly addresses stigma barrier"
        ), col$accent)),
        column(6, info_card("Key Risks", c(
          "AirPods Pro 2 at <strong style='font-weight:800'>$250</strong> with FDA-cleared hearing aid feature",
          "Meta could add hearing features to Ray-Ban Meta (same parent company)",
          "Chinese competitors (Cearvol, Elehear) at $250\u2013$400",
          "Uncertain consumer adoption, <strong style='font-weight:800'>83% coverage gap</strong> remains",
          "Regulatory complexity across US, EU, and Asia-Pacific"
        ), col$red))
      )
    )
  })
  
  output$plot_market_growth <- renderPlotly({
    plot_ly(df_market, x = ~Year) %>%
      add_trace(y = ~Actual, type = "scatter", mode = "lines+markers",
                name = "Actual", line = list(color = col$accent, width = 3),
                marker = list(color = col$accent, size = 8),
                fill = "tozeroy", fillcolor = paste0(col$accent, "15")) %>%
      add_trace(y = ~Forecast, type = "scatter", mode = "lines+markers",
                name = "Forecast", line = list(color = col$blue, width = 2, dash = "dash"),
                marker = list(color = col$blue, size = 8),
                fill = "tozeroy", fillcolor = paste0(col$blue, "10")) %>%
      plotly_layout_dark(
        yaxis = list(title = "USD Billions", gridcolor = col$border, zerolinecolor = col$border),
        xaxis = list(title = "", gridcolor = col$border)
      )
  })
  
  # ──────────────────────────────────────────────
  # TAB 2: PORTER'S FIVE FORCES
  # ──────────────────────────────────────────────
  output$tab_porter <- renderUI({
    tagList(
      section_hdr("Porter's Five Forces", "Strategic analysis of market competition and industry structure"),
      tags$div(
        style = "display:flex; align-items:center; gap:8px; margin-top:-10px; margin-bottom:16px;",
        tags$span(style = "font-size:10px; font-weight:700; text-transform:uppercase; letter-spacing:1.5px; color:var(--muted);",
                  "Point of View:"),
        tags$span(style = "padding:3px 10px; background:rgba(255,255,255,0.09); border:1px solid rgba(255,255,255,0.2); border-radius:12px; font-size:11px; font-weight:700; color:white;",
                  "Nuance Hearing")
      ),

      local({
        arr <- "font-size:24px; color:#94a3b8; display:flex; align-items:center; justify-content:center;"

        pbox <- function(title, level, color, icon_name, description) {
          tags$div(
            class = "porter-box",
            style = paste0("border-top:3px solid ", color, ";"),
            tags$div(class = "porter-icon",
                     style = paste0("color:", color, ";"),
                     tags$i(class = paste0("fas fa-", icon_name))),
            tags$h5(title),
            tags$div(class = "porter-level",
                     style = paste0("background:", color, "20; color:", color, "; border:1px solid ", color, "50;"),
                     level),
            tags$p(description)
          )
        }

        tags$div(
          style = "margin-bottom:18px;",
          tags$div(class = "porter-grid",

            tags$div(), tags$div(),
            pbox("Threat of New Entrants", "MEDIUM / HIGH", col$orange, "door-open",
                 "High R&D, FDA/CE barriers, IP moats. OTC deregulation (2022) partially lowers the bar."),
            tags$div(), tags$div(),

            tags$div(), tags$div(),
            tags$div(style = arr, tags$i(class = "fas fa-arrow-down")),
            tags$div(), tags$div(),

            pbox("Bargaining Power of Suppliers", "MEDIUM", "#eab308", "industry",
                 "Few MEMS & chipset vendors, but EssilorLuxottica's scale and vertical integration offset dependency."),
            tags$div(style = arr, tags$i(class = "fas fa-arrow-right")),
            pbox("Competitive Rivalry", "MEDIUM", "#eab308", "chess",
                 "No established brand loyalty. Competition on design, AI features, price, and optical distribution."),
            tags$div(style = arr, tags$i(class = "fas fa-arrow-left")),
            pbox("Bargaining Power of Buyers", "LOW / MEDIUM", "#16a34a", "users",
                 "High-end niche, few comparable bundles. OTC market still early; price sensitivity rising."),

            tags$div(), tags$div(),
            tags$div(style = arr, tags$i(class = "fas fa-arrow-up")),
            tags$div(), tags$div(),

            tags$div(), tags$div(),
            pbox("Threat of Substitutes", "HIGH", col$red, "shuffle",
                 "AirPods Pro 2 (~$250, FDA-cleared), traditional HA + glasses combo, OTC aids, captioning glasses."),
            tags$div(), tags$div()
          )
        )
      }),
      
      section_hdr("Threat of Substitutes", "Comparison by threat level, price range, and performance"),
      fluidRow(style = "margin-bottom:16px;",
        column(7, tags$div(class = "sci-card", style = "height:100%;", DTOutput("table_subs"))),
        column(5,
          tags$div(
            class = "sci-card",
            style = "height:100%; display:flex; flex-direction:column;",
            tags$div(
              style = paste0("font-size:10px; font-weight:800; letter-spacing:2px; text-transform:uppercase;",
                             " color:var(--muted); text-align:center; margin-bottom:8px;"),
              "Forces Intensity Radar"
            ),
            echarts4rOutput("plot_porter_radar", height = "280px")
          )
        )
      ),
      
      insight_box(
        "Key Strategic Insight",
        "The main substitution risk comes from products addressing hearing loss through entirely different form factors, especially <strong style='font-weight:800'>AirPods Pro 2 at ~$250</strong> leveraging Apple's massive installed base. Nuance Audio's competitive moat lies in its unique combination of <strong style='font-weight:800'>medical certification + invisible design + optical distribution network</strong>, a bundle no substitute currently replicates.",
        col$orange, "bolt"
      ),
      
      section_hdr("Force Details"),
      info_card("Threat of New Entrants, MEDIUM / HIGH", c(
        "<strong style='font-weight:800'>Barriers:</strong> High R&D + FDA/CE clearance, IP & beamforming know-how, EssilorLuxottica's 18K retail network",
        "<strong style='font-weight:800'>Enablers:</strong> FDA OTC deregulation (2022), tech giants with massive R&D, Chinese OTC brands at lower prices"
      ), col$orange),
      info_card("Bargaining Power of Suppliers, MEDIUM", c(
        "<strong style='font-weight:800'>Increasing:</strong> Specialized MEMS microphones and audio chipsets sourced from a limited pool of vendors",
        "<strong style='font-weight:800'>Decreasing:</strong> EssilorLuxottica's scale (\u20AC25B+ revenue) and vertical integration; Nuance Hearing IP acquisition (2023) reduces external dependency"
      ), "#eab308"),
      info_card("Competitive Rivalry, MEDIUM", c(
        "<strong style='font-weight:800'>No established brand loyalty</strong> in the emerging \"hearing glasses\" category, first-mover advantage still up for grabs",
        "Competition plays out on <strong style='font-weight:800'>design, AI features, price, and optical distribution</strong> rather than clinical performance alone",
        "Traditional HA incumbents (Sonova/Phonak, Demant/Oticon, WS Audiology) dominate clinical channels but have not yet entered the eyewear-hybrid segment"
      ), "#eab308"),
      info_card("Bargaining Power of Buyers, LOW / MEDIUM", c(
        "<strong style='font-weight:800'>High-end niche</strong>: few comparable bundles combining corrective eyewear + hearing amplification in one device",
        "<strong style='font-weight:800'>Optical distribution</strong> (LensCrafters, Pearle Vision) limits direct price-comparison shopping",
        "<strong style='font-weight:800'>Pressure rising</strong>: early-stage OTC market and $250 AirPods Pro 2 alternative are increasing price sensitivity"
      ), col$accent),
      info_card("Threat of Substitutes, HIGH", c(
        "<strong style='font-weight:800'>AirPods Pro 2</strong> (~$250, FDA-cleared hearing aid feature) leveraging Apple's massive installed base",
        "<strong style='font-weight:800'>Traditional hearing aids + glasses combo</strong> ($1K–$6K), superior audiological performance through clinical channels",
        "<strong style='font-weight:800'>Discreet OTC aids</strong> and <strong style='font-weight:800'>captioning glasses</strong> address the same need with different form factors"
      ), col$red)
    )
  })
  
  output$plot_porter_radar <- renderEcharts4r({
    df_porter %>%
      e_charts(Force) %>%
      e_radar(Score, max = 100, name = "Intensity") %>%
      e_radar_opts(shape = "circle",
                   splitArea = list(areaStyle = list(color = c("transparent"))),
                   axisLine  = list(lineStyle = list(color = "#cbd5e1")),
                   splitLine = list(lineStyle = list(color = "#e2e8f0")),
                   name = list(textStyle = list(color = "#1e293b", fontSize = 12))) %>%
      e_color(col$accent) %>%
      e_tooltip(show = FALSE) %>%
      e_legend(show = FALSE) %>%
      e_theme_custom('{"backgroundColor":"transparent","textStyle":{"color":"#1e293b"}}')
  })
  
  output$plot_porter_bar <- renderPlotly({
    colors <- c(col$orange, col$red, col$orange, col$accent, col$orange)
    plot_ly(df_porter, y = ~Force, x = ~Score, type = "bar", orientation = "h",
            marker = list(color = colors, cornerradius = 6),
            text = ~Level, textposition = "outside",
            textfont = list(size = 11, color = col$dim)) %>%
      plotly_layout_dark(
        xaxis = list(range = c(0, 100), title = "Intensity Score"),
        yaxis = list(title = "", categoryorder = "array", categoryarray = rev(df_porter$Force))
      )
  })
  
  output$table_subs <- renderDT({
    datatable(df_subs, options = list(dom = "t", pageLength = 10, ordering = FALSE),
              rownames = FALSE, class = "display") %>%
      formatStyle("Threat",
                  target = "row",
                  backgroundColor = styleEqual(
                    c("HIGH", "MEDIUM", "LOW-MED"),
                    c("#fee2e2", "#ffedd5", "#d1fae5")
                  )) %>%
      formatStyle("Threat",
                  fontWeight = "bold",
                  color = styleEqual(
                    c("HIGH", "MEDIUM", "LOW-MED"),
                    c(col$red, col$orange, col$accent)
                  ))
  })
  
  # ──────────────────────────────────────────────
  # TAB 3: VUCA
  # ──────────────────────────────────────────────
  vuca_sel <- reactiveVal("V")
  
  output$tab_vuca <- renderUI({
    # Dati per la sezione interattiva
    vuca_data <- list(
      V = list(
        title = "Volatility", sub = "Market & Technological Changes", color = col$red, pastel = "#ffe4e4",
        metrics = list(c("H1 2025 Sales Growth", "+200%"), c("Smart Glasses Market", "$2.5B")),
        factors = c(
          "Rapid tech cycles: AI integration shifting product expectations every 6 months.",
          "New entrants: emergence of 'captioning glasses' (CES 2026) as a new category.",
          "Price volatility: current price approximately <$1,000, reduced from the initial launch price to counter Chinese brands.",
          "Regulatory dynamism: OTC regulations evolving quickly across US, EU, and Asia-Pacific."
        )
      ),
      U = list(
        title = "Uncertainty", sub = "Future Industry Evolution", color = col$orange, pastel = "#fff3e0",
        metrics = list(c("Coverage Gap", "83%"), c("Planning Horizon", "12-24 Months")),
        factors = c(
          "Adoption uncertainty: will the 83% non-user gap close due to improved design?",
          "Big Tech moves: uncertainty regarding Apple or Meta integrating medical features.",
          "Technical standards: competition between computational audio and bone conduction.",
          "Regulatory harmonization: timeline for uniform OTC standards between US and EU."
        )
      ),
      C = list(
        title = "Complexity", sub = "Interconnected Market Dynamics", color = col$purple, pastel = "#ede9fe",
        metrics = list(c("Converging Industries", "4"), c("Retail Network", "18,000+ Stores")),
        factors = c(
          "Multi-industry convergence: Eyewear + Medical + Consumer Electronics + AI.",
          "Complex partner ecosystem: Nuance Hearing, AudioNova, Transitions, and retail channels.",
          "Critical Supply Chain: medical-grade audio chipsets, lenses, and MEMS microphones.",
          "Reimbursement models: navigating diverse health insurance systems across EU countries."
        )
      ),
      A = list(
        title = "Ambiguity", sub = "Undefined Industry Boundaries", color = col$cyan, pastel = "#e0f9fd",
        metrics = list(c("Product Category", "Hybrid"), c("Primary Target", "Consumer-Med")),
        factors = c(
          "Hybrid product: is it a hearing aid, fashion eyewear, or a smart wearable?",
          "Distribution channels: ambiguity between optical retail vs. audiological centers.",
          "Value perception: purchased as a lifestyle gadget or a necessary medical solution?",
          "Brand identity: EssilorLuxottica's transition toward a 'health-tech' positioning."
        )
      )
    )
    
    sel <- vuca_sel()
    v <- vuca_data[[sel]]
    
    tagList(
      section_hdr("VUCA Framework Analysis", "Strategic challenges for Nuance Audio in the hearing & smart eyewear market"),
      
      # --- 1. HOVER CARDS 2x2 ---
      tags$style(HTML("
        .vuca-hover-card { cursor: default; }
        .vuca-drop {
          max-height: 0;
          overflow: hidden;
          transition: max-height 0.4s ease, padding 0.3s ease;
          padding: 0 16px;
          border-top-width: 0 !important;
        }
        .vuca-hover-card:hover .vuca-drop {
          max-height: 400px;
          padding: 14px 16px;
          border-top-width: 2px !important;
        }
      ")),
      tags$div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-bottom: 30px;",
        
        # COMPLEXITY
        tags$div(class = "sci-card vuca-hover-card",
                 style = paste0("border: 2px solid ", col$purple, "; border-top: 4px solid ", col$purple,
                                "; margin-bottom:0; padding:0; background:#ede9fe; overflow:hidden;"),
                 tags$div(style = "padding:28px 20px 20px; text-align:center;",
                          tags$div(style = paste0("font-size:38px; color:", col$purple, "; margin-bottom:10px;"),
                                   tags$i(class = "fas fa-sitemap", `aria-hidden` = "true")),
                          tags$h5(style = paste0("color:", col$purple, "; font-weight:800; margin:0; font-size:15px;"),
                                  "COMPLEXITY")
                 ),
                 tags$div(class = "vuca-drop",
                          style = paste0("background:", col$purple, "18; border-top: 2px solid ", col$purple, ";"),
                          tags$p(style = "font-size:11px; color:#334155; line-height:1.5; margin:0;",
                                 tags$strong("Characteristics: "), "Convergence of Eyewear, Medical, Tech, and AI sectors with interconnected global supply chains."),
                          tags$p(style = "font-size:11px; color:#334155; line-height:1.5; margin:8px 0 0;",
                                 tags$strong("Example: "), "Managing medical-grade MEMS microphones and optical lenses while navigating diverse EU insurance policies."),
                          tags$p(style = "font-size:11px; color:#334155; line-height:1.5; margin:8px 0 0;",
                                 tags$strong("Approach: "), "Leverage EssilorLuxottica's vertical integration and specialized cross-functional teams.")
                 )
        ),
        
        # VOLATILITY
        tags$div(class = "sci-card vuca-hover-card",
                 style = paste0("border: 2px solid ", col$red, "; border-top: 4px solid ", col$red,
                                "; margin-bottom:0; padding:0; background:#ffe4e4; overflow:hidden;"),
                 tags$div(style = "padding:28px 20px 20px; text-align:center;",
                          tags$div(style = paste0("font-size:38px; color:", col$red, "; margin-bottom:10px;"),
                                   tags$i(class = "fas fa-bolt", `aria-hidden` = "true")),
                          tags$h5(style = paste0("color:", col$red, "; font-weight:800; margin:0; font-size:15px;"),
                                  "VOLATILITY")
                 ),
                 tags$div(class = "vuca-drop",
                          style = paste0("background:", col$red, "18; border-top: 2px solid ", col$red, ";"),
                          tags$p(style = "font-size:11px; color:#334155; line-height:1.5; margin:0;",
                                 tags$strong("Characteristics: "), "Unstable market duration with unexpected entry of low-cost Chinese competitors and rapid AI cycles."),
                          tags$p(style = "font-size:11px; color:#334155; line-height:1.5; margin:8px 0 0;",
                                 tags$strong("Example: "), "Smart glasses sales surged 200% in H1 2025, shifting consumer expectations almost overnight."),
                          tags$p(style = "font-size:11px; color:#334155; line-height:1.5; margin:8px 0 0;",
                                 tags$strong("Approach: "), "Build agility into the SaMD model to enable frequent over-the-air (OTA) software updates.")
                 )
        ),
        
        # AMBIGUITY
        tags$div(class = "sci-card vuca-hover-card",
                 style = paste0("border: 2px solid ", col$cyan, "; border-top: 4px solid ", col$cyan,
                                "; margin-bottom:0; padding:0; background:#e0f9fd; overflow:hidden;"),
                 tags$div(style = "padding:28px 20px 20px; text-align:center;",
                          tags$div(style = paste0("font-size:38px; color:", col$cyan, "; margin-bottom:10px;"),
                                   tags$i(class = "fas fa-question-circle", `aria-hidden` = "true")),
                          tags$h5(style = paste0("color:", col$cyan, "; font-weight:800; margin:0; font-size:15px;"),
                                  "AMBIGUITY")
                 ),
                 tags$div(class = "vuca-drop",
                          style = paste0("background:", col$cyan, "18; border-top: 2px solid ", col$cyan, ";"),
                          tags$p(style = "font-size:11px; color:#334155; line-height:1.5; margin:0;",
                                 tags$strong("Characteristics: "), "Causal relationships are completely unclear; the product defies traditional market categories."),
                          tags$p(style = "font-size:11px; color:#334155; line-height:1.5; margin:8px 0 0;",
                                 tags$strong("Example: "), "Determining if Nuance Audio should be benchmarked as a medical hearing aid or a fashion accessory."),
                          tags$p(style = "font-size:11px; color:#334155; line-height:1.5; margin:8px 0 0;",
                                 tags$strong("Approach: "), "Experiment with hybrid distribution channels (optical vs. audiological) and test consumer value perception.")
                 )
        ),
        
        # UNCERTAINTY
        tags$div(class = "sci-card vuca-hover-card",
                 style = paste0("border: 2px solid ", col$orange, "; border-top: 4px solid ", col$orange,
                                "; margin-bottom:0; padding:0; background:#fff3e0; overflow:hidden;"),
                 tags$div(style = "padding:16px 20px 20px; text-align:center;",
                          tags$div(style = paste0("display:inline-block; margin-bottom:8px; padding:3px 10px;",
                                                  " border-radius:12px; background:", col$orange, "25;",
                                                  " border:1px solid ", col$orange, "60; font-size:9px;",
                                                  " font-weight:700; color:", col$orange, ";",
                                                  " text-transform:uppercase; letter-spacing:1.5px;"),
                                   tags$i(class = "fas fa-crosshairs", style = "margin-right:3px;"),
                                   " Primary Focus"),
                          tags$div(style = paste0("font-size:38px; color:", col$orange, "; margin-bottom:10px;"),
                                   tags$i(class = "fas fa-cloud", `aria-hidden` = "true")),
                          tags$h5(style = paste0("color:", col$orange, "; font-weight:800; margin:0; font-size:15px;"),
                                  "UNCERTAINTY")
                 ),
                 tags$div(class = "vuca-drop",
                          style = paste0("background:", col$orange, "18; border-top: 2px solid ", col$orange, ";"),
                          tags$p(style = "font-size:11px; color:#334155; line-height:1.5; margin:0;",
                                 tags$strong("Characteristics: "), "Basic cause and effect are known, but lack of information on competitor moves introduces uncertainty into the future outlook."),
                          tags$p(style = "font-size:11px; color:#334155; line-height:1.5; margin:8px 0 0;",
                                 tags$strong("Example: "), "Potential entry of Apple or Meta into the medical-grade hearing space could disrupt the adoption curve."),
                          tags$p(style = "font-size:11px; color:#334155; line-height:1.5; margin:8px 0 0;",
                                 tags$strong("Approach: "), "Invest in competitive intelligence to monitor patent filings and emerging tech breakthroughs.")
                 )
        )
      ),
      
      # --- FOCUS SELECTION: UNCERTAINTY BRIDGE ---
      tags$div(
        class = "sci-card",
        style = paste0(
          "border-top:4px solid ", col$orange, "; background:#fff3e0;",
          " margin-bottom:24px;"
        ),
        tags$div(style = "display:flex; align-items:center; gap:12px; margin-bottom:14px;",
          tags$div(
            style = paste0("width:38px; height:38px; border-radius:10px; background:", col$orange, "30;",
                           " display:flex; align-items:center; justify-content:center; flex-shrink:0;"),
            tags$i(class = "fas fa-crosshairs", style = paste0("font-size:18px; color:", col$orange, ";"))
          ),
          tags$div(
            tags$div(style = paste0("font-size:10px; text-transform:uppercase; font-weight:700;",
                                    " letter-spacing:1.2px; color:", col$orange, "; margin-bottom:2px;"),
                     "VUCA Intelligence Focus"),
            tags$h5(style = "font-size:15px; font-weight:800; color:#1e293b; margin:0;",
                    "Why UNCERTAINTY became our primary analytical lens")
          )
        ),
        tags$p(style = "font-size:13px; color:#475569; line-height:1.6; margin-bottom:18px;",
               "Among the four VUCA dimensions, ", tags$strong("UNCERTAINTY"), " was selected as the driving lens for this intelligence project.",
               " Unlike complexity (structural) or ambiguity (categorical), uncertainty in Nuance Audio's environment stems from two ",
               tags$em("measurable, intelligence-actionable"), " forces: adoption decisions by millions of potential users, and strategic moves by Big Tech players.",
               " Both are knowable unknowns, exactly the type of signal that competitive intelligence can reduce and turn into strategic advantage."),
        # Flow row 1: Adoption Uncertainty → KIT 1 → KIQ 1 + KIQ 2
        tags$div(style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap; margin-bottom:10px;",
          tags$div(
            style = paste0("padding:6px 12px; border-radius:8px; background:", col$orange, "20;",
                           " border:1px solid ", col$orange, "50; font-size:11px; font-weight:700;",
                           " color:", pastel_text(col$orange), ";"),
            tags$i(class = "fas fa-users", style = "margin-right:5px;"), "Adoption Uncertainty (83% gap)"
          ),
          tags$i(class = "fas fa-arrow-right", style = paste0("color:", col$orange, "99; font-size:11px;")),
          tags$div(
            style = paste0("padding:5px 11px; border-radius:8px; background:", col$accent, "20;",
                           " border:1px solid ", col$accent, "50; font-size:10px; font-weight:700;",
                           " color:", pastel_text(col$accent), "; text-transform:uppercase;"),
            tags$i(class = "fas fa-layer-group", style = "margin-right:4px;"), "KIT 1: Strategic Decisions"
          ),
          tags$i(class = "fas fa-arrow-right", style = paste0("color:", col$orange, "99; font-size:11px;")),
          tags$div(style = "display:flex; gap:6px; flex-wrap:wrap;",
            tags$div(
              style = paste0("padding:4px 9px; border-radius:6px; background:", pastel_text(col$accent), "15;",
                             " border:1px solid ", pastel_text(col$accent), "40; font-size:10px; font-weight:700;",
                             " color:", pastel_text(col$accent), ";"),
              tags$i(class = "fas fa-circle-question", style = "margin-right:3px;"), "KIQ 1"
            ),
            tags$div(
              style = paste0("padding:4px 9px; border-radius:6px; background:", pastel_text(col$cyan), "15;",
                             " border:1px solid ", pastel_text(col$cyan), "40; font-size:10px; font-weight:700;",
                             " color:", pastel_text(col$cyan), ";"),
              tags$i(class = "fas fa-circle-question", style = "margin-right:3px;"), "KIQ 2"
            )
          )
        ),
        # Flow row 2: Big Tech Uncertainty → KIT 2 → KIQ 3
        tags$div(style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
          tags$div(
            style = paste0("padding:6px 12px; border-radius:8px; background:", col$orange, "20;",
                           " border:1px solid ", col$orange, "50; font-size:11px; font-weight:700;",
                           " color:", pastel_text(col$orange), ";"),
            tags$i(class = "fas fa-bolt", style = "margin-right:5px;"), "Big Tech Uncertainty (Apple, Meta)"
          ),
          tags$i(class = "fas fa-arrow-right", style = paste0("color:", col$orange, "99; font-size:11px;")),
          tags$div(
            style = paste0("padding:5px 11px; border-radius:8px; background:", col$orange, "20;",
                           " border:1px solid ", col$orange, "50; font-size:10px; font-weight:700;",
                           " color:", pastel_text(col$orange), "; text-transform:uppercase;"),
            tags$i(class = "fas fa-layer-group", style = "margin-right:4px;"), "KIT 2: Early Warnings"
          ),
          tags$i(class = "fas fa-arrow-right", style = paste0("color:", col$orange, "99; font-size:11px;")),
          tags$div(
            style = paste0("padding:4px 9px; border-radius:6px; background:", pastel_text(col$orange), "15;",
                           " border:1px solid ", pastel_text(col$orange), "40; font-size:10px; font-weight:700;",
                           " color:", pastel_text(col$orange), ";"),
            tags$i(class = "fas fa-circle-question", style = "margin-right:3px;"), "KIQ 3"
          )
        )
      ),

      # --- 2. INTERACTIVE BUTTONS ---
      tags$div(style = "display:flex; gap:10px; margin-bottom:20px;",
               lapply(names(vuca_data), function(letter) {
                 vd <- vuca_data[[letter]]
                 is_active <- (letter == sel)
                 tags$div(
                   style = paste0(
                     "flex:1; padding:15px; border-radius:12px; cursor:pointer; text-align:center; border:3px solid ",
                     if (is_active) paste0(vd$color, ";") else paste0(col$border, ";"),
                     " background:", if (is_active) vd$pastel else paste0(vd$pastel, "80"),
                     "; transition: all 0.2s;"
                   ),
                   onclick = paste0("Shiny.setInputValue('vuca_click', '", letter, "', {priority:'event'});"),
                   tags$div(style = paste0("font-size:24px; font-weight:900; color:", vd$color, ";"), letter)
                 )
               })
      ),
      
      # --- 3. DYNAMIC CONTENT (Details) ---
      tags$div(class = "sci-card", style = paste0("border-top:3px solid ", v$color, "; background:", v$pastel, ";"),
               tags$h4(style=paste0("color:", v$color, "; font-weight:800; margin-bottom:5px;"), v$title),
               tags$p(style="font-size:12px; color:#64748b; margin-bottom:20px;", v$sub),
               # Metrics
               tags$div(style = "display:flex; gap:14px; margin-bottom:20px; flex-wrap:wrap;",
                        lapply(v$metrics, function(m) {
                          tags$div(style = paste0("background:rgba(255,255,255,0.6); border:1px solid ", v$color, "40; border-radius:10px; padding:10px 15px;"),
                                   tags$div(style = "font-size:10px; color:#64748b; text-transform:uppercase;", m[1]),
                                   tags$div(style = paste0("font-size:18px; font-weight:800; color:", v$color, ";"), m[2])
                          )
                        })
               ),
               # Factors list
               tags$div(
                 lapply(seq_along(v$factors), function(i) {
                   tags$div(style = "display:flex; gap:12px; align-items:flex-start; margin-bottom:12px;",
                            tags$div(style = paste0("min-width:24px; height:24px; border-radius:6px; background:", v$color, "30; display:flex; align-items:center; justify-content:center; font-size:12px; color:", v$color, "; font-weight:700;"), i),
                            tags$p(style = "font-size:13px; color:#334155; line-height:1.6; margin:0;", v$factors[i])
                   )
                 })
               )
      ),
      
      # --- 4. STRATEGIC RECOMMENDATIONS ---
      section_hdr("Strategic Recommendations", "Turning VUCA challenges into competitive moats"),
      fluidRow(
        column(4, tags$div(style = paste0(
          "background:#d1fae5; border:1px solid ", col$accent, "50; border-top:3px solid ", col$accent, ";",
          "border-radius:14px; padding:24px; margin-bottom:18px; min-height:230px;",
          "transition:transform 0.22s, box-shadow 0.22s; cursor:default;"
        ),
          onmouseover = "this.style.transform='translateY(-4px)'; this.style.boxShadow='0 12px 32px rgba(0,229,160,0.18)';",
          onmouseout  = "this.style.transform=''; this.style.boxShadow='';",
          tags$div(style = paste0("width:35px; height:35px; border-radius:8px; background:", col$accent, "30; display:flex; align-items:center; justify-content:center; font-size:18px; font-weight:900; color:#065f46; margin-bottom:12px;"), "1"),
          tags$h4(style = "font-size:14px; font-weight:700; color:#065f46; margin:0 0 8px;", "Integrated Ecosystem"),
          tags$p(style = "font-size:12px; color:#065f4699; line-height:1.6;",
                 "Leverage the 18,000+ retail network to overcome access barriers. EssilorLuxottica's strength lies in vertical integration: frames + lenses + hearing technology in one purchase.")
        )),
        column(4, tags$div(style = paste0(
          "background:#dbeafe; border:1px solid ", col$blue, "50; border-top:3px solid ", col$blue, ";",
          "border-radius:14px; padding:24px; margin-bottom:18px; min-height:230px;",
          "transition:transform 0.22s, box-shadow 0.22s; cursor:default;"
        ),
          onmouseover = "this.style.transform='translateY(-4px)'; this.style.boxShadow='0 12px 32px rgba(79,172,254,0.18)';",
          onmouseout  = "this.style.transform=''; this.style.boxShadow='';",
          tags$div(style = paste0("width:35px; height:35px; border-radius:8px; background:", col$blue, "30; display:flex; align-items:center; justify-content:center; font-size:18px; font-weight:900; color:#1e3a8a; margin-bottom:12px;"), "2"),
          tags$h4(style = "font-size:14px; font-weight:700; color:#1e3a8a; margin:0 0 8px;", "Software & AI Flywheel"),
          tags$p(style = "font-size:12px; color:#1e3a8a99; line-height:1.6;",
                 "The SaMD model enables continuous OTA updates. Invest in AI-driven noise cancellation and real-time translation to distance the brand from low-cost Chinese competitors.")
        )),
        column(4, tags$div(style = paste0(
          "background:#ffedd5; border:1px solid ", col$orange, "50; border-top:3px solid ", col$orange, ";",
          "border-radius:14px; padding:24px; margin-bottom:18px; min-height:230px;",
          "transition:transform 0.22s, box-shadow 0.22s; cursor:default;"
        ),
          onmouseover = "this.style.transform='translateY(-4px)'; this.style.boxShadow='0 12px 32px rgba(255,159,67,0.18)';",
          onmouseout  = "this.style.transform=''; this.style.boxShadow='';",
          tags$div(style = paste0("width:35px; height:35px; border-radius:8px; background:", col$orange, "30; display:flex; align-items:center; justify-content:center; font-size:18px; font-weight:900; color:#9a3412; margin-bottom:12px;"), "3"),
          tags$h4(style = "font-size:14px; font-weight:700; color:#9a3412; margin:0 0 8px;", "Lifestyle Moats"),
          tags$p(style = "font-size:12px; color:#9a341299; line-height:1.6;",
                 "83% of those in need avoid support due to 'stigma'. Nuance Audio must position itself as a fashion accessory, turning medical necessity into technological desire.")
        ))
      )
    )
  })
  
  observeEvent(input$vuca_click, { vuca_sel(input$vuca_click) })
  
  # ──────────────────────────────────────────────
  # TAB 4: MARKET INTELLIGENCE
  # ──────────────────────────────────────────────
  output$tab_market <- renderUI({
    tagList(
      section_hdr("Market Intelligence", "Data from Counterpoint Research, Grand View Research, SAG \u2014 Synthesized from GenAI lab results"),
      
      tags$div(class = "row-stats",
               tags$div(stat_block("AI Smart Glasses 2026", "$5.6B", "4\u00d7 growth from 2025", col$accent)),
               tags$div(stat_block("Shipments 2026", "20M", "Units globally", col$blue)),
               tags$div(stat_block("Audio Segment", "28%+", "Largest by product type", col$purple)),
               tags$div(stat_block("CAGR Range", "12\u201324%", "Varies by scope", col$orange))
      ),
      
      fluidRow(
        column(6,
               tags$div(class = "sci-card",
                        tags$h4(style = "font-size:13px; color:var(--muted); text-transform:uppercase; letter-spacing:1.5px; margin:0 0 14px;", "Global Shipments (M Units)"),
                        plotlyOutput("plot_shipments", height = "280px")
               )
        ),
        column(6,
               tags$div(class = "sci-card",
                        tags$h4(style = "font-size:13px; color:var(--muted); text-transform:uppercase; letter-spacing:1.5px; margin:0 0 14px;", "Market by Segment (2025)"),
                        plotlyOutput("plot_segments", height = "280px")
               )
        )
      ),
      fluidRow(
        column(6,
               tags$div(class = "sci-card",
                        tags$h4(style = "font-size:13px; color:var(--muted); text-transform:uppercase; letter-spacing:1.5px; margin:0 0 14px;", "Regional Market Share"),
                        plotlyOutput("plot_regions", height = "280px")
               )
        ),
        column(6,
               tags$div(class = "sci-card",
                        tags$h4(style = "font-size:13px; color:var(--muted); text-transform:uppercase; letter-spacing:1.5px; margin:0 0 14px;", "Average Selling Price Trend"),
                        plotlyOutput("plot_asp", height = "280px")
               )
        )
      ),
      
      insight_box(
        "Data Divergence Warning",
        "Market estimates vary significantly across sources due to different scope definitions (smart glasses vs. smart glass vs. AI smart glasses). CAGR ranges from <strong style='font-weight:800'>12% to 24%</strong> and 2025 market size from <strong style='font-weight:800'>$2.5B to $18.4B</strong>. Europe's position also varies (32\u201343% share). All quantitative data should be verified against primary sources.",
        col$blue, "exclamation-triangle"
      )
    )
  })
  
  output$plot_shipments <- renderPlotly({
    plot_ly(df_ship, x = ~Year, y = ~Units, type = "bar",
            marker = list(color = col$accent, cornerradius = 6)) %>%
      plotly_layout_dark(yaxis = list(title = "Million Units"))
  })
  
  output$plot_segments <- renderPlotly({
    seg_colors <- c(col$accent, col$blue, col$purple, col$orange, col$muted)
    plot_ly(df_seg, labels = ~Segment, values = ~Share, type = "pie",
            hole = 0.5, marker = list(colors = seg_colors, line = list(width = 0)),
            textinfo = "label+percent", textfont = list(size = 11, color = "white")) %>%
      plotly_layout_dark(showlegend = FALSE)
  })
  
  output$plot_regions <- renderPlotly({
    reg_colors <- c(col$blue, col$accent, col$orange, col$muted)
    plot_ly(df_reg, y = ~Region, x = ~Share, type = "bar", orientation = "h",
            marker = list(color = reg_colors, cornerradius = 6),
            text = ~paste0(Share, "%"), textposition = "outside",
            textfont = list(size = 11, color = col$dim)) %>%
      plotly_layout_dark(xaxis = list(title = "% Share", range = c(0, 42)),
                         yaxis = list(title = ""))
  })
  
  output$plot_asp <- renderPlotly({
    plot_ly(df_asp, x = ~Year, y = ~ASP, type = "scatter", mode = "lines+markers",
            line = list(color = col$orange, width = 3),
            marker = list(color = col$orange, size = 9),
            fill = "tozeroy", fillcolor = paste0(col$orange, "12")) %>%
      plotly_layout_dark(yaxis = list(title = "USD", range = c(200, 500)))
  })
  
  # ──────────────────────────────────────────────
  # TAB 5: COMPETITORS
  # ──────────────────────────────────────────────
  output$tab_competitors <- renderUI({
    tagList(
      section_hdr("Competitive Map", "Key players in the hearing glasses / smart audio wearables ecosystem"),
      
      tags$div(class = "sci-card",
               tags$h4(style = "font-size:13px; color:var(--muted); text-transform:uppercase; letter-spacing:1.5px; margin:0 0 14px;", "AI Smart Glasses Market Share 2025"),
               plotlyOutput("plot_mshare", height = "320px")
      ),
      
      section_hdr("Competitor Comparison Matrix", "Product features and positioning comparison"),
      tags$div(class = "sci-card", DTOutput("table_comp")),
      
      section_hdr("Competitive Rivalry Map", "Direct, convergent, and adjacent competitors"),
      fluidRow(
        lapply(1:nrow(df_rivalry), function(i) {
          r <- df_rivalry[i,]
          tc <- switch(r$Type, Direct = col$red, Convergent = col$orange, Adjacent = col$blue, col$dim)
          column(6,
                 tags$div(class = "sci-card", style = paste0(
                   "border-left:4px solid ", tc, ";",
                   "background:", pastel_bg(tc), ";"
                 ),
                 tags$div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:6px;",
                          tags$span(style = paste0("font-size:14px; font-weight:700; color:", pastel_text(tc), ";"), r$Competitor),
                          sci_badge(r$Type, tc)
                 ),
                 tags$p(style = paste0("font-size:12px; color:", pastel_text(tc), "cc; margin:0; line-height:1.55;"), r$Description)
                 )
          )
        })
      )
    )
  })
  
  output$plot_mshare <- renderPlotly({
    ms_colors <- c(col$accent, col$blue, col$orange, col$red, col$purple, col$cyan)
    plot_ly(df_comp, labels = ~Product, values = ~MarketShare, type = "pie",
            hole = 0.45, marker = list(colors = ms_colors, line = list(width = 0)),
            textinfo = "label+percent", textfont = list(size = 11, color = "white")) %>%
      plotly_layout_dark(showlegend = TRUE, legend = list(x = 1.02, y = 0.5))
  })
  
  output$table_comp <- renderDT({
    df_show <- df_comp[, c("Product","Focus","Technology","Price","Target","Strength")]
    datatable(df_show, options = list(dom = "t", pageLength = 10, ordering = FALSE),
              rownames = FALSE, class = "display")
  })
  
  # ──────────────────────────────────────────────
  # TAB 6: SCOPE & KITs/KIQs
  # ──────────────────────────────────────────────
  kit_sel <- reactiveVal("KIT1")
  
  output$tab_scope <- renderUI({
    # 1. Nuovi dati aggiornati con le giustificazioni SMART
    kit_data <- list(
      KIT1 = list(title = "Strategic Decisions & Actions", color = col$accent,
                  pastel_hi = "#d1fae5", pastel_lo = "#ecfdf5", title_dark = "#065f46",
                  desc = "Nuance Audio's positioning and market penetration strategy in the OTC hearing glasses segment in the USA and Europe.",
                  kiqs = list(
                    list(type = "Polar", q = HTML("Do consumer acceptance and stigma-related topics (T3: Stigma & Acceptance + T6: Eyewear UX) show a growing trend in the hearing-glasses literature between 2020 and 2025, confirming adoption barriers as the primary strategic challenge for<br>Nuance Audio?"),
                         indicators = "BERTopic topic volume/year; Scopus corpus 2018–2025; Topics over time chart", analysis = "Descriptive",
                         smart = list(S="Topics T3 and T6 of Scopus corpus", M="Papers per topic per year", A="Corpus already loaded in the app", R="Consumer adoption is the core of KIT 1", T="2020–2025")),
                    list(type = "Alternative", q = HTML("Should distribution prioritize proprietary optical channels (LensCrafters) or independent audiology channels<br>for EU penetration?"),
                         indicators = "Store count, % coverage; annual reports; geospatial analysis", analysis = "Exploratory",
                         smart = list(S="Two defined channels", M="Sales volume per channel", A="Distribution data", R="Go-to-market strategy", T="EU launch 2025")),
                    list(type = "Q-word", q = "How is Nuance Audio perceived by adults 50+ regarding stigma, comfort, and usefulness vs. traditional hearing aids within 6 months of US launch?",
                         indicators = "Sentiment score, keyword freq; social media, forums; NLP", analysis = "Descriptive + Exploratory",
                         smart = list(S="Defined target, defined variables", M="Sentiment analysis, surveys", A="Social data, reviews", R="Adoption", T="6 months from launch"))
                  )),
      KIT2 = list(title = "Early Warnings", color = col$orange,
                  pastel_hi = "#ffedd5", pastel_lo = "#fff7ed", title_dark = "#9a3412",
                  desc = "Emerging technological and competitive threats that could erode Nuance Audio's first-mover advantage over the next 12–24 months.",
                  kiqs = list(
                    list(type = "Polar", q = HTML("Has Apple filed patents related to hearing aid functionality integrated into eyewear devices in the last<br>24 months?"),
                         indicators = "Patent count, IPC class; Google Patents, USPTO", analysis = "Descriptive + Predictive",
                         smart = list(S="Apple, eyewear patents", M="Yes/no with count", A="Public patent databases", R="Threat assessment", T="24 months")),
                    list(type = "Alternative", q = HTML("Most imminent threat: big tech entry (Apple, Google) or traditional hearing aid manufacturers (Phonak,<br>Oticon) with eyewear solutions?"),
                         indicators = "Signals per category; press releases, job postings, M&A", analysis = "Exploratory",
                         smart = list(S="Two competitor categories", M="Patent activity, partnerships, announcements", A="OSINT data", R="Early warning", T="12-24 months", m_wide = TRUE)),
                    list(type = "Q-word", q = HTML("What emerging technologies (bone conduction, AI audio, biometric sensors) are competitors developing that<br>could offer superior advantage by 2027?"),
                         indicators = "Publication/patent frequency; Scopus, patents, tech media", analysis = "Descriptive + Predictive",
                         smart = list(S="Defined technologies, competitors", M="Patents, papers, product announcements", A="Public sources", R="Technological disruption", T="By 2027", m_wide = TRUE))
                  )),
      KIT3 = list(title = "Key Players & Positioning", color = col$blue,
                  pastel_hi = "#dbeafe", pastel_lo = "#eff6ff", title_dark = "#1e3a8a",
                  desc = "Map and positioning of key players in the hearing glasses / smart audio wearables ecosystem.",
                  kiqs = list(
                    list(type = "Polar", q = HTML("Have Sonova and Demant announced partnerships with eyewear companies to develop hearing glasses by<br>April 2026?"),
                         indicators = "Yes/no + details; investor relations, industry press", analysis = "Descriptive",
                         smart = list(S="Specific variables", M="Measurable metrics", A="Achievable data", R="Relevant partnerships", T="April 2026")),
                    list(type = "Alternative", q = "In smart audio glasses, who holds larger share as of Q4 2025: Ray-Ban Meta or Nuance Audio?",
                         indicators = "Estimated units; analyst reports (IDC, Counterpoint)", analysis = "Descriptive + Inferential",
                         smart = list(S="Direct comparison", M="Estimated units sold", A="Analyst reports", R="Positioning", T="Q4 2025")),
                    list(type = "Q-word", q = "Who are emerging startups in hearing glasses/audio wearables with >$5M funding in the last 18 months?",
                         indicators = "Startup count, funding; Crunchbase, PitchBook", analysis = "Exploratory + Predictive",
                         smart = list(S="Startups, funding threshold", M="Crunchbase/PitchBook data", A="Achievable public data", R="Relevant competition", T="18 months"))
                  ))
    )
    
    sel <- kit_sel()
    k <- kit_data[[sel]]
    
    # 2. Funzione di appoggio per creare lo "schettino" SMART
    smart_ui <- function(s, m, a, r, t_val, m_class = "tooltip-m") {
      tagList(
        tags$style(HTML("
          .tooltip-s { --bs-tooltip-bg: #fde047; --bs-tooltip-color: #000; font-weight: 600; }
          .tooltip-m, .tooltip-m-wide { --bs-tooltip-bg: #7dd3fc; --bs-tooltip-color: #000; font-weight: 600; }
          .tooltip-a { --bs-tooltip-bg: #fb923c; --bs-tooltip-color: #000; font-weight: 600; }
          .tooltip-r { --bs-tooltip-bg: #86efac; --bs-tooltip-color: #000; font-weight: 600; }
          .tooltip-t { --bs-tooltip-bg: #f472b6; --bs-tooltip-color: #000; font-weight: 600; }
          
          .tooltip-s .tooltip-inner,
          .tooltip-m .tooltip-inner,
          .tooltip-a .tooltip-inner,
          .tooltip-r .tooltip-inner,
          .tooltip-t .tooltip-inner {
            white-space: normal !important;
            max-width: 110px !important;
            text-align: center;
            line-height: 1.4;
          }
          
          /* CLASSE SPECIALE MANUALE PER M PIÙ LARGA E SPOSTATA A SINISTRA */
          .tooltip-m-wide .tooltip-inner {
            white-space: normal !important;
            max-width: 160px !important; /* Molto più larga */
            text-align: center;
            line-height: 1.4;
            transform: translateX(-35px); /* Spinta a sinistra per sfruttare lo spazio vuoto */
          }
          
          .tooltip-s .tooltip-inner { transform: translateX(-15px); }
          .tooltip-m .tooltip-inner { transform: translateX(-15px); }
          .tooltip-a .tooltip-inner { transform: translateX(-15px); }
          .tooltip-r .tooltip-inner { transform: translateX(-15px); }
          .tooltip-t .tooltip-inner { transform: translateX(-25px); } 

          .smart-staircase { display: flex; align-items: flex-end; margin-left: auto; padding-top: 15px; }
          .smart-step { display: flex; flex-direction: column; align-items: center; width: 62px; }

          .smart-letter {
            width: 24px; height: 24px; display: flex; align-items: center; justify-content: center;
            color: #000; font-weight: 900; font-size: 14px; box-shadow: 2px 2px 5px rgba(0,0,0,0.3);
            cursor: pointer; border-radius: 1px; font-family: 'Outfit', sans-serif;
          }

          .post-it-s { background: #fde047; }
          .post-it-m { background: #7dd3fc; }
          .post-it-a { background: #fb923c; }
          .post-it-r { background: #86efac; }
          .post-it-t { background: #f472b6; }

          .smart-hline { position: relative; width: 100%; height: 2px; background-color: #94a3b8; margin: 6px 0 3px 0; }
          .smart-riser { position: absolute; left: 0; top: 0; width: 2px; height: 14px; background-color: #94a3b8; }
          .smart-label { font-size: 7px; font-weight: 700; color: #334155; text-transform: uppercase; letter-spacing: 0.3px; }
        ")),
        
        tags$div(class = "smart-staircase",
                 tags$div(class = "smart-step", style = "margin-bottom: 0px;",
                          tooltip(tags$div(class = "smart-letter post-it-s", "S"), paste("Specific:", s), placement = "top", options = list(customClass = "tooltip-s")),
                          tags$div(class = "smart-hline"),
                          tags$div(class = "smart-label", "Specific")
                 ),
                 tags$div(class = "smart-step", style = "margin-bottom: 12px;",
                          # QUI USIAMO LA VARIABILE m_class CHE DECIDE QUALE STILE APPLICARE
                          tooltip(tags$div(class = "smart-letter post-it-m", "M"), paste("Measurable:", m), placement = "top", options = list(customClass = m_class)),
                          tags$div(class = "smart-hline", tags$div(class = "smart-riser")),
                          tags$div(class = "smart-label", "Measurable")
                 ),
                 tags$div(class = "smart-step", style = "margin-bottom: 24px;",
                          tooltip(tags$div(class = "smart-letter post-it-a", "A"), paste("Achievable:", a), placement = "top", options = list(customClass = "tooltip-a")),
                          tags$div(class = "smart-hline", tags$div(class = "smart-riser")),
                          tags$div(class = "smart-label", "Achievable")
                 ),
                 tags$div(class = "smart-step", style = "margin-bottom: 36px;",
                          tooltip(tags$div(class = "smart-letter post-it-r", "R"), paste("Relevant:", r), placement = "top", options = list(customClass = "tooltip-r")),
                          tags$div(class = "smart-hline", tags$div(class = "smart-riser")),
                          tags$div(class = "smart-label", "Relevant")
                 ),
                 tags$div(class = "smart-step", style = "margin-bottom: 48px;",
                          tooltip(tags$div(class = "smart-letter post-it-t", "T"), paste("Timely:", t_val), placement = "top", options = list(customClass = "tooltip-t")),
                          tags$div(class = "smart-hline", tags$div(class = "smart-riser")),
                          tags$div(class = "smart-label", "Timely")
                 )
        )
      )
    }
    
    tagList(
      section_hdr("Scope, KITs & KIQs", "From strategic direction to operationalized intelligence questions"),
      
      # Matrice di Rumsfeld (Design Celeste 2x2)
      tags$div(
        style = "background-color: #e8f4f8; padding: 24px; border-radius: 12px; margin-bottom: 24px; color: #080c14; box-shadow: inset 0 0 10px rgba(0,0,0,0.05);",
        tags$div(
          style = "display: grid; grid-template-columns: 160px 1fr 1fr; grid-gap: 15px; align-items: stretch;",
          
          # Intestazioni di colonna
          tags$div(), # Cella vuota in alto a sinistra
          tags$div(style = "text-align: center; font-weight: 800; font-size: 18px; color: #080c14;", "...that we know"),
          tags$div(style = "text-align: center; font-weight: 800; font-size: 18px; color: #080c14;", "...that we don't know"),
          
          # Riga 1 (What we know...)
          tags$div(style = "text-align: right; font-weight: 800; font-size: 18px; color: #080c14; display: flex; align-items: center; justify-content: flex-end; padding-right: 15px;", "What we know..."),
          
          # Quadrante 1 — Known-Knowns
          tags$div(class = "rumsfeld-cell",
            tags$div(class = "rumsfeld-cell-header",
              tags$i(class = "fas fa-circle-check rumsfeld-cell-icon"),
              tags$div(
                tags$strong(style = "color:#0f1724; font-size:14px;", "Q1: Known-Knowns"),
                tags$span(style = "color:#556378; font-size:11px; display:block; margin-top:2px;", "What we know we know")
              )
            ),
            tags$div(class = "rumsfeld-drop",
              tags$hr(style = "border:none; border-top:1px solid #cbd5e1; margin:0 0 10px;"),
              tags$p(style = "margin:0; color:#334155; font-size:12.5px; line-height:1.6;",
                "Nuance Audio is an EssilorLuxottica product born from the 2023 acquisition of Nuance Hearing. It combines corrective eyewear and open-ear hearing amplification in a single OTC device for adults 18+ with mild-to-moderate hearing loss. Price: ~$1,200. FDA-approved (US) and CE-marked (EU). US launch: April 2025 via LensCrafters, Target Optical, Pearle Vision. EU launch: starting from Italy in Q1 2025, then France, Germany, UK. Direct competitors: Ray-Ban Meta, AirPods Pro 2 (hearing aid feature), Eargo, Jabra Enhance.")
            )
          ),

          # Quadrante 3 — Known-Unknowns
          tags$div(class = "rumsfeld-cell",
            tags$div(class = "rumsfeld-cell-header",
              tags$i(class = "fas fa-magnifying-glass rumsfeld-cell-icon"),
              tags$div(
                tags$strong(style = "color:#0f1724; font-size:14px;", "Q3: Known-Unknowns"),
                tags$span(style = "color:#556378; font-size:11px; display:block; margin-top:2px;", "What we know we don't know")
              )
            ),
            tags$div(class = "rumsfeld-drop",
              tags$hr(style = "border:none; border-top:1px solid #cbd5e1; margin:0 0 10px;"),
              tags$p(style = "margin:0; color:#334155; font-size:12.5px; line-height:1.6;",
                "What will the actual adoption rate of Nuance Audio be vs. traditional hearing aids? How will traditional hearing aid manufacturers (Phonak, Oticon, Starkey) react to this disruption? What is the target consumer perception regarding the stigma of an 'invisible' hearing aid vs. a traditional one? How will the OTC regulatory framework evolve in Europe vs. the US? What is the actual clinical effectiveness compared to traditional hearing aids?")
            )
          ),

          # Riga 2 (What we don't know...)
          tags$div(style = "text-align:right; font-weight:800; font-size:18px; color:#080c14; display:flex; align-items:center; justify-content:flex-end; padding-right:15px;", "What we don't know..."),

          # Quadrante 2 — Unknown-Knowns
          tags$div(class = "rumsfeld-cell",
            tags$div(class = "rumsfeld-cell-header",
              tags$i(class = "fas fa-eye-slash rumsfeld-cell-icon"),
              tags$div(
                tags$strong(style = "color:#0f1724; font-size:14px;", "Q2: Unknown-Knowns"),
                tags$span(style = "color:#556378; font-size:11px; display:block; margin-top:2px;", "Latent knowledge")
              )
            ),
            tags$div(class = "rumsfeld-drop",
              tags$hr(style = "border:none; border-top:1px solid #cbd5e1; margin:0 0 10px;"),
              tags$p(style = "margin:0; color:#334155; font-size:12.5px; line-height:1.6;",
                "Literature identifies key wearable adoption factors (perceived usefulness, ease of use, stigma, comfort, price, privacy) per Kalantari 2017 and Koutromanos & Kazakou 2023, but these have not been specifically applied to Nuance Audio. The OTC hearing aid market was deregulated by the FDA in 2022, opening significant competitive space. EssilorLuxottica's dominant eyewear position (Ray-Ban, Oakley, LensCrafters) could provide enormous distribution advantages.")
            )
          ),

          # Quadrante 4 — Unknown-Unknowns
          tags$div(class = "rumsfeld-cell",
            tags$div(class = "rumsfeld-cell-header",
              tags$i(class = "fas fa-circle-question rumsfeld-cell-icon"),
              tags$div(
                tags$strong(style = "color:#0f1724; font-size:14px;", "Q4: Unknown-Unknowns"),
                tags$span(style = "color:#556378; font-size:11px; display:block; margin-top:2px;", "What we don't know we don't know")
              )
            ),
            tags$div(class = "rumsfeld-drop",
              tags$hr(style = "border:none; border-top:1px solid #cbd5e1; margin:0 0 10px;"),
              tags$p(style = "margin:0; color:#334155; font-size:12.5px; line-height:1.6;",
                "New technologies (e.g., advanced bone conduction, neural implants) could make the open-ear approach obsolete. An unexpected competitor (e.g., Apple, Google, Samsung) could enter the hearing glasses segment with stronger ecosystem integration. Potential long-term side effects of prolonged unmonitored open-ear amplification. Cultural shifts that fully normalize hearing aids, eliminating the competitive advantage of invisibility.")
            )
          )
        )
      ),
      
      # 2. IL NUOVO SCOPE STATEMENT
      tags$div(
        class = "scope-statement-box",
        style = "background-color: #ffffff; padding: 24px; border-radius: 12px; margin-bottom: 24px; border: 1px solid #e2e8f0; box-shadow: 0 2px 8px rgba(0,0,0,0.06);",
        tags$h4(style = "font-size:16px; font-weight:800; color: #1e293b; margin:0 0 10px; display:flex; align-items:center; gap:6px;",
                "Scope Statement"),
        tags$p(style = "font-size:14px; color: #475569; line-height:1.75; margin:0; font-style:italic;",
               "“To what extent is EssilorLuxottica’s Nuance Audio capable of creating a new market category (‘hearing glasses’) that overcomes traditional hearing aid adoption barriers, and which competitive, technological, and regulatory factors will determine its success in the European and American markets over the next 12–24 months?”"
        )
      ),
      
      # KIT selector
      tags$div(style = "display:flex; gap:10px; margin-bottom:20px;",
               lapply(names(kit_data), function(kn) {
                 kd <- kit_data[[kn]]
                 is_active <- (kn == sel)
                 tags$div(
                   class = "kit-card",
                   style = paste0(
                     "flex:1; padding:14px 18px; border-radius:12px; cursor:pointer;",
                     "border:2px solid ", if (is_active) kd$color else paste0(kd$color, "60"), ";",
                     "background:", if (is_active) kd$pastel_hi else kd$pastel_lo, ";",
                     "transition:all 0.2s;"
                   ),
                   onclick = paste0("Shiny.setInputValue('kit_click', '", kn, "', {priority:'event'});"),
                   tags$div(style = paste0("font-size:11px; font-weight:700; color:", kd$color, "; letter-spacing:1px;"),
                            gsub("KIT","KIT ", kn)),
                   tags$div(style = paste0("font-size:13px; font-weight:600; color:", kd$title_dark, "; margin-top:4px;"),
                            kd$title)
                 )
               })
      ),
      
      # KIQ content
      tags$div(
        lapply(k$kiqs, function(kiq) {
                   
                   # Riquadro principale
                   tags$div(class = "kiq-card",
                            style = paste0(
                     "background:", k$pastel_lo, ";",
                     "border-radius:10px; padding:18px;",
                     "border:1px solid ", k$color, "40;",
                     "margin-bottom:12px; display:flex; align-items:center; justify-content:space-between; gap:20px;"
                   ),
                            # --- COLONNA SINISTRA ---
                            tags$div(style = "flex:1;",
                                     tags$div(style = "display:flex; gap:8px; margin-bottom:12px;",
                                              sci_badge(kiq$type, k$color),
                                              sci_badge(kiq$analysis, k$title_dark)
                                     ),
                                     tags$p(style = paste0("font-size:13px; color:", k$title_dark, "; margin:0 0 8px; font-weight:600; line-height:1.55;"), kiq$q),
                                     tags$div(style = paste0("font-size:11px; color:", k$title_dark, "99;"),
                                              tags$strong(style = paste0("color:", k$title_dark, ";"), "Indicators: "), kiq$indicators
                                     )
                            ),
                            # --- COLONNA DESTRA: SMART ---
                            tags$div(style = "flex-shrink:0;",
                                     smart_ui(kiq$smart$S, kiq$smart$M, kiq$smart$A, kiq$smart$R, kiq$smart$T,
                                              if(isTRUE(kiq$smart$m_wide)) "tooltip-m-wide" else "tooltip-m")
                            )
                   )
        })
      )
    )
  })

  observeEvent(input$kit_click, { kit_sel(input$kit_click) })
  
  # ──────────────────────────────────────────────
  # TAB 7: QUERY DESIGN
  # ──────────────────────────────────────────────
  output$tab_queries <- renderUI({

    queries <- list(
      list(id = "Q1", title = "General Technological Landscape", scope = "Broad", color = col$accent,
           pastel = "#d1fae5", dark = "#065f46",
           query = 'TITLE-ABS-KEY("smart glasses" OR "audio glasses" OR "hearing glasses")\nAND TITLE-ABS-KEY("wearable technology" OR "smart eyewear")',
           results = "Nuance Audio, Vue Lite 2, XanderGlasses, Ray-Ban Meta. Market data: $2.46B \u2192 $14.38B (CAGR 24.2%)"),
      list(id = "Q2", title = "Hearing & Assistive Technology", scope = "Narrow", color = col$blue,
           pastel = "#dbeafe", dark = "#1e3a8a",
           query = 'TITLE-ABS-KEY("smart glasses" OR "hearing glasses" OR "audio glasses")\nAND TITLE-ABS-KEY("hearing aid" OR "augmented hearing" OR "bone conduction"\n  OR "assistive listening")',
           results = "Bone conduction solutions, InterHearing. Key insight: Nuance uses open-ear + beamforming (not bone conduction) \u2014 important differentiator"),
      list(id = "Q3", title = "Consumer Adoption", scope = "Strategic", color = col$purple,
           pastel = "#ede9fe", dark = "#4c1d95",
           query = 'TITLE-ABS-KEY("smart glasses" OR "wearable technology")\nAND TITLE-ABS-KEY("consumer adoption" OR "technology acceptance"\n  OR "user experience")\nAND TITLE-ABS-KEY("eyewear" OR "hearing")',
           results = "IDC: Ray-Ban Meta 900K+ sales Q4 2024; 1.5B affected by hearing loss; design, comfort, usefulness as key TAM/UTAUT factors"),
      list(id = "Q4", title = "Market & Competition", scope = "Quantitative", color = col$orange,
           pastel = "#ffedd5", dark = "#9a3412",
           query = 'TITLE-ABS-KEY("smart glasses" OR "smart eyewear" OR "audio glasses")\nAND TITLE-ABS-KEY("market" OR "competitive landscape"\n  OR "industry analysis")\nAND NOT TITLE-ABS-KEY("virtual reality headset" OR "VR headset")',
           results = "Audio >28% share; Meta 73\u201382% share; ASP declining $450 \u2192 $250 (2024\u20132028); 45M units by 2028"),
      list(id = "Q5", title = "Innovation & CES 2026", scope = "Competitor", color = col$red,
           pastel = "#fee2e2", dark = "#7f1d1d",
           query = 'TITLE-ABS-KEY("hearing glasses" OR "audio glasses" OR "smart glasses")\nAND TITLE-ABS-KEY("digital transformation" OR "emerging technology"\n  OR "innovation")\nAND TITLE-ABS-KEY("eyewear" OR "hearing")',
           results = "CES 2026: Cearvol Lyra (NeuroFlow AI 2.0), Alango (zero-latency). Ray-Ban Meta 'Conversation Focus'. Nuance SNR improvement 4.4 dB")
    )
    
    tagList(
      tags$script(HTML("
        function showQuery(id) {
          var ids = ['q1','q2','q3','q4','q5'];
          ids.forEach(function(qid) {
            var el = document.getElementById('query-detail-' + qid);
            if (!el) return;
            el.style.display = (qid === id) ? 'block' : 'none';
          });
        }
        function hideQuery(id) {
          var el = document.getElementById('query-detail-' + id);
          if (el && !el.dataset.pinned) el.style.display = 'none';
        }
        function toggleQuery(id) {
          var el = document.getElementById('query-detail-' + id);
          if (!el) return;
          if (el.dataset.pinned) {
            el.dataset.pinned = '';
            el.style.display = 'none';
          } else {
            el.dataset.pinned = '1';
            showQuery(id);
            el.scrollIntoView({behavior:'smooth', block:'nearest'});
          }
        }
      ")),

      section_hdr("Query Design", "Systematic query design for Scopus and web search \u2014 from broad landscape to specific competitive intelligence"),

      # Query overview grid \u2014 cliccabile
      tags$div(style = "display:flex; gap:10px; margin-bottom:16px;",
               lapply(queries, function(q) {
                 tags$div(
                   id = paste0("query-btn-", tolower(q$id)),
                   style = paste0(
                     "flex:1; background:", q$pastel, ";",
                     "border:1px solid ", q$color, "50;",
                     "border-radius:12px; padding:16px; text-align:center;",
                     "border-top:3px solid ", q$color, ";",
                     "cursor:pointer; transition:transform 0.18s, box-shadow 0.18s;"
                   ),
                   onmouseover = paste0("this.style.transform='translateY(-3px)'; this.style.boxShadow='0 6px 18px rgba(0,0,0,0.14)'; showQuery('", tolower(q$id), "');"),
                   onmouseout  = paste0("this.style.transform=''; this.style.boxShadow=''; hideQuery('", tolower(q$id), "');"),
                   onclick = paste0("toggleQuery('", tolower(q$id), "');"),
                   tags$div(style = paste0("font-size:22px; font-weight:800; color:", q$color, "; font-family:'JetBrains Mono',monospace;"), q$id),
                   tags$div(style = paste0("font-size:11px; color:", q$dark, "; font-weight:600; margin-top:4px;"), q$title),
                   tags$div(style = "margin-top:6px;", sci_badge(q$scope, q$color))
                 )
               })
      ),

      # Query details (nascosti di default)
      lapply(queries, function(q) {
        tags$div(
          id = paste0("query-detail-", tolower(q$id)),
          style = paste0(
            "display:none;",
            "background:", q$pastel, ";",
            "border:1px solid ", q$color, "40;",
            "border-left:4px solid ", q$color, ";",
            "border-radius:14px; padding:24px; margin-bottom:12px;",
            "scroll-margin-top:12px;"
          ),
          tags$div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:12px;",
                   tags$div(style = "display:flex; gap:10px; align-items:center;",
                            tags$span(style = paste0("font-size:18px; font-weight:800; color:", q$color, "; font-family:'JetBrains Mono',monospace;"), q$id),
                            tags$span(style = paste0("font-size:14px; font-weight:600; color:", q$dark, ";"), q$title)
                   ),
                   sci_badge(paste(q$scope, "\u2714"), q$color)
          ),
          tags$pre(class = "query-code-light", hl_query(q$query)),
          tags$div(style = paste0("font-size:12px; color:", q$dark, "cc; line-height:1.65; margin-top:12px;"),
                   tags$strong(style = paste0("color:", q$dark, ";"), "Key Results: "), q$results
          )
        )
      }),

      # Recommended final query — layout strutturato compatto
      local({
        op_badge <- function(label, color) {
          tags$div(
            style = paste0(
              "display:inline-flex; align-items:center; margin:3px 0;",
              "font-size:9px; font-weight:800; letter-spacing:1px;",
              "color:", color, "; font-family:'JetBrains Mono',monospace;"
            ),
            tags$i(class="fas fa-arrow-down", style=paste0("margin-right:5px; font-size:8px; color:", color, "70;")),
            label
          )
        }

        term_chip <- function(term, color) {
          tags$span(
            style = paste0(
              "display:inline-block; padding:2px 8px; border-radius:20px; margin:2px 2px 2px 0;",
              "font-size:10.5px; font-family:'JetBrains Mono',monospace;",
              "background:", color, "15; color:", color, "; border:1px solid ", color, "35;"
            ),
            paste0('"', term, '"')
          )
        }

        fn_label <- function(color) {
          tags$span(
            style = paste0(
              "font-size:9px; font-weight:700; color:", color, "80;",
              "font-family:'JetBrains Mono',monospace; margin-right:8px; letter-spacing:0.5px; flex-shrink:0;"
            ),
            "TITLE-ABS-KEY"
          )
        }

        query_row <- function(terms, color) {
          tags$div(
            style = paste0(
              "display:flex; align-items:center; flex-wrap:wrap;",
              "background:", color, "0a; border:1px solid ", color, "22;",
              "border-radius:8px; padding:6px 12px; margin:2px 0;"
            ),
            fn_label(color),
            lapply(terms, function(t) term_chip(t, color))
          )
        }

        tags$div(
          style = "background:#ffffff; border:1px solid #e2e8f0; border-left:3px solid #334155; border-radius:14px; padding:16px 20px; margin-bottom:18px;",
          tags$div(
            style = "display:flex; align-items:center; gap:8px; margin-bottom:12px;",
            tags$i(class="fas fa-bookmark", style="color:#334155; font-size:12px;"),
            tags$span(style="font-size:13px; font-weight:700; color:#0f1724;",
                      "Recommended Final Query for Scopus")
          ),
          query_row(c("smart glasses", "hearing glasses", "audio glasses", "smart eyewear", "hearing aid glasses"), "#334155"),
          op_badge("AND", "#d97706"),
          query_row(c("hearing", "audio", "assistive", "wearable"), "#334155"),
          op_badge("AND NOT", "#dc2626"),
          query_row(c("virtual reality headset"), "#334155"),
          op_badge("AND", "#d97706"),
          tags$div(
            style = "display:inline-flex; align-items:center; gap:8px; background:#33415510; border:1px solid #33415530; border-radius:8px; padding:6px 12px; margin:2px 0;",
            tags$span(style="font-size:9px; font-weight:700; color:#33415580; font-family:'JetBrains Mono',monospace; letter-spacing:0.5px;", "PUBYEAR"),
            tags$span(style="font-size:12px; color:#334155; font-family:'JetBrains Mono',monospace; font-weight:700;", "> 2017")
          )
        )
      })
    )
  })
  
  # ──────────────────────────────────────────────
  # TAB 8: GENAI FOR CI
  # ──────────────────────────────────────────────
  output$tab_genai <- renderUI({

    # Dati dal tuo schema per Nuance Audio
    # Dati dal tuo schema per Nuance Audio (Translated to English)
    create_data <- list(
      C = list(letter = "C", label = "Character", color = "#4facfe", pastel = "#dbeafe",
               title = "Who is the AI representing?",
               desc = "Act as a senior Competitive Intelligence analyst specializing in the eyewear and wearable technology sector.",
               detail = "Expertise: Experience in evaluating emerging markets and med-tech convergence."),
      R = list(letter = "R", label = "Request", color = "#ff5757", pastel = "#ffe4e4",
               title = "What is the specific request?",
               desc = "Provide a comprehensive overview of the smart glasses market (2024-2026).",
               detail = "Focus: Nuance Audio, key players (Meta, Huawei), core technologies (directional audio), and adoption barriers."),
      E = list(letter = "E", label = "Examples", color = "#a78bfa", pastel = "#ede9fe",
               title = "What style do we want?",
               desc = "Structure the analysis as a strategic intelligence briefing.",
               detail = "Include: Thematic segments, data tables (market size, CAGR), and a competitor comparison matrix."),
      A = list(letter = "A", label = "Adjustments", color = "#ff9f43", pastel = "#fff3e0",
               title = "What refinements are needed?",
               desc = "Focus on global and EMEA markets. Consider both consumer and medical segments.",
               detail = "Note: Highlight the dynamics between AudioNova and EssilorLuxottica vs Ray-Ban Meta."),
      T = list(letter = "T", label = "Type", color = "#f472b6", pastel = "#fce7f3",
               title = "Type of output",
               desc = "A 1500-word structured report.",
               detail = "Sections: Executive summary, thematic chapters, tables, and strategic conclusions."),
      E2 = list(letter = "E", label = "Extras", color = "#00e5a0", pastel = "#e0faf2",
                title = "Extra commands",
                desc = "List the 3-5 most relevant sources used.",
                detail = "Bonus: Flag areas with contradictory data and suggest further investigation steps.")
    )

    # Helper: render one detail box (hidden by default; CSS reveals it on hover)
    render_create_detail <- function(item, key) {
      tags$div(
        class = paste0("sci-card create-detail create-detail-", key),
        style = paste0(
          "border-top: 4px solid ", item$color, "; background:", item$pastel, ";",
          "gap: 30px; align-items: center;"
        ),
        tags$div(style = paste0("font-size:100px; font-weight:900; color:", item$color, "50; line-height:1; flex-shrink:0;"), item$letter),
        tags$div(style = "flex-grow:1;",
                 tags$div(style = paste0("color:", item$color, "; font-weight:800; text-transform:uppercase; font-size:12px; letter-spacing:2px;"), item$label),
                 tags$h3(style = paste0("margin: 5px 0 15px 0; color:", item$color, "; font-size:20px; font-weight:800;"), item$title),
                 tags$p(style = "font-size:15px; color:#334155; line-height:1.6; font-weight:500;", item$desc),
                 tags$div(style = "background: rgba(255,255,255,0.55); padding: 12px; border-radius: 8px; border-left: 3px solid rgba(0,0,0,0.12); margin-top:15px;",
                          tags$p(style = "font-size:13px; color:#64748b; margin:0;", tags$i(item$detail))
                 )
        )
      )
    }

    tagList(
      section_hdr("GenAI for Competitive Intelligence", "Prompt engineering with the CREATE framework + evaluation of GenAI outputs across 3 source categories"),

      # CSS: i riquadri di dettaglio sono nascosti finché non si passa sopra
      # con il mouse sul rispettivo bottone CREATE.
      tags$style(HTML("
        .create-section .create-detail { display: none; }
        .create-section:has(.create-btn-C:hover)  .create-detail-C,
        .create-section:has(.create-btn-R:hover)  .create-detail-R,
        .create-section:has(.create-btn-E:hover)  .create-detail-E,
        .create-section:has(.create-btn-A:hover)  .create-detail-A,
        .create-section:has(.create-btn-T:hover)  .create-detail-T,
        .create-section:has(.create-btn-E2:hover) .create-detail-E2 { display: flex; }
        .create-btn { transition: all 0.25s ease; }
        .create-btn:hover {
          border-color: var(--btn-color) !important;
          background: var(--btn-bg) !important;
          transform: translateY(-3px);
          box-shadow: 0 8px 22px rgba(0,0,0,0.12);
        }
      ")),

      tags$div(class = "create-section",
        # --- SELETTORE CREATE ---
        tags$div(style = "display:flex; gap:10px; margin-bottom:25px;",
                 lapply(names(create_data), function(n) {
                   item <- create_data[[n]]
                   tags$div(
                     class = paste0("vuca-btn create-btn create-btn-", n),
                     style = paste0(
                       "--btn-color:", item$color, "; --btn-bg:", item$pastel, ";",
                       "flex:1; padding:20px 10px; border-radius:12px; cursor:pointer; text-align:center;",
                       "border: 3px solid var(--border);",
                       "background:", paste0(item$pastel, "80"), ";"
                     ),
                     tags$div(style = paste0("font-size:32px; font-weight:900; color:", item$color, ";"), item$letter),
                     tags$div(style = "font-size:10px; text-transform:uppercase; letter-spacing:1px; color:#334155; margin-top:5px;", item$label)
                   )
                 })
        ),

        # --- AREE DETTAGLIO (una per lettera, mostrate solo on hover) ---
        lapply(names(create_data), function(n) render_create_detail(create_data[[n]], n))
      ),

      # Visualizzazione del Prompt Finale (stile neutro)
      tags$div(
        style = paste0(
          "margin-top:20px; margin-bottom:30px; padding:16px 18px;",
          "background:#f1f5f9; border:1px solid #cbd5e1; border-left:3px solid #64748b;",
          "border-radius:14px; transition:transform 0.22s, box-shadow 0.22s;"
        ),
        onmouseover = "this.style.transform='translateY(-2px)'; this.style.boxShadow='0 8px 24px rgba(0,0,0,0.12)';",
        onmouseout  = "this.style.transform=''; this.style.boxShadow='';",
        tags$div(style = "font-size:10px; color:#475569; text-transform:uppercase; letter-spacing:1.5px; font-weight:700; margin-bottom:10px;", "Composite Prompt Preview"),
        tags$p(style = "font-size:12px; color:#334155; line-height:1.65; margin:0; font-family:'JetBrains Mono',monospace;",
               paste0("", create_data$C$desc, " ", create_data$R$desc, " ", create_data$E$desc, " ", create_data$A$desc, " ", create_data$T$desc, " ", create_data$E2$desc))
      ),

      # Evaluation chart
      section_hdr("Output Quality Evaluation", "Rating GenAI output quality (max 5 stars)"),
      tags$div(class = "sci-card", plotlyOutput("plot_eval", height = "240px")),
      
      # Tool comparison
      section_hdr("Tool Comparison", "Strengths, weaknesses, and biases of different GenAI sources"),
      fluidRow(
        column(4, tags$div(class = "sci-card", style = paste0("border-top:3px solid ", col$accent, "; background:#e0faf2;"),
                           tags$div(style = paste0("font-size:18px; font-weight:800; color:", col$accent, "; margin-bottom:12px;"), "Perplexity"),
                           tags$div(style = "margin-bottom:10px;",
                                    tags$div(style = "font-size:10px; color:#64748b; text-transform:uppercase; letter-spacing:1px; margin-bottom:4px;", "Strength"),
                                    tags$div(style = paste0("font-size:12px; color:", col$accent, "; line-height:1.5;"), "Up-to-date data with verifiable source citations")
                           ),
                           tags$div(
                             tags$div(style = "font-size:10px; color:#64748b; text-transform:uppercase; letter-spacing:1px; margin-bottom:4px;", "Weakness"),
                             tags$div(style = paste0("font-size:12px; color:", col$orange, "; line-height:1.5;"), "Risk of commercial/SEO bias; divergent estimates")
                           )
        )),
        column(4, tags$div(class = "sci-card", style = paste0("border-top:3px solid ", col$blue, "; background:#dbeafe;"),
                           tags$div(style = paste0("font-size:18px; font-weight:800; color:", col$blue, "; margin-bottom:12px;"), "Elicit"),
                           tags$div(style = "margin-bottom:10px;",
                                    tags$div(style = "font-size:10px; color:#64748b; text-transform:uppercase; letter-spacing:1px; margin-bottom:4px;", "Strength"),
                                    tags$div(style = paste0("font-size:12px; color:", col$accent, "; line-height:1.5;"), "Scientific rigour; peer-reviewed literature synthesis")
                           ),
                           tags$div(
                             tags$div(style = "font-size:10px; color:#64748b; text-transform:uppercase; letter-spacing:1px; margin-bottom:4px;", "Weakness"),
                             tags$div(style = paste0("font-size:12px; color:", col$orange, "; line-height:1.5;"), "Time lag; limited industry coverage")
                           )
        )),
        column(4, tags$div(class = "sci-card", style = paste0("border-top:3px solid ", col$purple, "; background:#ede9fe;"),
                           tags$div(style = paste0("font-size:18px; font-weight:800; color:", col$purple, "; margin-bottom:12px;"), "ChatGPT / Claude"),
                           tags$div(style = "margin-bottom:10px;",
                                    tags$div(style = "font-size:10px; color:#64748b; text-transform:uppercase; letter-spacing:1px; margin-bottom:4px;", "Strength"),
                                    tags$div(style = paste0("font-size:12px; color:", col$accent, "; line-height:1.5;"), "Strong synthesis; flexible output formatting")
                           ),
                           tags$div(
                             tags$div(style = "font-size:10px; color:#64748b; text-transform:uppercase; letter-spacing:1px; margin-bottom:4px;", "Weakness"),
                             tags$div(style = paste0("font-size:12px; color:", col$orange, "; line-height:1.5;"), "Confabulation risk; data not always verifiable")
                           )
        ))
      ),
      
      # Convergent / Divergent
      fluidRow(
        column(6, info_card("Convergent Information", c(
          "Smart glasses market: strong double-digit CAGR growth (all sources agree)",
          "Meta: undisputed leader in consumer segment",
          "Audio segment: largest by product type (28%+)",
          "AI integration: primary technological driver",
          "Adoption barriers: price, privacy, stigma (consistently identified)"
        ), col$accent)),
        column(6, info_card("Divergent Information", c(
          "Market size 2025: <strong style='font-weight:800'>$2.5B to $18.4B</strong> (depends on definition)",
          "CAGR ranges from <strong style='font-weight:800'>10% to 24%</strong> across sources",
          "Europe's share: 32% to 43% depending on source",
          "Long-term projections highly uncertain",
          "Nuance Audio's actual adoption rate remains unknown"
        ), col$orange))
      ),
      
      # CEO Recommendation
      insight_box(
        "CEO Recommendation",
        "Use LLMs as an <strong style='font-weight:800'>acceleration and support tool</strong> for strategic decisions, but <strong style='font-weight:800'>never as a sole source</strong>. LLMs excel at initial landscape mapping (delivering in minutes what takes days manually), generating alternative scenarios, and identifying hidden assumptions. However, quantitative data must always be verified against primary sources. The primary risk is <strong style='color:#ff9f43'>overconfidence</strong>: LLMs present information authoritatively, masking genuine uncertainty. Recommendation: systematically integrate LLMs into the CI process within a framework of human validation, source triangulation, and critical review, <em>'AI is an accelerator, not a substitute for critical thinking.'</em>",
        col$purple, "lightbulb"
      )
    )
  })
  output$plot_eval <- renderPlotly({
    
    # 1. Riduciamo la larghezza della tendina mandando a capo il commento
    wrapped_comments <- sapply(df_eval$Comment, function(x) {
      paste(strwrap(x, width = 70), collapse = "<br>")
    })
    
    # 2. Creiamo le stringhe con le stelline in base allo score
    star_ratings <- sapply(df_eval$Score, function(s) {
      paste0(strrep("★", s), strrep("☆", 5 - s))
    })
    
    plot_ly(df_eval, y = ~Criterion, x = ~Score, type = "bar", orientation = "h",
            marker = list(
              color = c(col$blue, col$purple, col$accent, col$orange),
              cornerradius = 6
            ),
            text      = ~paste0(Score, " / 5"),
            # 3. Sostituiamo il nome del criterio con le stelline ingrandite e colorate
            hovertext = ~paste0("<span style='font-size:20px; color:#000000;'>", star_ratings, "</span><br><br>", wrapped_comments),
            hoverinfo = "text",
            hoverlabel = list(align = "left"),
            textposition = "outside",
            textfont = list(size = 12, color = "#1e293b")) %>%
      plotly_layout_dark(
        # 4. Rendiamo l'asse X più visibile (linee, griglia e font)
        xaxis = list(
          range = c(0, 5.8),
          title = list(text = "Score (out of 5)", font = list(color = "#1e293b", size = 13)),
          dtick = 1,
          showline = TRUE, linecolor = "#475569", linewidth = 2, # Linea asse marcata
          gridcolor = "#e2e8f0", # Griglia chiara
          tickfont = list(color = "#1e293b", size = 12)
        ),
        # 5. Rendiamo l'asse Y più visibile
        yaxis = list(
          title = list(text = "Criterion", standoff = 30, font = list(color = "#1e293b", size = 13)),
          categoryorder = "array",
          categoryarray = rev(df_eval$Criterion),
          showline = TRUE, linecolor = "#475569", linewidth = 2, # Linea asse marcata
          tickfont = list(color = "#1e293b", size = 12)
        )
      ) %>%
      layout(
        dragmode = FALSE,
        margin = list(l = 280, r = 20, t = 40, b = 40)
      )
  })
  
  # ────────────────────────────────────────────────────────────────────────
  # TAB 8: TEXT ANALYSIS  (Lab 5 — bibliometric + topic modelling)
  # ────────────────────────────────────────────────────────────────────────
  output$tab_textanalysis <- renderUI({
    tagList(
      section_hdr(
        "Text Analysis on the Scientific Literature",
        HTML(paste0(
          "Bibliometric analysis (<em>bibliometrix</em>, <em>tidytext</em>) and topic modelling ",
          "(<em>BERTopic</em>) on the Scopus corpus retrieved with the validated query. ",
          "The literature directly answers our KIT/KIQ on emerging technologies, ",
          "key players and adoption barriers."
        ))
      ),
      
      # ── Top stats row ──────────────────────────────────────────────────
      tags$div(class = "row-stats",
               tags$div(stat_block("Documents",      sum(df_lab5_annual$articles), "Scopus, 2018–2025", col$accent)),
               tags$div(stat_block("Years",          paste0(min(df_lab5_annual$year),"–",max(df_lab5_annual$year)), "Time span", col$blue)),
               # tags$div(stat_block("Countries",      nrow(df_lab5_countries), "with ≥1 publication", col$orange)),
               tags$div(stat_block("Topics found",   nrow(df_lab5_topics), "BERTopic, n_min=8", col$purple)),
               tags$div(stat_block("Top keyword",    df_lab5_keywords$keyword[1], paste0("freq = ", df_lab5_keywords$n[1]), col$cyan))
      ),
      
      # ── Methodology ────────────────────────────────────────────────────
      tags$div(class = "sci-card", style = paste0(
        "border-left:4px solid ", col$accent, ";",
        "background:", pastel_bg(col$accent), ";"
      ),
      tags$h4(style = paste0("font-size:15px; font-weight:700; color:", pastel_text(col$accent), "; margin:0 0 14px;"),
              "Methodology, Text Analysis"),
               tags$pre(class = "query-code-light", style = "margin-bottom:14px;",
                        hl_query(paste0(
                          'TITLE-ABS-KEY("smart glasses" OR "hearing glasses" OR "audio glasses"\n',
                          '  OR "smart eyewear" OR "hearing aid glasses")\n',
                          'AND TITLE-ABS-KEY("hearing" OR "audio" OR "assistive" OR "wearable")\n',
                          'AND NOT TITLE-ABS-KEY("virtual reality headset")\n',
                          'AND PUBYEAR > 2017'
                        ))),
               tags$div(style = "display:grid; grid-template-columns:repeat(3, 1fr); gap:14px; margin-top:18px;",
                        insight_box("Part 1, Bibliometric",
                                    "<code>bibliometrix::biblioAnalysis</code> for descriptive stats and <code>conceptualStructure</code> + <code>biblioNetwork</code> for co-word and collaboration networks.",
                                    col$orange, "chart-bar"),
                        insight_box("Part 1, Tidytext",
                                    "<code>tidytext::unnest_tokens</code> &amp; <code>bind_tf_idf</code> for occurrence and term importance; <code>widyr::pairwise_count</code> + <code>tidygraph</code>/<code>ggraph</code> for the co-occurrence network.",
                                    col$blue, "calculator"),
                        insight_box("Part 2, BERTopic",
                                    "Embeddings (<code>all-MiniLM-L6-v2</code>) → UMAP → HDBSCAN → c-TF-IDF. Output: 7 topics, intertopic distance map, evolution over time.",
                                    col$purple, "robot")
               )
      ),
      
      # ── A) Annual production ───────────────────────────────────────────
      section_hdr("Annual scientific production",
                  "Number of Scopus-indexed papers per year on smart/hearing glasses and adjacent wearable-audio topics"),
      tags$div(class = "sci-card", plotlyOutput("plot_lab5_annual", height = "300px")),
      
      # ── B) Top countries + Top sources ─────────────────────────────────
      #fluidRow(
      # column(6,
      #       section_hdr("Top countries", "Most productive countries (corresponding-author affiliation)"),
      #      tags$div(class = "sci-card", plotlyOutput("plot_lab5_countries", height = "360px"))
      #),
      #column(6,
      #      section_hdr("Top sources", "Journals and conferences with most papers in the corpus"),
      #    tags$div(class = "sci-card", plotlyOutput("plot_lab5_sources", height = "360px"))
      #)
      #),
      # ── B) Top sources ─────────────────────────────────
      section_hdr("Top sources", "Journals and conferences with most papers in the corpus"),
      tags$div(class = "sci-card", plotlyOutput("plot_lab5_sources", height = "360px")),
      
      
      # ── C) Top author keywords (occurrence) ────────────────────────────
      section_hdr("Author keywords, Occurrence",
                  HTML("<code>tidytext::count()</code> on the <em>DE</em> field; bar chart of the 25 most frequent keywords")),
      tags$div(class = "sci-card", plotlyOutput("plot_lab5_keywords", height = "520px")),
      
      # ── D) Co-occurrence (bigrams) ─────────────────────────────────────
      section_hdr("Co-occurrence, Top bigrams",
                  HTML("Most frequent two-word expressions in titles + abstracts (<code>widyr::pairwise_count</code>)")),
      tags$div(class = "sci-card", plotlyOutput("plot_lab5_bigrams", height = "480px")),

      # ── E) BERTopic — Topic distribution ───────────────────────────────
      section_hdr("BERTopic, Topic distribution",
                  HTML("Documents per topic. Each topic is labelled with its <em>c-TF-IDF</em> top words and mapped onto the project's KIT.")),
      tags$div(class = "sci-card", plotlyOutput("plot_lab5_topics_bar", height = "400px")),
      
      # ── F) Topics over time ────────────────────────────────────────────
      section_hdr("Topics over time",
                  "Topic dynamics across 2018–2025: what is growing, what is plateauing"),
      tags$div(class = "sci-card", plotlyOutput("plot_lab5_topics_time", height = "420px")),

      # ── G) Topic table + insights ──────────────────────────────────────
      section_hdr("Topic table & strategic reading",
                  HTML("Each row links one BERTopic cluster to the project KITs (Strategic decisions, Early warnings, Key players)")),
      tags$div(class = "sci-card",
               DT::DTOutput("table_lab5_topics")),
      
      # ── H) Most cited papers ───────────────────────────────────────────
      section_hdr("Most cited papers",
                  "Top 10 most cited documents in the Nuance Audio Scopus corpus, with KIT mapping"),
      tags$div(class = "sci-card",
               DT::DTOutput("table_lab5_top_cited")),
      
      # ── J) Word cloud + Topic cluster map ────────────────────────────────
      section_hdr("Keyword Landscape", "Word cloud of 150 most frequent author keywords from the Scopus corpus"),
      tags$div(style = "border-radius:14px; overflow:hidden; margin-bottom:16px;",
        wordcloud2Output("plot_wordcloud", height = "420px")
      ),
      section_hdr("BERTopic Cluster Map", "2D topic map via MDS on Jaccard similarity — bubble size = number of papers per topic"),
      tags$div(class = "sci-card",
        plotlyOutput("plot_topic_cluster", height = "380px")
      ),
      insight_box("Reading the cluster map",
        paste0("Topics that share more vocabulary are placed closer together. ",
               "<strong>T0 (Open-ear) and T2 (Beamforming)</strong> cluster together as they share the acoustic engineering vocabulary — ",
               "this is Nuance Audio's technical core. ",
               "<strong>T1 (AR / AI Captioning)</strong> is isolated, confirming it represents a distinct and ",
               "rapidly growing research direction driven by Big Tech. ",
               "<strong>T3 (Stigma & Acceptance)</strong> sits apart, reflecting a social-science cluster ",
               "with little vocabulary overlap with engineering topics."),
        col$blue, "diagram-project"),

      # ── I) Closing strategic insight ───────────────────────────────────
      fluidRow(
        column(6, insight_box("What the literature confirms",
                              paste0(
                                "<strong>AR + AI captioning</strong> (Topic 1) is the fastest-growing line of research, ",
                                "consistent with Ray-Ban Meta's 'Conversation Focus' feature and the convergence threat ",
                                "Nuance Audio faces from big-tech ecosystems.<br><br>",
                                "<strong>Open-ear devices and beamforming</strong> (Topics 0+2) form the technical core of ",
                                "Nuance Audio's product, with mature and steady scientific output."
                              ), col$accent, "check-circle")),
        column(6, insight_box("What the literature flags as a risk",
                              paste0(
                                "<strong>Acceptance, stigma and elderly users</strong> (Topic 3) is taking off only now ",
                                "(post-2022): adoption science is still catching up with the new OTC hearing-aid market, ",
                                "so primary research (sentiment analysis on reviews, surveys) remains essential.<br><br>",
                                "<strong>Bone conduction</strong> (Topic 5) is declining in relative weight, confirming the ",
                                "open-ear approach as the dominant design choice."
                              ), col$orange, "exclamation-triangle"))
      )
    )
  })
  
  # ── A) Annual production ─────────────────────────────────────────────────
  output$plot_lab5_annual <- renderPlotly({
    plot_ly(df_lab5_annual[df_lab5_annual$year <= 2025, ], x = ~year, y = ~articles, type = "bar",
            marker = list(color = col$accent, line = list(width = 0)),
            text = ~articles, textposition = "outside",
            textfont = list(color = col$dim, size = 10),
            hovertemplate = "%{x}: <b>%{y}</b> articles<extra></extra>") %>%
      plotly_layout_dark(
        xaxis = list(title = "", dtick = 1),
        yaxis = list(title = "Articles")
      ) %>%
      layout(
        yaxis  = list(range = c(0, 255)),
        margin = list(l = 50, r = 20, t = 28, b = 40)
      )
  })
  
  # ── B1) Top countries ────────────────────────────────────────────────────
  #output$plot_lab5_countries <- renderPlotly({
  # df <- df_lab5_countries[order(df_lab5_countries$articles), ]
  #plot_ly(df,
  #       x = ~articles, y = ~factor(country, levels = country),
  #      type = "bar", orientation = "h",
  #     marker = list(color = col$blue, line = list(width = 0)),
  #    text = ~articles, textposition = "outside",
  #   textfont = list(color = col$dim, size = 10),
  #  hovertemplate = "%{y}: <b>%{x}</b><extra></extra>") %>%
  #  plotly_layout_dark(
  #   xaxis = list(title = "Articles"),
  #  yaxis = list(title = ""),
  # margin = list(l = 130)
  #)
  #})
  
  # ── B2) Top sources ──────────────────────────────────────────────────────
  output$plot_lab5_sources <- renderPlotly({
    df <- df_lab5_sources[order(df_lab5_sources$articles), ]
    plot_ly(df,
            x = ~articles, y = ~factor(source, levels = source),
            type = "bar", orientation = "h",
            marker = list(color = col$purple, line = list(width = 0)),
            text = ~articles, textposition = "outside",
            textfont = list(color = col$dim, size = 10),
            hovertemplate = "%{y}: <b>%{x}</b><extra></extra>") %>%
      plotly_layout_dark(
        xaxis = list(title = "Articles"),
        yaxis = list(title = ""),
        margin = list(l = 220)
      )
  })
  
  # ── C) Top keywords ──────────────────────────────────────────────────────
  output$plot_lab5_keywords <- renderPlotly({
    df <- head(df_lab5_keywords[order(-df_lab5_keywords$n), ], 25)
    df <- df[order(df$n), ]
    plot_ly(df,
            x = ~n, y = ~factor(keyword, levels = keyword),
            type = "bar", orientation = "h",
            marker = list(color = col$accent, line = list(width = 0)),
            text = ~n, textposition = "outside",
            textfont = list(color = col$dim, size = 10),
            hovertemplate = "%{y}: <b>%{x}</b><extra></extra>") %>%
      plotly_layout_dark(
        xaxis = list(title = "Frequency"),
        yaxis = list(title = ""),
        margin = list(l = 200)
      )
  })
  
  # ── D) Top bigrams ───────────────────────────────────────────────────────
  output$plot_lab5_bigrams <- renderPlotly({
    df <- df_lab5_bigrams[order(df_lab5_bigrams$n), ]
    plot_ly(df,
            x = ~n, y = ~factor(pair, levels = pair),
            type = "bar", orientation = "h",
            marker = list(color = col$cyan, line = list(width = 0)),
            text = ~n, textposition = "outside",
            textfont = list(color = col$dim, size = 10),
            hovertemplate = "%{y}: <b>%{x}</b><extra></extra>") %>%
      plotly_layout_dark(
        xaxis = list(title = "Co-occurrence count"),
        yaxis = list(title = ""),
        margin = list(l = 220)
      )
  })
  
  # ── E) Topic distribution ────────────────────────────────────────────────
  output$plot_lab5_topics_bar <- renderPlotly({
    df <- df_lab5_topics[order(-df_lab5_topics$count), ]
    df$short_label <- paste0("T", df$topic, " — ",
                             substr(df$label, 1, 40),
                             ifelse(nchar(df$label) > 40, "…", ""))
    df <- df[order(df$count), ]
    plot_ly(df,
            x = ~count, y = ~factor(short_label, levels = short_label),
            type = "bar", orientation = "h",
            marker = list(color = col$purple, line = list(width = 0)),
            text = ~count, textposition = "outside",
            textfont = list(color = col$dim, size = 10),
            hovertemplate = "%{y}<br><b>%{x}</b> documents<extra></extra>") %>%
      plotly_layout_dark(
        xaxis = list(title = "Documents"),
        yaxis = list(title = ""),
        margin = list(l = 320)
      )
  })
  
  # ── F) Topics over time (multi-line) ─────────────────────────────────────
  output$plot_lab5_topics_time <- renderPlotly({
    palette_topics <- c(col$accent, col$blue, col$orange,
                        col$purple, col$cyan, col$pink, col$red)
    p <- plot_ly()
    for (i in 0:6) {
      sub <- df_lab5_topics_time[df_lab5_topics_time$topic == i, ]
      p <- p %>% add_trace(
        x = sub$year, y = sub$count,
        name = paste0("T", i, " — ", df_lab5_topics$label[i + 1]),
        type = "scatter", mode = "lines+markers",
        line = list(color = palette_topics[i + 1], width = 2.4),
        marker = list(color = palette_topics[i + 1], size = 6),
        hovertemplate = "%{x}: %{y} docs<extra></extra>"
      )
    }
    p %>% plotly_layout_dark(
      xaxis = list(title = "", dtick = 1),
      yaxis = list(title = "Documents"),
      legend = list(orientation = "h", y = -0.25, font = list(size = 10))
    )
  })

  # ── F-bis) BERTopic canonical visualisations ───────────────────────────
  # Replicano visualize_topics / visualize_barchart / visualize_heatmap /
  # visualize_hierarchy del notebook BERTopic, partendo dalla matrice di
  # similarità Jaccard tra topic (calcolata in global.R sui top_words).

  # F1) Intertopic Distance Map (bubble scatter MDS)
  output$plot_lab5_topics_map <- renderPlotly({
    palette_topics <- c(col$accent, col$blue, col$orange,
                        col$purple, col$cyan, col$pink, col$red)
    df <- df_lab5_topics_map
    df$color <- palette_topics[seq_len(nrow(df))]

    plot_ly(df,
            x = ~x, y = ~y,
            type = "scatter", mode = "markers+text",
            text = ~topic, textposition = "middle center",
            textfont = list(color = "#0f1724", size = 12, family = "JetBrains Mono"),
            marker = list(
              size = ~count, sizemode = "area",
              sizeref = 2 * max(df$count) / (55^2), sizemin = 14,
              color = ~color, opacity = 0.78,
              line = list(color = "#0f1724", width = 1.4)
            ),
            hovertemplate = paste0(
              "<b>%{text}</b>, ", df$label,
              "<br>Documents: ", df$count,
              "<br>KIT: ", df$kit,
              "<br><i>", df$top_words, "</i><extra></extra>"
            )) %>%
      plotly_layout_dark(
        xaxis = list(title = "MDS dim 1", zeroline = TRUE,
                     zerolinecolor = "#cbd5e1", zerolinewidth = 1),
        yaxis = list(title = "MDS dim 2", zeroline = TRUE,
                     zerolinecolor = "#cbd5e1", zerolinewidth = 1),
        showlegend = FALSE,
        margin = list(l = 50, r = 30, t = 20, b = 50)
      )
  })

  # F2) Top words per topic (bar chart facetted)
  output$plot_lab5_topics_words <- renderPlotly({
    palette_topics <- c(col$accent, col$blue, col$orange,
                        col$purple, col$cyan, col$pink, col$red)
    topics <- unique(df_lab5_topics_words$topic)
    n_t <- length(topics)
    n_cols <- 3
    n_rows <- ceiling(n_t / n_cols)

    trunc_label <- function(s, max_len = 30) {
      ifelse(nchar(s) > max_len, paste0(substr(s, 1, max_len - 1), "…"), s)
    }

    plots <- lapply(seq_along(topics), function(i) {
      sub <- df_lab5_topics_words[df_lab5_topics_words$topic == topics[i], ]
      sub <- sub[order(sub$rank, decreasing = TRUE), ]  # bar dal basso
      plot_ly(sub,
              x = ~score, y = ~factor(word, levels = sub$word),
              type = "bar", orientation = "h",
              marker = list(color = palette_topics[i], opacity = 0.9,
                            line = list(color = "#0f1724", width = 0.4)),
              text = ~sprintf("%.2f", score), textposition = "outside",
              textfont = list(color = "#1e293b", size = 10),
              hovertemplate = paste0("<b>", topics[i], "</b> — ", sub$topic_label[1],
                                     "<br>%{y}: %{x:.2f}<extra></extra>"),
              showlegend = FALSE) %>%
        layout(
          xaxis = list(title = "", range = c(0, 1.18),
                       showgrid = FALSE, showticklabels = FALSE,
                       zerolinecolor = "#cbd5e1"),
          yaxis = list(title = "", tickfont = list(size = 10, color = "#1e293b"))
        )
    })

    # Annotazioni di titolo: ancorate al dominio di ciascun subplot
    # (xref = "x{i} domain", yref = "y{i} domain") così sono sempre sopra
    # il rispettivo grafico, indipendentemente dai margini globali.
    annot <- lapply(seq_along(topics), function(i) {
      suffix <- if (i == 1) "" else as.character(i)
      list(text = paste0("<b>", topics[i], "</b> — ",
                         trunc_label(df_lab5_topics$label[i])),
           x = 0.5, y = 1,
           xref = paste0("x", suffix, " domain"),
           yref = paste0("y", suffix, " domain"),
           xanchor = "center", yanchor = "bottom",
           yshift = 6,
           showarrow = FALSE,
           font = list(size = 11, color = "#1e293b", family = "Outfit"))
    })

    subplot(plots, nrows = n_rows,
            margin = c(0.025, 0.025, 0.05, 0.05),
            shareX = FALSE, shareY = FALSE) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             margin = list(l = 90, r = 40, t = 30, b = 10),
             annotations = annot,
             showlegend = FALSE) %>%
      config(displayModeBar = FALSE)
  })

  # F3) Topic Similarity Heatmap
  output$plot_lab5_topics_heatmap <- renderPlotly({
    mat <- lab5_topic_jaccard
    labels <- rownames(mat)
    plot_ly(
      x = labels, y = labels, z = mat,
      type = "heatmap",
      colorscale = list(c(0, "#f8fafc"), c(0.5, "#60a5fa"), c(1, "#1e3a8a")),
      zmin = 0, zmax = max(mat),
      hovertemplate = "Topic %{x} ↔ Topic %{y}<br>Jaccard: %{z:.2f}<extra></extra>",
      colorbar = list(title = list(text = "Jaccard", font = list(color = "#1e293b")),
                      tickfont = list(color = "#1e293b"))
    ) %>%
      plotly_layout_dark(
        xaxis = list(title = "", side = "bottom",
                     tickfont = list(color = "#1e293b", size = 11)),
        yaxis = list(title = "", autorange = "reversed",
                     tickfont = list(color = "#1e293b", size = 11)),
        margin = list(l = 60, r = 30, t = 20, b = 50)
      )
  })

  # F4) Topic Hierarchy Dendrogram (base R, renderPlot)
  output$plot_lab5_topics_dendro <- renderPlot({
    op <- par(mar = c(4, 1, 1, 26), bg = "#f1f5f9", cex = 0.95, family = "sans")
    on.exit(par(op))
    dend <- as.dendrogram(lab5_topic_hclust)
    plot(dend, horiz = TRUE, axes = TRUE, xlab = "Distance (1 − Jaccard)",
         edgePar = list(col = "#475569", lwd = 1.6),
         nodePar = list(lab.col = "#0f1724", pch = NA))
  }, bg = "#f1f5f9")

  # ── G) Topic table ───────────────────────────────────────────────────────
  output$table_lab5_topics <- DT::renderDT({
    df <- df_lab5_topics
    df$topic <- paste0("T", df$topic)
    DT::datatable(
      df[, c("topic", "label", "count", "top_words", "KIT_link")],
      colnames = c("Topic", "Label", "Documents", "Top words (c-TF-IDF)", "KIT link"),
      options  = list(pageLength = 7, dom = "t", ordering = FALSE,
                      columnDefs = list(list(className = "dt-left", targets = "_all"))),
      rownames = FALSE, class = "stripe hover", escape = FALSE
    ) %>% DT::formatStyle("count", fontWeight = "bold", color = col$accent)
  })
  
  # ── H) Most cited papers ─────────────────────────────────────────────────
  output$table_lab5_top_cited <- DT::renderDT({
    DT::datatable(
      df_lab5_top_cited,
      colnames = c("Title", "Year", "Citations", "KIT"),
      options  = list(pageLength = 10, dom = "t", ordering = FALSE,
                      columnDefs = list(list(className = "dt-left", targets = "_all"))),
      rownames = FALSE, class = "stripe hover"
    ) %>% DT::formatStyle("cites", fontWeight = "bold", color = col$accent)
  })

  # ── J) Word cloud — wordcloud2 ───────────────────────────────────────────
  output$plot_wordcloud <- renderWordcloud2({
    df <- df_wordcloud[, c("word", "freq")]
    tier_pal <- c(
      rep(col$accent, 10),
      rep(col$blue,   15),
      rep("#475569",  25),
      rep("#94a3b8",  nrow(df) - 50)
    )
    wordcloud2(
      data            = df,
      size            = 0.55,
      color           = tier_pal,
      backgroundColor = pastel_bg(col$accent),
      fontFamily      = "Outfit",
      minRotation     = -pi/6,
      maxRotation     =  pi/6,
      rotateRatio     = 0.35,
      gridSize        = 8
    )
  })

  # ── J2) Topic cluster map — document scatter around MDS centroids ─────────
  output$plot_topic_cluster <- renderPlotly({
    df_docs   <- df_cluster_docs
    df_center <- df_topic_cluster

    p <- plot_ly()

    for (i in seq_len(nrow(df_center))) {
      pts <- df_docs[df_docs$topic == df_center$topic[i], ]
      p <- p %>% add_trace(
        data       = pts,
        x          = ~x, y = ~y,
        type       = "scatter", mode = "markers",
        name       = paste0(df_center$topic[i], ": ", df_center$label[i]),
        marker     = list(
          color   = pts$color[1],
          size    = 7,
          opacity = 0.72,
          line    = list(width = 0)
        ),
        hovertemplate = paste0(
          "<b>", df_center$topic[i], "</b>: ", df_center$label[i],
          "<extra></extra>"
        ),
        showlegend = TRUE
      )
    }

    # Topic labels at centroids
    p <- p %>% add_annotations(
      x          = df_center$x,
      y          = df_center$y,
      text       = paste0("<b>", df_center$topic, "</b>"),
      showarrow  = FALSE,
      font       = list(size = 11, color = "#0f172a", family = "Outfit"),
      bgcolor    = "rgba(255,255,255,0.75)",
      borderpad  = 3
    )

    p %>% plotly_layout_dark(
      xaxis      = list(title = "D1", zeroline = TRUE, zerolinecolor = "#e2e8f0",
                        showgrid = FALSE, tickfont = list(size = 9)),
      yaxis      = list(title = "D2", zeroline = TRUE, zerolinecolor = "#e2e8f0",
                        showgrid = FALSE, tickfont = list(size = 9)),
      legend     = list(orientation = "v", x = 1.01, y = 0.95,
                        font = list(size = 9, family = "Outfit"),
                        bgcolor = "rgba(255,255,255,0.6)"),
      margin     = list(l = 40, r = 180, t = 10, b = 40)
    )
  })

  # ────────────────────────────────────────────────────────────────────────
  # TAB 9-bis: CO-WORD NETWORKS (Bibliometrix on scopus_glasses)
  # Replica integrale di bibliometrix::biblioAnalysis + conceptualStructure
  # + biblioNetwork dal corpus scopus_glasses.csv. PNG renderizzati statici.
  # ────────────────────────────────────────────────────────────────────────
  output$tab_cowords <- renderUI({
    tagList(
      section_hdr(
        "Co-word Networks, Conceptual Structure (Bibliometrix)",
        HTML(paste0(
          "Full replication of <code>bibliometrix::biblioAnalysis</code> + ",
          "<code>conceptualStructure(field=\"DE\", method=\"MDS\", clust=5)</code> + ",
          "<code>biblioNetwork(network=\"keywords\")</code> applied to the corpus ",
          "<code>scopus_glasses.csv</code>. Two-level analysis: <strong>full corpus</strong> ",
          "(smart-glasses literature) and <strong>AUDIO/Hearing sub-corpus</strong>, aligned with ",
          "the Nuance Audio positioning."
        ))
      ),

      # Methodology banner
      tags$div(
        class = "methodology-banner",
        style = paste0(
          "margin-bottom: 20px; padding: 18px 22px; border-radius: 14px;",
          "background:", pastel_bg(col$purple), "; border-left:4px solid ", col$purple, ";"
        ),
        tags$div(style = paste0("font-size:11px; font-weight:800; letter-spacing:2px; text-transform:uppercase; color:", col$purple, ";"),
                 "Methodology"),
        tags$p(style = paste0("font-size:13px; color:", pastel_text(col$purple), "; line-height:1.6; margin:6px 0 0;"),
               HTML(paste0(
                 "The reference script (<code>Bibliometric and Coword Analysis_Bibliometrix.R</code>) ",
                 "performs: (1) <code>convert2df</code> to import the Scopus CSV, ",
                 "(2) <code>biblioAnalysis</code> for descriptive statistics, ",
                 "(3) <code>conceptualStructure</code> for the conceptual map via MDS on Author Keywords, ",
                 "(4) <code>biblioNetwork</code> for the co-occurrence network."
               )))
      ),

      # 1) Conceptual Structure Map — full corpus + insight box affiancati
      section_hdr("Conceptual Structure Map, Full corpus (MDS)",
                  HTML("Co-word analysis on <em>Author Keywords</em> via Multi-Dimensional Scaling. Unsupervised thematic clusters organizing the smart-glasses research landscape.")),
      tags$div(style = "margin-bottom: 28px;",
        fluidRow(
          column(7,
            tags$img(src = "bibliometrix/05_conceptual_structure_MDS.png",
                     style = "width:100%; height:auto; border-radius:10px; display:block;",
                     alt = "Conceptual Structure MDS - full corpus")
          ),
          column(5,
            insight_box(
              "Strategic Reading, Full MDS",
              paste0(
                "The <strong style='font-weight:800'>central blue cluster</strong> (smart-glasses, computer vision, wearable computing, AR, ",
                "user experience) is the <em>technological core</em> where Nuance Audio is positioned. ",
                "The <strong style='font-weight:800'>upper green cluster</strong> (TTS, object recognition, ultrasonic) ",
                "covers <em>assistive</em> applications for visual impairment. ",
                "The <strong style='font-weight:800'>lower yellow cluster</strong> (IoT, IMU, heart rate) ",
                "points toward the <em>medical-wearable</em> trajectory: KIT 2 must monitor this pole. ",
                "The <strong style='font-weight:800'>isolated pink AR/HMD cluster</strong> confirms that AR-headsets are a separate category: ",
                "Nuance Audio can claim 'hearing glasses' without competing with AR/VR."
              ),
              col$blue, "diagram-project"
            )
          )
        )
      ),

      # 2) Keyword co-occurrence network — full corpus (centrata, larghezza ridotta)
      section_hdr("Keyword Co-occurrence Network, Full corpus",
                  HTML("Output of <code>biblioNetwork(network=\"keywords\")</code>: co-occurrence clusters among Author Keywords. The stronger the link, the more frequent the co-occurrence.")),
      tags$div(style = "max-width:78%; margin: 0 auto 32px;",
        tags$img(src = "bibliometrix/06_keyword_cooccurrence_network.png",
                 style = "width:100%; height:auto; border-radius:10px; display:block;",
                 alt = "Keyword co-occurrence network - full corpus")
      ),

      # 3) Conceptual Structure — AUDIO sub-corpus + insight box affiancati
      section_hdr("Conceptual Structure Map, AUDIO/Hearing sub-corpus (MDS)",
                  HTML("The same <code>conceptualStructure</code> applied to the subset filtered on hearing/audio/assistive: the exact niche in which Nuance Audio operates.")),
      tags$div(style = "margin-bottom: 28px;",
        fluidRow(
          column(7,
            tags$img(src = "bibliometrix/09_conceptual_structure_AUDIO.png",
                     style = "width:100%; height:auto; border-radius:10px; display:block;",
                     alt = "Conceptual Structure MDS - AUDIO sub-corpus")
          ),
          column(5,
            insight_box(
              "Strategic Reading, AUDIO MDS",
              paste0(
                "The AUDIO sub-corpus reveals vertical clusters: ",
                "<strong style='font-weight:800'>purple</strong> (wearable + ultrasonic + visual impairment) ",
                "= integrated assistive products, the <em>application context</em> of Nuance Audio. ",
                "<strong style='font-weight:800'>green</strong> (TTS, OCR, YOLO) = AI/perception capabilities. ",
                "<strong style='font-weight:800'>red</strong> (esp32-cam, microcontroller) = low-cost prototyping (R&amp;D barriers are lower than they appear). ",
                "<strong style='font-weight:800'>teal</strong> (dementia, transfer learning, indoor localization) = ",
                "medical-cognitive applications, a market trajectory for KIT 1."
              ),
              col$accent, "headphones"
            )
          )
        )
      ),

      # 4+5) Keyword network AUDIO + Pairwise network — affiancati
      section_hdr("Co-occurrence & Pairwise Networks, AUDIO/Hearing sub-corpus",
                  HTML("<code>biblioNetwork</code> keyword network and <code>widyr::pairwise_count</code> abstract-term network for the AUDIO sub-corpus.")),
      tags$div(style = "margin-bottom: 16px;",
        fluidRow(
          column(6,
            tags$div(style = "margin-bottom: 8px;",
              tags$span(style = "font-size:11px; font-weight:700; color:var(--muted); text-transform:uppercase; letter-spacing:1px;",
                        "Keyword Co-occurrence Network")
            ),
            tags$img(src = "bibliometrix/08_keyword_network_AUDIO.png",
                     style = "width:100%; height:460px; object-fit:contain; border-radius:10px; display:block; background:#fff;",
                     alt = "Keyword co-occurrence network - audio")
          ),
          column(6,
            tags$div(style = "margin-bottom: 8px;",
              tags$span(style = "font-size:11px; font-weight:700; color:var(--muted); text-transform:uppercase; letter-spacing:1px;",
                        "Pairwise Co-occurrence Network")
            ),
            tags$div(
              style = "background:#f1f5f9; border-radius:10px; height:460px; display:flex; align-items:center; justify-content:center; overflow:hidden;",
              tags$img(src = "bibliometrix/11_pairwise_network_AUDIO.png",
                       style = "width:100%; height:100%; object-fit:contain; display:block;",
                       alt = "Pairwise word network - audio")
            )
          )
        )
      )
    )
  })

  # ────────────────────────────────────────────────────────────────────────
  # TAB 9-ter: BERTOPIC MAPS (canonical BERTopic visualisations)
  # Replica di visualize_topics, visualize_barchart, visualize_heatmap,
  # visualize_hierarchy dal notebook Scientific_Paper_Analysis_with_BERTopic.
  # ────────────────────────────────────────────────────────────────────────
  output$tab_bertopicmaps <- renderUI({
    tagList(
      section_hdr(
        "BERTopic, Topic Map, Words & Hierarchy",
        HTML(paste0(
          "Full replication of the canonical visualizations from the notebook ",
          "<code>Scientific_Paper_Analysis_with_BERTopic.ipynb</code> ",
          "(<code>visualize_topics</code>, <code>visualize_barchart</code>, ",
          "<code>visualize_heatmap</code>, <code>visualize_hierarchy</code>) ",
          "applied to the corpus <code>scopus_glasses.csv</code>."
        ))
      ),

      # Methodology banner
      tags$div(
        class = "methodology-banner",
        style = paste0(
          "margin-bottom: 20px; padding: 18px 22px; border-radius: 14px;",
          "background:", pastel_bg(col$cyan), "; border-left:4px solid ", col$cyan, ";"
        ),
        tags$div(style = paste0("font-size:11px; font-weight:800; letter-spacing:2px; text-transform:uppercase; color:", col$cyan, ";"),
                 "Methodology"),
        tags$p(style = paste0("font-size:13px; color:", pastel_text(col$cyan), "; line-height:1.6; margin:6px 0 0;"),
               HTML(paste0(
                 "The notebook applies BERTopic (embeddings <code>all-MiniLM-L6-v2</code> → UMAP → HDBSCAN → c-TF-IDF) ",
                 "and produces 4 canonical visualizations in addition to the topic table and temporal evolution ",
                 "(already in the Text Analysis tab). These are replicated here in R/Plotly using the c-TF-IDF top-words of the 7 topics ",
                 "identified in the corpus, using <strong>Jaccard similarity</strong> as a proxy for cosine similarity ",
                 "between embeddings (the topic-topic similarity matrix underlies all 4 visualizations)."
               )))
      ),

      # 1) Intertopic Distance Map
      section_hdr("Intertopic Distance Map, MDS",
                  HTML("Replica of <code>topic_model.visualize_topics()</code>: each bubble is a topic, size = number of documents, distance ≈ lexical dissimilarity (Jaccard on top-words).")),
      tags$div(class = "sci-card", style = "margin-bottom: 12px;",
               plotlyOutput("plot_lab5_topics_map", height = "440px")),
      tags$div(style = "margin-bottom: 28px;",
        insight_box(
          "Strategic Reading, Topic Map",
          paste0(
            "Topics <strong style='font-weight:800'>T0 (Open-ear audio)</strong>, ",
            "<strong style='font-weight:800'>T2 (Hearing-aid + beamforming)</strong> and ",
            "<strong style='font-weight:800'>T4 (Deep learning for speech)</strong> tend to cluster closely: ",
            "they represent the <em>technological core</em> where Nuance Audio competes head-on (KIT 2). ",
            "<strong style='font-weight:800'>T1 (AR + AI captioning)</strong> stands apart: it is the disruption vector ",
            "driven by Meta/Apple, KIT 2 (early warnings). ",
            "<strong style='font-weight:800'>T3 (acceptance/stigma)</strong> and <strong style='font-weight:800'>T6 (eyewear UX)</strong> ",
            "define the <em>consumer adoption</em> landscape (KIT 1). ",
            "<strong style='font-weight:800'>T5 (bone-conduction)</strong> is isolated: a marginal technological alternative, worth monitoring but not a priority."
          ),
          col$blue, "diagram-project"
        )
      ),

      # 2) Top words per topic (c-TF-IDF style)
      section_hdr("Top words per topic, c-TF-IDF",
                  HTML("Replica of <code>topic_model.visualize_barchart()</code>: for each topic, the most distinctive words with their c-TF-IDF score (descending pseudo-score).")),
      tags$div(class = "sci-card", style = "margin-bottom: 28px;",
               plotlyOutput("plot_lab5_topics_words", height = "600px")),

      # 3) Topic Similarity Heatmap
      section_hdr("Topic Similarity Heatmap",
                  HTML("Replica of <code>topic_model.visualize_heatmap()</code>: Jaccard similarity matrix between topics; higher values = semantically closer topics.")),
      tags$div(class = "sci-card", style = "margin-bottom: 28px;",
               plotlyOutput("plot_lab5_topics_heatmap", height = "420px")),

      # 4) Topic Hierarchy Dendrogram
      section_hdr("Topic Hierarchy Dendrogram",
                  HTML("Replica of <code>topic_model.visualize_hierarchy()</code> / <code>get_topic_tree()</code>: hierarchical clustering (<em>average linkage</em>) on the 1−Jaccard distance.")),
      tags$div(
        style = "border-radius: 12px; overflow: hidden; margin-bottom: 12px;",
        plotOutput("plot_lab5_topics_dendro", height = "420px")
      ),
      tags$div(style = "margin-bottom: 16px;",
        insight_box(
          "Strategic Reading, Hierarchy",
          paste0(
            "The hierarchy confirms two macro-branches: ",
            "<strong style='font-weight:800'>(1) hearing-tech core</strong> (T0, T2, T4), this is Nuance Audio's product perimeter, ",
            "where competition is vertical and patent-driven (KIT 2); ",
            "<strong style='font-weight:800'>(2) consumer/AR layer</strong> (T1, T3, T6), this is where the ",
            "<em>category creation</em> of 'hearing glasses' and the go-to-market play out (KIT 1, KIT 3). ",
            "The isolation of T5 (bone-conduction) suggests it is not currently a direct substitution threat, but ",
            "an adjacent technological niche."
          ),
          col$purple, "sitemap"
        )
      )
    )
  })

  # ────────────────────────────────────────────────────────────────────────
  # TAB 10: PATENT ANALYSIS (Erre Quadro Methodology)
  # ────────────────────────────────────────────────────────────────────────
  
  # Dati REALI estratti da Espacenet (File CSV Erre Quadro)
  df_pat_trend <- data.frame(
    Year = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024),
    Patents = c(39, 26, 39, 55, 59, 52, 99, 111, 65, 76, 71)
  )
  
  df_pat_assignee <- data.frame(
    Assignee = c("Snap Inc.", "Meta Platforms", "Connor Robert A.", "Ingeniospec LLC", "Oakley Inc.", "Solos Tech", "Bank of America", "Essilor Int."),
    Count = c(128, 43, 17, 13, 13, 13, 12, 10)
  )
  
  df_pat_ipc <- data.frame(
    IPC = c("G02B27 (Optical systems)", "G02C11 (Non-optical specs)", "G06F3 (I/O arrangements)", "G02C5 (Spectacle construction)", "H04R1 (Loudspeakers)"),
    Count = c(375, 336, 223, 195, 84)
  )
  
  output$tab_patents <- renderUI({
    tagList(
      section_hdr("Patent Data Analysis", "IP Landscape on Smart Glasses & Hearing Technologies (Espacenet)"),
      
      tags$div(class = "sci-card", style = paste0(
        "border-left:5px solid ", col$accent, ";",
        "background:", pastel_bg(col$accent), ";"
      ),
      tags$h4(style = paste0("color:", pastel_text(col$accent), "; margin-top:0; font-weight:800; font-size:15px;"), "Erre Quadro Methodology, Espacenet Query"),
      tags$p(style = paste0("color:", pastel_text(col$accent), "cc; line-height:1.6; font-size:13px;"),
             "To analyze Intellectual Property (IP), we queried the Espacenet database focusing on the intersection between visual devices (eyewear) and hearing technologies, extracting and processing statistical data for Assignees and IPC classes."),
      tags$pre(class = "query-code-light", style = "margin-top:10px;",
               hl_query('(ti="smart glasses" OR ti="audio glasses" OR ti="eyewear") AND (txt="hearing" OR txt="audio" OR txt="acoustic")')
      )),
      
      tags$div(class = "row-stats",
               tags$div(stat_block("Total Patents", "849", "Espacenet Results", col$accent)),
               tags$div(stat_block("Top Applicant", "Snap Inc.", "128 Patents", col$blue)),
               tags$div(stat_block("Dominant IPC", "G02B27", "Optical Systems", col$orange)),
               tags$div(stat_block("Peak Year", "2021", "111 Priority Filings", col$purple))
      ),
      
      fluidRow(
        column(6,
               tags$div(class = "sci-card",
                        tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;", "Patent Filings Trend (Priority Year)"),
                        plotlyOutput("plot_pat_trend", height = "260px")
               )
        ),
        column(6,
               tags$div(class = "sci-card",
                        tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;", "Top Applicants (Assignees)"),
                        plotlyOutput("plot_pat_assignee", height = "260px")
               )
        )
      ),
      
      fluidRow(
        column(6,
               tags$div(class = "sci-card",
                        tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;", "Technology Landscape (Top IPC Classes)"),
                        plotlyOutput("plot_pat_ipc", height = "220px")
               )
        ),
        column(6,
               insight_box("IP Strategic Insight", 
                           "The extracted Espacenet data reveals a crucial dynamic: <strong>traditional medical and hearing aid companies are entirely absent from the Top 10</strong>. Intellectual property is dominated by consumer/social tech giants like <strong>Snap Inc. and Meta Platforms</strong>. This confirms the VUCA analysis warning: the true competitive threat to Nuance Audio is asymmetric, arriving from lifestyle eyewear rather than the healthcare sector.", 
                           col$orange, "exclamation-triangle")
        )
      )
    )
  })
  
  # Render Plot: Trend
  output$plot_pat_trend <- renderPlotly({
    plot_ly(df_pat_trend, x = ~Year, y = ~Patents, type = "scatter", mode = "lines+markers",
            line = list(color = col$blue, width = 3, shape = "spline"),
            marker = list(color = col$blue, size = 8),
            fill = "tozeroy", fillcolor = paste0(col$blue, "20")) %>%
      plotly_layout_dark(xaxis = list(title = "", tickmode = "linear", dtick = 2), yaxis = list(title = "Patents Filed"))
  })
  
  # Render Plot: Assignees
  output$plot_pat_assignee <- renderPlotly({
    df <- df_pat_assignee[order(df_pat_assignee$Count), ]
    plot_ly(df, x = ~Count, y = ~factor(Assignee, levels = Assignee), type = "bar", orientation = "h",
            marker = list(color = col$accent, line = list(width = 0)),
            text = ~Count, textposition = "outside", textfont = list(color = col$dim, size = 11)) %>%
      plotly_layout_dark(xaxis = list(title = "Number of Patents"), yaxis = list(title = ""), margin = list(l = 120, r = 60))
  })
  
  # ── BLOCCO BLINDATO PER IL GRAFICO DELLE CLASSI IPC ──
  output$plot_pat_ipc <- renderPlotly({
    
    # 1. Ricreiamo il dataframe localmente per sicurezza totale
    df_sicuro <- data.frame(
      IPC = c("G02B27 (Optical Systems)", "G02C11 (Non-optical Specs)", 
              "G06F3 (Data Processing)", "G02C5 (Spectacle Frames)", 
              "H04R1 (Acoustic Transducers)"),
      Count = c(375, 336, 223, 195, 84)
    )
    
    # 2. Ordiniamo i dati PRIMA di chiamare Plotly
    df_sicuro <- df_sicuro[order(df_sicuro$Count), ]
    
    # 3. Blocchiamo l'ordine delle etichette (factor)
    df_sicuro$IPC <- factor(df_sicuro$IPC, levels = df_sicuro$IPC)
    
    # 4. Creiamo il grafico basico
    p <- plot_ly(
      data = df_sicuro, 
      x = ~Count, 
      y = ~IPC, 
      type = "bar", 
      orientation = "h",
      marker = list(color = "#a78bfa"), 
      text = ~Count,
      textposition = "outside",
      textfont = list(color = "#1e293b", size = 11)
    )

    # 5. Aggiungiamo i layout
    p <- layout(p,
                xaxis = list(title = "Number of Patents", range = c(0, 430),
                             gridcolor = "#e2e8f0", tickfont = list(color="#1e293b"), titlefont=list(color="#1e293b")),
                yaxis = list(title = "", tickfont = list(color="#1e293b")),
                margin = list(l = 200, r = 20, t = 10, b = 40),
                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor = "rgba(0,0,0,0)"
    )
    
    # 6. ECCO LA MAGIA: Nascondiamo la barra degli strumenti fastidiosa!
    p <- config(p, displayModeBar = FALSE)
    
    # 7. Inviamo il grafico alla UI
    return(p)
  })

  # ════════════════════════════════════════════════════════════════════════════
  # TAB 10-bis: ERRE QUADRO LAB — Smart Glasses Exercise (7 tasks)
  # Replica esercizio "Smart Glasses Exercise" su Espacenet (10.644 patent totali)
  # ════════════════════════════════════════════════════════════════════════════

  # ── Render charts (T7) ────────────────────────────────────────────────────
  output$plot_eq_priority_year <- renderPlotly({
    df <- df_eq_priority_year[df_eq_priority_year$year >= 2000 &
                              df_eq_priority_year$year <= 2025, ]
    df <- df[order(df$year), ]
    plot_ly(df, x = ~year, y = ~count, type = "bar",
            marker = list(color = col$blue, opacity = 0.9,
                          line = list(color = "#0f1724", width = 0.4)),
            text = ~count, textposition = "outside",
            textfont = list(color = "#1e293b", size = 10),
            hovertemplate = "Priority %{x}: %{y} patents<extra></extra>") %>%
      plotly_layout_dark(
        xaxis = list(title = "Priority year", dtick = 2),
        yaxis = list(title = "Patents (full dataset)"),
        margin = list(l = 60, r = 30, t = 30, b = 40)
      )
  })

  output$plot_eq_applicants <- renderPlotly({
    df <- head(df_eq_applicants, 15)
    df <- df[order(df$count), ]
    plot_ly(df, x = ~count, y = ~factor(name, levels = name),
            type = "bar", orientation = "h",
            marker = list(color = col$accent, opacity = 0.92,
                          line = list(color = "#0f1724", width = 0.4)),
            text = ~count, textposition = "outside",
            textfont = list(color = "#1e293b", size = 10),
            hovertemplate = "%{y}<br>%{x} patents<extra></extra>") %>%
      plotly_layout_dark(
        xaxis = list(title = "Patents"),
        yaxis = list(title = ""),
        margin = list(l = 250, r = 80, t = 20, b = 40)
      )
  })

  output$plot_eq_inventors <- renderPlotly({
    df <- head(df_eq_inventors, 15)
    df <- df[order(df$count), ]
    plot_ly(df, x = ~count, y = ~factor(name, levels = name),
            type = "bar", orientation = "h",
            marker = list(color = col$purple, opacity = 0.92,
                          line = list(color = "#0f1724", width = 0.4)),
            text = ~count, textposition = "outside",
            textfont = list(color = "#1e293b", size = 10),
            hovertemplate = "%{y}<br>%{x} patents<extra></extra>") %>%
      plotly_layout_dark(
        xaxis = list(title = "Patents"),
        yaxis = list(title = ""),
        margin = list(l = 220, r = 50, t = 20, b = 40)
      )
  })

  output$plot_eq_countries <- renderPlotly({
    df <- head(df_eq_app_country, 15)
    df <- df[order(df$count), ]
    plot_ly(df, x = ~count, y = ~factor(country_name, levels = country_name),
            type = "bar", orientation = "h",
            marker = list(color = col$orange, opacity = 0.92,
                          line = list(color = "#0f1724", width = 0.4)),
            text = ~count, textposition = "outside",
            textfont = list(color = "#1e293b", size = 10),
            hovertemplate = "%{y}<br>%{x} patents<extra></extra>") %>%
      plotly_layout_dark(
        xaxis = list(title = "Patents"),
        yaxis = list(title = ""),
        margin = list(l = 170, r = 50, t = 20, b = 40)
      )
  })

  output$plot_eq_ipc <- renderPlotly({
    df <- head(df_eq_ipc, 15)
    df$tag <- ifelse(df$label != "", paste0(df$ipc, " — ", df$label), df$ipc)
    df <- df[order(df$count), ]
    plot_ly(df, x = ~count, y = ~factor(tag, levels = tag),
            type = "bar", orientation = "h",
            marker = list(color = col$pink, opacity = 0.92,
                          line = list(color = "#0f1724", width = 0.4)),
            text = ~count, textposition = "outside",
            textfont = list(color = "#1e293b", size = 10),
            hovertemplate = "%{y}<br>%{x} patents<extra></extra>") %>%
      plotly_layout_dark(
        xaxis = list(title = "Patents"),
        yaxis = list(title = ""),
        margin = list(l = 280, r = 100, t = 20, b = 40)
      )
  })

  # T5 — Holographic charts
  output$plot_eq_holo_timeline <- renderPlotly({
    df <- as.data.frame(table(df_eq_holo$priority_year), stringsAsFactors = FALSE)
    names(df) <- c("year", "count")
    df <- df[df$year != "" & !is.na(df$year), ]
    df$year <- as.integer(df$year)
    df <- df[!is.na(df$year) & df$year <= 2025, ]
    df <- df[order(df$year), ]
    plot_ly(df, x = ~year, y = ~count, type = "bar",
            marker = list(color = col$cyan, opacity = 0.92,
                          line = list(color = "#0f1724", width = 0.4)),
            text = ~count, textposition = "outside",
            textfont = list(color = "#1e293b", size = 10),
            hovertemplate = "%{x}: %{y} patents<extra></extra>") %>%
      plotly_layout_dark(
        xaxis = list(title = "Priority year", dtick = 2),
        yaxis = list(title = "Patents"),
        margin = list(l = 50, r = 30, t = 20, b = 40)
      )
  })

  output$plot_eq_holo_countries <- renderPlotly({
    df <- as.data.frame(table(df_eq_holo$country), stringsAsFactors = FALSE)
    names(df) <- c("country", "count")
    df <- df[order(df$count), ]
    df$country_name <- ifelse(df$country %in% names(.country_names),
                              .country_names[df$country], df$country)
    plot_ly(df, x = ~count, y = ~factor(country_name, levels = country_name),
            type = "bar", orientation = "h",
            marker = list(color = col$cyan, opacity = 0.92,
                          line = list(color = "#0f1724", width = 0.4)),
            text = ~count, textposition = "outside",
            textfont = list(color = "#1e293b", size = 10),
            hovertemplate = "%{y}<br>%{x} patents<extra></extra>") %>%
      plotly_layout_dark(
        xaxis = list(title = "Patents (out of 60)"),
        yaxis = list(title = ""),
        margin = list(l = 160, r = 50, t = 20, b = 40)
      )
  })

  # ── Tables ────────────────────────────────────────────────────────────────
  output$table_eq_top20 <- DT::renderDT({
    df <- head(df_eq_main, 20)[, c("No","Title","Applicants","Publication number",
                                    "Earliest priority","IPC","country")]
    names(df) <- c("#","Title","Applicant","Publ. no.","Priority","IPC","Country")
    DT::datatable(df,
                  options = list(pageLength = 20, dom = "t", ordering = FALSE,
                                 columnDefs = list(list(className = "dt-left", targets = "_all"))),
                  rownames = FALSE, class = "stripe hover")
  })

  output$table_eq_holo <- DT::renderDT({
    df <- df_eq_holo[, c("No","Title","Applicants","Publication number",
                         "Earliest priority","IPC","country")]
    names(df) <- c("#","Title","Applicant","Publ. no.","Priority","IPC","Country")
    DT::datatable(df,
                  options = list(pageLength = 10, dom = "tp", ordering = FALSE,
                                 columnDefs = list(list(className = "dt-left", targets = "_all"))),
                  rownames = FALSE, class = "stripe hover")
  })

  # ── UI ─────────────────────────────────────────────────────────────────────
  output$tab_errequadro <- renderUI({

    # Helper per i task header
    task_hdr <- function(num, title, subtitle = NULL) {
      tags$div(
        style = paste0("margin-top:34px; margin-bottom:14px; display:flex; gap:14px; align-items:center;"),
        tags$div(style = paste0(
          "flex:0 0 auto; width:42px; height:42px; border-radius:50%;",
          "background:", col$accent, "; color:#0f1724; font-weight:900; font-size:16px;",
          "display:flex; align-items:center; justify-content:center; font-family:'JetBrains Mono';"
        ), paste0("T", num)),
        tags$div(
          tags$div(style = "font-size:18px; font-weight:800; color:var(--text);", title),
          if (!is.null(subtitle))
            tags$div(style = "font-size:12px; color:var(--dim); margin-top:2px;", subtitle)
        )
      )
    }

    # Helper: token (chip) per la decomposizione query
    chip <- function(text, color) {
      tags$span(style = paste0(
        "display:inline-block; padding:3px 10px; margin:2px 3px;",
        "background:", color, "20; color:", color, "; border:1px solid ", color, "40;",
        "border-radius:14px; font-size:11px; font-weight:600; font-family:'JetBrains Mono';"
      ), text)
    }

    tagList(
      section_hdr(
        "Erre Quadro, Smart Glasses Exercise",
        HTML(paste0(
          "Full replication of the <strong>Erre Quadro, Smart Glasses Exercise</strong> ",
          "(7 tasks) conducted on <strong>Espacenet</strong> on 2026-05-20. ",
          "Main dataset: <strong>10,644 patents</strong> retrieved with the optimized query; ",
          "<strong>60 patents</strong> for the holographic query (prior art assessment)."
        ))
      ),

      # Methodology banner
      tags$div(
        class = "methodology-banner",
        style = paste0(
          "margin-bottom:20px; padding:18px 22px; border-radius:14px;",
          "background:", pastel_bg(col$accent), "; border-left:4px solid ", col$accent, ";"
        ),
        tags$div(style = paste0("font-size:11px; font-weight:800; letter-spacing:2px; text-transform:uppercase; color:", col$accent, ";"),
                 "Methodology"),
        tags$p(style = paste0("font-size:13px; color:", pastel_text(col$accent), "; line-height:1.6; margin:6px 0 0;"),
               HTML(paste0(
                 "<strong>Task 1-3</strong>: query design decomposed into core/synonyms/related tech/application/IPC/NOT, ",
                 "visualized as an AND/OR/NOT logic graph, and optimized. ",
                 "<strong>Task 4</strong>: extraction of top-20 patents (out of 10,644 total). ",
                 "<strong>Task 5</strong>: separate query on <em>holographic smart glasses</em> for prior art. ",
                 "<strong>Task 6</strong>: domain classification tree. ",
                 "<strong>Task 7</strong>: aggregate statistics (applicants, countries, priority year, IPC, inventors) ",
                 "extracted from <code>Filters → Charts/Graphs overview</code> and computed on the <strong>full 10,644-patent dataset</strong>."
               )))
      ),

      # ────────────────────────────── T1 ──────────────────────────────
      task_hdr("1", "Build a Research Query",
               "Structured decomposition into Core / Synonyms / Related / Application / IPC / NOT"),

      tags$div(class = "sci-card",
        tags$h4(style = "color:var(--text); margin-top:0; font-weight:800; font-size:14px;",
                "Espacenet query (decomposed)"),

        tags$div(style = "margin-bottom:10px;",
          tags$div(style = paste0("font-size:11px; font-weight:700; text-transform:uppercase; color:", col$accent, "; margin-bottom:4px;"), "Core keywords"),
          chip('"smart glasses"', col$accent), chip('"smart eyewear"', col$accent),
          chip('"audio glasses"', col$accent), chip('"hearing glasses"', col$accent)
        ),
        tags$div(style = "margin-bottom:10px;",
          tags$div(style = paste0("font-size:11px; font-weight:700; text-transform:uppercase; color:", col$blue, "; margin-bottom:4px;"), "Synonyms"),
          chip('"intelligent eyewear"', col$blue), chip('"AR glasses"', col$blue),
          chip('"augmented reality glasses"', col$blue), chip('"head mounted display"', col$blue)
        ),
        tags$div(style = "margin-bottom:10px;",
          tags$div(style = paste0("font-size:11px; font-weight:700; text-transform:uppercase; color:", col$purple, "; margin-bottom:4px;"), "Related technologies"),
          chip('"augmented reality"', col$purple), chip('"mixed reality"', col$purple),
          chip('"display"', col$purple), chip('"sensor"', col$purple),
          chip('"connectivity"', col$purple), chip('"heads-up"', col$purple),
          chip('"hands-free"', col$purple), chip('"beamforming"', col$purple)
        ),
        tags$div(style = "margin-bottom:10px;",
          tags$div(style = paste0("font-size:11px; font-weight:700; text-transform:uppercase; color:", col$orange, "; margin-bottom:4px;"), "Application fields"),
          chip('"wearable"', col$orange)
        ),
        tags$div(style = "margin-bottom:10px;",
          tags$div(style = paste0("font-size:11px; font-weight:700; text-transform:uppercase; color:", col$cyan, "; margin-bottom:4px;"), "IPC classes (top 5)"),
          chip("G02B27, Optical systems", col$cyan),
          chip("G06F3, I/O arrangements", col$cyan),
          chip("H04N5, Pictorial TV", col$cyan),
          chip("G09G5, Display indicators", col$cyan),
          chip("G06T19, 3D image manipulation", col$cyan)
        ),
        tags$div(style = "margin-bottom:14px;",
          tags$div(style = paste0("font-size:11px; font-weight:700; text-transform:uppercase; color:", col$red, "; margin-bottom:4px;"), "Exclusions (NOT)"),
          chip('"contact lens"', col$red), chip('"VR headset"', col$red),
          chip('"virtual reality headset"', col$red)
        ),

        tags$div(style = "font-size:11px; font-weight:700; text-transform:uppercase; color:var(--dim); margin-bottom:6px;",
                 "Full Espacenet query"),
        tags$pre(class = "query-code-light", style = "margin:0;",
                 HTML(paste0(
                   '(<span style="color:#00c98b">ti=</span>"smart glasses" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">ti=</span>"smart eyewear" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">ti=</span>"audio glasses" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">ti=</span>"hearing glasses" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">ti=</span>"AR glasses" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">ti=</span>"augmented reality glasses" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">ti=</span>"intelligent eyewear" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">ti=</span>"head mounted display")',
                   '\n  <span style="color:#7c3aed">AND</span> (<span style="color:#00c98b">txt=</span>"wearable" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">txt=</span>"augmented reality" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">txt=</span>"mixed reality" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">txt=</span>"display" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">txt=</span>"sensor" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">txt=</span>"connectivity" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">txt=</span>"heads-up" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">txt=</span>"hands-free" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">txt=</span>"beamforming")',
                   '\n  <span style="color:#dc2626">NOT</span> (<span style="color:#00c98b">ti=</span>"contact lens" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">ti=</span>"VR headset" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">ti=</span>"virtual reality headset")'
                 )))
      ),

      # ────────────────────────────── T2 ──────────────────────────────
      task_hdr("2", "Visualize AND / OR Logic",
               "Venn diagram: intersection between Core/Synonyms (title) and Related/Application (text), with NOT as exclusion"),

      tags$div(
        tags$div(style = "position:relative; width:100%; height:360px; background:#f1f5f9; border-radius:10px;",
          # Venn diagram - 3 circles + NOT zone
          tags$div(style = paste0(
            "position:absolute; left:13%; top:18%; width:38%; height:64%; border-radius:50%;",
            "background:", col$accent, "30; border:2px solid ", col$accent, "; ",
            "display:flex; align-items:center; justify-content:center;"),
            tags$div(style = "text-align:center;",
              tags$div(style = paste0("font-size:11px; font-weight:800; color:", col$accent, "; text-transform:uppercase; letter-spacing:1px;"), "TITLE"),
              tags$div(style = "font-size:13px; font-weight:700; color:#0f1724; margin-top:4px;", "Core + Synonyms"),
              tags$div(style = "font-size:10px; color:#475569; margin-top:2px; max-width:140px;", "smart glasses, smart eyewear, AR glasses, HMD…")
            )
          ),
          tags$div(style = paste0(
            "position:absolute; left:42%; top:18%; width:38%; height:64%; border-radius:50%;",
            "background:", col$purple, "30; border:2px solid ", col$purple, "; ",
            "display:flex; align-items:center; justify-content:center;"),
            tags$div(style = "text-align:center; margin-left:35%;",
              tags$div(style = paste0("font-size:11px; font-weight:800; color:", col$purple, "; text-transform:uppercase; letter-spacing:1px;"), "TEXT"),
              tags$div(style = "font-size:13px; font-weight:700; color:#0f1724; margin-top:4px;", "Related + Application"),
              tags$div(style = "font-size:10px; color:#475569; margin-top:2px; max-width:140px;", "wearable, AR, MR, display, sensor…")
            )
          ),
          # NOT badge
          tags$div(style = paste0(
            "position:absolute; right:3%; top:5%; padding:8px 14px;",
            "background:", col$red, "; color:#fff; border-radius:8px; font-weight:800;",
            "box-shadow:0 4px 12px rgba(0,0,0,0.15);"
          ),
            tags$i(class = "fas fa-ban"), " NOT ",
            tags$span(style = "font-weight:500; font-size:11px;", "contact lens · VR headset")
          ),
          # AND label
          tags$div(style = paste0(
            "position:absolute; left:47%; top:46%; transform:translateX(-50%); padding:6px 14px;",
            "background:#fff; color:#0f1724; border:2px solid #0f1724; border-radius:8px;",
            "font-weight:900; font-family:'JetBrains Mono'; font-size:14px;"
          ), "AND")
        ),
        tags$p(style = "font-size:12px; color:var(--dim); margin-top:14px; line-height:1.6;",
               HTML(paste0(
                 "<strong>Logic</strong>: the intersection (AND) between Title (Core + Synonyms) and Text ",
                 "(Related + Application) selects patents that <em>truly address</em> smart glasses ",
                 "(not merely those that mention the term generically). NOT removes known false positives (contact lens, VR headset)."
               )))
      ),

      # ────────────────────────────── T3 ──────────────────────────────
      task_hdr("3", "Optimize the Query",
               "From a broad initial query (~60k results) to the optimized query (10,644 patents)"),

      fluidRow(
        column(6, tags$div(class = "sci-card", style = paste0("border-left:4px solid ", col$red, ";"),
          tags$div(style = paste0("font-size:11px; font-weight:800; text-transform:uppercase; color:", col$red, "; letter-spacing:2px;"),
                   "Before, too broad"),
          tags$pre(class = "query-code-light", style = "margin:8px 0 0 0; font-size:11px;",
                   'ti="smart glasses" OR ti="smart eyewear"\n  OR ti="AR glasses"'),
          tags$div(style = "margin-top:10px;",
            sci_badge("~60k results", col$red),
            sci_badge("noisy / over-broad", col$red)
          ),
          tags$p(style = "font-size:11px; color:var(--dim); margin-top:10px; line-height:1.55;",
                 "Title only, no thematic filter, no NOT: includes patents for corrective lenses, generic VR, sunglasses with sensors.")
        )),
        column(6, tags$div(class = "sci-card", style = paste0("border-left:4px solid ", col$accent, ";"),
          tags$div(style = paste0("font-size:11px; font-weight:800; text-transform:uppercase; color:", col$accent, "; letter-spacing:2px;"),
                   "After, optimized"),
          tags$pre(class = "query-code-light", style = "margin:8px 0 0 0; font-size:11px;",
                   '(ti=core_terms) AND (txt=tech_application)\n  NOT (ti=exclusions)'),
          tags$div(style = "margin-top:10px;",
            sci_badge("10.644 results", col$accent),
            sci_badge("higher precision", col$accent),
            sci_badge("usable for top-20", col$accent)
          ),
          tags$p(style = "font-size:11px; color:var(--dim); margin-top:10px; line-height:1.55;",
                 "Added title × text intersection to ensure thematic relevance + NOT to eliminate known false positives.")
        ))
      ),

      # ────────────────────────────── T4 ──────────────────────────────
      task_hdr("4", "Espacenet Results, Top 20 Patents",
               "Out of 10,644 total patents, first 500 downloaded sorted by relevance; top 20 shown here as a sample"),

      tags$div(class = "row-stats",
               tags$div(stat_block("Total patents", "10 644", "Espacenet (2026-05-20)", col$accent)),
               tags$div(stat_block("Downloaded", "500", "CSV (Espacenet limit)", col$blue)),
               tags$div(stat_block("Top displayed", "20", "Sample for analysis", col$orange)),
               tags$div(stat_block("Priority span", "1972–2026", "44 years coverage", col$purple))
      ),
      tags$div(class = "sci-card", DT::DTOutput("table_eq_top20")),

      # ────────────────────────────── T5 ──────────────────────────────
      task_hdr("5", "Holographic Smart Glasses, Prior Art Search",
               "Focused query on 'holographic' for prior art product existence assessment"),

      tags$div(class = "sci-card",
        tags$div(style = "font-size:11px; font-weight:700; text-transform:uppercase; color:var(--dim); margin-bottom:6px;",
                 "Holographic prior art query"),
        tags$pre(class = "query-code-light", style = "margin:0;",
                 HTML(paste0(
                   '(<span style="color:#00c98b">ti=</span>"holographic" <span style="color:#7c3aed">AND</span> ',
                   '(<span style="color:#00c98b">ti=</span>"smart glasses" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">ti=</span>"smart eyewear" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">ti=</span>"head mounted display" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">ti=</span>"AR glasses" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">ti=</span>"augmented reality glasses"))',
                   '\n  <span style="color:#d97706">OR</span> (<span style="color:#00c98b">ti=</span>"holographic glasses" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">ti=</span>"holographic eyewear" <span style="color:#d97706">OR</span> ',
                   '<span style="color:#00c98b">ti=</span>"holographic HMD")'
                 )))
      ),

      tags$div(class = "row-stats",
               tags$div(stat_block("Holographic patents", "60", "Out of 10.644 main", col$cyan)),
               tags$div(stat_block("Ratio", "0.56%", "Sub-1% = prior art gap", col$pink)),
               tags$div(stat_block("First filing", min(as.integer(df_eq_holo$priority_year), na.rm = TRUE),
                                   "Mature concept", col$orange)),
               tags$div(stat_block("Latest filing", max(as.integer(df_eq_holo$priority_year), na.rm = TRUE),
                                   "Still active", col$accent))
      ),

      fluidRow(
        column(6, tags$div(class = "sci-card",
          tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;",
                   "Holographic filings by priority year"),
          plotlyOutput("plot_eq_holo_timeline", height = "260px")
        )),
        column(6, tags$div(class = "sci-card",
          tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;",
                   "Holographic filings by country"),
          plotlyOutput("plot_eq_holo_countries", height = "260px")
        ))
      ),

      insight_box(
        "Strategic Reading, Prior Art Assessment",
        paste0(
          "Of the 10,644 smart-glasses patents identified, only <strong style='font-weight:800'>60 (0.56%)</strong> ",
          "explicitly address <strong>holographic display</strong>. The sub-1% ratio confirms that ",
          "<em>integration of holographic displays in smart glasses is still a research niche</em>, ",
          "not a mature product. For Nuance Audio this means: <strong>(1)</strong> no technological lock-in ",
          "in the market; <strong>(2)</strong> open space to position as 'hearing-first' without competing ",
          "head-on with AR/VR display roadmaps; <strong>(3)</strong> an opportunity to monitor the 60 patent holders ",
          "(mostly universities and hardware startups) as an <em>early signal</em> for KIT 2 (early warnings)."
        ),
        col$cyan, "lightbulb"
      ),

      tags$div(class = "sci-card", DT::DTOutput("table_eq_holo")),

      # ────────────────────────────── T6 ──────────────────────────────
      task_hdr("6", "Classification Tree",
               "Domain decomposition of Smart Glasses by Components / Problems / Functionalities"),

      tags$div(class = "sci-card",
        fluidRow(
          # Components
          column(4,
            tags$div(style = paste0("padding:14px; border-radius:12px; background:", pastel_bg(col$blue), "; border-left:4px solid ", col$blue, ";"),
              tags$div(style = paste0("font-size:13px; font-weight:800; color:", pastel_text(col$blue), "; margin-bottom:10px;"),
                       tags$i(class = "fas fa-microchip"), " Components"),
              tags$ul(style = paste0("margin:0; padding-left:18px; color:", pastel_text(col$blue), "; font-size:12px; line-height:1.8;"),
                tags$li(tags$strong("Optical:"), " waveguide, micro-display, projector, lens, beam-splitter"),
                tags$li(tags$strong("Audio:"), " open-ear speaker, bone conduction, microphone array, beamforming chip"),
                tags$li(tags$strong("Sensors:"), " IMU, camera, ambient light, EMG, eye-tracking"),
                tags$li(tags$strong("Compute:"), " low-power SoC, edge AI accelerator, NPU"),
                tags$li(tags$strong("Connectivity:"), " BLE, Wi-Fi 6/7, UWB"),
                tags$li(tags$strong("Power:"), " micro-battery, energy harvesting"),
                tags$li(tags$strong("Frame:"), " lightweight composite, hinge, nose-pad")
              )
            )
          ),
          # Problems
          column(4,
            tags$div(style = paste0("padding:14px; border-radius:12px; background:", pastel_bg(col$red), "; border-left:4px solid ", col$red, ";"),
              tags$div(style = paste0("font-size:13px; font-weight:800; color:", pastel_text(col$red), "; margin-bottom:10px;"),
                       tags$i(class = "fas fa-triangle-exclamation"), " Problems to Solve"),
              tags$ul(style = paste0("margin:0; padding-left:18px; color:", pastel_text(col$red), "; font-size:12px; line-height:1.8;"),
                tags$li(tags$strong("Power:"), " battery life vs weight trade-off"),
                tags$li(tags$strong("Heat:"), " thermal management on temple"),
                tags$li(tags$strong("Display:"), " brightness outdoor, FOV, vergence-accommodation"),
                tags$li(tags$strong("Audio:"), " SNR, directional pickup, leakage"),
                tags$li(tags$strong("Privacy:"), " camera/mic always-on social acceptance"),
                tags$li(tags$strong("Comfort:"), " weight, fit, prescription compatibility"),
                tags$li(tags$strong("Stigma:"), " visibility of \"medical\" device"),
                tags$li(tags$strong("Cost:"), " BoM vs consumer price point")
              )
            )
          ),
          # Functionalities
          column(4,
            tags$div(style = paste0("padding:14px; border-radius:12px; background:", pastel_bg(col$accent), "; border-left:4px solid ", col$accent, ";"),
              tags$div(style = paste0("font-size:13px; font-weight:800; color:", pastel_text(col$accent), "; margin-bottom:10px;"),
                       tags$i(class = "fas fa-bolt"), " Functionalities"),
              tags$ul(style = paste0("margin:0; padding-left:18px; color:", pastel_text(col$accent), "; font-size:12px; line-height:1.8;"),
                tags$li(tags$strong("Hearing:"), " speech enhancement, beamforming, OTC hearing aid"),
                tags$li(tags$strong("Vision:"), " AR overlay, captioning, translation"),
                tags$li(tags$strong("Capture:"), " photo/video, lifelog, POV streaming"),
                tags$li(tags$strong("Navigation:"), " turn-by-turn, indoor wayfinding"),
                tags$li(tags$strong("Assistant:"), " voice AI, contextual prompts, dictation"),
                tags$li(tags$strong("Health:"), " heart rate, posture, fall detection"),
                tags$li(tags$strong("Productivity:"), " hands-free workflow, remote assist"),
                tags$li(tags$strong("Entertainment:"), " music, podcasts, gaming")
              )
            )
          )
        )
      ),

      insight_box(
        "Strategic Reading, Classification Tree",
        paste0(
          "Nuance Audio's positioning on the tree: <strong style='font-weight:800'>Hearing (functionality) + ",
          "Beamforming chip + Open-ear speaker + Microphone array (components)</strong>. ",
          "The <em>highest-impact problems</em> for go-to-market are <strong>Stigma</strong> ",
          "(differentiation vs traditional glasses) and <strong>Cost</strong> (the $1,200 vs $250 AirPods Pro gap). ",
          "<em>Adjacent functionalities easy to add</em> in future versions: speech captioning (AR overlay), ",
          "voice assistant, fall detection, already technologically covered by key player patents (Snap, Meta, Goertek)."
        ),
        col$accent, "sitemap"
      ),

      # ────────────────────────────── T7 ──────────────────────────────
      task_hdr("7", "Statistics on Full Dataset (10.644 patent)",
               HTML("Extracted from <code>Filters → View charts/graphs overview</code> in Espacenet. ",
                    "Exact figures on the full dataset (not just the 500 downloaded).")),

      # Stats row
      tags$div(class = "row-stats",
               tags$div(stat_block("Top applicant", "Goertek Tech",
                                   paste0(format(df_eq_applicants$count[1], big.mark=" "), " patents"), col$accent)),
               tags$div(stat_block("Top country (app.)", df_eq_app_country$country_name[1],
                                   paste0(format(df_eq_app_country$count[1], big.mark=" "), " patents"), col$orange)),
               tags$div(stat_block("Peak priority year",
                                   df_eq_priority_year$year[which.max(df_eq_priority_year$count)],
                                   paste0(max(df_eq_priority_year$count), " filings"), col$purple)),
               tags$div(stat_block("Top IPC", df_eq_ipc$ipc[1],
                                   paste0(format(df_eq_ipc$count[1], big.mark=" "), " — ", df_eq_ipc$label[1]), col$pink)),
               tags$div(stat_block("Top inventor", df_eq_inventors$name[1],
                                   paste0(df_eq_inventors$count[1], " patents"), col$blue))
      ),

      # Priority year
      section_hdr("Priority year distribution",
                  "Temporal dynamics of IP filings (2000–2025)"),
      tags$div(class = "sci-card", plotlyOutput("plot_eq_priority_year", height = "300px")),

      # 2x2 grid
      fluidRow(
        column(6, tags$div(class = "sci-card",
          tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;",
                   "Top 15 Applicants"),
          plotlyOutput("plot_eq_applicants", height = "400px")
        )),
        column(6, tags$div(class = "sci-card",
          tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;",
                   "Top 15 Inventors"),
          plotlyOutput("plot_eq_inventors", height = "400px")
        ))
      ),
      fluidRow(
        column(6, tags$div(class = "sci-card",
          tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;",
                   "Top 15 Countries (applicants)"),
          plotlyOutput("plot_eq_countries", height = "380px")
        )),
        column(6, tags$div(class = "sci-card",
          tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;",
                   "Top 15 IPC classes"),
          plotlyOutput("plot_eq_ipc", height = "380px")
        ))
      ),

      insight_box(
        "Strategic Reading, Patent Landscape",
        paste0(
          "The 10,644-patent dataset confirms what was already highlighted in the Patent Analysis tab, but at a larger scale: ",
          "<strong style='font-weight:800'>Goertek (China, 447+307+259 = 1,013 patents across its 3 entities)</strong> ",
          "is the <em>true IP leader</em>, followed by Seiko Epson (374) and HTC (217). ",
          "Nationally, <strong>US (3,218), JP (2,590), KR (1,975), CN (1,395)</strong> dominate: ",
          "Europe is marginal (DE 290, GB 101, FR 59), confirming the asymmetric competitive risk for EssilorLuxottica. ",
          "The <strong>2021 peak (74 priority filings)</strong> coincides with the Ray-Ban Stories launch: post-pandemic, ",
          "the entire ecosystem accelerated. <strong>G02B27 (Optical systems, 7,760 patents, 73% of the total)</strong> is the ",
          "technologically dominant class: the true battleground is the <em>optical stack</em>, not audio."
        ),
        col$accent, "chart-pie"
      )
    )
  })

  # ════════════════════════════════════════════════════════════════════════════
  # TAB 9: STRATEGIC STORYBOARD (DATA VIZ LAB FINAL — RIFORMULATA)
  # ════════════════════════════════════════════════════════════════════════════
  si_box <- function(text, color) {
    tags$div(
      style = paste0("margin-top:14px; padding:10px 14px; border-radius:8px; border-left:3px solid ", color, "; background:", color, "12;"),
      tags$div(style = paste0("font-size:10px; text-transform:uppercase; font-weight:700; color:", color, "; letter-spacing:1px; margin-bottom:4px;"),
               tags$i(class = "fas fa-lightbulb", `aria-hidden` = "true"), " Strategic Insight"),
      tags$p(style = "font-size:12px; color:var(--dim); margin:0; line-height:1.55;", text)
    )
  }
  
  output$tab_storyboard <- renderUI({
    tagList(
      tags$style(HTML(paste0("
        #kiq_nav .nav-item:nth-child(1) .nav-link         { color:", col$accent, " !important; }
        #kiq_nav .nav-item:nth-child(1) .nav-link.active  { background:rgba(0,229,160,.12) !important; border-color:rgba(0,229,160,.25) !important; color:", col$accent, " !important; }
        #kiq_nav .nav-item:nth-child(2) .nav-link         { color:", col$cyan, " !important; }
        #kiq_nav .nav-item:nth-child(2) .nav-link.active  { background:rgba(34,211,238,.12) !important; border-color:rgba(34,211,238,.25) !important; color:", col$cyan, " !important; }
        #kiq_nav .nav-item:nth-child(3) .nav-link         { color:", col$orange, " !important; }
        #kiq_nav .nav-item:nth-child(3) .nav-link.active  { background:rgba(255,159,67,.12) !important; border-color:rgba(255,159,67,.25) !important; color:", col$orange, " !important; }
      "))),
      section_hdr("Data Visualization", "Strategic Storytelling & Visual Communication Map"),
      
      navset_pill(
        id = "kiq_nav",
        
        nav_panel(
          title = tagList(icon("chart-line"), " KIQ 1: Consumer Acceptance & Adoption Barriers"),
          value = "kiq1",
          tags$div(style = "padding-top:18px;",
            tags$div(class = "sci-card",
              style = paste0("border-top:3px solid ", col$accent, "; background:", pastel_bg(col$accent), "; margin-bottom:16px;"),

              # KIT → KIQ provenance row
              tags$div(style = "display:flex; align-items:center; gap:8px; margin-bottom:12px; flex-wrap:wrap;",
                tags$div(
                  style = paste0("display:inline-flex; align-items:center; gap:6px; padding:4px 10px;",
                                 " background:", col$accent, "20; border:1px solid ", col$accent, "50;",
                                 " border-radius:20px;"),
                  tags$i(class = "fas fa-layer-group", style = paste0("font-size:10px; color:", col$accent, ";")),
                  tags$span(style = paste0("font-size:10px; font-weight:700; color:", col$accent,
                                           "; text-transform:uppercase; letter-spacing:0.8px;"),
                            "KIT 1: Strategic Decisions & Actions")
                ),
                tags$i(class = "fas fa-arrow-right",
                       style = paste0("font-size:10px; color:", pastel_text(col$accent), "60;")),
                tags$div(
                  style = paste0("display:inline-flex; align-items:center; gap:6px; padding:4px 10px;",
                                 " background:", pastel_text(col$accent), "12; border:1px solid ",
                                 pastel_text(col$accent), "30; border-radius:20px;"),
                  tags$i(class = "fas fa-circle-question", style = paste0("font-size:10px; color:", pastel_text(col$accent), ";")),
                  tags$span(style = paste0("font-size:10px; font-weight:700; color:", pastel_text(col$accent),
                                           "; text-transform:uppercase; letter-spacing:0.8px;"),
                            "KIQ 1: Consumer Acceptance & Adoption Barriers")
                )
              ),

              tags$hr(style = paste0("border:none; border-top:1px solid ", col$accent, "25; margin:0 0 12px;")),
              tags$h5(style = paste0("color:", pastel_text(col$accent), "; font-weight:700; font-size:15px; margin:0 0 10px;"),
                      tags$i(class = "fas fa-circle-question", style = paste0("font-size:13px; color:", col$accent, "; margin-right:6px;")),
                      "Do consumer acceptance and stigma-related topics (T3: Stigma & Acceptance + T6: Eyewear UX) show a growing trend in the hearing-glasses literature between 2020 and 2025, confirming adoption barriers as the primary strategic challenge for Nuance Audio?"),
              tags$div(style = paste0("background:", col$accent, "15; padding:10px 14px; border-radius:6px; border-left:4px solid ", col$accent, ";"),
                tags$span(style = paste0("font-size:10px; text-transform:uppercase; color:", pastel_text(col$accent), "80; font-weight:700;"),
                          tags$i(class = "fas fa-bolt", style = "margin-right:4px;"), "Potential Decision: "),
                tags$span(style = paste0("font-size:12px; color:", pastel_text(col$accent), ";"),
                          "Yes: T3 (Stigma & Acceptance) and T6 (Eyewear UX) are growing clusters in the 2020–2025 period. Nuance Audio's go-to-market must address perception and lifestyle positioning before clinical performance.")
              )
            ),
            fluidRow(
              column(6,
                tags$div(class = "sci-card",
                  tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;",
                           "BERTopic — Topic Distribution (Scopus 2018–2025)"),
                  plotlyOutput("plot_kiq1_a", height = "230px"), uiOutput("si_kiq1_a")
                )
              ),
              column(6,
                tags$div(class = "sci-card",
                  tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;",
                           "Topic Evolution Over Time (2018–2025)"),
                  plotlyOutput("plot_kiq1_b", height = "230px"), uiOutput("si_kiq1_b")
                )
              )
            )
          )
        ),

        nav_panel(
          title = tagList(icon("users"), " KIQ 2: Social Stigma & Barriers"),
          value = "kiq2",
          tags$div(style = "padding-top:18px;",
            tags$div(class = "sci-card",
              style = paste0("border-top:3px solid ", col$cyan, "; background:", pastel_bg(col$cyan), "; margin-bottom:16px;"),

              # KIT → KIQ provenance row
              tags$div(style = "display:flex; align-items:center; gap:8px; margin-bottom:12px; flex-wrap:wrap;",
                tags$div(
                  style = paste0("display:inline-flex; align-items:center; gap:6px; padding:4px 10px;",
                                 " background:", col$cyan, "20; border:1px solid ", col$cyan, "50;",
                                 " border-radius:20px;"),
                  tags$i(class = "fas fa-layer-group", style = paste0("font-size:10px; color:", col$cyan, ";")),
                  tags$span(style = paste0("font-size:10px; font-weight:700; color:", col$cyan,
                                           "; text-transform:uppercase; letter-spacing:0.8px;"),
                            "KIT 1: Strategic Decisions & Actions")
                ),
                tags$i(class = "fas fa-arrow-right",
                       style = paste0("font-size:10px; color:", pastel_text(col$cyan), "60;")),
                tags$div(
                  style = paste0("display:inline-flex; align-items:center; gap:6px; padding:4px 10px;",
                                 " background:", pastel_text(col$cyan), "12; border:1px solid ",
                                 pastel_text(col$cyan), "30; border-radius:20px;"),
                  tags$i(class = "fas fa-circle-question", style = paste0("font-size:10px; color:", pastel_text(col$cyan), ";")),
                  tags$span(style = paste0("font-size:10px; font-weight:700; color:", pastel_text(col$cyan),
                                           "; text-transform:uppercase; letter-spacing:0.8px;"),
                            "KIQ 2: User Adoption & Lifestyle Barriers")
                )
              ),

              tags$hr(style = paste0("border:none; border-top:1px solid ", col$cyan, "25; margin:0 0 12px;")),
              tags$h5(style = paste0("color:", pastel_text(col$cyan), "; font-weight:700; font-size:15px; margin:0 0 10px;"),
                      tags$i(class = "fas fa-circle-question", style = paste0("font-size:13px; color:", col$cyan, "; margin-right:6px;")),
                      "What are the primary non-clinical barriers affecting the adoption of wearable hearing technologies?"),
              tags$div(style = paste0("background:", col$cyan, "15; padding:10px 14px; border-radius:6px; border-left:4px solid ", col$cyan, ";"),
                tags$span(style = paste0("font-size:10px; text-transform:uppercase; color:", pastel_text(col$cyan), "80; font-weight:700;"),
                          tags$i(class = "fas fa-bolt", style = "margin-right:4px;"), "Potential Decision: "),
                tags$span(style = paste0("font-size:12px; color:", pastel_text(col$cyan), ";"),
                          "Push marketing communication towards fashion aesthetics rather than the medical aspects of the device.")
              )
            ),
            fluidRow(
              column(6,
                tags$div(class = "sci-card",
                  tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;",
                           "Academic Focus on UX & Stigma (Trend)"),
                  plotlyOutput("plot_kiq2_a", height = "230px"), uiOutput("si_kiq2_a")
                )
              ),
              column(6,
                tags$div(class = "sci-card",
                  tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;",
                           "BERTopic Profile (Topic 3: Stigma & Acceptance)"),
                  plotlyOutput("plot_kiq2_b", height = "230px"), uiOutput("si_kiq2_b")
                )
              )
            )
          )
        ),

        nav_panel(
          title = tagList(icon("shield-alt"), " KIQ 3: Big Tech Asymmetric Threat"),
          value = "kiq3",
          tags$div(style = "padding-top:18px;",
            tags$div(class = "sci-card",
              style = paste0("border-top:3px solid ", col$orange, "; background:", pastel_bg(col$orange), "; margin-bottom:16px;"),

              # KIT → KIQ provenance row
              tags$div(style = "display:flex; align-items:center; gap:8px; margin-bottom:12px; flex-wrap:wrap;",
                tags$div(
                  style = paste0("display:inline-flex; align-items:center; gap:6px; padding:4px 10px;",
                                 " background:", col$orange, "20; border:1px solid ", col$orange, "50;",
                                 " border-radius:20px;"),
                  tags$i(class = "fas fa-layer-group", style = paste0("font-size:10px; color:", col$orange, ";")),
                  tags$span(style = paste0("font-size:10px; font-weight:700; color:", col$orange,
                                           "; text-transform:uppercase; letter-spacing:0.8px;"),
                            "KIT 2: Early Warnings")
                ),
                tags$i(class = "fas fa-arrow-right",
                       style = paste0("font-size:10px; color:", pastel_text(col$orange), "60;")),
                tags$div(
                  style = paste0("display:inline-flex; align-items:center; gap:6px; padding:4px 10px;",
                                 " background:", pastel_text(col$orange), "12; border:1px solid ",
                                 pastel_text(col$orange), "30; border-radius:20px;"),
                  tags$i(class = "fas fa-circle-question", style = paste0("font-size:10px; color:", pastel_text(col$orange), ";")),
                  tags$span(style = paste0("font-size:10px; font-weight:700; color:", pastel_text(col$orange),
                                           "; text-transform:uppercase; letter-spacing:0.8px;"),
                            "KIQ 3: Early Warnings on Competitive Disruption")
                )
              ),

              tags$hr(style = paste0("border:none; border-top:1px solid ", col$orange, "25; margin:0 0 12px;")),
              tags$h5(style = paste0("color:", pastel_text(col$orange), "; font-weight:700; font-size:15px; margin:0 0 10px;"),
                      tags$i(class = "fas fa-circle-question", style = paste0("font-size:13px; color:", col$orange, "; margin-right:6px;")),
                      "Which competitors and emerging technologies are building an asymmetric advantage in the sector?"),
              tags$div(style = paste0("background:", col$orange, "15; padding:10px 14px; border-radius:6px; border-left:4px solid ", col$orange, ";"),
                tags$span(style = paste0("font-size:10px; text-transform:uppercase; color:", pastel_text(col$orange), "80; font-weight:700;"),
                          tags$i(class = "fas fa-bolt", style = "margin-right:4px;"), "Potential Decision: "),
                tags$span(style = paste0("font-size:12px; color:", pastel_text(col$orange), ";"),
                          "Accelerate software M&A and proprietary AI investments to counter the imminent roll-out of hybrid features by Meta and Apple.")
              )
            ),
            fluidRow(
              column(6,
                tags$div(class = "sci-card",
                  tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;",
                           "BERTopic: Topics Dynamics (2018-2025)"),
                  plotlyOutput("plot_kiq3_a", height = "230px"), uiOutput("si_kiq3_a")
                )
              ),
              column(6,
                tags$div(class = "sci-card",
                  tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;",
                           "Espacenet: Top Corporate Patent Assignees"),
                  plotlyOutput("plot_kiq3_b", height = "230px"), uiOutput("si_kiq3_b")
                )
              )
            )
          )
        )
      )
    )
  })
  
  output$si_kiq1_a <- renderUI({ si_box("T3 (Stigma & Acceptance) and T6 (Eyewear UX) are the two largest consumer-facing clusters in the corpus, confirming that adoption barriers dominate the scientific agenda on hearing glasses.", col$accent) })
  output$si_kiq1_b <- renderUI({ si_box("T3 and T6 show a clear upward trend from 2020 to 2025, while purely technical topics (T0, T2) plateau. This confirms that consumer acceptance — not technology — is the primary strategic challenge for Nuance Audio.", col$accent) })
  output$si_kiq2_a <- renderUI({ si_box("Longitudinal analysis shows that academic focus on user experience and social barriers has surged post-2022, perfectly aligning with the launch of lifestyle-oriented hybrid devices.", col$cyan) })
  output$si_kiq2_b <- renderUI({ si_box("BERTopic extraction isolates a specific semantic cluster (Topic 3) dedicated to 'stigma', 'acceptance', and 'adoption', proving that the main obstacle is a psychological and aesthetic barrier.", col$cyan) })
  output$si_kiq3_a <- renderUI({ si_box("BERTopic monitoring reveals the explosion of AR and AI Captioning (Topic 1), the technological backbone of Big Tech, which is currently outpacing traditional open-ear audio research.", col$orange) })
  output$si_kiq3_b <- renderUI({ si_box("Espacenet data highlights a critical vulnerability: tech giants (Snap, Meta) completely dominate the IP landscape, while traditional optical leaders (EssilorLuxottica) lag significantly behind.", col$orange) })
  
  output$plot_kiq1_a <- renderPlotly({
    df <- df_lab5_topics[order(-df_lab5_topics$count), ]
    df$short_label <- paste0("T", df$topic, " — ", substr(df$label, 1, 40), ifelse(nchar(df$label) > 40, "…", ""))
    df <- df[order(df$count), ]
    plot_ly(df,
            x = ~count, y = ~factor(short_label, levels = short_label),
            type = "bar", orientation = "h",
            marker = list(color = col$purple, line = list(width = 0)),
            text = ~count, textposition = "outside",
            textfont = list(color = col$dim, size = 9),
            hovertemplate = "%{y}<br><b>%{x}</b> documents<extra></extra>") %>%
      plotly_layout_dark(
        xaxis = list(title = "Documents", tickfont = list(size = 10)),
        yaxis = list(title = "", tickfont = list(size = 9)),
        margin = list(l = 280, r = 40, t = 10, b = 30)
      )
  })

  output$plot_kiq1_b <- renderPlotly({
    palette_topics <- c(col$accent, col$blue, col$orange, col$purple, col$cyan, col$pink, col$red)
    p <- plot_ly()
    for (i in 0:6) {
      sub <- df_lab5_topics_time[df_lab5_topics_time$topic == i, ]
      p <- p %>% add_trace(
        x = sub$year, y = sub$count,
        name = paste0("T", i, " — ", df_lab5_topics$label[i + 1]),
        type = "scatter", mode = "lines+markers",
        line   = list(color = palette_topics[i + 1], width = 2),
        marker = list(color = palette_topics[i + 1], size = 5),
        hovertemplate = "%{x}: %{y} docs<extra></extra>"
      )
    }
    p %>% plotly_layout_dark(
      xaxis = list(title = "", dtick = 1, tickfont = list(size = 10)),
      yaxis = list(title = "Documents", tickfont = list(size = 10)),
      legend = list(orientation = "h", y = -0.35, font = list(size = 9)),
      margin = list(l = 40, r = 20, t = 10, b = 60)
    )
  })
  
  output$plot_kiq2_a <- renderPlotly({
    df <- head(df_lab5_bigrams[order(-df_lab5_bigrams$n), ], 10)
    df <- df[order(df$n), ]
    plot_ly(df, x = ~n, y = ~factor(pair, levels = pair), type = "bar", orientation = "h", marker = list(color = paste0(col$accent, "cc")), text = ~n, textposition = "outside") %>%
      plotly_layout_dark(xaxis = list(title = "Count", tickfont = list(size = 10)), yaxis = list(title = "", tickfont = list(size = 10)), margin = list(l = 160, r = 40, t = 10, b = 30))
  })
  
  # ── CORREZIONE APPLICATA AL GRAFICO KIQ2-B ──
  output$plot_kiq2_b <- renderPlotly({
    df <- head(df_lab5_keywords[order(-df_lab5_keywords$n), ], 12)
    df <- df[order(df$n), ]                                        
    plot_ly(df, x = ~n, y = ~factor(keyword, levels = keyword), type = "bar", orientation = "h", marker = list(color = paste0(col$purple, "cc")), text = ~n, textposition = "outside") %>%
      plotly_layout_dark(xaxis = list(title = "Frequency", tickfont = list(size = 10)), yaxis = list(title = "", tickfont = list(size = 10)), margin = list(l = 160, r = 40, t = 10, b = 30))
  })
  
  output$plot_kiq3_a <- renderPlotly({
    palette_topics <- c(col$accent, col$blue, col$orange, col$purple)
    short_labels <- c("T0 Open-ear", "T1 AR Captions", "T2 Beamforming", "T3 Stigma")
    p <- plot_ly()
    for (i in 0:3) {
      sub <- df_lab5_topics_time[df_lab5_topics_time$topic == i, ]
      p <- p %>% add_trace(x = sub$year, y = sub$count, name = short_labels[i + 1], type = "scatter", mode = "lines+markers", line = list(color = palette_topics[i + 1], width = 2.5))
    }
    p %>% plotly_layout_dark(xaxis = list(title = "", tickfont = list(size = 10)), yaxis = list(title = "Docs", tickfont = list(size = 10)), legend = list(orientation = "h", x = 0, y = -0.3, font = list(size = 9)), margin = list(l = 40, r = 20, t = 10, b = 50))
  })
  
  output$plot_kiq3_b <- renderPlotly({
    df <- df_pat_assignee[order(df_pat_assignee$Count), ]
    plot_ly(df, x = ~Count, y = ~factor(Assignee, levels = Assignee), type = "bar", orientation = "h", marker = list(color = col$orange), text = ~Count, textposition = "outside") %>%
      plotly_layout_dark(xaxis = list(title = "Patents", tickfont = list(size = 10)), yaxis = list(title = "", tickfont = list(size = 10)), margin = list(l = 150, r = 65, t = 10, b = 30))
  })

  # ──────────────────────────────────────────────────────────────────────────────
  # TAB EXECUTIVE SUMMARY
  # ──────────────────────────────────────────────────────────────────────────────
  output$tab_executive <- renderUI({

    bullet <- function(icon_name, text, color) {
      tags$div(
        style = "display:flex; gap:8px; align-items:flex-start; margin-bottom:7px;",
        tags$i(class = paste0("fas fa-", icon_name),
               style = paste0("color:", color, "; font-size:12px; margin-top:2px; flex-shrink:0;")),
        tags$span(style = paste0("font-size:12px; color:", pastel_text(color), "; line-height:1.5;"), HTML(text))
      )
    }

    kiq_card <- function(kiq_label, icon_name, title, b1, b2, b3, color) {
      tags$div(
        class = "sci-card",
        style = paste0("border-top:3px solid ", color, "; background:", pastel_bg(color), "; height:100%;"),
        tags$div(
          style = "text-align:center; margin-bottom:12px;",
          tags$div(
            style = paste0("display:inline-flex; align-items:center; justify-content:center;",
                           " width:48px; height:48px; border-radius:50%;",
                           " background:", color, "22; border:2px solid ", color, "55; margin-bottom:8px;"),
            tags$i(class = paste0("fas fa-", icon_name),
                   style = paste0("font-size:22px; color:", color, ";"))
          ),
          tags$div(style = paste0("font-size:9px; font-weight:700; letter-spacing:2px;",
                                  " text-transform:uppercase; color:", color, ";"), kiq_label),
          tags$div(style = paste0("font-size:13px; font-weight:700; color:", pastel_text(color),
                                  "; margin-top:6px; line-height:1.35;"), title)
        ),
        bullet("check-circle", b1, color),
        bullet("check-circle", b2, color),
        bullet("check-circle", b3, color)
      )
    }

    rec_card <- function(icon_name, title, b1, b2, color) {
      tags$div(
        class = "sci-card",
        style = paste0("border-left:4px solid ", color, "; background:", pastel_bg(color), ";"),
        tags$div(
          style = "display:flex; align-items:center; gap:10px; margin-bottom:10px;",
          tags$div(
            style = paste0("flex-shrink:0; width:36px; height:36px; border-radius:10px;",
                           " background:", color, "22; border:1px solid ", color, "50;",
                           " display:flex; align-items:center; justify-content:center;"),
            tags$i(class = paste0("fas fa-", icon_name),
                   style = paste0("font-size:16px; color:", color, ";"))
          ),
          tags$span(style = paste0("font-size:13px; font-weight:700; color:", pastel_text(color), ";"), title)
        ),
        bullet("arrow-right", b1, color),
        bullet("arrow-right", b2, color)
      )
    }

    msg_card <- function(icon_name, title, text, color) {
      tags$div(
        class = "sci-card",
        style = paste0("border-top:3px solid ", color, "; background:", pastel_bg(color),
                       "; text-align:center; height:100%;"),
        tags$i(class = paste0("fas fa-", icon_name),
               style = paste0("font-size:30px; color:", color, "; margin-bottom:10px; display:block;")),
        tags$div(style = paste0("font-size:13px; font-weight:700; color:", pastel_text(color),
                                "; margin-bottom:8px;"), title),
        tags$p(style = paste0("font-size:12px; color:", pastel_text(color), "cc; line-height:1.55; margin:0;"),
               HTML(text))
      )
    }

    tagList(
      section_hdr("Executive Summary", "What this analysis found and what Nuance Audio should do next"),

      # ── Motivational banner ──────────────────────────────────────────────────
      tags$div(
        class = "sci-card",
        style = paste0(
          "background:linear-gradient(135deg, #0f172a 0%, #1e293b 60%, #0f2a1e 100%);",
          " border:1px solid ", col$accent, "40;",
          " text-align:center; padding:32px 28px; margin-bottom:4px;"
        ),
        tags$div(
          style = paste0("font-size:11px; font-weight:700; letter-spacing:3px; text-transform:uppercase;",
                         " color:", col$accent, "; margin-bottom:14px;"),
          tags$i(class = "fas fa-quote-left", style = "margin-right:6px;"),
          "Strategic Vision",
          tags$i(class = "fas fa-quote-right", style = "margin-left:6px;")
        ),
        tags$div(
          style = paste0(
            "font-size:22px; font-weight:900; line-height:1.35; margin-bottom:14px;",
            " background:linear-gradient(90deg, white 0%, ", col$accent, " 100%);",
            " -webkit-background-clip:text; -webkit-text-fill-color:transparent; background-clip:text;"
          ),
          "In a market defined by stigma and disruption,", tags$br(),
          "the brand that makes hearing invisible", tags$br(),
          "will make itself unforgettable."
        ),
        tags$p(
          style = "font-size:13px; color:#94a3b8; line-height:1.65; max-width:640px; margin:0 auto;",
          "Nuance Audio sits at the intersection of medical-grade hearing technology and premium optical frames.",
          tags$br(),
          "The window is open. The science is validated. The main threat is a perception battle that can be won now."
        )
      ),

      # ── Key metrics row ──────────────────────────────────────────────────────
      tags$div(style = "margin-bottom:4px;",
        fluidRow(
          column(3, tags$div(
            class = "stat-block",
            style = paste0("border-top:3px solid ", col$accent, "; background:", pastel_bg(col$accent), "; text-align:center;"),
            tags$i(class = "fas fa-file-lines", style = paste0("font-size:22px; color:", col$accent, "; margin-bottom:4px; display:block;")),
            tags$div(class = "stat-value", style = paste0("color:", col$accent, ";"), "200+"),
            tags$div(class = "stat-label", style = paste0("color:", pastel_text(col$accent), "99;"), "Scientific papers"),
            tags$div(class = "stat-sub",   style = paste0("color:", pastel_text(col$accent), ";"), "Scopus, 2024")
          )),
          column(3, tags$div(
            class = "stat-block",
            style = paste0("border-top:3px solid ", col$blue, "; background:", pastel_bg(col$blue), "; text-align:center;"),
            tags$i(class = "fas fa-certificate", style = paste0("font-size:22px; color:", col$blue, "; margin-bottom:4px; display:block;")),
            tags$div(class = "stat-value", style = paste0("color:", col$blue, ";"), "849"),
            tags$div(class = "stat-label", style = paste0("color:", pastel_text(col$blue), "99;"), "Active patents"),
            tags$div(class = "stat-sub",   style = paste0("color:", pastel_text(col$blue), ";"), "Espacenet IP landscape")
          )),
          column(3, tags$div(
            class = "stat-block",
            style = paste0("border-top:3px solid ", col$orange, "; background:", pastel_bg(col$orange), "; text-align:center;"),
            tags$i(class = "fas fa-crown", style = paste0("font-size:22px; color:", col$orange, "; margin-bottom:4px; display:block;")),
            tags$div(class = "stat-value", style = paste0("color:", col$orange, ";"), "128"),
            tags$div(class = "stat-label", style = paste0("color:", pastel_text(col$orange), "99;"), "Patents: Snap Inc."),
            tags$div(class = "stat-sub",   style = paste0("color:", pastel_text(col$orange), ";"), "Top IP holder in sector")
          )),
          column(3, tags$div(
            class = "stat-block",
            style = paste0("border-top:3px solid ", col$cyan, "; background:", pastel_bg(col$cyan), "; text-align:center;"),
            tags$i(class = "fas fa-brain", style = paste0("font-size:22px; color:", col$cyan, "; margin-bottom:4px; display:block;")),
            tags$div(class = "stat-value", style = paste0("color:", col$cyan, ";"), "#1"),
            tags$div(class = "stat-label", style = paste0("color:", pastel_text(col$cyan), "99;"), "Barrier: stigma"),
            tags$div(class = "stat-sub",   style = paste0("color:", pastel_text(col$cyan), ";"), "Psychological, not clinical")
          ))
        )
      ),

      # ── KIQ answers ──────────────────────────────────────────────────────────
      section_hdr("Key Intelligence Questions", "Three questions, three evidence-based answers"),
      fluidRow(style = "margin-bottom:4px;",
        column(4,
          kiq_card(
            "KIQ 1: Market Technology Window", "chart-line",
            "Is the R&D landscape ready?",
            "Scopus: exponential growth, 200+ papers published in 2024 alone",
            "Patents: massive industrial investment, post-2021 dip is a 18-month blind spot artifact",
            "Verdict: the technology window is open <strong>right now</strong>",
            col$accent
          )
        ),
        column(4,
          kiq_card(
            "KIQ 2: Social Stigma & Barriers", "users",
            "What blocks consumer adoption?",
            "BERTopic isolates a stigma/acceptance/adoption cluster as the dominant theme",
            "Academic focus on social barriers surged sharply post-2022",
            "Verdict: win through <strong>design and lifestyle</strong>, not audiological specs",
            col$cyan
          )
        ),
        column(4,
          kiq_card(
            "KIQ 3: Big Tech Asymmetric Threat", "shield-alt",
            "Who is building structural advantage?",
            "Snap Inc. holds 128 patents; Meta Platforms holds 43 in this space",
            "AR and AI Captioning (BERTopic T1) is outpacing open-ear audio research",
            "Verdict: the threat is asymmetric and <strong>already underway</strong>",
            col$orange
          )
        )
      ),

      # ── Recommendations ───────────────────────────────────────────────────────
      section_hdr("Strategic Recommendations", "Four priority actions to consolidate and defend Nuance Audio's position"),
      fluidRow(
        column(6,
          rec_card("rocket", "Launch now with a fashion-first narrative",
            "Lead with <strong>design aesthetics and lifestyle identity</strong>, not hearing aid specs",
            "First-mover advantage is still available but the window is closing fast",
            col$accent)
        ),
        column(6,
          rec_card("store", "Weaponize the EssilorLuxottica distribution moat",
            "<strong>18,000 optical retail stores</strong> (LensCrafters, Pearle Vision) no tech startup can replicate",
            "Deepen exclusivity agreements to turn the network into a structural entry barrier",
            col$blue)
        )
      ),
      tags$div(style = "height:8px;"),
      fluidRow(
        column(6,
          rec_card("microchip", "Invest in software and AI defensibility",
            "Meta and Apple are building AI captioning that will commoditize the hardware layer",
            "Develop <strong>proprietary adaptive audio AI</strong> that a software update cannot replicate",
            col$orange)
        ),
        column(6,
          rec_card("shield-halved", "Make FDA certification the cornerstone of trust",
            "Apple, Snap, and Meta cannot claim medical clearance without 3-5 years of regulatory process",
            "Anchor every B2B healthcare message and B2C trust narrative on this credential",
            col$purple)
        )
      ),

      # ── Key Messages ──────────────────────────────────────────────────────────
      section_hdr("Key Messages", "What to remember from this intelligence report"),
      fluidRow(
        column(4, msg_card("lock", "The moat is the bundle",
          "No competitor combines <strong>FDA certification + invisible design + optical distribution</strong> in a single product. Protect all three.",
          col$accent)),
        column(4, msg_card("triangle-exclamation", "The threat is asymmetric",
          "The danger comes from <strong>lifestyle tech companies</strong>, not traditional hearing aid makers. The arena has shifted to consumer electronics.",
          col$orange)),
        column(4, msg_card("eye-slash", "The data gap is a warning",
          "The 18-month patent blind spot means Big Tech's build-up is <strong>already stronger</strong> than Espacenet currently shows.",
          col$blue))
      ),

      # ── Closing banner ────────────────────────────────────────────────────────
      tags$div(style = "height:8px;"),
      tags$div(
        class = "sci-card",
        style = paste0(
          "background:", pastel_bg(col$accent), ";",
          " border:1px solid ", col$accent, "40;",
          " text-align:center; padding:22px 28px;"
        ),
        tags$div(
          style = "display:flex; align-items:center; justify-content:center; gap:12px; margin-bottom:8px;",
          tags$i(class = "fas fa-flag",        style = paste0("font-size:22px; color:", col$accent, ";")),
          tags$i(class = "fas fa-arrow-right", style = paste0("font-size:14px; color:", col$accent, "80;")),
          tags$i(class = "fas fa-bullseye",    style = paste0("font-size:22px; color:", col$accent, ";"))
        ),
        tags$div(
          style = paste0("font-size:15px; font-weight:800; color:", pastel_text(col$accent), ";"),
          "The intelligence is clear. The strategy is coherent. The next move belongs to Nuance Audio."
        )
      )
    )
  })

} # <--- FINE DEL FILE SERVER.R (CHIUSURA PERFETTA E CORRETTA)
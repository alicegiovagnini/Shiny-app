# ═══════════════════════════════════════════════════════════════════════════════
# SERVER
# ═══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  # ── Sincronizza la sidebar con l'area principale ──
  observeEvent(input$main_nav, {
    nav_select("main_content", selected = input$main_nav)
  })
  # ──────────────────────────────────────────────
  # TAB 0: INTRODUCTION AND GUIDE (README)
  # ──────────────────────────────────────────────
  output$tab_intro <- renderUI({
    tagList(
      # 1. Welcome Hero Banner
      tags$div(class = "hero-banner",
               tags$div(style = "display:flex; align-items:center; gap:18px; margin-bottom:20px;",
                        tags$img(
                          src = "porter/loghi/logo_nuance.png",
                          style = "height:52px; width:auto; filter:invert(1) brightness(1.8);",
                          alt = "Nuance Audio"
                        ),
                        tags$div(
                          tags$div(style = "font-size:11px; letter-spacing:3px; text-transform:uppercase; color:var(--accent); font-weight:700; margin-bottom:2px;", "EssilorLuxottica"),
                          tags$div(style = "font-size:11px; letter-spacing:2px; text-transform:uppercase; color:var(--muted); font-weight:500;", "Strategic & Competitive Intelligence")
                        )
               ),
               tags$h1(style = "font-size:32px; font-weight:900; color:white; margin:0 0 10px; letter-spacing:-0.7px;",
                       "Competitive Intelligence: Nuance Audio"),
               tags$p(style = "font-size:15px; color:var(--dim); line-height:1.75; margin:0; max-width:850px;",
                      "Welcome to the SCI project interactive dashboard. This application is a strategic repository designed to explore the market entry of Nuance Audio (EssilorLuxottica). Explore analytical frameworks, competitor analysis, and bibliometric data by navigating through the sidebar menu.")
      ),
      
      # 2. Concept Section — 4 blocchi su una singola riga orizzontale
      fluidRow(
        column(3,
               tags$div(class = "sci-card", style = "height: 100%;",
                        tags$div(style = "margin-bottom: 14px; text-align: center;",
                                 tags$img(src = "intro_invisible.jpg",
                                          style = "max-width: 100%; height: 160px; object-fit: cover; border-radius: 8px;")
                        ),
                        tags$h3(style = "color: white; font-weight: 800; font-size: 16px; margin-bottom: 10px; text-align: center;",
                                "Invisible and comfortable"),
                        tags$p(style = "color: var(--dim); font-size: 13px; line-height: 1.6; margin:0; text-align: center;",
                               "Lightweight eyewear with open-ear acoustic solutions integrated into the frames. No bulky earbuds, no visible technology.")
               )
        ),
        column(3,
               tags$div(class = "sci-card", style = "height: 100%;",
                        tags$div(style = "margin-bottom: 14px; text-align: center;",
                                 tags$img(src = "intro_sight.jpg",
                                          style = "max-width: 100%; height: 160px; object-fit: cover; border-radius: 8px;")
                        ),
                        tags$h3(style = "color: white; font-weight: 800; font-size: 16px; margin-bottom: 10px; text-align: center;",
                                "Sight and hearing in one solution"),
                        tags$p(style = "color: var(--dim); font-size: 13px; line-height: 1.6; margin:0; text-align: center;",
                               "Why choose between sight support and hearing support? Nuance Audio glasses do both, breaking down social stigma.")
               )
        ),
        column(3,
               tags$div(class = "sci-card", style = "height: 100%;",
                        tags$div(style = "margin-bottom: 14px; text-align: center;",
                                 tags$img(src = "intro_app.png",
                                          style = "max-width: 100%; height: 160px; object-fit: cover; border-radius: 8px;")
                        ),
                        tags$h3(style = "color: white; font-weight: 800; font-size: 16px; margin-bottom: 10px; text-align: center;",
                                "Easy to use"),
                        tags$p(style = "color: var(--dim); font-size: 13px; line-height: 1.6; margin:0; text-align: center;",
                               "Start using them easily with the Nuance Audio App and video tutorials that will guide you step by step. You won't need an expert.")
               )
        ),
        column(3,
               tags$div(class = "sci-card", style = "height: 100%;",
                        tags$div(style = "margin-bottom: 14px; text-align: center;",
                                 tags$video(src = "intro_transitions.mp4", type = "video/mp4",
                                            autoplay = NA, loop = NA, muted = NA, playsinline = NA,
                                            style = "max-width: 100%; height: 160px; object-fit: cover; border-radius: 8px;")
                        ),
                        tags$h3(style = "color: white; font-weight: 800; font-size: 16px; margin-bottom: 10px; text-align: center;",
                                "Adaptable, for all-day wear"),
                        tags$p(style = "color: var(--dim); font-size: 13px; line-height: 1.6; margin:0; text-align: center;",
                               "Our non-prescription glasses feature Transitions® technology, with lenses that adapt to light to ensure clear and effortless vision.")
               )
        )
      ),
      
      tags$br(),
      
      # 3. Application Guide (README style)
      section_hdr("Navigation Guide", "What you will find in the different sections of the Dashboard"),
      fluidRow(
        column(6,
               info_card("1. Strategic Frameworks", c(
                 "<strong>Porter's Five Forces:</strong> Assessment of the competitive environment and industry structure (threat of substitutes, rivalry, etc.).",
                 "<strong>VUCA Analysis:</strong> Interactive map of strategic challenges categorized by Volatility, Uncertainty, Complexity, and Ambiguity."
               ), col$accent)
        ),
        column(6,
               info_card("2. Intelligence Methodology", c(
                 "<strong>Scope & KITs/KIQs:</strong> Defining intelligence objectives using the SMART approach and the Rumsfeld matrix.",
                 "<strong>Query Design:</strong> How search queries were structured to map the technological and competitive landscape."
               ), col$blue)
        )
      ),
      fluidRow(
        column(6,
               info_card("3. Analysis & Data", c(
                 "<strong>GenAI for CI:</strong> Evaluation of the use of Generative Artificial Intelligence (CREATE Framework) to support investigations.",
                 "<strong>Text Analysis:</strong> Bibliometric analysis and topic modeling (BERTopic) on the paper corpus extracted from Scopus."
               ), col$purple)
        ),
        column(6,
               info_card("4. Decision Synthesis", c(
                 "<strong>Data Viz Lab:</strong> The concluding storyboard that links analysis to visual data to define strategic positioning."
               ), col$orange)
        )
      ),
      
      tags$hr(style = "border-color: var(--border); margin: 30px 0;"),
      
      # I DUE BOTTONI DIREZIONALI
      fluidRow(
        column(6,
               tags$div(class = "sci-card", 
                        style = "text-align:center; padding: 40px 20px; border-top: 4px solid #00e5a0; cursor:pointer; transition: transform 0.2s;",
                        onmouseover = "this.style.transform='scale(1.02)'", 
                        onmouseout = "this.style.transform='scale(1)'",
                        onclick = "Shiny.setInputValue('nav_goto', 'exec_summary', {priority: 'event'});",
                        tags$div(style = "font-size: 45px; margin-bottom: 15px;", "🎯"),
                        tags$h3(style = "color: white; font-weight: 800; margin-bottom: 10px;", "The Results"),
                        tags$p(style = "color: var(--dim); font-size: 13px; line-height: 1.5; margin:0;", 
                               "Il cuore della presentazione strategica: Executive Summary, Scenario di Mercato (VUCA/Porter) e Decisioni Finali.")
               )
        ),
        column(6,
               tags$div(class = "sci-card", 
                        style = "text-align:center; padding: 40px 20px; border-top: 4px solid #4facfe; cursor:pointer; transition: transform 0.2s;",
                        onmouseover = "this.style.transform='scale(1.02)'", 
                        onmouseout = "this.style.transform='scale(1)'",
                        onclick = "Shiny.setInputValue('nav_goto', 'scope', {priority: 'event'});",
                        tags$div(style = "font-size: 45px; margin-bottom: 15px;", "⚙️"),
                        tags$h3(style = "color: white; font-weight: 800; margin-bottom: 10px;", "The Process"),
                        tags$p(style = "color: var(--dim); font-size: 13px; line-height: 1.5; margin:0;", 
                               "Il workflow metodologico dei laboratori: KIT/KIQ, Query Design, GenAI Prompting e Text Analysis bibliometrica.")
               )
        )
      )
    )
  })
  
  # Questo "ascolta" il click sui bottoni e cambia pagina
  observeEvent(input$nav_goto, {
    nav_select("main_nav", selected = input$nav_goto)
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
                          tags$p(style = paste0("font-size:14px; color:", col$accent, "; margin:3px 0 0; font-weight:600;"), "AudioNova & EssilorLuxottica — Hearing Glasses")
                        )
               ),
               tags$p(style = "font-size:14px; color:var(--dim); line-height:1.75; margin:0; max-width:820px;", HTML(
                 "Nuance Audio sits at the intersection of <strong style='color:white'>eyewear</strong>, <strong style='color:white'>hearing aids</strong>, and <strong style='color:white'>wearable technology</strong>. As the first FDA-cleared hearing glasses, it targets the <strong style='color:white'>1.5 billion</strong> people worldwide affected by hearing loss — where the adoption rate remains at only 17%. This app synthesizes market intelligence, competitive analysis, and strategic frameworks to support decision-making."
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
      section_hdr("Market Growth Trajectory", "Smart glasses market revenue projections (USD Billions) — Sources: SAG, Grand View Research"),
      tags$div(class = "sci-card", plotlyOutput("plot_market_growth", height = "320px")),
      
      # Strengths / Risks
      fluidRow(
        column(6, info_card("Key Strengths", c(
          "First-mover in FDA-cleared hearing glasses (January 2025)",
          "EssilorLuxottica distribution: <strong style='color:white'>18,000+ stores</strong> globally",
          "Price ~\u00bc of traditional hearing aids ($1,100 vs $4,000+)",
          "SaMD model enables OTA software updates",
          "Invisible design directly addresses stigma barrier"
        ), col$accent)),
        column(6, info_card("Key Risks", c(
          "AirPods Pro 2 at <strong style='color:white'>$250</strong> with FDA-cleared hearing aid feature",
          "Meta could add hearing features to Ray-Ban Meta (same parent company)",
          "Chinese competitors (Cearvol, Elehear) at $250\u2013$400",
          "Uncertain consumer adoption — <strong style='color:white'>83% coverage gap</strong> remains",
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
      
      tags$div(class = "sci-card",
               tags$div(class = "porter-grid",
                        
                        # Riga 1: Nuovi Entrati
                        tags$div(), tags$div(),
                        tags$div(class = "porter-box", style = "border: 3px solid #bf5000; background: #fcd9b0; box-shadow: 0 0 14px rgba(191,80,0,0.35);",
                                 tags$h5("Threat of New Entrants"),
                                 tags$p(tags$strong(class="status-u", "Medium/High"), " high technological, multidisciplinary barrier and intellectual properties"), 
                                 tags$div(
                                   style = "display: flex; flex-direction: row; justify-content: center; align-items: center; gap: 15px; margin-top: 15px;",
                                   tags$img(src = "porter/entrants/new_entrants1.png", class = "thematic-img", style = "margin-top: 0; height: 45px;"),
                                   tags$img(src = "porter/entrants/new_entrants2.png", class = "thematic-img", style = "margin-top: 0; height: 45px;"),
                                   tags$img(src = "porter/entrants/new_entrants.png", class = "thematic-img", style = "margin-top: 0; height: 45px;")
                                 )
                        ),
                        tags$div(), tags$div(),
                        
                        # Riga 2: Freccia giù
                        tags$div(), tags$div(),
                        tags$div(class = "porter-arrow", tags$i(class = "fas fa-arrow-down", `aria-hidden` = "true")),
                        tags$div(), tags$div(),
                        
                        # Riga 3: Fornitori -> Rivalità <- Acquirenti
                        tags$div(class = "porter-box", style = "border: 3px solid #bf5000; background: #fcd9b0; box-shadow: 0 0 14px rgba(191,80,0,0.35);",
                                 tags$h5("Bargaining Power of Suppliers"),
                                 tags$p(tags$strong(class="status-u", "Medium/High:"), " EssilorLuxottica has the full domain in the smartglasses market (pov: AudioNova)"), 
                                 tags$div(
                                   style = "display: flex; flex-direction: row; justify-content: center; align-items: center; gap: 15px; margin-top: 15px;",
                                   tags$img(src = "porter/suppliers/suppliers.png", class = "thematic-img", style = "margin-top: 0; height: 45px;"),
                                   tags$img(src = "porter/suppliers/suppliers1.png", class = "thematic-img", style = "margin-top: 0; height: 45px;"),
                                   tags$img(src = "porter/suppliers/suppliers2.png", class = "thematic-img", style = "margin-top: 0; height: 45px;")
                                   
                                 )
                        ),
                        tags$div(class = "porter-arrow", tags$i(class = "fas fa-arrow-right", `aria-hidden` = "true")),
                        tags$div(class = "porter-box", style = "border: 3px solid #d97000; background: #fdebc0; box-shadow: 0 0 14px rgba(217,112,0,0.35);",
                                 tags$h5("Competitive Rivalry"),
                                 tags$p(tags$strong(class="status-u", "Medium:"), " most potential users have never used a hearing aid, so they have no loyalty to any brand."), 
                                 # Contenitore dei loghi
                                 tags$div(
                                   style = "display: flex; flex-direction: column; align-items: center; gap: 10px; margin-top: 15px;",
                                   
                                   # Prima riga: Apple e Meta
                                   tags$div(
                                     style = "display: flex; flex-direction: row; justify-content: center; align-items: center; gap: 15px;",
                                     tags$img(src = "porter/loghi/logo_apple.svg", alt = "Apple", style = "height: 25px;"),
                                     tags$img(src = "porter/loghi/logo_meta.png", alt = "Meta", style = "height: 25px;")
                                   ),
                                   
                                   # Seconda riga: Sonova e Amplifon
                                   tags$div(
                                     style = "display: flex; flex-direction: row; justify-content: center; align-items: center; gap: 15px;",
                                     tags$img(src = "porter/loghi/logo_sonova.png", alt = "Sonava", style = "height: 25px;"),
                                     tags$img(src = "porter/loghi/logo_amplifon.png", alt = "Amplifon", style = "height: 25px;")
                                   )
                                 )
                        ),
                        tags$div(class = "porter-arrow", tags$i(class = "fas fa-arrow-left", `aria-hidden` = "true")),
                        tags$div(class = "porter-box", style = "border: 3px solid #c8a000; background: #fdf0a0; box-shadow: 0 0 14px rgba(200,160,0,0.35);",
                                 tags$h5("Bargaining Power of Buyers"),
                                 tags$p(tags$strong(class="status-u", "Low/Medium:"), " target high-end. The absence of comparable alternatives reduces their price sensitivity."), 
                                 tags$div(
                                   style = "display: flex; flex-direction: row; justify-content: center; align-items: center; gap: 15px; margin-top: 15px;",
                                   tags$img(src = "porter/buyers/buyers.png", class = "thematic-img", style = "margin-top: 0; height: 45px;" ),
                                   tags$img(src = "porter/buyers/buyers1.png", class = "thematic-img", style = "margin-top: 0; height: 45px;" )
                                 )
                        ),
                        
                        # Riga 4: Freccia su
                        tags$div(), tags$div(),
                        tags$div(class = "porter-arrow", tags$i(class = "fas fa-arrow-up", `aria-hidden` = "true")),
                        tags$div(), tags$div(),
                        
                        # Riga 5: Sostituti
                        tags$div(), tags$div(),
                        tags$div(class = "porter-box", style = "border: 3px solid #e82020; background: #fcd0d0; box-shadow: 0 0 14px rgba(232,32,32,0.35);",
                                 tags$h5("Threat of Substitutes"),
                                 tags$p(tags$strong(class="status-u", "High:"), " existence of alternative solutions such as the combined use of traditional eyeglasses and medical hearing aids"), 
                                 tags$div(
                                   style = "display: flex; flex-direction: row; justify-content: center; align-items: center; gap: 15px; margin-top: 15px;",
                                   tags$img(src = "porter/substitutes/substitutes.png", class = "thematic-img", style = "margin-top: 0; height: 45px;"),
                                   tags$img(src = "porter/substitutes/substitutes1.png", class = "thematic-img", style = "margin-top: 0; height: 15px;"),
                                   tags$img(src = "porter/substitutes/substitutes2.png", class = "thematic-img", style = "margin-top: 0; height: 45px;")
                                 )
                        ),
                        tags$div(), tags$div()
               )
      ),
      
      section_hdr("Threat of Substitutes", "Comparison by threat level, price range, and performance"),
      tags$div(class = "sci-card", DTOutput("table_subs")),
      
      insight_box(
        "Key Strategic Insight",
        "The main substitution risk comes from products addressing hearing loss through entirely different form factors, especially <strong style='color:white'>AirPods Pro 2 at ~$250</strong> leveraging Apple's massive installed base. Nuance Audio's competitive moat lies in its unique combination of <strong style='color:white'>medical certification + invisible design + optical distribution network</strong> — a bundle no substitute currently replicates.",
        col$orange, "bolt"
      ),
      
      section_hdr("Force Details"),
      info_card("Threat of New Entrants — MEDIUM-HIGH", c(
        "<strong style='color:white'>Barriers:</strong> High R&D + FDA/CE clearance, IP & beamforming know-how, EssilorLuxottica's 18K retail network",
        "<strong style='color:white'>Enablers:</strong> FDA OTC deregulation (2022), tech giants with massive R&D, Chinese OTC brands at lower prices"
      ), col$orange),
      info_card("Bargaining Power of Buyers — MEDIUM-HIGH", c(
        "Large potential market: ~1.25B people with mild-moderate hearing loss globally",
        "Price-sensitive market: $1,100 Nuance vs $250 AirPods Pro, many OTC alternatives",
        "Low switching costs: OTC devices don't require audiologist recalibration"
      ), col$orange),
      info_card("Bargaining Power of Suppliers — MEDIUM", c(
        "<strong style='color:white'>Increasing:</strong> Specialized MEMS microphones, audio chipsets from limited suppliers",
        "<strong style='color:white'>Decreasing:</strong> EssilorLuxottica's vertical integration (\u20AC25B+ revenue), acquired Nuance Hearing IP (2023)"
      ), col$accent),
      info_card("Competitive Rivalry — MEDIUM-HIGH", c(
        "Direct: Sonova (Phonak), Demant (Oticon), WS Audiology, Cearvol (Lyra)",
        "Convergent: Apple AirPods Pro 2 (FDA-cleared, $250, massive user base)",
        "Adjacent: Meta Ray-Ban Meta (same parent, could add hearing features)"
      ), col$orange)
    )
  })
  
  output$plot_porter_radar <- renderEcharts4r({
    df_porter %>%
      e_charts(Force) %>%
      e_radar(Score, max = 100, name = "Intensity") %>%
      e_radar_opts(shape = "circle",
                   splitArea = list(areaStyle = list(color = c("transparent"))),
                   axisLine = list(lineStyle = list(color = col$border)),
                   splitLine = list(lineStyle = list(color = col$border))) %>%
      e_color(col$accent) %>%
      e_tooltip(trigger = "item") %>%
      e_legend(show = FALSE) %>%
      e_theme_custom('{"backgroundColor":"transparent"}')
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
              rownames = FALSE, class = "display")
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
          "Price volatility: dropped from $1,200 at launch to <$1,000 to counter Chinese brands.",
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
        }
        .vuca-hover-card:hover .vuca-drop {
          max-height: 400px;
          padding: 14px 16px;
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
                 tags$div(style = "padding:28px 20px 20px; text-align:center;",
                          tags$div(style = paste0("font-size:38px; color:", col$orange, "; margin-bottom:10px;"),
                                   tags$i(class = "fas fa-cloud", `aria-hidden` = "true")),
                          tags$h5(style = paste0("color:", col$orange, "; font-weight:800; margin:0; font-size:15px;"),
                                  "UNCERTAINTY")
                 ),
                 tags$div(class = "vuca-drop",
                          style = paste0("background:", col$orange, "18; border-top: 2px solid ", col$orange, ";"),
                          tags$p(style = "font-size:11px; color:#334155; line-height:1.5; margin:0;",
                                 tags$strong("Characteristics: "), "Basic cause and effect are known, but lack of information on competitor moves muddies the future."),
                          tags$p(style = "font-size:11px; color:#334155; line-height:1.5; margin:8px 0 0;",
                                 tags$strong("Example: "), "Potential entry of Apple or Meta into the medical-grade hearing space could disrupt the adoption curve."),
                          tags$p(style = "font-size:11px; color:#334155; line-height:1.5; margin:8px 0 0;",
                                 tags$strong("Approach: "), "Invest in competitive intelligence to monitor patent filings and emerging tech breakthroughs.")
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
        column(4, tags$div(class = "sci-card", style = paste0("border-top:3px solid ", col$accent, "; min-height:230px;"),
                           tags$div(style = paste0("width:35px; height:35px; border-radius:8px; background:", col$accent, "12; display:flex; align-items:center; justify-content:center; font-size:18px; font-weight:900; color:", col$accent, "; margin-bottom:12px;"), "1"),
                           tags$h4(style = "font-size:14px; font-weight:700; color:white; margin:0 0 8px;", "Integrated Ecosystem"),
                           tags$p(style = "font-size:12px; color:var(--dim); line-height:1.6;", 
                                  "Leverage the 18,000+ retail network to overcome access barriers. EssilorLuxottica's strength lies in vertical integration: frames + lenses + hearing technology in one purchase.")
        )),
        column(4, tags$div(class = "sci-card", style = paste0("border-top:3px solid ", col$blue, "; min-height:230px;"),
                           tags$div(style = paste0("width:35px; height:35px; border-radius:8px; background:", col$blue, "12; display:flex; align-items:center; justify-content:center; font-size:18px; font-weight:900; color:", col$blue, "; margin-bottom:12px;"), "2"),
                           tags$h4(style = "font-size:14px; font-weight:700; color:white; margin:0 0 8px;", "Software & AI Flywheel"),
                           tags$p(style = "font-size:12px; color:var(--dim); line-height:1.6;", 
                                  "The SaMD model enables continuous OTA updates. Invest in AI-driven noise cancellation and real-time translation to distance the brand from low-cost Chinese competitors.")
        )),
        column(4, tags$div(class = "sci-card", style = paste0("border-top:3px solid ", col$orange, "; min-height:230px;"),
                           tags$div(style = paste0("width:35px; height:35px; border-radius:8px; background:", col$orange, "12; display:flex; align-items:center; justify-content:center; font-size:18px; font-weight:900; color:", col$orange, "; margin-bottom:12px;"), "3"),
                           tags$h4(style = "font-size:14px; font-weight:700; color:white; margin:0 0 8px;", "Lifestyle Moats"),
                           tags$p(style = "font-size:12px; color:var(--dim); line-height:1.6;", 
                                  "83% of those in need avoid support due to 'stigma'. Nuance Audio must position itself as a fashion accessory, turning medical necessity into technological desire.")
        )
        )
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
        "Market estimates vary significantly across sources due to different scope definitions (smart glasses vs. smart glass vs. AI smart glasses). CAGR ranges from <strong style='color:white'>12% to 24%</strong> and 2025 market size from <strong style='color:white'>$2.5B to $18.4B</strong>. Europe's position also varies (32\u201343% share). All quantitative data should be verified against primary sources.",
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
                 tags$div(class = "sci-card", style = paste0("border-left:4px solid ", tc, ";"),
                          tags$div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:6px;",
                                   tags$span(style = "font-size:14px; font-weight:700; color:white;", r$Competitor),
                                   sci_badge(r$Type, tc)
                          ),
                          tags$p(style = "font-size:12px; color:var(--dim); margin:0; line-height:1.55;", r$Description)
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
                  desc = "Nuance Audio's positioning and market penetration strategy in the OTC hearing glasses segment in the USA and Europe.",
                  kiqs = list(
                    list(type = "Polar", q = HTML("Is the OTC price of $1,200 competitive compared to the average of OTC hearing aids on the US market as of<br>Q2 2025?"),
                         indicators = "OTC prices; e-commerce, FDA listing; web scraping", analysis = "Descriptive",
                         smart = list(S="Nuance price vs OTC average", M="numerical comparison", A="public data", R="pricing = key adoption barrier", T="Q2 2025")),
                    list(type = "Alternative", q = HTML("Should distribution prioritize proprietary optical channels (LensCrafters) or independent audiology channels<br>for EU penetration?"),
                         indicators = "Store count, % coverage; annual reports; geospatial analysis", analysis = "Exploratory",
                         smart = list(S="two defined channels", M="sales volume per channel", A="distribution data", R="go-to-market strategy", T="EU launch 2025")),
                    list(type = "Q-word", q = "How is Nuance Audio perceived by adults 50+ regarding stigma, comfort, and usefulness vs. traditional hearing aids within 6 months of US launch?",
                         indicators = "Sentiment score, keyword freq; social media, forums; NLP", analysis = "Descriptive + Exploratory",
                         smart = list(S="defined target, defined variables", M="sentiment analysis, surveys", A="social data, reviews", R="adoption", T="6 months from launch"))
                  )),
      KIT2 = list(title = "Early Warnings", color = col$orange,
                  desc = "Emerging technological and competitive threats that could erode Nuance Audio's first-mover advantage over the next 12–24 months.",
                  kiqs = list(
                    list(type = "Polar", q = HTML("Has Apple filed patents related to hearing aid functionality integrated into eyewear devices in the last<br>24 months?"),
                         indicators = "Patent count, IPC class; Google Patents, USPTO", analysis = "Descriptive + Predictive",
                         smart = list(S="Apple, eyewear patents", M="yes/no with count", A="public patent databases", R="threat assessment", T="24 months")),
                    list(type = "Alternative", q = HTML("Most imminent threat: big tech entry (Apple, Google) or traditional hearing aid manufacturers (Phonak,<br>Oticon) with eyewear solutions?"),
                         indicators = "Signals per category; press releases, job postings, M&A", analysis = "Exploratory",
                         smart = list(S="two competitor categories", M="patent activity, partnerships, announcements", A="OSINT data", R="early warning", T="12-24 months", m_wide = TRUE)),
                    list(type = "Q-word", q = HTML("What emerging technologies (bone conduction, AI audio, biometric sensors) are competitors developing that<br>could offer superior advantage by 2027?"),
                         indicators = "Publication/patent frequency; Scopus, patents, tech media", analysis = "Descriptive + Predictive",
                         smart = list(S="defined technologies, competitors", M="patents, papers, product announcements", A="public sources", R="technological disruption", T="by 2027", m_wide = TRUE))
                  )),
      KIT3 = list(title = "Key Players & Positioning", color = col$blue,
                  desc = "Map and positioning of key players in the hearing glasses / smart audio wearables ecosystem.",
                  kiqs = list(
                    list(type = "Polar", q = HTML("Have Sonova and Demant announced partnerships with eyewear companies to develop hearing glasses by<br>April 2026?"),
                         indicators = "Yes/no + details; investor relations, industry press", analysis = "Descriptive",
                         smart = list(S="Specific variables", M="Measurable metrics", A="Achievable data", R="Relevant partnerships", T="April 2026")),
                    list(type = "Alternative", q = "In smart audio glasses, who holds larger share as of Q4 2025: Ray-Ban Meta or Nuance Audio?",
                         indicators = "Estimated units; analyst reports (IDC, Counterpoint)", analysis = "Descriptive + Inferential",
                         smart = list(S="direct comparison", M="estimated units sold", A="analyst reports", R="positioning", T="Q4 2025")),
                    list(type = "Q-word", q = "Who are emerging startups in hearing glasses/audio wearables with >$5M funding in the last 18 months?",
                         indicators = "Startup count, funding; Crunchbase, PitchBook", analysis = "Exploratory + Predictive",
                         smart = list(S="startups, funding threshold", M="Crunchbase/PitchBook data", A="Achievable public data", R="Relevant competition", T="18 months"))
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

          .smart-hline { position: relative; width: 100%; height: 2px; background-color: #e0e7ef; margin: 6px 0 3px 0; }
          .smart-riser { position: absolute; left: 0; top: 0; width: 2px; height: 14px; background-color: #e0e7ef; }
          .smart-label { font-size: 7px; font-weight: 700; color: #e0e7ef; text-transform: uppercase; letter-spacing: 0.3px; }
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
          
          # Quadrante 1
          tags$div(
            style = "background: #fdfdfd; border: 2px dashed #94a3b8; padding: 15px; border-radius: 8px; font-size: 13px; line-height: 1.6;",
            tags$strong(style = "color: #0f1724; font-size: 14px;", "Q1: Known-Knowns"), 
            tags$span(style = "color: #556378; font-size: 12px;", " (What we know we know)"),
            tags$hr(style="margin:8px 0; border-color:#cbd5e1;"),
            tags$p(style="margin:0; color:#334155;", "Nuance Audio is an EssilorLuxottica product born from the 2023 acquisition of Nuance Hearing (Israeli startup). It combines corrective eyewear and open-ear hearing amplification in a single OTC device for adults 18+ with mild-to-moderate hearing loss. Price: ~$1,200. FDA-approved (US) and CE-marked (EU). US launch: April 2025 via LensCrafters, Target Optical, Pearle Vision. EU launch: starting from Italy in Q1 2025, then France, Germany, UK. Direct competitors: Ray-Ban Meta, AirPods Pro 2 (hearing aid feature), Eargo, Jabra Enhance.")
          ),
          
          # Quadrante 3
          tags$div(
            style = "background: #fdfdfd; border: 2px dashed #94a3b8; padding: 15px; border-radius: 8px; font-size: 13px; line-height: 1.6;",
            tags$strong(style = "color: #0f1724; font-size: 14px;", "Q3: Known-Unknowns"), 
            tags$span(style = "color: #556378; font-size: 12px;", " (What we know we don't know)"),
            tags$hr(style="margin:8px 0; border-color:#cbd5e1;"),
            tags$p(style="margin:0; color:#334155;", "What will the actual adoption rate of Nuance Audio be vs. traditional hearing aids? How will traditional hearing aid manufacturers (Phonak, Oticon, Starkey) react to this disruption? What is the target consumer perception regarding the stigma of an 'invisible' hearing aid vs. a traditional one? How will the OTC regulatory framework evolve in Europe vs. the US? What is the actual clinical effectiveness compared to traditional hearing aids?")
          ),
          
          # Riga 2 (What we don't know...)
          tags$div(style = "text-align: right; font-weight: 800; font-size: 18px; color: #080c14; display: flex; align-items: center; justify-content: flex-end; padding-right: 15px;", "What we don't know..."),
          
          # Quadrante 2
          tags$div(
            style = "background: #fdfdfd; border: 2px dashed #94a3b8; padding: 15px; border-radius: 8px; font-size: 13px; line-height: 1.6;",
            tags$strong(style = "color: #0f1724; font-size: 14px;", "Q2: Unknown-Knowns"), 
            tags$span(style = "color: #556378; font-size: 12px;", " (Latent knowledge)"),
            tags$hr(style="margin:8px 0; border-color:#cbd5e1;"),
            tags$p(style="margin:0; color:#334155;", "Literature identifies key wearable adoption factors (perceived usefulness, ease of use, stigma, comfort, price, privacy) per Kalantari 2017 and Koutromanos & Kazakou 2023, but these have not been specifically applied to Nuance Audio. The OTC hearing aid market was deregulated by the FDA in 2022, opening significant competitive space. EssilorLuxottica's dominant eyewear position (Ray-Ban, Oakley, LensCrafters) could provide enormous distribution advantages.")
          ),
          
          # Quadrante 4
          tags$div(
            style = "background: #fdfdfd; border: 2px dashed #94a3b8; padding: 15px; border-radius: 8px; font-size: 13px; line-height: 1.6;",
            tags$strong(style = "color: #0f1724; font-size: 14px;", "Q4: Unknown-Unknowns"), 
            tags$span(style = "color: #556378; font-size: 12px;", " (What we don't know we don't know)"),
            tags$hr(style="margin:8px 0; border-color:#cbd5e1;"),
            tags$p(style="margin:0; color:#334155;", "New technologies (e.g., advanced bone conduction, neural implants) could make the open-ear approach obsolete. An unexpected competitor (e.g., Apple, Google, Samsung) could enter the hearing glasses segment with stronger ecosystem integration. Potential long-term side effects of prolonged unmonitored open-ear amplification. Cultural shifts that fully normalize hearing aids, eliminating the competitive advantage of invisibility.")
          )
        )
      ),
      
      # 2. IL NUOVO SCOPE STATEMENT (Sfondo verdolino pastello)
      tags$div(
        style = "background-color: #e6f7ef; padding: 24px; border-radius: 12px; margin-bottom: 24px; box-shadow: inset 0 0 10px rgba(0,0,0,0.05);",
        tags$h4(style = "font-size:16px; font-weight:800; color: #008f63; margin:0 0 10px; display:flex; align-items:center; gap:6px;", 
                "Scope Statement"),
        tags$p(style = "font-size:14px; color: #1e293b; line-height:1.75; margin:0; font-style:italic;",
               "“To what extent is EssilorLuxottica’s Nuance Audio capable of creating a new market category (‘hearing glasses’) that overcomes traditional hearing aid adoption barriers, and which competitive, technological, and regulatory factors will determine its success in the European and American markets over the next 12–24 months?”"
        )
      ),
      
      # KIT selector
      tags$div(style = "display:flex; gap:10px; margin-bottom:20px;",
               lapply(names(kit_data), function(kn) {
                 kd <- kit_data[[kn]]
                 is_active <- (kn == sel)
                 tags$div(
                   style = paste0(
                     "flex:1; padding:14px 18px; border-radius:12px; cursor:pointer; border:2px solid ",
                     if (is_active) paste0(kd$color, ";") else paste0(col$border, ";"),
                     " background:", if (is_active) paste0(kd$color, "0d;") else paste0(col$card, ";")
                   ),
                   onclick = paste0("Shiny.setInputValue('kit_click', '", kn, "', {priority:'event'});"),
                   tags$div(style = paste0("font-size:11px; font-weight:700; color:", kd$color, "; letter-spacing:1px;"), gsub("KIT","KIT ", kn)),
                   tags$div(style = "font-size:13px; font-weight:600; color:white; margin-top:4px;", kd$title)
                 )
               })
      ),
      
      # KIQ content
      tags$div(class = "sci-card", style = paste0("border-top:3px solid ", k$color, ";"),
               tags$p(style = "font-size:13px; color:var(--dim); margin-bottom:20px; line-height:1.6;", k$desc),
               tags$div(
                 lapply(k$kiqs, function(kiq) {
                   
                   # Riquadro principale: ora è un contenitore orizzontale (display: flex)
                   tags$div(style = paste0("background:", col$card2, "; border-radius:10px; padding:18px; border:1px solid ", col$border, "; margin-bottom:12px; display: flex; align-items: center; justify-content: space-between; gap: 20px;"),
                            
                            # --- COLONNA SINISTRA: Badge, Domanda, Indicatori ---
                            tags$div(style = "flex: 1;", # 'flex: 1' permette a questa colonna di allargarsi e mandare il testo a capo
                                     
                                     # Riga dei Badge
                                     tags$div(style = "display:flex; gap:8px; margin-bottom:12px;",
                                              sci_badge(kiq$type, k$color),
                                              sci_badge(kiq$analysis, col$text) # Assicurati di usare col$text o il colore che hai scelto
                                     ),
                                     
                                     # Testo della Domanda
                                     tags$p(style = "font-size:13px; color:white; margin:0 0 8px; font-weight:500; line-height:1.55;", kiq$q),
                                     
                                     # Indicatori
                                     tags$div(style = "font-size:11px; color:var(--muted);",
                                              tags$strong(style = "color:var(--dim);", "Indicators: "), kiq$indicators
                                     )
                            ),
                            
                            # --- COLONNA DESTRA: Scalinata SMART ---
                            tags$div(style = "flex-shrink: 0;",
                                     smart_ui(kiq$smart$S, kiq$smart$M, kiq$smart$A, kiq$smart$R, kiq$smart$T, 
                                              if(isTRUE(kiq$smart$m_wide)) "tooltip-m-wide" else "tooltip-m")
                            )
                   )
                 })
               )
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
           query = 'TITLE-ABS-KEY("smart glasses" OR "audio glasses" OR "hearing glasses")\nAND TITLE-ABS-KEY("wearable technology" OR "smart eyewear")',
           results = "Nuance Audio, Vue Lite 2, XanderGlasses, Ray-Ban Meta. Market data: $2.46B \u2192 $14.38B (CAGR 24.2%)"),
      list(id = "Q2", title = "Hearing & Assistive Technology", scope = "Narrow", color = col$blue,
           query = 'TITLE-ABS-KEY("smart glasses" OR "hearing glasses" OR "audio glasses")\nAND TITLE-ABS-KEY("hearing aid" OR "augmented hearing" OR "bone conduction"\n  OR "assistive listening")',
           results = "Bone conduction solutions, InterHearing. Key insight: Nuance uses open-ear + beamforming (not bone conduction) \u2014 important differentiator"),
      list(id = "Q3", title = "Consumer Adoption", scope = "Strategic", color = col$purple,
           query = 'TITLE-ABS-KEY("smart glasses" OR "wearable technology")\nAND TITLE-ABS-KEY("consumer adoption" OR "technology acceptance"\n  OR "user experience")\nAND TITLE-ABS-KEY("eyewear" OR "hearing")',
           results = "IDC: Ray-Ban Meta 900K+ sales Q4 2024; 1.5B affected by hearing loss; design, comfort, usefulness as key TAM/UTAUT factors"),
      list(id = "Q4", title = "Market & Competition", scope = "Quantitative", color = col$orange,
           query = 'TITLE-ABS-KEY("smart glasses" OR "smart eyewear" OR "audio glasses")\nAND TITLE-ABS-KEY("market" OR "competitive landscape"\n  OR "industry analysis")\nAND NOT TITLE-ABS-KEY("virtual reality headset" OR "VR headset")',
           results = "Audio >28% share; Meta 73\u201382% share; ASP declining $450 \u2192 $250 (2024\u20132028); 45M units by 2028"),
      list(id = "Q5", title = "Innovation & CES 2026", scope = "Competitor", color = col$red,
           query = 'TITLE-ABS-KEY("hearing glasses" OR "audio glasses" OR "smart glasses")\nAND TITLE-ABS-KEY("digital transformation" OR "emerging technology"\n  OR "innovation")\nAND TITLE-ABS-KEY("eyewear" OR "hearing")',
           results = "CES 2026: Cearvol Lyra (NeuroFlow AI 2.0), Alango (zero-latency). Ray-Ban Meta 'Conversation Focus'. Nuance SNR improvement 4.4 dB")
    )
    
    tagList(
      section_hdr("Query Design", "Systematic query design for Scopus and web search \u2014 from broad landscape to specific competitive intelligence"),
      
      # Query overview grid
      tags$div(style = "display:flex; gap:10px; margin-bottom:24px;",
               lapply(queries, function(q) {
                 tags$div(style = paste0("flex:1; background:", col$card, "; border:1px solid ", q$color, "30; border-radius:12px; padding:16px; text-align:center; border-top:3px solid ", q$color, ";"),
                          tags$div(style = paste0("font-size:22px; font-weight:800; color:", q$color, "; font-family:'JetBrains Mono',monospace;"), q$id),
                          tags$div(style = "font-size:11px; color:white; font-weight:600; margin-top:4px;", q$title),
                          tags$div(style = "margin-top:6px;", sci_badge(q$scope, q$color))
                 )
               })
      ),
      
      # Query details
      lapply(queries, function(q) {
        tags$div(class = "sci-card", style = paste0("border-left:4px solid ", q$color, ";"),
                 tags$div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:12px;",
                          tags$div(style = "display:flex; gap:10px; align-items:center;",
                                   tags$span(style = paste0("font-size:18px; font-weight:800; color:", q$color, "; font-family:'JetBrains Mono',monospace;"), q$id),
                                   tags$span(style = "font-size:14px; font-weight:600; color:white;", q$title)
                          ),
                          sci_badge(paste(q$scope, "\u2714"), q$color)
                 ),
                 tags$pre(class = "query-code", q$query),
                 tags$div(style = "font-size:12px; color:var(--dim); line-height:1.65; margin-top:12px;",
                          tags$strong(style = "color:var(--muted);", "Key Results: "), q$results
                 )
        )
      }),
      
      # Recommended final query
      tags$div(class = "sci-card", style = paste0("border:1px solid ", col$accent, "30;"),
               tags$h4(style = paste0("font-size:14px; font-weight:700; color:", col$accent, "; margin:0 0 12px;"),
                       tags$i(class = "fas fa-bookmark", `aria-hidden` = "true"), " Recommended Final Query for Scopus"),
               tags$pre(class = "query-code", style = paste0("border-color:", col$accent, "30;"),
                        'TITLE-ABS-KEY("smart glasses" OR "hearing glasses" OR "audio glasses"
  OR "smart eyewear" OR "hearing aid glasses")
AND TITLE-ABS-KEY("hearing" OR "audio" OR "assistive" OR "wearable")
AND NOT TITLE-ABS-KEY("virtual reality headset")
AND PUBYEAR > 2017')
      )
    )
  })
  
  # ──────────────────────────────────────────────
  # TAB 8: GENAI FOR CI
  # ──────────────────────────────────────────────
  create_sel <- reactiveVal("C")
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
    
    sel_c <- create_sel()
    # Gestione speciale per la seconda 'E' di Extras
    curr <- if(sel_c == "E_ext") create_data$E2 else create_data[[sel_c]]
    
    tagList(
      section_hdr("GenAI for Competitive Intelligence", "Prompt engineering with the CREATE framework + evaluation of GenAI outputs across 3 source categories"),
      
      # --- SELETTORE INTERATTIVO (Stile Immagine 2) ---
      tags$div(style = "display:flex; gap:10px; margin-bottom:25px;",
               lapply(names(create_data), function(n) {
                 item <- create_data[[n]]
                 # ID speciale per la seconda E per non sovrapporsi
                 click_id <- if(n == "E2") "E_ext" else n 
                 is_active <- (sel_c == click_id)
                 
                 tags$div(
                   class = "vuca-btn",
                   style = paste0(
                     "flex:1; padding:20px 10px; border-radius:12px; cursor:pointer; text-align:center; transition: all 0.3s; border: 3px solid ",
                     if (is_active) item$color else "var(--border)", "; ",
                     "background:", if (is_active) item$pastel else paste0(item$pastel, "80"), ";"
                   ),
                   onclick = paste0("Shiny.setInputValue('create_click', '", click_id, "', {priority:'event'});"),
                   tags$div(style = paste0("font-size:32px; font-weight:900; color:", item$color, ";"), item$letter),
                   tags$div(style = "font-size:10px; text-transform:uppercase; letter-spacing:1px; color:#334155; margin-top:5px;", item$label)
                 )
               })
      ),
      
      # --- AREA DETTAGLIO DINAMICA ---
      tags$div(class = "sci-card", style = paste0("border-top: 4px solid ", curr$color, "; background:", curr$pastel, "; display: flex; gap: 30px; align-items: center;"),
               # Lettera grande a sinistra
               tags$div(style = paste0("font-size:100px; font-weight:900; color:", curr$color, "50; line-height:1; flex-shrink:0;"), curr$letter),
               
               # Testo a destra
               tags$div(style = "flex-grow:1;",
                        tags$div(style = paste0("color:", curr$color, "; font-weight:800; text-transform:uppercase; font-size:12px; letter-spacing:2px;"), curr$label),
                        tags$h3(style = paste0("margin: 5px 0 15px 0; color:", curr$color, "; font-size:20px; font-weight:800;"), curr$title),
                        tags$p(style = "font-size:15px; color:#334155; line-height:1.6; font-weight:500;", curr$desc),
                        tags$div(style = "background: rgba(255,255,255,0.55); padding: 12px; border-radius: 8px; border-left: 3px solid rgba(0,0,0,0.12); margin-top:15px;",
                                 tags$p(style = "font-size:13px; color:#64748b; margin:0;", tags$i(curr$detail))
                        )
               )
      ),
      
      # Visualizzazione del Prompt Finale (opzionale, molto figo)
      tags$div(style = "margin-top:20px; margin-bottom:30px;",
               tags$h5(style="font-size:12px; color:var(--muted); text-transform:uppercase; margin-bottom:10px;", "Composite Prompt Preview"),
               tags$div(class = "query-code", style="opacity:0.8; font-size:11px;",
                        paste0("Act as a ", create_data$C$desc, " ", create_data$R$desc, " ", create_data$E$desc, " ", create_data$A$desc, " ", create_data$T$desc, " ", create_data$E2$desc))
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
          "Market size 2025: <strong style='color:white'>$2.5B to $18.4B</strong> (depends on definition)",
          "CAGR ranges from <strong style='color:white'>10% to 24%</strong> across sources",
          "Europe's share: 32% to 43% depending on source",
          "Long-term projections highly uncertain",
          "Nuance Audio's actual adoption rate remains unknown"
        ), col$orange))
      ),
      
      # CEO Recommendation
      insight_box(
        "CEO Recommendation",
        "Use LLMs as an <strong style='color:white'>acceleration and support tool</strong> for strategic decisions, but <strong style='color:white'>never as a sole source</strong>. LLMs excel at initial landscape mapping (delivering in minutes what takes days manually), generating alternative scenarios, and identifying hidden assumptions. However, quantitative data must always be verified against primary sources. The primary risk is <strong style='color:#ff9f43'>overconfidence</strong>: LLMs present information authoritatively, masking genuine uncertainty. Recommendation: systematically integrate LLMs into the CI process within a framework of human validation, source triangulation, and critical review — <em>'AI is an accelerator, not a substitute for critical thinking.'</em>",
        col$purple, "lightbulb"
      )
    )
  })
  observeEvent(input$create_click, { create_sel(input$create_click) })
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
            hovertext = ~paste0("<span style='font-size:20px; color:", col$orange, ";'>", star_ratings, "</span><br><br>", wrapped_comments),
            hoverinfo = "text",
            hoverlabel = list(align = "left"), 
            textposition = "outside",
            textfont = list(size = 12, color = "white")) %>%
      plotly_layout_dark(
        # 4. Rendiamo l'asse X più visibile (linee, griglia e font)
        xaxis = list(
          range = c(0, 5.8), 
          title = list(text = "Score (out of 5)", font = list(color = "white", size = 13)), 
          dtick = 1,
          showline = TRUE, linecolor = col$dim, linewidth = 2, # Linea asse marcata
          gridcolor = col$muted, # Griglia più chiara
          tickfont = list(color = "white", size = 12)
        ),
        # 5. Rendiamo l'asse Y più visibile
        yaxis = list(
          title = list(text = "Criterion", standoff = 30, font = list(color = "white", size = 13)), 
          categoryorder = "array", 
          categoryarray = rev(df_eval$Criterion),
          showline = TRUE, linecolor = col$dim, linewidth = 2, # Linea asse marcata
          tickfont = list(color = "white", size = 12)
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
          "(<em>BERTopic</em>) on the Scopus corpus retrieved with the validated Lab&nbsp;5 query. ",
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
      tags$div(class = "sci-card",
               tags$h4(style = "font-size:15px; font-weight:700; color:white; margin:0 0 14px;",
                       "Methodology — Lab 5 (Text Analysis)"),
               tags$div(class = "query-code",
                        paste0(
                          'TITLE-ABS-KEY("smart glasses" OR "hearing glasses" OR "audio glasses"\n',
                          '              OR "smart eyewear" OR "hearing aid glasses")\n',
                          'AND TITLE-ABS-KEY("hearing" OR "audio" OR "assistive" OR "wearable")\n',
                          'AND NOT TITLE-ABS-KEY("virtual reality headset")\n',
                          'AND PUBYEAR > 2017'
                        )),
               tags$div(style = "display:grid; grid-template-columns:repeat(3, 1fr); gap:14px; margin-top:18px;",
                        insight_box("Part 1 — Bibliometric",
                                    "<code>bibliometrix::biblioAnalysis</code> for descriptive stats and <code>conceptualStructure</code> + <code>biblioNetwork</code> for co-word and collaboration networks.",
                                    col$accent, "chart-bar"),
                        insight_box("Part 1 — Tidytext",
                                    "<code>tidytext::unnest_tokens</code> &amp; <code>bind_tf_idf</code> for occurrence and term importance; <code>widyr::pairwise_count</code> + <code>tidygraph</code>/<code>ggraph</code> for the co-occurrence network.",
                                    col$blue, "calculator"),
                        insight_box("Part 2 — BERTopic",
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
      section_hdr("Author keywords — Occurrence",
                  HTML("<code>tidytext::count()</code> on the <em>DE</em> field; bar chart of the 25 most frequent keywords")),
      tags$div(class = "sci-card", plotlyOutput("plot_lab5_keywords", height = "520px")),
      
      # ── D) Co-occurrence (bigrams) ─────────────────────────────────────
      section_hdr("Co-occurrence — Top bigrams",
                  HTML("Most frequent two-word expressions in titles + abstracts (<code>widyr::pairwise_count</code>)")),
      tags$div(class = "sci-card", plotlyOutput("plot_lab5_bigrams", height = "480px")),
      
      # ── E) BERTopic — Topic distribution ───────────────────────────────
      section_hdr("BERTopic — Topic distribution",
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
                                "<strong>Bone conduction</strong> (Topic 5) is declining in relative weight — confirming the ",
                                "open-ear approach as the dominant design choice."
                              ), col$orange, "exclamation-triangle"))
      )
    )
  })
  
  # ── A) Annual production ─────────────────────────────────────────────────
  output$plot_lab5_annual <- renderPlotly({
    plot_ly(df_lab5_annual, x = ~year, y = ~articles, type = "bar",
            marker = list(color = col$accent, line = list(width = 0)),
            text = ~articles, textposition = "outside",
            textfont = list(color = col$dim, size = 10),
            hovertemplate = "%{x}: <b>%{y}</b> articles<extra></extra>") %>%
      plotly_layout_dark(
        xaxis = list(title = "", dtick = 1),
        yaxis = list(title = "Articles")
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
      section_hdr("Patent Data Analysis", "IP Landscape on Smart Glasses & Hearing Technologies (Espacenet - Lab Erre Quadro)"),
      
      tags$div(class = "sci-card", style = paste0("border-left: 5px solid ", col$accent, ";"),
               tags$h4(style="color:white; margin-top:0; font-weight:800; font-size: 15px;", "Erre Quadro Methodology — Espacenet Query"),
               tags$p(style="color:var(--dim); line-height: 1.6; font-size: 13px;", 
                      "To analyze Intellectual Property (IP), we queried the Espacenet database focusing on the intersection between visual devices (eyewear) and hearing technologies, extracting and processing statistical data for Assignees and IPC classes."),
               tags$pre(class = "query-code", style = paste0("border-color:", col$accent, "30; margin-top: 10px;"),
                        '(ti="smart glasses" OR ti="audio glasses" OR ti="eyewear") AND (txt="hearing" OR txt="audio" OR txt="acoustic")')
      ),
      
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
      plotly_layout_dark(xaxis = list(title = "Number of Patents"), yaxis = list(title = ""), margin = list(l = 120))
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
      textfont = list(color = "white", size = 11)
    )
    
    # 5. Aggiungiamo i layout
    p <- layout(p,
                xaxis = list(title = "Number of Patents", gridcolor = "#1c2840", tickfont = list(color="white"), titlefont=list(color="white")),
                yaxis = list(title = "", tickfont = list(color="white")),
                margin = list(l = 200, r = 40, t = 10, b = 40),
                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor = "rgba(0,0,0,0)"
    )
    
    # 6. ECCO LA MAGIA: Nascondiamo la barra degli strumenti fastidiosa!
    p <- config(p, displayModeBar = FALSE)
    
    # 7. Inviamo il grafico alla UI
    return(p)
  })
  
  # ═══════════════════════════════════════════════════════════════════════════════
  # TAB 9: STRATEGIC STORYBOARD (DATA VIZ LAB FINAL — RIFORMULATA)
  # ═══════════════════════════════════════════════════════════════════════════════
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
      section_hdr("Data Viz Lab", "Strategic Storytelling & Visual Communication Map"),
      
      tags$div(class = "sci-card",
               style = paste0("border-left:5px solid ", col$accent, "; background:linear-gradient(135deg,rgba(0,229,160,.05) 0%,rgba(0,0,0,0) 100%); margin-bottom:20px;"),
               tags$h4(style = "color:white; margin-top:0; font-weight:800;", "Narrative Path: The Nuance Audio Positioning Moat"),
               tags$p(style = "color:var(--dim); line-height:1.6; font-size:14px; margin:0;",
                      "This module fulfills the deliverable requirements for Data Viz Lab 5, explicitly connecting business Key Intelligence Questions (KIQs) to analytical charts derived from real data.")
      ),
      
      navset_pill(
        id = "kiq_nav",
        
        nav_panel(
          title = tagList(icon("chart-line"), " KIQ 1 — Market Technology Window"),
          value = "kiq1",
          tags$div(style = "padding-top:18px;",
                   tags$div(class = "sci-card", style = paste0("border-top:3px solid ", col$blue, "; margin-bottom:16px;"),
                            tags$div(style = paste0("font-size:11px; font-weight:700; color:", col$blue, "; text-transform:uppercase; letter-spacing:1px; margin-bottom:6px;"), "KIT 1 — STRATEGIC PACKAGING & TIMING"),
                            tags$h5(style = "color:white; font-weight:700; font-size:16px; margin:0 0 10px;",
                                    tooltip(tags$span(style = "cursor: help; border-bottom: 1px dashed rgba(255,255,255,0.4);", "Does the scientific research landscape support the emergence of this new product category?"),
                                            "Rephrased to assess technology readiness rather than market maturity.", placement = "top")),
                            tags$div(style = paste0("background:", col$blue, "15; padding:10px 14px; border-radius:6px; border-left:4px solid ", col$blue, ";"),
                                     tags$span(style = "font-size:10px; text-transform:uppercase; color:#94a3b8; font-weight:700;", "Potential Decision — "),
                                     tags$span(style = "font-size:12px; color:white;", "Confirm immediate go-to-market: scientific research has reached maturity, opening the commercial window.")
                            )
                   ),
                   fluidRow(
                     column(6,
                            tags$div(class = "sci-card",
                                     tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;", "Scientific Papers Trend (Scopus Data)"),
                                     plotlyOutput("plot_kiq1_a", height = "230px"), uiOutput("si_kiq1_a")
                            )
                     ),
                     column(6,
                            tags$div(class = "sci-card",
                                     tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;", "Patent Filing Growth (Espacenet Data)"),
                                     plotlyOutput("plot_kiq1_b", height = "230px"), uiOutput("si_kiq1_b")
                            )
                     )
                   )
          )
        ),
        
        nav_panel(
          title = tagList(icon("users"), " KIQ 2 — Social Stigma & Barriers"),
          value = "kiq2",
          tags$div(style = "padding-top:18px;",
                   tags$div(class = "sci-card", style = paste0("border-top:3px solid ", col$accent, "; margin-bottom:16px;"),
                            tags$div(style = paste0("font-size:11px; font-weight:700; color:", col$accent, "; text-transform:uppercase; letter-spacing:1px; margin-bottom:6px;"), "KIT 1 — USER ADOPTION AND LIFESTYLE BARRIERS"),
                            tags$h5(style = "color:white; font-weight:700; font-size:16px; margin:0 0 10px;",
                                    tooltip(tags$span(style = "cursor: help; border-bottom: 1px dashed rgba(255,255,255,0.4);", "What non-clinical barriers affect the adoption of traditional hearing aids?"),
                                            "Explicitly derived from your KIT 1 (Strategic Decisions).", placement = "top")),
                            tags$div(style = paste0("background:", col$accent, "15; padding:10px 14px; border-radius:6px; border-left:4px solid ", col$accent, ";"),
                                     tags$span(style = "font-size:10px; text-transform:uppercase; color:#94a3b8; font-weight:700;", "Potential Decision — "),
                                     tags$span(style = "font-size:12px; color:white;", "Push marketing communication towards fashion aesthetics rather than the medical aspects of the device.")
                            )
                   ),
                   fluidRow(
                     column(6,
                            tags$div(class = "sci-card",
                                     tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;", "Co-word Analysis (Bigrams Overview)"),
                                     plotlyOutput("plot_kiq2_a", height = "230px"), uiOutput("si_kiq2_a")
                            )
                     ),
                     column(6,
                            tags$div(class = "sci-card",
                                     tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;", "Literature Author Keywords"),
                                     plotlyOutput("plot_kiq2_b", height = "230px"), uiOutput("si_kiq2_b")
                            )
                     )
                   )
          )
        ),
        
        nav_panel(
          title = tagList(icon("shield-alt"), " KIQ 3 — Big Tech Asymmetric Threat"),
          value = "kiq3",
          tags$div(style = "padding-top:18px;",
                   tags$div(class = "sci-card", style = paste0("border-top:3px solid ", col$orange, "; margin-bottom:16px;"),
                            tags$div(style = paste0("font-size:11px; font-weight:700; color:", col$orange, "; text-transform:uppercase; letter-spacing:1px; margin-bottom:6px;"), "KIT 2 — EARLY WARNINGS ON COMPETITIVE DISRUPTION"),
                            tags$h5(style = "color:white; font-weight:700; font-size:16px; margin:0 0 10px;",
                                    tooltip(tags$span(style = "cursor: help; border-bottom: 1px dashed rgba(255,255,255,0.4);", "Which competitors are building an asymmetric IP advantage in the sector?"),
                                            "Derived from your KIT 2 to identify disruptive threats from big tech.", placement = "top")),
                            tags$div(style = paste0("background:", col$orange, "15; padding:10px 14px; border-radius:6px; border-left:4px solid ", col$orange, ";"),
                                     tags$span(style = "font-size:10px; text-transform:uppercase; color:#94a3b8; font-weight:700;", "Potential Decision — "),
                                     tags$span(style = "font-size:12px; color:white;", "Increase investments in proprietary software before the roll-out of acoustic features by Meta and Apple.")
                            )
                   ),
                   fluidRow(
                     column(6,
                            tags$div(class = "sci-card",
                                     tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;", "BERTopic — Topics Dynamics (2018-2025)"),
                                     plotlyOutput("plot_kiq3_a", height = "230px"), uiOutput("si_kiq3_a")
                            )
                     ),
                     column(6,
                            tags$div(class = "sci-card",
                                     tags$div(style = "font-size:11px; font-weight:600; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:10px;", "Espacenet — Top Corporate Patent Assignees"),
                                     plotlyOutput("plot_kiq3_b", height = "230px"), uiOutput("si_kiq3_b")
                            )
                     )
                   )
          )
        )
      )
    )
  })
  
  output$si_kiq1_a <- renderUI({ si_box("Scopus scientific production shows a mature trend consolidated over the last 3 years, confirming the solidity of the audio-visual technological foundations.", col$blue) })
  output$si_kiq1_b <- renderUI({ si_box("The trend of real patents highlights massive industrial growth, confirming that the market is ready from an IP perspective.", col$blue) })
  output$si_kiq2_a <- renderUI({ si_box("The co-occurrence of scientific keywords places 'stigma' and 'acceptance' at the center of the non-medical debate, validating the invisibility positioning.", col$accent) })
  output$si_kiq2_b <- renderUI({ si_box("Terminological clusters confirm the need for a hybrid design to intercept users who reject healthcare aesthetics.", col$accent) })
  output$si_kiq3_a <- renderUI({ si_box("BERTopic monitoring reveals the explosion of AI Captioning and AR-related interaction technologies, the backbone of Big Tech.", col$orange) })
  output$si_kiq3_b <- renderUI({ si_box("Espacenet data shows the undisputed IP leadership of Snap Inc and Meta Platforms, locking down the market with strategic patents.", col$orange) })
  
  output$plot_kiq1_a <- renderPlotly({
    plot_ly(df_lab5_annual[df_lab5_annual$year <= 2025, ], x = ~year, y = ~articles, type = "scatter", mode = "lines+markers",
            line   = list(color = col$blue, width = 3, shape = "spline"), marker = list(color = col$blue, size = 7, line = list(color = "white", width = 1.5)),
            fill = "tozeroy", fillcolor = paste0(col$blue, "25")) %>%
      plotly_layout_dark(xaxis = list(title = "", tickfont = list(size = 10)), yaxis = list(title = "Docs", tickfont = list(size = 10)), margin = list(l = 40, r = 20, t = 10, b = 30))
  })
  
  output$plot_kiq1_b <- renderPlotly({
    plot_ly(df_pat_trend, x = ~Year, y = ~Patents, type = "scatter", mode = "lines+markers",
            line   = list(color = col$cyan, width = 3, shape = "spline"), marker = list(color = col$cyan, size = 7),
            fill = "tozeroy", fillcolor = paste0(col$cyan, "25")) %>%
      plotly_layout_dark(xaxis = list(title = "", tickfont = list(size = 10)), yaxis = list(title = "Patents", tickfont = list(size = 10)), margin = list(l = 40, r = 20, t = 10, b = 30))
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
      plotly_layout_dark(xaxis = list(title = "Patents", tickfont = list(size = 10)), yaxis = list(title = "", tickfont = list(size = 10)), margin = list(l = 150, r = 40, t = 10, b = 30))
  })
} # <--- FINE DEL FILE SERVER.R (CHIUSURA PERFETTA E CORRETTA)
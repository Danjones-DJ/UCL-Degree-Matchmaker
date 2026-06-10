# =============================================================================
# UCL Degree Explorer — Shiny app ("Spotify" edition, v3)
# -----------------------------------------------------------------------------
# v3 changes:
#   * Subject matching rebuilt. Instead of a single fits/doesn't signal, the
#     app scans each course's A-level subject requirements for known subject
#     names and shows them per subject:
#       - GREEN  = mentioned in the requirements AND you take it
#       - RED    = mentioned in the requirements but you DON'T take it
#     (e.g. you take Physics but not Maths -> "Mathematics" shows red,
#     "Physics" shows green). The modal shows the full requirement text with
#     the same inline highlighting.
#   * New module filter: "Must include modules" — pick one or more compulsory
#     modules and only courses containing ALL of them are shown. Matching
#     modules are highlighted in the modal tracklist.
#
# Carried over from v2: match scoring vs your grades (exact/stretch/reach
# good, undermatch flagged & demotable), "Best match" sorting, and the
# 5-choice UCAS-style comparison shortlist.
#
# Data: expects `UCL.v2` in the environment, otherwise reads
# datasets/UCL_v2.csv — adjust the path as needed.
# =============================================================================

# Packages --------------------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, shiny, bslib, scales, htmltools)

options(scipen = 999)

# Data ------------------------------------------------------------------------
if (exists("UCL.v2")) {
  df <- UCL.v2
} else {
  df <- read_rds("data/UCL_v2.rds")
}

df <- df %>% mutate(.row_id = row_number())

CARDS_PER_PAGE <- 24
MAX_CHOICES    <- 5    # UCAS allows five applications

MAX_FAC_RANK <- max(df$faculty_competitiveness_rank, na.rm = TRUE)
MAX_UCL_RANK <- max(df$ucl_competitiveness_rank,     na.rm = TRUE)

strip_label <- function(x) sub("^[^:\n]{1,40}:\\s*\n?", "", x %||% "")

# Every compulsory module across all courses, for the module filter.
# NB: base subsetting here on purpose — `scales::discard()` masks
# `purrr::discard()` (scales loads after tidyverse), and the scales version
# chokes on a formula with "comparison (>=) is not possible for language types".
ALL_MODULES <- df$all_compulsory_modules %>%
  strsplit(" | ", fixed = TRUE) %>%
  unlist() %>%
  trimws()
ALL_MODULES <- sort(unique(ALL_MODULES[!is.na(ALL_MODULES) & ALL_MODULES != ""]))

# Grade ladder ------------------------------------------------------------------
# Consistent with `grade_points` in the data (A* = 6, A = 5, B = 4, C = 3, D = 2).
GRADE_LADDER <- c(
  "A*A*A*" = 18, "A*A*A" = 17, "A*AA" = 16, "AAA" = 15, "AAB" = 14,
  "ABB"    = 13, "BBB"   = 12, "BBC"  = 11, "BCC" = 10, "CCC" = 9,
  "CCD"    = 8,  "CDD"   = 7,  "DDD"  = 6
)

# Subject vocabulary ------------------------------------------------------------
# Canonical A-level subjects, each with the lowercase variants we scan
# requirement text for. Longest variants are matched first so "Further
# Mathematics" never gets half-claimed by "Mathematics".
SUBJECT_VOCAB <- list(
  "Further Mathematics"   = c("further mathematics", "further maths"),
  "Mathematics"           = c("mathematics", "maths"),
  "Physics"               = "physics",
  "Chemistry"             = "chemistry",
  "Biology"               = "biology",
  "English Literature"    = "english literature",
  "English Language"      = "english language",
  "History"               = "history",
  "Ancient History"       = "ancient history",
  "Classical Civilisation"= c("classical civilisation", "classical civilization"),
  "Geography"             = "geography",
  "Economics"             = "economics",
  "Business Studies"      = "business studies",
  "Psychology"            = "psychology",
  "Sociology"             = "sociology",
  "Politics"              = c("politics", "government and politics"),
  "Philosophy"            = "philosophy",
  "Religious Studies"     = "religious studies",
  "Computer Science"      = c("computer science", "computing"),
  "French"                = "french",
  "Spanish"               = "spanish",
  "German"                = "german",
  "Italian"               = "italian",
  "Latin"                 = "latin",
  "Greek"                 = "greek",
  "Art and Design"        = c("art and design", "fine art"),
  "Design and Technology" = c("design and technology", "design technology"),
  "Music"                 = "music",
  "Drama"                 = c("drama", "theatre studies"),
  "Law"                   = "law",
  "Statistics"            = "statistics"
)

ALEVEL_SUBJECTS <- names(SUBJECT_VOCAB)

# Map a (possibly free-typed) user subject onto its canonical name.
canonical_subject <- function(s) {
  sl  <- tolower(trimws(s))
  hit <- names(SUBJECT_VOCAB)[vapply(SUBJECT_VOCAB,
                                     function(a) sl %in% a, logical(1))]
  if (length(hit) > 0) hit[1] else trimws(s)
}

# Find every subject mention in a requirement string. Returns a data frame of
# (start, len, subject), longest-match-wins, no overlaps — so the text can be
# rebuilt with inline highlights.
find_mentions <- function(req, vocab) {
  if (is.na(req) || trimws(req) == "") return(NULL)
  
  res <- list()
  for (cs in names(vocab)) {
    for (p in vocab[[cs]]) {
      g <- gregexpr(paste0("\\b", p, "\\b"), req,
                    ignore.case = TRUE, perl = TRUE)[[1]]
      if (g[1] != -1) {
        res[[length(res) + 1]] <- data.frame(
          start   = as.integer(g),
          len     = attr(g, "match.length"),
          subject = cs
        )
      }
    }
  }
  if (length(res) == 0) return(NULL)
  
  m <- do.call(rbind, res)
  m <- m[order(m$start, -m$len), ]
  
  keep <- rep(TRUE, nrow(m)); last_end <- 0
  for (i in seq_len(nrow(m))) {
    if (m$start[i] <= last_end) keep[i] <- FALSE
    else last_end <- m$start[i] + m$len[i] - 1
  }
  m[keep, , drop = FALSE]
}

# Requirement text -> HTML with each mentioned subject wrapped in a green
# (you take it) or red (you don't) span.
req_to_html <- function(req, mentions, my_canon) {
  if (is.null(mentions) || nrow(mentions) == 0) {
    return(htmltools::htmlEscape(req %||% ""))
  }
  out <- ""; pos <- 1
  for (i in seq_len(nrow(mentions))) {
    s <- mentions$start[i]; l <- mentions$len[i]
    out  <- paste0(out, htmltools::htmlEscape(substr(req, pos, s - 1)))
    word <- substr(req, s, s + l - 1)
    cls  <- if (mentions$subject[i] %in% my_canon) "sp-hl-have" else "sp-hl-miss"
    out  <- paste0(out, sprintf("<span class='%s'>%s</span>",
                                cls, htmltools::htmlEscape(word)))
    pos <- s + l
  }
  paste0(out, htmltools::htmlEscape(substr(req, pos, nchar(req))))
}

# Match category / sort key (overmatch good, undermatch bad) --------------------
match_category <- function(diff) {
  case_when(
    diff <  0 ~ "Undermatch",
    diff == 0 ~ "Exact match",
    diff == 1 ~ "Stretch +1",
    TRUE      ~ paste0("Reach +", diff)
  )
}
match_priority <- function(diff) {
  if_else(diff >= 0, as.numeric(diff), 50 + abs(as.numeric(diff)))
}

# UI --------------------------------------------------------------------------
sp_theme <- bs_theme(
  version      = 5,
  bg           = "#121212",
  fg           = "#FFFFFF",
  primary      = "#1DB954",
  secondary    = "#B3B3B3",
  success      = "#1ED760",
  base_font    = font_google("Figtree"),
  heading_font = font_google("Figtree"),
  "border-radius" = "0.5rem"
)

SP_GREEN  <- "#1DB954"
SP_GREEN2 <- "#1ED760"
SP_CARD   <- "#181818"
SP_GREY   <- "#535353"
SP_MUTED  <- "#B3B3B3"

sp_css <- HTML("
  body { background-color: #121212; }

  /* ---------- Navbar ---------- */
  .navbar { background-color: #000000 !important; border-bottom: 1px solid #222; }
  .navbar-brand {
    font-weight: 800; letter-spacing: -0.02em; color: #fff !important;
    display: flex; align-items: center; gap: 10px;
  }
  .navbar-brand::before {
    content: '\\25B6';
    display: inline-flex; align-items: center; justify-content: center;
    width: 30px; height: 30px; border-radius: 50%;
    background: #1DB954; color: #000; font-size: 0.7rem; padding-left: 2px;
  }
  .nav-link { color: #B3B3B3 !important; font-weight: 700; }
  .nav-link.active { color: #fff !important; }

  /* ---------- Sidebar ---------- */
  .bslib-sidebar-layout > .sidebar { background-color: #000; border-right: 1px solid #222; }
  .sidebar label { color: #fff; font-weight: 700; font-size: 0.86rem; }
  .sidebar .help-block { color: #6a6a6a; font-size: 0.78rem; }
  .sp-side-head {
    font-size: 0.68rem; font-weight: 800; letter-spacing: 0.16em;
    text-transform: uppercase; color: #1DB954; margin: 6px 0 2px;
  }
  .sp-side-rule { border-top: 1px solid #222; margin: 14px 0 10px; }

  .form-control, .selectize-input {
    background-color: #242424 !important; border: 1px solid #242424 !important;
    color: #fff !important; border-radius: 0.5rem !important; box-shadow: none !important;
  }
  .form-control:focus, .selectize-input.focus { border-color: #1DB954 !important; }
  .form-control::placeholder { color: #757575; }
  .selectize-input > input { color: #fff !important; }
  .selectize-dropdown {
    background-color: #282828; color: #fff;
    border: 1px solid #3e3e3e; border-radius: 0.5rem;
  }
  .selectize-dropdown .option.active { background-color: #1DB954; color: #000; }
  .selectize-input .item {
    background: #1DB954 !important; color: #000 !important;
    border-radius: 500px !important; font-weight: 700;
    padding: 1px 9px !important; border: none !important;
  }
  .form-check-input { background-color: #242424; border-color: #555; }
  .form-check-input:checked { background-color: #1DB954; border-color: #1DB954; }
  .form-check-label { color: #d4d4d4; font-weight: 600; font-size: 0.82rem; }

  #card_count {
    display: inline-block; margin-top: 0.5rem;
    font-size: 0.74rem; font-weight: 700; letter-spacing: 0.12em;
    text-transform: uppercase; color: #1DB954;
  }

  /* ---------- Degree tiles ---------- */
  .sp-card {
    position: relative; cursor: pointer;
    background: #181818; border: none; border-radius: 10px;
    padding: 18px; height: 100%;
    transition: background .25s ease, opacity .25s ease;
    overflow: hidden;
  }
  .sp-card:hover { background: #282828; }
  .sp-card-under { opacity: 0.55; }
  .sp-card-under:hover { opacity: 0.9; }

  .sp-eyebrow {
    font-size: 0.66rem; font-weight: 700; letter-spacing: 0.14em;
    text-transform: uppercase; color: #1DB954; margin-bottom: 6px;
  }
  .sp-card h5 {
    font-weight: 800; font-size: 1.0rem; line-height: 1.25;
    color: #fff; margin: 0 0 4px; letter-spacing: -0.01em;
    padding-right: 34px;
  }
  .sp-faculty { color: #B3B3B3; font-size: 0.8rem; margin: 0 0 12px; }

  .sp-chip {
    display: inline-block; font-size: 0.7rem; font-weight: 700;
    color: #fff; background: rgba(255,255,255,0.10);
    border-radius: 500px; padding: 3px 10px; margin: 0 6px 6px 0;
  }

  /* Match badges */
  .sp-match {
    display: inline-block; font-size: 0.7rem; font-weight: 800;
    border-radius: 500px; padding: 3px 10px; margin: 0 6px 6px 0;
  }
  .sp-match-exact   { background: #1DB954; color: #000; }
  .sp-match-stretch { background: transparent; color: #1ED760; border: 1px solid #1DB954; }
  .sp-match-reach   { background: transparent; color: #F59B23; border: 1px solid #F59B23; }
  .sp-match-under   { background: transparent; color: #8a8a8a; border: 1px solid #555; }

  /* Per-subject requirement chips: green = you take it, red = you don't */
  .sp-req-label {
    font-size: 0.62rem; font-weight: 800; letter-spacing: 0.12em;
    text-transform: uppercase; color: #6a6a6a; margin: 8px 0 4px;
  }
  .sp-req-chip {
    display: inline-block; font-size: 0.68rem; font-weight: 700;
    border-radius: 500px; padding: 2px 9px; margin: 0 5px 5px 0;
  }
  .sp-req-have {
    background: rgba(29,185,84,0.15); color: #1ED760; border: 1px solid #1DB954;
  }
  .sp-req-miss {
    background: rgba(233,20,41,0.12); color: #ff5c74; border: 1px solid #e91429;
  }
  .sp-req-none { color: #6a6a6a; font-size: 0.72rem; font-weight: 600; }

  /* Inline highlights inside requirement text (modal) */
  .sp-hl-have { color: #1ED760; font-weight: 800; }
  .sp-hl-miss { color: #ff5c74; font-weight: 800; }

  .sp-pop { margin-top: 10px; }
  .sp-pop-label {
    font-size: 0.64rem; font-weight: 700; letter-spacing: 0.1em;
    text-transform: uppercase; color: #6a6a6a; margin-bottom: 4px;
  }
  .sp-pop-bar { height: 4px; border-radius: 2px; background: #404040; overflow: hidden; }
  .sp-pop-fill { height: 100%; background: #1DB954; border-radius: 2px; }

  .sp-play {
    position: absolute; right: 16px; bottom: 16px;
    width: 44px; height: 44px; border-radius: 50%;
    background: #1DB954; color: #000;
    display: flex; align-items: center; justify-content: center;
    font-size: 0.95rem; padding-left: 3px;
    box-shadow: 0 8px 16px rgba(0,0,0,0.45);
    opacity: 0; transform: translateY(8px);
    transition: opacity .25s ease, transform .25s ease, background .15s ease;
  }
  .sp-card:hover .sp-play { opacity: 1; transform: translateY(0); }
  .sp-play:hover { background: #1ED760; transform: scale(1.05); }

  .sp-add {
    position: absolute; top: 14px; right: 14px;
    width: 28px; height: 28px; border-radius: 50%;
    display: flex; align-items: center; justify-content: center;
    border: 1px solid #555; color: #B3B3B3; background: transparent;
    font-size: 0.95rem; line-height: 1; cursor: pointer;
    transition: border-color .15s ease, color .15s ease, background .15s ease;
  }
  .sp-add:hover { border-color: #fff; color: #fff; }
  .sp-add-on { background: #1DB954; border-color: #1DB954; color: #000; font-weight: 800; }
  .sp-add-on:hover { background: #1ED760; border-color: #1ED760; color: #000; }

  /* ---------- Buttons ---------- */
  .btn-sp {
    background: #1DB954; border: none; color: #000;
    font-weight: 800; letter-spacing: 0.02em;
    border-radius: 500px; padding: 10px 28px;
    transition: transform .12s ease, background .12s ease;
  }
  .btn-sp:hover { background: #1ED760; color: #000; transform: scale(1.04); }
  .btn-sp-outline {
    background: transparent; border: 1px solid #727272; color: #fff;
    font-weight: 700; border-radius: 500px; padding: 8px 22px;
  }
  .btn-sp-outline:hover { border-color: #fff; color: #fff; background: transparent; }
  .btn-sp-outline:disabled { opacity: 0.35; }

  /* ---------- Modal ---------- */
  .modal-content { background: #181818; border: none; border-radius: 12px; overflow: hidden; }
  .sp-modal-hero {
    background: linear-gradient(180deg, #1f5c38 0%, #14391f 55%, #181818 100%);
    padding: 44px 40px 28px;
  }
  .sp-modal-hero .sp-eyebrow { color: rgba(255,255,255,0.8); }
  .sp-modal-hero h1 {
    font-weight: 900; letter-spacing: -0.03em; color: #fff;
    font-size: clamp(1.6rem, 4vw, 2.6rem); margin: 0 0 10px;
  }
  .sp-meta { color: rgba(255,255,255,0.85); font-size: 0.88rem; font-weight: 600; }
  .sp-meta b { color: #fff; }

  .sp-section { padding: 26px 40px; }
  .sp-section + .sp-section { border-top: 1px solid rgba(255,255,255,0.06); }
  .sp-section h4 {
    font-size: 0.78rem; font-weight: 800; letter-spacing: 0.14em;
    text-transform: uppercase; color: #B3B3B3; margin-bottom: 14px;
  }
  .sp-body { color: #d4d4d4; font-size: 0.92rem; line-height: 1.65; white-space: pre-line; }
  .sp-grade-big { font-size: 2.2rem; font-weight: 900; color: #fff; letter-spacing: 0.04em; line-height: 1; }

  .sp-track {
    display: flex; align-items: center; gap: 16px;
    padding: 9px 12px; border-radius: 6px;
    color: #fff; font-size: 0.9rem; font-weight: 500;
  }
  .sp-track:hover { background: rgba(255,255,255,0.08); }
  .sp-track-num { color: #6a6a6a; width: 22px; text-align: right; font-variant-numeric: tabular-nums; }
  .sp-track:hover .sp-track-num { color: #1DB954; }
  .sp-track-code { margin-left: auto; color: #6a6a6a; font-size: 0.78rem; }
  .sp-track-hit { background: rgba(29,185,84,0.12); }
  .sp-track-hit .sp-track-num { color: #1ED760; }

  .sp-close {
    background: rgba(0,0,0,0.4); border: none; color: #fff;
    width: 34px; height: 34px; border-radius: 50%;
    font-size: 1rem; line-height: 1; cursor: pointer;
  }
  .sp-close:hover { background: rgba(0,0,0,0.7); }

  .sp-pageinfo { color: #B3B3B3; font-weight: 700; font-size: 0.85rem; }

  /* ---------- Compare ('Your choices') ---------- */
  .sp-health { background: #181818; border-radius: 12px; padding: 22px 26px; margin-bottom: 22px; }
  .sp-health h3 { font-weight: 900; color: #fff; letter-spacing: -0.02em; margin: 0 0 6px; }
  .sp-health-sub { color: #B3B3B3; font-size: 0.86rem; margin: 0 0 14px; }
  .sp-health-warn {
    border-left: 3px solid #F59B23; padding: 8px 14px; margin-top: 12px;
    color: #F5C77E; font-size: 0.85rem; background: rgba(245,155,35,0.08);
    border-radius: 0 8px 8px 0;
  }
  .sp-health-good {
    border-left: 3px solid #1DB954; padding: 8px 14px; margin-top: 12px;
    color: #9be8b9; font-size: 0.85rem; background: rgba(29,185,84,0.08);
    border-radius: 0 8px 8px 0;
  }

  .sp-compare-row { display: flex; gap: 16px; overflow-x: auto; padding-bottom: 12px; }
  .sp-compare-col {
    flex: 1 0 232px; max-width: 300px; background: #181818;
    border-radius: 12px; padding: 18px; display: flex; flex-direction: column;
  }
  .sp-compare-col h5 {
    font-weight: 800; font-size: 0.95rem; color: #fff; line-height: 1.3;
    margin: 0 0 4px; letter-spacing: -0.01em;
  }
  .sp-cmp-label {
    font-size: 0.62rem; font-weight: 800; letter-spacing: 0.14em;
    text-transform: uppercase; color: #6a6a6a; margin: 14px 0 4px;
  }
  .sp-cmp-value { color: #fff; font-weight: 700; font-size: 0.9rem; }
  .sp-remove {
    margin-top: 16px; background: transparent; border: 1px solid #555;
    color: #B3B3B3; border-radius: 500px; padding: 6px 16px;
    font-weight: 700; font-size: 0.78rem; cursor: pointer;
  }
  .sp-remove:hover { border-color: #fff; color: #fff; }
")

ui <- page_navbar(
  
  title = "UCL Degree Explorer",
  
  theme  = sp_theme,
  header = tags$head(tags$style(sp_css)),
  
  nav_panel(
    "Browse",
    
    page_sidebar(
      sidebar = sidebar(
        
        # ---- Your profile (drives the match logic) ----
        div(class = "sp-side-head", "Your profile"),
        
        selectInput(
          "my_grades", "Your predicted A-level grades",
          choices = c("Not set" = "", names(GRADE_LADDER)),
          selected = ""
        ),
        selectizeInput(
          "my_subjects", "Your A-level subjects",
          choices  = ALEVEL_SUBJECTS,
          multiple = TRUE,
          options  = list(create = TRUE, maxItems = 4,
                          placeholder = "Pick up to 4...")
        ),
        helpText("Subjects mentioned in a course's requirements show green if you take them, red if you don't."),
        
        checkboxInput(
          "hide_under",
          "Hide undermatches (aim high)",
          value = FALSE
        ),
        
        div(class = "sp-side-rule"),
        
        # ---- Filters ----
        div(class = "sp-side-head", "Filters"),
        
        textInput(
          "degree_search", "Search degrees",
          placeholder = "e.g. Economics, Neuroscience..."
        ),
        
        selectizeInput(
          "module_selected", "Must include modules",
          choices  = NULL,    # populated server-side (large list)
          multiple = TRUE,
          options  = list(placeholder = "e.g. Econometrics...")
        ),
        helpText("Only show courses whose compulsory modules include all of these."),
        
        selectInput(
          "faculty_selected", "Faculty",
          choices  = sort(unique(df$faculty)),
          multiple = TRUE
        ),
        
        selectInput(
          "type_selected", "Degree type",
          choices  = sort(unique(df$degree_type)),
          multiple = TRUE
        ),
        
        selectInput(
          "sort_by", "Sort by",
          choices = c("Best match for you" = "match",
                      "A to Z"             = "az",
                      "Toughest offer"     = "hi",
                      "Lowest offer"       = "lo"),
          selected = "match"
        ),
        
        textOutput("card_count"),
        
        width = 360
      ),
      
      tagList(
        uiOutput("degree_cards"),
        uiOutput("pagination_controls")
      )
    )
  ),
  
  nav_panel(
    "Your choices",
    div(
      style = "padding: 26px 30px;",
      uiOutput("compare_view")
    )
  )
)

# Server ----------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Populate the (large) module list server-side for snappy search.
  updateSelectizeInput(session, "module_selected",
                       choices = ALL_MODULES, server = TRUE)
  
  search_term <- reactive({ input$degree_search }) %>% debounce(400)
  
  current_page    <- reactiveVal(1)
  selected_course <- reactiveVal(NULL)
  compare_ids     <- reactiveVal(integer(0))
  
  # ---- Student profile -------------------------------------------------------
  student_points <- reactive({
    g <- input$my_grades
    if (is.null(g) || g == "") return(NA_real_)
    unname(GRADE_LADDER[g])
  })
  
  # Canonical versions of the student's subjects (handles free-typed entries).
  my_canon <- reactive({
    subs <- input$my_subjects %||% character(0)
    unique(vapply(subs, canonical_subject, character(1), USE.NAMES = FALSE))
  })
  
  # Detection vocabulary: the built-in subjects plus anything the student
  # free-typed that isn't already covered (so a custom subject like "Geology"
  # is still spotted in requirement text).
  vocab <- reactive({
    v      <- SUBJECT_VOCAB
    extras <- setdiff(my_canon(), names(v))
    for (e in extras) v[[e]] <- tolower(e)
    v
  })
  
  # ---- Scored data: match + subject mentions per course ----------------------
  scored_df <- reactive({
    pts <- student_points()
    vb  <- vocab()
    mc  <- my_canon()
    
    out <- df
    if (!is.na(pts)) {
      out <- out %>%
        mutate(
          match_diff = grade_points - pts,
          match_cat  = match_category(match_diff),
          match_pri  = match_priority(match_diff)
        )
    } else {
      out <- out %>%
        mutate(match_diff = NA_real_, match_cat = NA_character_, match_pri = 0)
    }
    
    mention_list <- lapply(out$A_Level_Subjects, find_mentions, vocab = vb)
    det  <- lapply(mention_list, function(m) if (is.null(m)) character(0) else unique(m$subject))
    
    out %>%
      mutate(
        req_mentions = mention_list,
        req_subjects = det,
        subj_have_n  = vapply(det, function(d) sum(d %in% mc),  integer(1)),
        subj_miss_n  = vapply(det, function(d) sum(!d %in% mc), integer(1))
      )
  })
  
  # ---- Reset to page 1 on any filter change -----------------------------------
  observeEvent(list(
    search_term(), input$faculty_selected, input$type_selected,
    input$module_selected, input$my_grades, input$my_subjects,
    input$hide_under, input$sort_by
  ), { current_page(1) }, ignoreInit = TRUE)
  
  observeEvent(input$prev_page, { current_page(max(1, current_page() - 1)) })
  observeEvent(input$next_page, {
    total_pages <- ceiling(nrow(filtered_degree_list()) / CARDS_PER_PAGE)
    current_page(min(total_pages, current_page() + 1))
  })
  
  # ---- Filtered + sorted list -------------------------------------------------
  filtered_degree_list <- reactive({
    
    term <- trimws(search_term() %||% "")
    data <- scored_df()
    
    if (!is.null(input$faculty_selected) && length(input$faculty_selected) > 0) {
      data <- data %>% filter(faculty %in% input$faculty_selected)
    }
    if (!is.null(input$type_selected) && length(input$type_selected) > 0) {
      data <- data %>% filter(degree_type %in% input$type_selected)
    }
    
    # Module filter: course must contain ALL selected modules.
    if (!is.null(input$module_selected) && length(input$module_selected) > 0) {
      data <- data %>%
        filter(Reduce(`&`, lapply(input$module_selected, function(m) {
          str_detect(all_compulsory_modules %||% "", fixed(m))
        })))
    }
    
    if (nchar(term) > 0) {
      data <- data %>% filter(grepl(term, title, ignore.case = TRUE))
    }
    
    if (isTRUE(input$hide_under) && !is.na(student_points())) {
      data <- data %>% filter(match_diff >= 0)
    }
    
    data <- switch(
      input$sort_by %||% "match",
      "match" = if (!is.na(student_points())) {
        # Within each match tier: fewest red (missing) subjects first,
        # then most green (held) subjects.
        data %>% arrange(match_pri, subj_miss_n, desc(subj_have_n), title)
      } else {
        data %>% arrange(subj_miss_n, desc(subj_have_n), title)
      },
      "az" = data %>% arrange(title),
      "hi" = data %>% arrange(desc(grade_points), title),
      "lo" = data %>% arrange(grade_points, title),
      data %>% arrange(title)
    )
    
    data
  })
  
  # ---- Result count -----------------------------------------------------------
  output$card_count <- renderText({
    total <- nrow(filtered_degree_list())
    page  <- current_page()
    pages <- max(1, ceiling(total / CARDS_PER_PAGE))
    paste0(total, " degrees \u2022 page ", page, " of ", pages)
  })
  
  # ---- Badge + chip helpers ----------------------------------------------------
  match_badge <- function(diff, cat) {
    if (is.na(diff)) return(NULL)
    cls <- case_when(
      diff <  0 ~ "sp-match sp-match-under",
      diff == 0 ~ "sp-match sp-match-exact",
      diff <= 2 ~ "sp-match sp-match-stretch",
      TRUE      ~ "sp-match sp-match-reach"
    )
    span(class = cls, cat)
  }
  
  # Per-subject chips: every subject detected in the requirements, green if
  # the student takes it, red if not.
  req_chips <- function(det, mc) {
    if (length(det) == 0) {
      return(tagList(
        div(class = "sp-req-label", "Required subjects"),
        span(class = "sp-req-none", "No specific subjects detected")
      ))
    }
    tagList(
      div(class = "sp-req-label", "Required subjects"),
      lapply(det, function(s) {
        span(class = paste("sp-req-chip",
                           if (s %in% mc) "sp-req-have" else "sp-req-miss"),
             if (s %in% mc) paste0("\u2713 ", s) else paste0("\u2717 ", s))
      })
    )
  }
  
  # ---- Shortlist toggle ---------------------------------------------------------
  observeEvent(input$toggle_compare, {
    id  <- as.integer(input$toggle_compare)
    cur <- compare_ids()
    
    if (id %in% cur) {
      compare_ids(setdiff(cur, id))
    } else if (length(cur) >= MAX_CHOICES) {
      showNotification(
        sprintf("UCAS gives you %d choices \u2014 remove one before adding another.", MAX_CHOICES),
        type = "warning"
      )
    } else {
      compare_ids(c(cur, id))
      ttl <- df$title[df$.row_id == id][1]
      showNotification(sprintf("Added to your choices: %s", ttl), type = "message")
    }
  })
  
  # ---- Paginated tiles -----------------------------------------------------------
  output$degree_cards <- renderUI({
    
    all_data <- filtered_degree_list()
    total    <- nrow(all_data)
    page     <- current_page()
    in_cmp   <- compare_ids()
    has_pts  <- !is.na(student_points())
    has_subj <- length(input$my_subjects %||% character(0)) > 0
    mc       <- my_canon()
    
    start <- (page - 1) * CARDS_PER_PAGE + 1
    end   <- min(page * CARDS_PER_PAGE, total)
    
    if (total == 0) {
      return(div(
        style = "padding: 60px 0; text-align: center;",
        h4("Nothing here yet", style = "color:#fff; font-weight:800;"),
        p("Try clearing a filter, removing a module, or unhiding undermatches.",
          style = "color:#B3B3B3;")
      ))
    }
    
    base_df <- all_data %>% slice(start:end)
    
    cards <- lapply(seq_len(nrow(base_df)), function(i) {
      
      row_id  <- base_df$.row_id[[i]]
      ttl     <- base_df$title[[i]]
      fac     <- base_df$faculty[[i]]
      typ     <- base_df$degree_type[[i]]
      grades  <- base_df$A_Level_Grades[[i]]
      uclr    <- base_df$ucl_competitiveness_rank[[i]]
      diff    <- base_df$match_diff[[i]]
      cat     <- base_df$match_cat[[i]]
      det     <- base_df$req_subjects[[i]]
      
      pop_pct  <- round(100 * (MAX_UCL_RANK - uclr + 1) / MAX_UCL_RANK)
      added    <- row_id %in% in_cmp
      under    <- has_pts && !is.na(diff) && diff < 0
      card_cls <- paste("sp-card", if (under) "sp-card-under" else "")
      
      div(
        class   = card_cls,
        onclick = sprintf("Shiny.setInputValue('card_clicked', '%s', {priority: 'event'})", row_id),
        
        tags$button(
          class   = paste("sp-add", if (added) "sp-add-on" else ""),
          title   = if (added) "Remove from your choices" else "Add to your choices",
          onclick = sprintf(
            "event.stopPropagation(); Shiny.setInputValue('toggle_compare', '%s', {priority: 'event'})",
            row_id
          ),
          if (added) "\u2713" else "+"
        ),
        
        div(class = "sp-eyebrow", typ),
        h5(ttl),
        p(class = "sp-faculty", sub("^Faculty of ", "", fac)),
        span(class = "sp-chip", grades),
        if (has_pts) match_badge(diff, cat),
        if (has_subj) req_chips(det, mc),
        div(
          class = "sp-pop",
          div(class = "sp-pop-label", "Competitiveness at UCL"),
          div(class = "sp-pop-bar",
              div(class = "sp-pop-fill", style = sprintf("width:%d%%;", pop_pct)))
        ),
        div(class = "sp-play", HTML("\u25B6"))
      )
    })
    
    rows <- lapply(
      split(cards, ceiling(seq_along(cards) / 3)),
      function(row_cards) {
        fluidRow(
          style = "row-gap: 16px; margin-bottom: 16px;",
          lapply(row_cards, function(crd) column(4, crd))
        )
      }
    )
    
    tagList(rows)
  })
  
  # ---- Pagination -----------------------------------------------------------------
  output$pagination_controls <- renderUI({
    total <- nrow(filtered_degree_list())
    if (total == 0) return(NULL)
    
    page        <- current_page()
    total_pages <- ceiling(total / CARDS_PER_PAGE)
    
    fluidRow(
      column(12,
             div(
               style = "display:flex; justify-content:center; align-items:center; gap:18px; padding:26px 0;",
               actionButton("prev_page", "\u2190 Previous", class = "btn-sp-outline",
                            disabled = if (page <= 1) "disabled" else NULL),
               span(class = "sp-pageinfo", paste("Page", page, "of", total_pages)),
               actionButton("next_page", "Next \u2192", class = "btn-sp-outline",
                            disabled = if (page >= total_pages) "disabled" else NULL)
             )
      )
    )
  })
  
  # ---- Modal -----------------------------------------------------------------------
  observeEvent(input$card_clicked, {
    
    row_id <- as.integer(input$card_clicked)
    course <- scored_df() %>% filter(.row_id == row_id)
    if (nrow(course) == 0) return(NULL)
    
    selected_course(row_id)
    
    ttl    <- course$title[1];          typ  <- course$degree_type[1]
    fac    <- course$faculty[1];        url  <- course$degree_url[1]
    grades <- course$A_Level_Grades[1]; subj <- course$A_Level_Subjects[1]
    about  <- strip_label(course$about_section[1])
    grad   <- strip_label(course$graduate_section[1])
    facr   <- course$faculty_competitiveness_rank[1]
    uclr   <- course$ucl_competitiveness_rank[1]
    diff   <- course$match_diff[1]
    cat    <- course$match_cat[1]
    det    <- course$req_subjects[[1]]
    ments  <- course$req_mentions[[1]]
    
    has_subj <- length(input$my_subjects %||% character(0)) > 0
    mc       <- my_canon()
    
    fac_pct <- round(100 * (MAX_FAC_RANK - facr + 1) / MAX_FAC_RANK)
    ucl_pct <- round(100 * (MAX_UCL_RANK - uclr + 1) / MAX_UCL_RANK)
    added   <- row_id %in% compare_ids()
    
    # Requirement text: inline-highlighted when subjects are set.
    req_display <- if (has_subj && !is.null(ments)) {
      HTML(req_to_html(subj, ments, mc))
    } else {
      subj %||% "None specified."
    }
    
    sel_mods <- input$module_selected %||% character(0)
    
    mods <- strsplit(course$all_compulsory_modules[1] %||% "", " | ", fixed = TRUE)[[1]]
    mods <- mods[nzchar(trimws(mods))]
    
    track_rows <- if (length(mods) > 0) {
      lapply(seq_along(mods), function(k) {
        m    <- trimws(mods[k])
        code <- str_extract(m, "\\(([^()]*)\\)$")
        name <- trimws(sub("\\(([^()]*)\\)$", "", m))
        hit  <- m %in% sel_mods
        div(class = paste("sp-track", if (hit) "sp-track-hit" else ""),
            span(class = "sp-track-num", k),
            span(name),
            span(class = "sp-track-code", gsub("[()]", "", code %||% "")))
      })
    } else {
      list(p("No compulsory modules listed for this course.", class = "sp-body"))
    }
    
    comp_bar <- function(label, pct) {
      div(style = "margin-bottom: 14px;",
          div(class = "sp-pop-label", label),
          div(class = "sp-pop-bar", style = "height:6px;",
              div(class = "sp-pop-fill", style = sprintf("width:%d%%;", pct))))
    }
    
    showModal(modalDialog(
      title = NULL, footer = NULL, size = "xl", easyClose = TRUE,
      class = "modal-fullscreen-lg-down",
      
      div(
        
        # Hero -----------------------------------------------------------------
        div(
          class = "sp-modal-hero",
          div(
            style = "display:flex; justify-content:space-between; align-items:flex-start;",
            div(class = "sp-eyebrow", "Undergraduate degree"),
            actionButton("close_modal", "\u2715", class = "sp-close")
          ),
          h1(ttl),
          p(class = "sp-meta",
            HTML(sprintf("<b>University College London</b> &bull; %s &bull; %s &bull; Typical offer <b>%s</b>",
                         fac, typ, grades))),
          div(style = "margin-top: 6px;",
              if (!is.na(diff)) match_badge(diff, cat),
              if (has_subj) req_chips(det, mc)),
          div(
            style = "margin-top: 16px; display:flex; gap:12px; flex-wrap:wrap;",
            a("Open course page \u2192", href = url, target = "_blank", class = "btn btn-sp"),
            tags$button(
              class = "btn btn-sp-outline",
              onclick = sprintf(
                "Shiny.setInputValue('toggle_compare', '%s', {priority: 'event'}); this.innerText = '\u2713 Updated';",
                row_id
              ),
              if (added) "\u2713 In your choices" else "+ Add to your choices"
            )
          )
        ),
        
        # Entry requirements + competitiveness ----------------------------------
        div(
          class = "sp-section",
          fluidRow(
            column(
              5,
              h4("Typical offer"),
              div(class = "sp-grade-big", grades),
              div(class = "sp-req-label", style = "margin-top: 14px;", "Subject requirements"),
              p(class = "sp-body", req_display)
            ),
            column(
              7,
              h4("Competitiveness"),
              comp_bar(sprintf("Within %s", sub("^Faculty of ", "faculty of ", fac)), fac_pct),
              comp_bar("Across all of UCL", ucl_pct),
              p(class = "sp-body", style = "font-size: 0.78rem; color: #6a6a6a;",
                "Fuller bars indicate more competitive entry, based on rank within the faculty and across UCL.")
            )
          )
        ),
        
        # Entry standards chart ---------------------------------------------------
        div(
          class = "sp-section",
          h4("Where this offer sits at UCL"),
          plotOutput("course_plot", height = "340px")
        ),
        
        # Module tracklist ----------------------------------------------------------
        div(
          class = "sp-section",
          h4(sprintf("Compulsory modules \u2022 %d", length(mods))),
          tagList(track_rows)
        ),
        
        # About + graduate attributes -------------------------------------------------
        div(
          class = "sp-section",
          fluidRow(
            column(7, h4("About this course"),   p(class = "sp-body", about)),
            column(5, h4("Graduate attributes"), p(class = "sp-body", grad))
          )
        )
      )
    ))
  })
  
  observeEvent(input$close_modal, { removeModal() })
  
  # ---- Entry-standards plot ------------------------------------------------------
  output$course_plot <- renderPlot({
    
    row_id <- selected_course()
    if (is.null(row_id)) return(NULL)
    
    course <- df %>% filter(.row_id == row_id)
    if (nrow(course) == 0) return(NULL)
    
    this_gp     <- course$grade_points[1]
    this_grades <- course$A_Level_Grades[1]
    my_pts      <- student_points()
    
    dist_df <- df %>%
      count(grade_points, A_Level_Grades) %>%
      group_by(grade_points) %>%
      summarise(n = sum(n), label = A_Level_Grades[which.max(n)], .groups = "drop") %>%
      mutate(is_this = grade_points == this_gp)
    
    subtitle_txt <- if (!is.na(my_pts)) {
      gap <- this_gp - my_pts
      verdict <- if (gap > 0) sprintf("an overmatch of +%d for you \u2014 ambitious, in a good way.", gap)
      else if (gap == 0) "an exact match for your grades."
      else sprintf("an undermatch of %d for you \u2014 the evidence says you can aim higher.", gap)
      sprintf("This course asks for %s; %s The white line marks your grades.", this_grades, verdict)
    } else {
      sprintf("This course asks for %s \u2014 shown in green against all %d degrees.", this_grades, nrow(df))
    }
    
    p <- ggplot(dist_df, aes(x = grade_points, y = n)) +
      geom_col(aes(fill = is_this), width = 0.62, show.legend = FALSE) +
      geom_text(aes(label = label, color = is_this),
                vjust = -0.7, fontface = "bold", size = 4, show.legend = FALSE) +
      scale_fill_manual(values = c(`TRUE` = SP_GREEN, `FALSE` = SP_GREY)) +
      scale_color_manual(values = c(`TRUE` = SP_GREEN2, `FALSE` = SP_MUTED)) +
      scale_x_continuous(breaks = sort(unique(dist_df$grade_points))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.18)))
    
    if (!is.na(my_pts)) {
      p <- p + geom_vline(xintercept = my_pts, color = "#FFFFFF",
                          linetype = "dashed", linewidth = 0.8, alpha = 0.9)
    }
    
    p +
      labs(
        title    = "Entry standards across UCL degrees",
        subtitle = str_wrap(subtitle_txt, 95),
        x = "Offer (grade points)", y = "Number of degrees"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.background    = element_rect(fill = SP_CARD, color = NA),
        panel.background   = element_rect(fill = SP_CARD, color = NA),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#2a2a2a", linewidth = 0.4),
        plot.title    = element_text(face = "bold", size = 17, color = "#FFFFFF"),
        plot.subtitle = element_text(size = 11, color = SP_MUTED, margin = margin(b = 12)),
        axis.title    = element_text(color = SP_MUTED, size = 10),
        axis.text     = element_text(color = SP_MUTED),
        axis.ticks    = element_blank()
      )
  }, bg = "transparent")
  
  # ---- Compare view ('Your choices') ------------------------------------------------
  output$compare_view <- renderUI({
    
    ids      <- compare_ids()
    has_pts  <- !is.na(student_points())
    has_subj <- length(input$my_subjects %||% character(0)) > 0
    mc       <- my_canon()
    
    if (length(ids) == 0) {
      return(div(
        style = "padding: 80px 0; text-align: center;",
        h3("Your shortlist is empty", style = "color:#fff; font-weight:900;"),
        p(style = "color:#B3B3B3; max-width: 520px; margin: 10px auto 0;",
          sprintf("Tap + on any degree in Browse to add it here. UCAS gives you %d choices \u2014 research on 184,000 UK students suggests using all of them ambitiously: exact matches and stretches, not safety-first undermatches.",
                  MAX_CHOICES))
      ))
    }
    
    sel <- scored_df() %>%
      filter(.row_id %in% ids) %>%
      arrange(match(.row_id, ids))
    
    # Portfolio health ---------------------------------------------------------
    health <- if (has_pts) {
      n_under <- sum(sel$match_diff <  0, na.rm = TRUE)
      n_exact <- sum(sel$match_diff == 0, na.rm = TRUE)
      n_over  <- sum(sel$match_diff >  0, na.rm = TRUE)
      
      msg <- if (n_under > 0) {
        div(class = "sp-health-warn",
            sprintf("%d of your %d choices undermatch your grades. Undermatching is the main way students land on lower-ranked courses than they could \u2014 consider swapping these for exact matches or stretches.",
                    n_under, length(ids)))
      } else {
        div(class = "sp-health-good",
            "No undermatches \u2014 every choice meets or stretches beyond your grades. That mirrors the application behaviour of the most successful applicants.")
      }
      
      div(
        p(class = "sp-health-sub",
          sprintf("Against your grades (%s): %d exact match%s \u2022 %d stretch/reach \u2022 %d undermatch%s \u2022 %d slot%s left",
                  input$my_grades,
                  n_exact, ifelse(n_exact == 1, "", "es"),
                  n_over,
                  n_under, ifelse(n_under == 1, "", "es"),
                  MAX_CHOICES - length(ids),
                  ifelse(MAX_CHOICES - length(ids) == 1, "", "s"))),
        msg
      )
    } else {
      p(class = "sp-health-sub",
        "Set your grades in Browse \u2192 Your profile to score this shortlist for over/undermatch.")
    }
    
    # Comparison columns ---------------------------------------------------------
    cols <- lapply(seq_len(nrow(sel)), function(i) {
      
      rid    <- sel$.row_id[[i]]
      diff   <- sel$match_diff[[i]]
      cat    <- sel$match_cat[[i]]
      det    <- sel$req_subjects[[i]]
      uclr   <- sel$ucl_competitiveness_rank[[i]]
      facr   <- sel$faculty_competitiveness_rank[[i]]
      n_mods <- length(strsplit(sel$all_compulsory_modules[[i]] %||% "", " | ", fixed = TRUE)[[1]])
      
      ucl_pct <- round(100 * (MAX_UCL_RANK - uclr + 1) / MAX_UCL_RANK)
      fac_pct <- round(100 * (MAX_FAC_RANK - facr + 1) / MAX_FAC_RANK)
      
      div(
        class = "sp-compare-col",
        div(class = "sp-eyebrow", sel$degree_type[[i]]),
        h5(sel$title[[i]]),
        p(class = "sp-faculty", sub("^Faculty of ", "", sel$faculty[[i]])),
        
        div(class = "sp-cmp-label", "Typical offer"),
        div(class = "sp-cmp-value", sel$A_Level_Grades[[i]]),
        
        if (has_pts) tagList(
          div(class = "sp-cmp-label", "Match for you"),
          match_badge(diff, cat)
        ),
        
        if (has_subj) req_chips(det, mc),
        
        div(class = "sp-cmp-label", "Competitiveness (UCL)"),
        div(class = "sp-pop-bar", style = "height:6px;",
            div(class = "sp-pop-fill", style = sprintf("width:%d%%;", ucl_pct))),
        
        div(class = "sp-cmp-label", "Competitiveness (faculty)"),
        div(class = "sp-pop-bar", style = "height:6px;",
            div(class = "sp-pop-fill", style = sprintf("width:%d%%;", fac_pct))),
        
        div(class = "sp-cmp-label", "Compulsory modules"),
        div(class = "sp-cmp-value", n_mods),
        
        div(style = "margin-top:auto;"),
        a("Course page \u2192", href = sel$degree_url[[i]], target = "_blank",
          class = "btn btn-sp", style = "margin-top:16px; text-align:center;"),
        tags$button(
          class = "sp-remove",
          onclick = sprintf("Shiny.setInputValue('toggle_compare', '%s', {priority: 'event'})", rid),
          "Remove"
        )
      )
    })
    
    tagList(
      div(
        class = "sp-health",
        h3(sprintf("Your choices \u2022 %d of %d", length(ids), MAX_CHOICES)),
        health
      ),
      div(class = "sp-compare-row", cols)
    )
  })
}

shinyApp(ui, server)

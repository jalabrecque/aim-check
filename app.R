# Aim Check -----------------------------------------------------------------
#
# A standalone version of the EpiCheck `aim_check` module that takes pasted
# text rather than a parsed paper.
#
# Deliberately depends on {shiny} alone -- no dplyr, no bslib, no DT -- so the
# WebAssembly bundle stays small and the app starts quickly in the browser.
# All scoring is base R.
#
# Nothing typed into this app is transmitted anywhere: under shinylive, R runs
# inside the user's browser via webR.

library(shiny)

# ---- patterns --------------------------------------------------------------

# Strong frames: explicit ownership of an aim. Decisive on their own -- a
# sentence saying "our aim was" is an aim statement even if it also cites
# prior work, so exclusions do not apply to these.
STRONG <- c(
  "\\bour\\s+((primary|main|principal|overall|specific|secondary|second|third|first|key|general|broad|further|additional)\\s+){0,2}(aims?|objectives?|goals?|purposes?)\\b",
  "\\b(aims?|objectives?|goals?|purposes?)\\s+of\\s+(this|the present|the current|our|the)\\s+((\\w+)\\s+){0,2}(stud(y|ies)|analys[ie]s|paper|work|article|investigation|report|research|project)\\b",
  "\\bwe\\s+(aim(ed)?|sought|set out|wished|intended)\\b",
  "\\b(this|the present|the current)\\s+(stud(y|ies)|analys[ie]s|paper|article|report)\\s+(aims?|aimed)\\b"
)

# Weak frames: first-person sentences that are often, but not always, aims.
# Subject to the exclusions below.
WEAK <- c(
  "\\bwe propose[d]?\\b",
  "\\b(in|for) (this|the present|the current) (stud(y|ies)|analys[ie]s|paper|article),? we\\b",
  "\\b(here|therefore|thus|hence|accordingly),? we\\b",
  "\\bwe (therefore |thus )?(conducted|undertook|performed|carried out|present|use[d]?|examine[d]?|estimate[d]?|assess(ed)?|evaluate[d]?|investigate[d]?|describe[d]?|develop(ed)?|compare[d]?|quantif(y|ied)|test(ed)?)\\b",
  "\\b(this|the present|the current) (stud(y|ies)|analys[ie]s|paper|article|report) (examine[sd]?|investigate[sd]?|assesse[sd]?|evaluate[sd]?|estimate[sd]?|describe[sd]?|use[sd]?|compare[sd]?|quantifie[sd]?)\\b",
  "\\bto (address|fill|overcome) (this|these|the) (gap|limitation|uncertaint|question)"
)

# Gap statements. In epidemiology introductions the aim is usually the
# sentence immediately following one of these.
GAP <- c(
  "\\b(little|less) is known\\b",
  "\\bfew stud(y|ies) have\\b",
  "\\bno stud(y|ies) (has|have)\\b",
  "\\bremains? (unclear|unknown|uncertain|poorly understood|to be)\\b",
  "\\bhas not been (studied|examined|investigated|established|assessed)\\b",
  "\\bevidence is (limited|sparse|scarce|lacking|mixed|inconsistent)\\b"
)

# Sentences describing someone else's work, or aim language used for an
# analytic choice rather than a research question.
EXCLUDE <- c(
  "\\bet al\\b",
  "\\b(previous|prior|earlier|other|several|numerous|many|recent) (stud(y|ies)|authors|research(ers)?|work|reports?|investigations?|trials?|analyses)\\b",
  "\\baim(ed)? to (adjust|control|account) for\\b"
)

# Task vocabulary. A lexical annotation only -- see the note in the UI.
SIGNALS <- list(
  causal = c(
    "\\beffects? of\\b", "\\bimpact of\\b", "\\binfluence of\\b",
    "\\beffectiveness\\b", "\\befficacy\\b", "\\bbenefits? of\\b",
    "\\bcaus(e|es|ed|al|ally|ation)\\b", "\\bcontribution of\\b",
    "\\bextent to which\\b", "\\battributable (risk|fraction)\\b",
    "\\btarget trial\\b", "\\bemulat", "\\bcounterfactual\\b",
    "\\bmediat", "\\b(direct|indirect|total) effects?\\b",
    "\\bwhether \\w+ (reduce[sd]?|increase[sd]?|prevent[sd]?|lower[sd]?)\\b"
  ),
  descriptive = c(
    "\\bdescribe\\b", "\\bcharacteri[sz]e\\b", "\\bprevalence of\\b",
    "\\bincidence of\\b", "\\bdistribution of\\b", "\\btrends in\\b",
    "\\bburden of\\b", "\\bpatterns of\\b", "\\bfrequency of\\b"
  ),
  predictive = c(
    "\\b(develop|derive|validate|update)\\w*\\b.{0,25}\\b(prediction|prognostic|risk) model\\b",
    "\\brisk score\\b", "\\bpredictive (performance|accuracy)\\b",
    "\\bdiscrimination\\b", "\\bcalibration\\b", "\\bc.statistic\\b",
    "\\bAUC\\b", "\\bidentify individuals at\\b"
  ),
  associational = c(
    "\\bassociation between\\b", "\\bassociated with\\b",
    "\\brelationship between\\b", "\\blink(ed)? (between|to)\\b",
    "\\bcorrelates of\\b", "\\brisk factors? for\\b",
    "\\bdeterminants? of\\b", "\\bpredictors? of\\b", "\\brole of\\b"
  )
)

# ---- helpers ---------------------------------------------------------------

g <- function(patterns, x) {
  grepl(paste(patterns, collapse = "|"), x, ignore.case = TRUE, perl = TRUE)
}

#' Split pasted text into sentences
#'
#' Abbreviations are masked before splitting so "e.g." and "et al." do not
#' create spurious sentence boundaries.
split_sentences <- function(txt) {
  txt <- gsub("[[:space:]]+", " ", trimws(txt))
  if (!nzchar(txt)) return(character(0))

  abbr <- c("e\\.g\\.", "i\\.e\\.", "et al\\.", "vs\\.", "cf\\.", "approx\\.",
            "ca\\.", "Fig\\.", "No\\.", "Dr\\.", "Prof\\.", "St\\.",
            "Inc\\.", "Ltd\\.", "\\bal\\.")
  for (a in abbr) {
    m <- regmatches(txt, gregexpr(a, txt, ignore.case = TRUE, perl = TRUE))[[1]]
    for (hit in unique(m)) {
      txt <- gsub(hit, gsub("\\.", "\u0001", hit), txt, fixed = TRUE)
    }
  }

  parts <- unlist(strsplit(txt, "(?<=[.!?])\\s+(?=[A-Z(\"'])", perl = TRUE))
  parts <- gsub("\u0001", ".", parts, fixed = TRUE)
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

score_sentences <- function(sentences) {
  n <- length(sentences)
  if (n == 0) return(NULL)

  strong <- g(STRONG, sentences)
  weak   <- g(WEAK, sentences)
  excl   <- g(EXCLUDE, sentences)
  gap    <- g(GAP, sentences)

  # a gap statement in the preceding sentence lends support
  gap_before <- c(FALSE, gap[-n])

  score <- (3 * strong) + (2 * (weak & !excl)) + (1 * gap_before)

  confidence <- ifelse(score >= 3, "high",
                ifelse(score == 2, "medium",
                ifelse(score == 1, "low", NA_character_)))

  sig <- vapply(sentences, function(s) {
    hits <- names(SIGNALS)[vapply(SIGNALS, function(p) g(p, s), logical(1))]
    if (!length(hits)) "none" else paste(hits, collapse = " + ")
  }, character(1), USE.NAMES = FALSE)

  data.frame(
    sentence   = sentences,
    confidence = confidence,
    signal     = sig,
    strong     = strong,
    weak       = weak,
    excluded   = excl & !strong,
    gap_before = gap_before,
    stringsAsFactors = FALSE
  )
}

#' Which task does the vocabulary point to, if any?
classify_signal <- function(sig) {
  tasks <- c("causal", "descriptive", "predictive")
  present <- tasks[vapply(tasks, function(t) any(grepl(t, sig, fixed = TRUE)),
                          logical(1))]
  if (length(present) == 1) present else "ambiguous"
}

EXAMPLE_TEXT <- paste(
  "Few studies have examined whether long-term exposure to ambient air",
  "pollution is related to cognitive decline in older adults.",
  "Our aim was to estimate the association between residential PM2.5",
  "concentrations and change in cognitive test scores over ten years in a",
  "population-based cohort."
)

# ---- ui --------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$title("Aim Check"),
    tags$style(HTML("
      html, body { font-size: 18px; }
      body { font-family: system-ui, -apple-system, 'Segoe UI', Roboto, Arial, sans-serif;
             color: #222; background: #fff; line-height: 1.55; }
      .container-fluid { max-width: 760px; margin: 0 auto; padding: 32px 16px 48px; }
      h1 { font-size: 28px; font-weight: 600; margin: 0 0 8px; }
      h2 { font-size: 18px; font-weight: 600; margin: 32px 0 8px; }
      p.intro { margin: 0 0 4px; }
      p.small, .small { font-size: 15px; color: #666; }
      textarea.form-control { font-size: 18px; line-height: 1.5; padding: 10px 12px;
                              border-radius: 4px; }
      .btn { font-size: 17px; padding: 8px 22px; border-radius: 4px; }
      .btn-primary { background: #1f4e79; border-color: #1f4e79; }
      a.action-button.example { font-size: 16px; margin-left: 16px; color: #1f4e79; }
      table.res { width: 100%; border-collapse: collapse; margin-top: 8px; }
      table.res td { padding: 10px 0; border-top: 1px solid #ddd; vertical-align: top; }
      table.res td.k { width: 190px; font-weight: 600; padding-right: 16px; }
      .status { font-weight: 600; }
      .ok   { color: #2f6b3a; }
      .warn { color: #8a5a00; }
      .bad  { color: #a1332a; }
      table.sent { width: 100%; border-collapse: collapse; font-size: 16px; }
      table.sent th { text-align: left; font-weight: 600; font-size: 14px; color: #555;
                      border-bottom: 1px solid #bbb; padding: 6px 8px 6px 0; }
      table.sent td { border-bottom: 1px solid #e5e5e5; padding: 8px 8px 8px 0;
                      vertical-align: top; }
      table.sent td.c { white-space: nowrap; font-size: 15px; color: #444; }
      .foot { margin-top: 36px; padding-top: 12px; border-top: 1px solid #ddd; }
      @media (max-width: 560px) { table.res td.k { width: 120px; } }
    "))
  ),

  h1("Aim Check"),
  p(class = "intro",
    "Paste the aim of your study as it appears in the abstract or at the end",
    "of the introduction."),
  p(class = "small", "The check runs in your browser. Your text is not sent anywhere."),

  textAreaInput("txt", NULL, value = "", rows = 6, width = "100%",
                placeholder = "The aim of this study was to ..."),

  div(
    actionButton("go", "Check", class = "btn-primary"),
    actionLink("example", "Load an example", class = "example")
  ),

  uiOutput("results")
)

# ---- server ----------------------------------------------------------------

server <- function(input, output, session) {

  observeEvent(input$example, {
    updateTextAreaInput(session, "txt", value = EXAMPLE_TEXT)
  })

  scored <- eventReactive(input$go, {
    score_sentences(split_sentences(input$txt))
  })

  output$results <- renderUI({
    req(input$go)
    d <- scored()

    if (is.null(d) || !nrow(d)) {
      return(p(class = "bad", style = "margin-top:24px;", "Please paste some text first."))
    }

    found <- d[!is.na(d$confidence), , drop = FALSE]
    if (nrow(found)) {
      rank <- match(found$confidence, c("high", "medium", "low"))
      found <- found[rank == min(rank), , drop = FALSE]
    }

    example_frame <- "e.g. \"The aim of this study was to ...\""

    # aim statement
    if (!nrow(found)) {
      aim <- list(span(class = "status bad", "Not found. "),
                  "No sentence states what the study set out to do. ",
                  "Consider stating it directly, ", example_frame, ".")
    } else if (all(found$confidence == "high")) {
      aim <- list(span(class = "status ok", "Found. "),
                  "The aim is stated explicitly.")
    } else {
      aim <- list(span(class = "status warn", "Unclear. "),
                  "A sentence reads like an aim but is not introduced as one. ",
                  "Consider stating it directly, ", example_frame, ".")
    }

    # type of question
    if (!nrow(found)) {
      qtype <- list(span(class = "small", "Not assessed (no aim found)."))
    } else {
      sig <- paste(found$signal, collapse = " ")
      task <- classify_signal(sig)
      assoc <- grepl("associational", sig, fixed = TRUE)
      if (task != "ambiguous") {
        qtype <- list(span(class = "status ok",
                           paste0(toupper(substring(task, 1, 1)), substring(task, 2), ". ")),
                      "The wording suggests a ", task, " question.",
                      if (assoc) " It also uses associational wording, which on its own does not indicate the type of question.")
      } else if (assoc) {
        qtype <- list(span(class = "status warn", "Unclear. "),
                      "Terms such as \"association between\", \"risk factors for\" and ",
                      "\"predictors of\" are used for descriptive, predictive and causal ",
                      "questions alike. State which one the study addresses.")
      } else {
        qtype <- list(span(class = "status warn", "Unclear. "),
                      "The wording does not indicate whether the question is ",
                      "descriptive, predictive or causal.")
      }
    }

    # sentences
    rows <- lapply(seq_len(nrow(d)), function(i) {
      r <- d[i, ]
      tags$tr(
        tags$td(r$sentence),
        tags$td(class = "c", if (is.na(r$confidence)) "no" else r$confidence),
        tags$td(class = "c", if (r$signal == "none") "" else gsub(" \\+ ", ", ", r$signal))
      )
    })

    tagList(
      tags$table(class = "res", style = "margin-top:28px;",
        tags$tr(tags$td(class = "k", "Aim statement"), tags$td(aim)),
        tags$tr(tags$td(class = "k", "Type of question"), tags$td(qtype))
      ),
      h2("Sentences"),
      tags$table(class = "sent",
        tags$tr(tags$th("Sentence"), tags$th("Aim"), tags$th("Wording")),
        rows
      ),
      div(class = "foot small",
          p("Based on wording only. It does not assess whether the question is ",
            "a good one or whether the design can answer it. Question types follow ",
            "Hernán, Hsu and Healy (2019), ", tags$em("Chance"), " 32(1):42–49."),
          p("Part of EpiCheck. ",
            tags$a(href = "https://jeremylabrecque.org", target = "_top",
                   "Jeremy Labrecque", .noWS = "after"), ", Erasmus MC."))
    )
  })
}

shinyApp(ui, server)

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
      body { font-family: Palatino, 'Palatino Linotype', 'Book Antiqua',
             Georgia, serif; color: #1a1a1a; background: #fbfaf8; }
      .container-fluid { max-width: 860px; margin: 0 auto; padding: 2.5rem 1.5rem; }
      h1 { font-size: 1.6rem; font-weight: normal; letter-spacing: .01em;
           margin: 0 0 .4rem 0; }
      .lede { color: #555; font-size: .95rem; margin-bottom: 1.8rem;
              line-height: 1.55; }
      textarea { font-family: Palatino, Georgia, serif !important;
                 font-size: .95rem !important; line-height: 1.5;
                 border: 1px solid #ccc7bf !important; border-radius: 2px; }
      .btn { border-radius: 2px; font-family: inherit; }
      .btn-default { background: #2d2d2d; color: #fff; border: none;
                     padding: .45rem 1.2rem; }
      .btn-link { color: #666; padding-left: 0; }
      .verdict { padding: .9rem 1.1rem; border-left: 4px solid #999;
                 background: #f3f1ed; margin: 1.6rem 0 1.2rem 0;
                 font-size: .97rem; line-height: 1.5; }
      .v-green  { border-left-color: #3f7d4e; }
      .v-yellow { border-left-color: #b8892b; }
      .v-red    { border-left-color: #a33b32; }
      .sent { padding: .7rem 0; border-bottom: 1px solid #e7e3dc; }
      .sent-text { font-size: .95rem; line-height: 1.5; }
      .meta { font-size: .8rem; color: #6b6b6b; margin-top: .3rem;
              font-family: -apple-system, 'Segoe UI', sans-serif; }
      .tag { display: inline-block; padding: .05rem .45rem; margin-right: .4rem;
             border: 1px solid #c9c4bb; border-radius: 2px; font-size: .75rem; }
      .note { font-size: .87rem; color: #555; line-height: 1.55;
              margin-top: 1.4rem; padding-top: 1rem;
              border-top: 1px solid #e7e3dc; }
      h2 { font-size: 1.05rem; font-weight: normal; margin: 1.8rem 0 .3rem 0; }
    "))
  ),

  h1("Aim Check"),
  div(class = "lede",
      "Paste the sentence or sentences in which your paper states what it set",
      "out to do — the abstract objective, the end of the introduction, or",
      "both. This tool reports whether an aim statement can be located, and",
      "what kind of research question its wording points to.",
      tags$br(),
      tags$span(style = "color:#777;",
                "Everything runs in your browser. No text is sent anywhere.")),

  textAreaInput("txt", NULL, value = "", rows = 7, width = "100%",
                placeholder = "Paste your aim statement here..."),

  div(
    actionButton("go", "Check aim"),
    actionLink("example", "use an example", style = "margin-left: 1rem;")
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
      return(div(class = "verdict v-red",
                 strong("No text entered."),
                 " Paste your aim statement above and press Check aim."))
    }

    found <- d[!is.na(d$confidence), , drop = FALSE]

    # keep only the best confidence level present
    if (nrow(found)) {
      rank <- match(found$confidence, c("high", "medium", "low"))
      found <- found[rank == min(rank), , drop = FALSE]
    }

    # --- verdict ---
    if (!nrow(found)) {
      verdict <- div(
        class = "verdict v-red",
        strong("No aim statement found."),
        " None of the sentences you pasted states what the study set out to",
        " do in a form a reader (or a text-mining tool) will recognise.",
        " Consider an explicit frame: ",
        tags$em("“Our aim was to…”"), ", ",
        tags$em("“The objective of this study was to…”"), ", or ",
        tags$em("“We sought to…”"), "."
      )
    } else {
      conf <- unique(found$confidence)
      cls <- if (all(conf == "high")) "v-green" else "v-yellow"
      msg <- if (all(conf == "high")) {
        list(strong("Aim statement found."),
             " The wording explicitly announces the study's objective.")
      } else {
        list(strong("Possible aim statement found, with lower confidence."),
             " The sentence below reads as an aim but does not announce itself",
             " as one. An explicit frame would make it unambiguous to a reader",
             " skimming the paper, and to anyone text-mining the literature.")
      }
      verdict <- div(class = paste("verdict", cls), msg)
    }

    # --- task signal ---
    task_block <- NULL
    if (nrow(found)) {
      sig <- paste(found$signal, collapse = " ")
      task <- classify_signal(sig)
      assoc <- grepl("associational", sig, fixed = TRUE)

      task_text <- if (task == "ambiguous" && assoc) {
        list(strong("The wording does not distinguish descriptive, predictive",
                    " and causal questions."),
             " Phrases such as ", tags$em("association between"), ", ",
             tags$em("risk factors for"), " and ", tags$em("predictors of"),
             " are used in epidemiology for all three. A reader cannot tell",
             " which question is being answered, and the three require",
             " different assumptions and license different interpretations.")
      } else if (task == "ambiguous") {
        list(strong("No task vocabulary detected."),
             " The aim does not signal whether the question is descriptive,",
             " predictive or causal.")
      } else {
        list("The vocabulary points to a ", strong(task), " question.",
             if (assoc) list(" Associational wording is also present, which",
                             " does not distinguish the three tasks."))
      }

      task_block <- div(
        class = paste("verdict", if (task == "ambiguous") "v-yellow" else "v-green"),
        task_text
      )
    }

    # --- sentence detail ---
    rows <- lapply(seq_len(nrow(d)), function(i) {
      r <- d[i, ]
      tags_ <- list()
      if (!is.na(r$confidence)) {
        tags_ <- c(tags_, list(span(class = "tag",
                                    paste("aim:", r$confidence))))
      }
      if (r$signal != "none") {
        tags_ <- c(tags_, list(span(class = "tag", r$signal)))
      }
      why <- c(
        if (r$strong) "explicit aim frame",
        if (r$weak && !r$strong && !r$excluded) "first-person study frame",
        if (r$excluded) "reads as a description of other work",
        if (r$gap_before) "follows a gap statement"
      )
      div(
        class = "sent",
        div(class = "sent-text", r$sentence),
        div(class = "meta",
            tags_,
            if (length(why)) paste0("— ", paste(why, collapse = "; "))
            else "— no aim frame detected")
      )
    })

    tagList(
      verdict,
      task_block,
      h2("Sentence by sentence"),
      div(rows),
      div(class = "note",
          strong("How to read this."),
          " The task label comes from vocabulary alone. It reports what the",
          " words signal, not what you meant, and it is not a judgement about",
          " whether the question is a good one or whether your design can",
          " answer it. A stated aim that names its task explicitly —",
          tags$em(" “we estimated the effect of…”"), ",",
          tags$em(" “we developed a prediction model for…”"), ",",
          tags$em(" “we described the distribution of…”"),
          " — leaves nothing for the reader to infer."),
      div(class = "note",
          "Part of ", tags$a(href = "https://jeremylabrecque.org",
                             "EpiCheck"),
          ". Aim taxonomy after Hernán, Hsu & Healy (2019), ",
          tags$em("Chance"), " 32(1), 42–49.")
    )
  })
}

shinyApp(ui, server)

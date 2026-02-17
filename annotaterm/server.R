source("global.R")

server <- function(input, output, session) {
  # One row per span (observation)
  annotations_rv <- reactiveVal(
    data.frame(
      term_id    = character(),  # T1, T2... for simple; C1, C2... for composite
      type       = character(),
      term_label = character(),
      segment    = character(),
      start      = integer(),
      finish     = integer(),
      color      = character(),  # internal
      stringsAsFactors = FALSE
    )
  )
  
  composite_builder <- reactiveVal(
    data.frame(
      segment = character(),
      start   = integer(),
      finish  = integer(),
      stringsAsFactors = FALSE
    )
  )
  
  simple_counter <- reactiveVal(0L)
  composite_counter <- reactiveVal(0L)
  
  # Palette cycles per mention
  palette <- c("#FFE066", "#B5E48C", "#A0C4FF", "#FFADAD")
  next_color_index <- reactiveVal(0L)
  
  take_next_color <- function() {
    i <- next_color_index() + 1L
    next_color_index(i)
    palette[(i - 1L) %% length(palette) + 1L]
  }
  
  # Duplicate check: exact same span already present (regardless of term_id)
  is_duplicate_span <- function(df, segment, start, finish) {
    any(df$segment == segment & df$start == start & df$finish == finish)
  }
  
  # Keep delete-by-term_id choices in sync
  observe({
    df <- annotations_rv()
    ids <- unique(df$term_id)
    updateSelectInput(session, "delete_term_id", choices = ids)
  })
  
  output$selection_text <- renderText({
    sel <- input$current_selection
    if (is.null(sel) || is.null(sel$text) || !nzchar(sel$text)) {
      return("No selection.")
    }
    sprintf("'%s' [start=%d, finish=%d]", sel$text, sel$start, sel$end)
  })
  
  # ---- Add simple term (T1, T2, ...) ----
  observeEvent(input$add_simple, {
    sel <- input$current_selection
    req(sel, sel$text, nzchar(sel$text), sel$start, sel$end)
    
    df <- annotations_rv()
    if (is_duplicate_span(df, sel$text, sel$start, sel$end)) {
      showNotification("This exact annotation already exists.", type = "message")
      return()
    }
    
    id_num <- simple_counter() + 1L
    simple_counter(id_num)
    term_id <- paste0("T", id_num)
    
    new_row <- data.frame(
      term_id    = term_id,
      type       = "simple",
      term_label = "",
      segment    = sel$text,
      start      = as.integer(sel$start),
      finish     = as.integer(sel$end),
      color      = take_next_color(),
      stringsAsFactors = FALSE
    )
    
    annotations_rv(rbind(df, new_row))
  })
  
  # ---- Composite builder ----
  observeEvent(input$reset_composite, {
    composite_builder(
      data.frame(
        segment = character(),
        start = integer(),
        finish = integer(),
        stringsAsFactors = FALSE
      )
    )
  })
  
  observeEvent(input$add_segment, {
    sel <- input$current_selection
    req(sel, sel$text, nzchar(sel$text), sel$start, sel$end)
    
    seg_df <- composite_builder()
    new_row <- data.frame(
      segment = sel$text,
      start   = as.integer(sel$start),
      finish  = as.integer(sel$end),
      stringsAsFactors = FALSE
    )
    
    # avoid identical duplicate segment span inside the builder
    is_dup <- seg_df$segment == new_row$segment &
      seg_df$start == new_row$start &
      seg_df$finish == new_row$finish
    
    if (!any(is_dup)) {
      composite_builder(rbind(seg_df, new_row))
    }
  })
  
  output$composite_segments_text <- renderText({
    seg_df <- composite_builder()
    if (nrow(seg_df) == 0) return("No segments yet.")
    paste(sprintf("%s [%d-%d]", seg_df$segment, seg_df$start, seg_df$finish), collapse = " + ")
  })
  
  # ---- Save composite (C1, C2, ...) ----
  observeEvent(input$save_composite, {
    seg_df <- composite_builder()
    
    if (nrow(seg_df) < 2) {
      showNotification("Composite terms need at least 2 segments.", type = "warning")
      return()
    }
    
    df <- annotations_rv()
    
    # Prevent saving if ANY segment span duplicates an existing annotation span
    dups <- mapply(
      function(seg, st, fn) is_duplicate_span(df, seg, st, fn),
      seg_df$segment, seg_df$start, seg_df$finish
    )
    
    if (any(dups)) {
      showNotification("Some composite segments already exist as annotations; not saved.", type = "warning")
      return()
    }
    
    id_num <- composite_counter() + 1L
    composite_counter(id_num)
    term_id <- paste0("C", id_num)
    
    color <- take_next_color()
    
    new_rows <- data.frame(
      term_id    = term_id,
      type       = "composite",
      term_label = "",
      segment    = seg_df$segment,
      start      = seg_df$start,
      finish     = seg_df$finish,
      color      = color,
      stringsAsFactors = FALSE
    )
    
    annotations_rv(rbind(df, new_rows))
    
    # reset builder
    composite_builder(
      data.frame(
        segment = character(),
        start = integer(),
        finish = integer(),
        stringsAsFactors = FALSE
      )
    )
  })
  
  # ---- Delete / clear ----
  observeEvent(input$delete_last, {
    df <- annotations_rv()
    if (nrow(df) == 0) return()
    
    last_id <- df$term_id[nrow(df)]
    annotations_rv(df[df$term_id != last_id, , drop = FALSE])
  })
  
  observeEvent(input$delete_term_id_btn, {
    df <- annotations_rv()
    id <- input$delete_term_id
    if (is.null(id) || !nzchar(id) || nrow(df) == 0) return()
    annotations_rv(df[df$term_id != id, , drop = FALSE])
  })
  
  observeEvent(input$clear_all, {
    annotations_rv(annotations_rv()[0, ])
    composite_builder(
      data.frame(
        segment = character(),
        start = integer(),
        finish = integer(),
        stringsAsFactors = FALSE
      )
    )
  })
  
  # ---- Table (hide color, hide label) ----
  output$annotations_table <- renderTable({
    df <- annotations_rv()
    if (nrow(df) == 0) return(NULL)
    df <- df[order(df$start, df$finish), ]
    df[, c("term_id", "type", "segment", "start", "finish")]
  })
  
  # ---- Export ----
  output$download_csv <- downloadHandler(
    filename = function() sprintf("annotaterm_%s.csv", Sys.Date()),
    content = function(file) {
      df <- annotations_rv()
      out <- df[, c("term_id", "type", "segment", "start", "finish")]
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$download_json <- downloadHandler(
    filename = function() sprintf("annotaterm_%s.json", Sys.Date()),
    content = function(file) {
      df <- annotations_rv()
      out <- df[, c("term_id", "type", "segment", "start", "finish")]
      
      grouped <- split(out, out$term_id)
      json_obj <- lapply(grouped, function(d) {
        list(
          term_id = d$term_id[1],
          type = d$type[1],
          spans = lapply(seq_len(nrow(d)), function(i) {
            list(segment = d$segment[i], start = d$start[i], finish = d$finish[i])
          })
        )
      })
      
      jsonlite::write_json(json_obj, file, auto_unbox = TRUE, pretty = TRUE)
    }
  )
  
  # ---- Overlap-friendly preview with tooltips ----
  output$annotated_text <- renderUI({
    text <- input$input_text
    df <- annotations_rv()
    
    if (!nzchar(text) || nrow(df) == 0) {
      return(HTML(htmlEscape(text)))
    }
    
    df <- df[!is.na(df$start) & !is.na(df$finish), ]
    if (nrow(df) == 0) return(HTML(htmlEscape(text)))
    df <- df[order(df$start, df$finish), ]
    
    text_len <- nchar(text)
    
    cuts <- sort(unique(c(
      1L,
      text_len + 1L,
      pmax(1L, pmin(text_len + 1L, df$start)),
      pmax(1L, pmin(text_len + 1L, df$finish + 1L))
    )))
    cuts <- cuts[cuts >= 1L & cuts <= text_len + 1L]
    
    bg_style <- function(cols_rgba) {
      cols_rgba <- unique(cols_rgba)
      if (length(cols_rgba) == 1) {
        return(sprintf("background:%s;", cols_rgba[1]))
      }
      cols_rgba <- cols_rgba[seq_len(min(length(cols_rgba), 4L))]
      n <- length(cols_rgba)
      stops <- seq(0, 100, length.out = n + 1)
      parts <- character()
      for (i in seq_len(n)) {
        parts <- c(parts, sprintf("%s %.1f%% %.1f%%", cols_rgba[i], stops[i], stops[i + 1]))
      }
      sprintf("background:linear-gradient(90deg,%s);", paste(parts, collapse = ","))
    }
    
    out <- character()
    
    for (i in seq_len(length(cuts) - 1L)) {
      st <- cuts[i]
      en <- cuts[i + 1L] - 1L
      if (st > en) next
      
      chunk <- substr(text, st, en)
      chunk_esc <- htmlEscape(chunk)
      
      active <- df$start <= st & df$finish >= en
      
      if (!any(active)) {
        out <- c(out, chunk_esc)
      } else {
        active_df <- df[active, , drop = FALSE]
        cols <- vapply(active_df$color, hex_to_rgba, character(1), alpha = 0.35)
        style <- bg_style(cols)
        
        # Tooltip: show all covering term_ids/types for this chunk
        tip <- paste(
          sprintf("%s (%s) [%d-%d]", active_df$term_id, active_df$type, active_df$start, active_df$finish),
          collapse = " | "
        )
        tip <- htmlEscape(tip)
        
        out <- c(out, sprintf(
          "<span class='term-span' title='%s' style='%s'>%s</span>",
          tip, style, chunk_esc
        ))
      }
    }
    
    HTML(paste(out, collapse = ""))
  })
}
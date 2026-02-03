server <- function(input, output, session) {
  annotations_rv <- reactiveVal(
    data.frame(
      term_id    = character(),
      type       = character(),
      term_label = character(),
      segment    = character(),
      start      = integer(),
      finish     = integer(),
      color      = character(),
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
  
  palette <- c("#FFE066", "#B5E48C", "#A0C4FF", "#FFADAD")
  next_color_index <- reactiveVal(0L)
  
  take_next_color <- function() {
    i <- next_color_index() + 1L
    next_color_index(i)
    palette[(i - 1L) %% length(palette) + 1L]
  }
  
  output$selection_text <- renderText({
    sel <- input$current_selection
    if (is.null(sel) || is.null(sel$text) || !nzchar(sel$text)) return("No selection.")
    sprintf("'%s' [start=%d, finish=%d]", sel$text, sel$start, sel$end)
  })
  
  observeEvent(input$add_simple, {
    sel <- input$current_selection
    req(sel, sel$text, nzchar(sel$text), sel$start, sel$end)
    
    id_num <- simple_counter() + 1L
    simple_counter(id_num)
    term_id <- paste0("T", id_num)
    
    label <- sel$text
    df <- annotations_rv()
    new_row <- data.frame(
      term_id    = term_id,
      type       = "simple",
      term_label = label,
      segment    = label,
      start      = as.integer(sel$start),
      finish     = as.integer(sel$end),
      color      = take_next_color(),
      stringsAsFactors = FALSE
    )
    annotations_rv(rbind(df, new_row))
  })
  
  observeEvent(input$reset_composite, {
    composite_builder(data.frame(segment = character(), start = integer(), finish = integer(), stringsAsFactors = FALSE))
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
    
    is_dup <- seg_df$segment == new_row$segment &
      seg_df$start == new_row$start &
      seg_df$finish == new_row$finish
    
    if (!any(is_dup)) composite_builder(rbind(seg_df, new_row))
  })
  
  output$composite_segments_text <- renderText({
    seg_df <- composite_builder()
    if (nrow(seg_df) == 0) return("No segments yet.")
    paste(sprintf("%s [%d-%d]", seg_df$segment, seg_df$start, seg_df$finish), collapse = " + ")
  })
  
  observeEvent(input$save_composite, {
    seg_df <- composite_builder()
    req(nrow(seg_df) > 0)
    
    id_num <- composite_counter() + 1L
    composite_counter(id_num)
    term_id <- paste0("C", id_num)
    
    color <- take_next_color()
    
    df <- annotations_rv()
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
    
    composite_builder(data.frame(segment = character(), start = integer(), finish = integer(), stringsAsFactors = FALSE))
  })
  
  output$annotations_table <- renderTable({
    df <- annotations_rv()
    if (nrow(df) == 0) return(NULL)
    df <- df[order(df$start, df$finish), ]
    df[, c("term_id", "type", "segment", "start", "finish")]
  })
  
  output$annotated_text <- renderUI({
    text <- input$input_text
    df <- annotations_rv()
    if (!nzchar(text) || nrow(df) == 0) return(HTML(htmlEscape(text)))
    
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
        cols <- vapply(df$color[active], hex_to_rgba, character(1), alpha = 0.35)
        style <- bg_style(cols)
        out <- c(out, sprintf("<span class='term-span' style='%s'>%s</span>", style, chunk_esc))
      }
    }
    
    HTML(paste(out, collapse = ""))
  })
}

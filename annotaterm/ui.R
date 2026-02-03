ui <- fluidPage(
  titlePanel("annotaterm demo"),
  
  tags$head(
    tags$style(HTML("
      #text_display {
        border: 1px solid #ddd;
        padding: 10px;
        min-height: 140px;
        white-space: pre-wrap;
      }
      .term-span {
        padding: 0 2px;
        border-radius: 3px;
        font-weight: 600;
      }
      .btn-row .btn { margin-right: 6px; }
    "))
  ),
  
  tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      var ta = document.getElementById('input_text');
      if (!ta) return;

      function sendSelection() {
        var start = ta.selectionStart;
        var end = ta.selectionEnd;
        if (start === end) return;
        var txt = ta.value.substring(start, end);

        Shiny.setInputValue('current_selection', {
          text: txt,
          start: start + 1,
          end: end
        }, {priority: 'event'});
      }

      ta.addEventListener('select', sendSelection);
      ta.addEventListener('mouseup', sendSelection);
      ta.addEventListener('keyup', function(e) {
        if (e.shiftKey &&
            (e.key === 'ArrowLeft' || e.key === 'ArrowRight' ||
             e.key === 'ArrowUp'   || e.key === 'ArrowDown')) {
          sendSelection();
        }
      });
    });
  ")),
  
  sidebarLayout(
    sidebarPanel(
      h4("Current selection"),
      verbatimTextOutput("selection_text"),
      
      hr(),
      h4("Simple terms"),
      actionButton("add_simple", "Add simple term (from selection)"),
      
      hr(),
      h4("Composite terms"),
      div(
        class = "btn-row",
        actionButton("reset_composite", "reset"),
        actionButton("add_segment", "add segment"),
        actionButton("save_composite", "save composite")
      ),
      br(), br(),
      strong("Current segments:"),
      verbatimTextOutput("composite_segments_text"),
      
      hr(),
      h4("Annotations"),
      tableOutput("annotations_table")
    ),
    
    mainPanel(
      h4("Text input (select here)"),
      textAreaInput(
        inputId = "input_text",
        label = NULL,
        value = "Waste management is important. Waste disposal is also important. Waste in general is a problem.\nWaste management and disposal.",
        width = "100%",
        rows = 8
      ),
      hr(),
      h4("Annotated preview"),
      div(id = "text_display", htmlOutput("annotated_text", inline = TRUE))
    )
  )
)

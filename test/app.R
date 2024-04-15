
jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"

library(shiny)
library(shinyjs)
runApp(list(
  ui = fluidPage(
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("disableTab", "enableTab")),
    inlineCSS(css),
    tabsetPanel(
      id = "navbar",
      tabPanel(title = "tab1", id = "tab1",
               br(),
               actionButton("btn", label = "View tab2 panel")),
      tabPanel(title = "tab2", id = "tab2")
    )
  ),
  server = function(input, output, session) {

    # disable tab2 on page load
    js$disableTab("tab2")

    observeEvent(input$btn, {
      # enable tab2 when clicking the button
      js$enableTab("tab2")
      # switch to tab2
      updateTabsetPanel(session, "navbar", "tab2")
    })
  }
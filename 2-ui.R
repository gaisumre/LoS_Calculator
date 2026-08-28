source("2.1-individual_tab.R")
source("2.2-facility_tab.R")


ui <- fluidPage(
  
  tags$head(
    tags$script(HTML("
    (function registerReadyHandler() {
      if (
        !window.Shiny ||
        typeof Shiny.addCustomMessageHandler !== 'function' ||
        typeof Shiny.setInputValue !== 'function'
      ) {
        window.setTimeout(registerReadyHandler, 25);
        return;
      }

      Shiny.addCustomMessageHandler('calculator-ready', function (message) {
        requestAnimationFrame(function () {
          requestAnimationFrame(function () {
            window.parent.postMessage(
              { type: 'calculator-ready' },
              window.location.origin
            );
          });
        });
      });

      Shiny.setInputValue(
        'calculator_ready_listener',
        Date.now(),
        { priority: 'event' }
      );
    })();
  "))
  ),
  
  uiOutput("page_ui")
)
urlHandler <-  function(input, output, session) {
    observeEvent(
      input$tabs, {
        # Drop the leading '#' symbol
        hash <- substring(getUrlHash(), 2)

        # This is so we don't 'update' to the tab we're already in (since clicking on
        # the sidebar already switches tabs.)
        if (hash != input$tabs) {
          # The 'push' argument is necessary so that the hash change event occurs and
          # so that the other observer is triggered.
          updateQueryString(paste0("#", input$tabs), mode = "push")
        }
      }
    )

    observeEvent(getUrlHash(), {
      hash <- substring(getUrlHash(), 2)
      updateTabItems(session, "tabs", hash)
    })
}

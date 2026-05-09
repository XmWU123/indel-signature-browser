# ==============================================================================
# Search Handlers
# This module handles search functionality
# ==============================================================================

# Search function definition
search_signature <- function(input, session, current_group, current_id83) {
  search_term <- trimws(input$search_input)

  if (search_term == "") {
    showModal(modalDialog(
      title = "Search Error",
      "Please enter a signature name to search.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    return()
  }

  matched89 <- grep(
    search_term,
    names(signature_groups),
    ignore.case = TRUE,
    value = TRUE
  )

  matched83_found <- character(0)
  matched83_names <- character(0)

  for (g_name in names(id83_groups)) {
    info <- id83_groups[[g_name]]
    if (any(grepl(search_term, info$members, ignore.case = TRUE))) {
      matched83_found <- c(matched83_found, g_name)
    }
    aliases <- c(
      g_name,
      paste0("ID", g_name),
      paste0("ID_", g_name),
      paste0("C_ID", g_name),
      paste0("C_ID_", g_name)
    )
    if (any(grepl(search_term, aliases, ignore.case = TRUE))) {
      matched83_names <- c(matched83_names, g_name)
    }
  }
  matched83 <- unique(c(matched83_found, matched83_names))

  # Log search results
  log_search(search_term, length(matched89), length(matched83))

  if (length(matched89) == 0 && length(matched83) == 0) {
    showModal(modalDialog(
      title = "Signature Not Found",
      paste0("'", search_term, "' not found in ID89 or ID83."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    return()
  }

  if (
    (length(matched89) > 0 && length(matched83) > 0) ||
      length(matched89) > 1 ||
      length(matched83) > 1
  ) {
    choices <- c()
    if (length(matched89) > 0) {
      choices <- c(choices, paste0(matched89, " ", CONFIG$search$type_labels$koh89))
    }
    if (length(matched83) > 0) {
      choices <- c(choices, paste0(matched83, " ", CONFIG$search$type_labels$cosmic83))
    }

    showModal(modalDialog(
      title = "Select Signature",
      tags$p(paste0("Multiple matches found for '", search_term, "':")),
      radioButtons("select_search", label = NULL, choices = choices),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_search", "Go", class = "btn-primary")
      )
    ))
    return()
  }

  if (length(matched89) == 1) {
    updateNavbarPage(session, "navbar", selected = CONFIG$tabs$KOH89)
    current_group(matched89[1])
    updateTextInput(session, "search_input", value = "")
    return()
  }
  if (length(matched83) == 1) {
    updateNavbarPage(session, "navbar", selected = CONFIG$tabs$COSMIC83)
    current_id83(matched83[1])
    updateTextInput(session, "search_input", value = "")
    return()
  }
}

# Initialize search handlers
init_search_handlers <- function(input, output, session, current_group, current_id83) {

  observeEvent(input$confirm_search, {
    choice <- input$select_search
    if (is.null(choice)) {
      return()
    }

    koh89_label <- CONFIG$search$type_labels$koh89
    cosmic83_label <- CONFIG$search$type_labels$cosmic83

    if (grepl(paste0("\\", koh89_label, "$"), choice)) {
      sig89 <- sub(paste0(" \\", koh89_label, "$"), "", choice)
      log_user_action("search_select", paste("89-type:", sig89))
      updateNavbarPage(session, "navbar", selected = CONFIG$tabs$KOH89)
      current_group(sig89)
    }

    if (grepl(paste0("\\", cosmic83_label, "$"), choice)) {
      sig83 <- sub(paste0(" \\", cosmic83_label, "$"), "", choice)
      log_user_action("search_select", paste("83-type:", sig83))
      updateNavbarPage(session, "navbar", selected = CONFIG$tabs$COSMIC83)
      current_id83(sig83)
    }
    updateTextInput(session, "search_input", value = "")
    removeModal()
  })

  observeEvent(input$search_btn, {
    log_user_action("search", input$search_input)
    search_signature(input, session, current_group, current_id83)
  })
}

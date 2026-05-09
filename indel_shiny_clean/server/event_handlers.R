# ==============================================================================
# Event Handlers
# This module handles navigation and click events
# ==============================================================================

# Initialize all event handlers
init_event_handlers <- function(input, output, session, current_group, current_id83) {

  # Home page navigation links
  observeEvent(input$home_goto_koh89, {
    log_user_action("home_goto_koh89")
    updateNavbarPage(session, "navbar", selected = CONFIG$tabs$KOH89)
  })

  observeEvent(input$home_goto_cosmic83, {
    log_user_action("home_goto_cosmic83")
    updateNavbarPage(session, "navbar", selected = CONFIG$tabs$COSMIC83)
  })

  # Signature group click handlers
  lapply(names(signature_groups), function(group_name) {
    observeEvent(input[[paste0("show_", group_name)]], {
      log_user_action("select_signature", group_name)
      current_group(group_name)
    })
  })

  observeEvent(input$back_to_list, {
    log_user_action("back_to_list")
    current_group(NULL)
  })

  # ID83 group click handlers
  lapply(names(id83_groups), function(id83_name) {
    observeEvent(input[[paste0("show_id83_", id83_name)]], {
      log_user_action("select_id83", id83_name)
      current_id83(id83_name)
    })
  })

  observeEvent(input$back_to_id83_list, {
    log_user_action("back_to_id83_list")
    current_id83(NULL)
  })

  # Image click modal handlers
  observe({
    all_imgs <- character()
    if (!is.null(current_group())) {
      sig <- signature_groups[[current_group()]]
      all_imgs <- unique(c(sig$imgs, sig$id83, sig$id476))
    }
    if (!is.null(current_id83())) {
      id83_info <- id83_groups[[current_id83()]]
      all_imgs <- c(all_imgs, id83_info$id83_all)
      for (member in id83_info$members) {
        sig <- signature_groups[[member]]
        if (!is.null(sig)) {
          all_imgs <- c(all_imgs, sig$imgs, sig$id83, sig$id476)
        }
      }
    }

    # Remove empty values
    all_imgs <- unique(all_imgs[!is.na(all_imgs) & nzchar(all_imgs)])

    # Bind click events for each image
    lapply(all_imgs, function(img) {
      observeEvent(input[[paste0("img_", img)]], ignoreInit = TRUE, {
        # Use helper function to get display title
        display_title <- get_image_display_title(img)

        log_user_action("view_image", img)

        showModal(modalDialog(
          title = display_title,
          easyClose = TRUE,
          size = "l",
          footer = NULL,
          tags$img(
            src = img,
            style = "width:100%; height:auto; border-radius:8px;box-shadow: 0 4px 12px rgba(0,0,0,0.15);"
          )
        ))
      })
    })
  })
}

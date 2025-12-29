# ==============================================================================
# Signature Display (89-Type Classification)
# This module handles the 89-type signature display rendering
# ==============================================================================

# Render the 89-type signature display
render_signature_display <- function(input, output, current_group) {
  output$signature_display <- renderUI({
    if (is.null(current_group())) {
      # Grid view - show all signatures
      fluidRow(
        lapply(names(signature_groups), function(group_name) {
          if (is.null(signature_groups[[group_name]])) {
            return(NULL)
          }
          sig <- signature_groups[[group_name]]
          render_thumbnail_card(
            id = group_name,
            input_id = paste0("show_", group_name),
            thumbnail_path = sig$thumbnail,
            title_color = CONFIG$colors$primary
          )
        })
      )
    } else {
      # Detail view - show selected signature
      sig <- signature_groups[[current_group()]]
      show_types <- input$show_types %||% c("ID89", "ID83", "ID476")
      current_selection <- if (is.null(input$show_types)) {
        c("ID89", "ID83", "ID476")
      } else {
        input$show_types
      }

      id89_imgs <- if ("ID89" %in% show_types) sig$imgs else character(0)
      id83_imgs <- if ("ID83" %in% show_types) sig$id83 else character(0)
      id476_imgs <- if ("ID476" %in% show_types) sig$id476 else character(0)

      id89_imgs <- id89_imgs[!is.na(id89_imgs) & nzchar(id89_imgs)]
      id83_imgs <- id83_imgs[!is.na(id83_imgs) & nzchar(id83_imgs)]
      id476_imgs <- id476_imgs[!is.na(id476_imgs) & nzchar(id476_imgs)]

      tagList(
        render_back_button("back_to_list", "Back to List"),
        h2(
          current_group(),
          style = sprintf(
            "color:%s; font-weight:600; margin-bottom:25px;",
            CONFIG$colors$primary
          )
        ),

        div(
          style = sprintf(
            "margin-bottom: 20px; padding: 15px; background: %s; border-radius:5px; border: 1px solid %s;",
            CONFIG$colors$light_bg,
            CONFIG$colors$border
          ),
          checkboxGroupInput(
            "show_types",
            "Select signature types to display:",
            choices = c(
              "89-Type" = "ID89",
              "83-Type" = "ID83",
              "476-Type" = "ID476"
            ),
            selected = current_selection,
            inline = TRUE
          )
        ),

        render_aetiology(sig$aetiology),

        # 89-Type Signature
        if (length(id89_imgs) >= 1 && image_exists(id89_imgs[1])) {
          div(
            class = "img-container",
            render_section_title(paste0(
              CONFIG$labels$TYPE_89,
              " ",
              CONFIG$labels$SIGNATURE
            )),
            # render_label(CONFIG$labels$SIGNATURE),
            render_image(id89_imgs[1], max_width = CONFIG$ui$image_max_width)
          )
        },

        # Sample Spectrums
        if (length(id89_imgs) > 1) {
          div(
            class = "img-container",
            render_section_title(paste(
              "Example tumor spectrum with signature",
              current_group()
            )),
            p(
              class = "text-muted",
              style = sprintf(
                "margin-top: -8px; margin-bottom: 12px; color:%s; font-size: 12px;",
                CONFIG$colors$muted
              ),
              "Sample A is the example tumor spectrum; Sample B is the partial spectrum contributed by all other signatures; Sample A-B is the difference."
            ),
            fluidRow(
              lapply(seq_along(id89_imgs[-1]), function(i) {
                nm <- c("A", "B", "A-B")[i]
                imgnm <- id89_imgs[-1][i]
                column(
                  4,
                  render_label(paste("Sample", nm)),
                  render_image(imgnm, max_width = "100%")
                )
              })
            )
          )
        },

        # 83-Type
        if (length(id83_imgs) > 0) {
          div(
            class = "img-container",
            render_section_title(paste0(
              CONFIG$labels$TYPE_83,
              " ",
              CONFIG$labels$SIGNATURE,
              " corresponding to ",
              current_group()
            )),
            if (length(id83_imgs) >= 1 && image_exists(id83_imgs[1])) {
              tagList(
                # render_label("Signature Spectrum"),
                div(
                  style = "margin-bottom:25px;",
                  render_image(
                    id83_imgs[1],
                    max_width = CONFIG$ui$image_max_width
                  )
                )
              )
            },
            if (length(id83_imgs) >= 2 && image_exists(id83_imgs[2])) {
              tagList(
                render_label("Sample A in 83-Type representation"),
                render_image(
                  id83_imgs[2],
                  max_width = CONFIG$ui$image_max_width
                )
              )
            }
          )
        },

        # 476-type
        if (length(id476_imgs) >= 1 && image_exists(id476_imgs[1])) {
          div(
            class = "img-container",
            render_section_title(paste0(
              CONFIG$labels$TYPE_476,
              " ",
              CONFIG$labels$SIGNATURE
            )),
            # render_label("Extended Signature Set"),
            p(
              style = sprintf(
                "font-size: 13px; color: %s; margin-top: -5px; margin-bottom: 10px;",
                CONFIG$colors$muted
              )
            ),
            render_image(id476_imgs[1], max_width = "100%")
          )
        },

        if (!is.null(sig$desc)) {
          div(
            style = sprintf(
              "background:%s; border-left:4px solid %s; padding:15px; border-radius:8px; margin-top:20px;",
              "#fff3cd",
              CONFIG$colors$warning
            ),
            icon("info-circle"),
            " ",
            sig$desc
          )
        }
      )
    }
  })
}

# ==============================================================================
# ID83 Display (83-Type Classification)
# This module handles the 83-type signature display rendering
# ==============================================================================

# Render the 83-type signature display
render_id83_display <- function(input, output, current_id83) {

  output$id83_display <- renderUI({
    if (is.null(current_id83())) {
      # Grid view - show all ID83 groups
      all_names <- names(id83_groups)
      if (length(all_names) == 0) {
        return(NULL)
      }

      # Group into rows of 4
      chunk_size <- CONFIG$ui$grid_columns
      id_chunks <- split(all_names, ceiling(seq_along(all_names) / chunk_size))

      tagList(
        lapply(id_chunks, function(chunk_names) {
          fluidRow(
            style = "margin-bottom: 20px;",
            lapply(chunk_names, function(id83_name) {
              id83_info <- id83_groups[[id83_name]]
              members_text <- paste(id83_info$members, collapse = ", ")
              thumbnail_path <- id83_info$thumbnail
              has_thumbnail <- image_exists(thumbnail_path)

              # Build extra content showing member signatures
              members_content <- div(
                style = sprintf(
                  "background:%s; padding:8px; border-radius:4px; text-align:left;",
                  CONFIG$colors$light_bg
                ),
                div(
                  style = sprintf(
                    "font-size:11px; color:%s; margin-bottom:5px; font-weight:bold;",
                    CONFIG$colors$muted
                  ),
                  "Corresponds to:"
                ),
                div(
                  style = sprintf(
                    "font-size:12px; color:%s; line-height:1.4;",
                    CONFIG$colors$primary_light
                  ),
                  members_text
                )
              )

              column(
                3,
                div(
                  class = "thumbnail-card",
                  style = sprintf(
                    "background: #fff; border: 1px solid #ddd; border-radius: 8px; padding: 15px; height: %s; overflow-y: auto; box-shadow: 0 2px 5px rgba(0,0,0,0.05); transition: box-shadow 0.3s;",
                    CONFIG$ui$card_height
                  ),
                  onmouseover = "this.style.boxShadow='0 5px 15px rgba(0,0,0,0.2)'",
                  onmouseout = "this.style.boxShadow='0 2px 5px rgba(0,0,0,0.05)'",

                  actionLink(
                    inputId = paste0("show_id83_", id83_name),
                    label = tagList(
                      h4(
                        id83_name,
                        style = sprintf(
                          "color:%s; margin-top:0; font-weight:700; text-align: center;",
                          CONFIG$colors$success
                        )
                      ),
                      div(
                        style = "height: 150px; display: flex; align-items: center; justify-content: center; margin-bottom: 10px; background: #f9f9f9; border-radius: 4px;",
                        if (has_thumbnail) {
                          tags$img(
                            src = thumbnail_path,
                            style = "max-height: 100%; max-width: 100%; border-radius: 4px;"
                          )
                        } else {
                          div(
                            style = "color:#ccc; text-align: center;",
                            icon("image", style = "font-size:32px; display: block;"),
                            tags$small("No Image")
                          )
                        }
                      ),
                      members_content
                    ),
                    style = "text-decoration: none; color: inherit; display: block;"
                  )
                )
              )
            })
          )
        })
      )
    } else {
      # Detail view - show selected ID83 group
      id83_info <- id83_groups[[current_id83()]]
      members <- id83_info$members
      id83_all_img <- id83_info$id83_all

      tagList(
        render_back_button("back_to_id83_list", "Back to 83-Type List"),
        h2(
          paste("83-Type:", current_id83()),
          style = sprintf(
            "color:%s; font-weight:600; margin-bottom:25px;",
            CONFIG$colors$success
          )
        ),

        # ID83 Signature image
        div(
          class = "id83-section",
          div(class = "id83-label", icon("layer-group"), " ", CONFIG$labels$SIGNATURE),
          render_image_or_placeholder(
            id83_all_img,
            max_width = CONFIG$ui$image_max_width
          )
        ),

        # Member details
        div(
          class = "id83-section",
          div(
            class = "id83-label",
            icon("dna"),
            " Corresponding 89-Type Signatures"
          ),
          lapply(members, function(member_name) {
            sig <- signature_groups[[member_name]]
            if (is.null(sig)) {
              return(NULL)
            }

            type89_spectrum <- if (length(sig$imgs) >= 1) sig$imgs[1] else NULL
            type89_sampleA <- if (length(sig$imgs) >= 2) sig$imgs[2] else NULL
            type83_filtered <- if (length(sig$id83) >= 2) sig$id83[2] else NULL

            div(
              class = "member-section",
              div(
                class = "member-name",
                icon("chevron-right"),
                " ",
                member_name
              ),

              # Aetiology (compact version)
              render_aetiology(sig$aetiology, compact = TRUE),

              # Member images
              fluidRow(
                if (image_exists(type89_spectrum)) {
                  column(
                    4,
                    render_label(paste0(CONFIG$labels$TYPE_89, " ", CONFIG$labels$SIGNATURE)),
                    render_image(type89_spectrum, max_width = "100%")
                  )
                },
                if (image_exists(type89_sampleA)) {
                  column(
                    4,
                    render_label("89-Type Sample A"),
                    render_image(type89_sampleA, max_width = "100%")
                  )
                },
                if (image_exists(type83_filtered)) {
                  column(
                    4,
                    render_label("Sample A (83-Type)"),
                    render_image(type83_filtered, max_width = "100%")
                  )
                }
              )
            )
          })
        )
      )
    }
  })
}

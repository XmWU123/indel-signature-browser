# ==============================================================================
# Helper Functions
# Reusable UI components and utility functions
# ==============================================================================

#' Render a signature image with optional click handler
#'
#' @param img Character. Path to image file relative to www/
#' @param max_width Character. CSS max-width value (default: "700px")
#' @param full_width Logical. Whether to use 100% width (default: TRUE)
#' @param clickable Logical. Whether to add click handler for modal (default: TRUE)
#' @param class Character. Additional CSS class (default: "signature-img")
#' @return Shiny img tag or NULL if image doesn't exist
render_image <- function(img,
                         max_width = "700px",
                         full_width = TRUE,
                         clickable = TRUE,
                         class = "signature-img") {

  if (is.null(img) || is.na(img) || !nzchar(img)) {
    return(NULL)
  }

  if (!file.exists(file.path("www", img))) {
    return(NULL)
  }

  width_style <- if (full_width) "width:100%;" else ""
  style <- sprintf("max-width:%s; %s", max_width, width_style)

  onclick <- if (clickable) {
    sprintf("Shiny.setInputValue('img_%s', Date.now());", img)
  } else {
    NULL
  }

  tags$img(
    src = img,
    class = class,
    style = style,
    onclick = onclick
  )
}

#' Render a placeholder for missing images
#'
#' @param message Character. Message to display (default: "No image available")
#' @param icon_name Character. FontAwesome icon name (default: "image")
#' @return Shiny div element
render_placeholder <- function(message = "No image available", icon_name = "image") {
  div(
    style = "color:#95a5a6; text-align:center; padding:40px;",
    icon(icon_name, style = "font-size:48px;"),
    br(),
    message
  )
}

#' Render an image or placeholder if image doesn't exist
#'
#' @param img Character. Path to image file relative to www/
#' @param max_width Character. CSS max-width value
#' @param placeholder_msg Character. Message for placeholder
#' @return Shiny img tag or placeholder div
render_image_or_placeholder <- function(img,
                                         max_width = "700px",
                                         placeholder_msg = "No image available") {
  result <- render_image(img, max_width = max_width)
  if (is.null(result)) {
    render_placeholder(placeholder_msg)
  } else {
    result
  }
}

#' Render an aetiology information box
#'
#' @param text Character. Aetiology text
#' @param compact Logical. Use compact styling (default: FALSE)
#' @return Shiny div element or NULL if text is empty
render_aetiology <- function(text, compact = FALSE) {
  if (is.null(text) || is.na(text) || !nzchar(trimws(text))) {
    return(NULL)
  }

  if (compact) {
    # Compact version for member listings
    div(
      style = "background:#fff3cd; padding:12px; border-radius:6px; margin-bottom:15px; border-left:3px solid #ffc107;",
      tags$strong(
        style = "color:#856404; font-size:12px;",
        icon("lightbulb"),
        " Proposed Aetiology: "
      ),
      tags$span(
        style = "color:#856404; font-size:13px;",
        text
      )
    )
  } else {
    # Full version for detail pages
    div(
      style = "background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%); padding:20px; border-radius:12px; margin-bottom:25px; border-left:5px solid #2ecc71; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
      div(
        style = "display:flex; align-items:center; margin-bottom:10px;",
        icon("lightbulb", style = "font-size:24px; color:#27ae60; margin-right:12px;"),
        tags$span(
          style = "font-size:16px; color:#27ae60; font-weight:700; text-transform:uppercase; letter-spacing:1px;",
          "Proposed Aetiology"
        )
      ),
      tags$p(
        style = "font-size:16px; color:#2c3e50; line-height:1.8; margin:0;",
        text
      )
    )
  }
}

#' Render a section title
#'
#' @param title Character. Section title text
#' @param class Character. CSS class (default: "img-section-title")
#' @return Shiny div element
render_section_title <- function(title, class = "img-section-title") {
  div(class = class, title)
}

#' Render an image label
#'
#' @param label Character. Label text
#' @param class Character. CSS class (default: "img-label")
#' @return Shiny div element
render_label <- function(label, class = "img-label") {
  div(class = class, label)
}

#' Render a back button
#'
#' @param id Character. Button input ID
#' @param label Character. Button label (default: "Back to List")
#' @return Shiny actionButton
render_back_button <- function(id, label = "Back to List") {
  actionButton(
    id,
    paste("\u2190", label),
    class = "btn-back",
    style = "margin-bottom:20px;"
  )
}

#' Render a thumbnail card for grid view
#'
#' @param id Character. Signature/group ID
#' @param input_id Character. ActionLink input ID
#' @param thumbnail_path Character. Path to thumbnail image
#' @param title_color Character. CSS color for title
#' @param extra_content Shiny tag. Additional content below image
#' @return Shiny column element
render_thumbnail_card <- function(id,
                                   input_id,
                                   thumbnail_path = NULL,
                                   title_color = "#2c3e50",
                                   extra_content = NULL) {

  has_thumbnail <- !is.null(thumbnail_path) &&
                   length(thumbnail_path) > 0 &&
                   nzchar(thumbnail_path) &&
                   file.exists(file.path("www", thumbnail_path))

  column(
    3,
    div(
      class = "thumbnail-card",
      actionLink(
        inputId = input_id,
        label = tagList(
          h4(id, style = sprintf("color:%s;font-weight:bold;margin-top:0;", title_color)),
          if (has_thumbnail) {
            tags$img(
              src = thumbnail_path,
              style = "width:100%; max-width:200px; height:auto;border-radius:5px; transition: transform 0.2s;"
            )
          } else {
            div(
              style = "height:120px; line-height:120px; color:#95a5a6;",
              icon("image", style = "font-size:48px;")
            )
          },
          extra_content
        ),
        style = "text-decoration: none;color: inherit; display: block; cursor:pointer;"
      )
    )
  )
}

#' Check if an image file exists
#'
#' @param img Character. Image filename
#' @param dir Character. Directory (default: "www")
#' @return Logical
image_exists <- function(img, dir = "www") {
  if (is.null(img) || is.na(img) || !nzchar(img)) {
    return(FALSE)
  }
  file.exists(file.path(dir, img))
}

#' Filter images to only existing ones
#'
#' @param imgs Character vector. Image filenames
#' @return Character vector of existing images
filter_existing_images <- function(imgs) {
  imgs <- imgs[!is.na(imgs) & nzchar(imgs)]
  imgs[sapply(imgs, image_exists)]
}

#' Get display title for image modal based on filename pattern
#'
#' @param img Character. Image filename
#' @return Character. Display title
get_image_display_title <- function(img) {
  if (grepl("signature\\.89spectrum", img)) {
    "89-Type Signature"
  } else if (grepl("_89spectrumA", img)) {
    "Sample A (Original Spectrum)"
  } else if (grepl("_89spectrumB", img)) {
    "Sample B (Reconstructed)"
  } else if (grepl("_89spectrumC", img)) {
    "Sample A-B (Residual)"
  } else if (grepl("_83all", img)) {
    "83-Type Signature"
  } else if (grepl("_83filtered", img)) {
    "Sample A in 83-Type Representation"
  } else if (grepl("_476all", img)) {
    "476-Type Signature"
  } else if (grepl("Thumbnail", img)) {
    "Group Thumbnail"
  } else {
    "Signature View"
  }
}

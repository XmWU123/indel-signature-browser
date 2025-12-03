# About Tab Component
create_about_tab <- function() {
  tabPanel(
    "About",
    icon = icon("info-circle"),
    fluidRow(
      column(10, offset = 1,
             div(class = "img-container",
                 h2("About",
                    style = "color:#2c3e50; font-weight:700; margin-bottom:35px; text-align:center; font-size:32px;"),

                 h3("Contact Us",
                    style = "color:#3498db; font-weight:700; margin-top:25px; margin-bottom:15px; font-size:24px;"),
                 tags$p(style = "font-size:17px; line-height:1.9; color:#555;",
                        "If you experience any issues or have suggestions while visiting this website, don't hesitate to reach out to us."
                 ),

                 hr(style = "margin: 35px 0; border-top: 2px solid #3498db; opacity: 0.3;"),

                 h3("Main Contributors",
                    style = "color:#2ecc71; font-weight:700; margin-top:25px; margin-bottom:15px; font-size:24px;"),
                 tags$p(style = "font-size:17px; line-height:2.2; color:#555;",
                        "• Xueming Wu", tags$br(),
                        "• Mo Liu", tags$br(),
                        "• Steverozen"
                 ),

                 hr(style = "margin: 35px 0; border-top: 2px solid #2ecc71; opacity: 0.3;"),

                 h3("Email",
                    style = "color:#e74c3c; font-weight:700; margin-top:25px; margin-bottom:15px; font-size:24px;"),
                 tags$p(style = "font-size:17px; line-height:1.9; color:#555;",
                        icon("envelope", style = "color:#e74c3c; margin-right:10px; font-size:20px;"),
                        tags$a(href = "mailto:wuxm8523@gmail.com",
                               "wuxm8523@gmail.com",
                               style = "color:#3498db; text-decoration:none; font-weight:600;")
                 ),

                 hr(style = "margin: 45px 0; border-top: 1px solid #bdc3c7;"),

                 tags$p(style = "text-align:center; color:#7f8c8d; margin-top:40px; font-size:15px; font-weight:500;",
                        "© 2025 Indel Signature Browser. All rights reserved."
                 )
             )
      )
    )
  )
}

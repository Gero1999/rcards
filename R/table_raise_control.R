table_raise_control <- function(legal, version, language) {
  details <- table_action_details("holdem", "raise")
  label <- action_label("raise", language)
  label <- paste0(toupper(substr(label, 1L, 1L)), substring(label, 2L))
  description <- ui(details[[2L]], language)
  minimum <- format(legal$min_raise_to, scientific = FALSE, trim = TRUE)
  maximum <- format(legal$max_raise_to, scientific = FALSE, trim = TRUE)

  shiny::tags$div(class = "rc-raise-control",
    shiny::tags$button(type = "button", class = "rc-action rc-raise-toggle",
      `data-rc-raise-toggle` = "", `aria-controls` = "rc_raise_editor", `aria-expanded` = "false",
      title = description, table_button_content(label, details[[1L]])),
    shiny::tags$div(id = "rc_raise_editor", class = "rc-raise-editor", `data-rc-raise-editor` = "",
      hidden = NA,
      shiny::tags$label(`for` = "amount", class = "rc-raise-range",
        title = ui("Raise TO total this betting round", language),
        ui("Raise total (%s to %s)", language, minimum, maximum)),
      shiny::tags$input(id = "amount", class = "form-control rc-raise-amount", type = "number",
        inputmode = "numeric", min = minimum, max = maximum, step = 1, value = minimum, required = NA),
      shiny::tags$button(type = "button", class = "rc-action rc-raise-confirm",
        `data-rc-action` = "raise", `data-rc-version` = version,
        `data-rc-raise-label` = ui("Raise to %s", language), title = description,
        table_button_content(ui("Raise to %s", language, minimum), details[[1L]])),
      shiny::tags$button(type = "button", class = "rc-action rc-secondary rc-raise-cancel",
        `data-rc-raise-cancel` = "", ui("Cancel", language))))
}

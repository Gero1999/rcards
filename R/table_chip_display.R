table_pot_tag <- function(amount, language) {
  amount <- format(amount, scientific = FALSE, trim = TRUE)
  shiny::tags$div(class = "rc-pot-display", `data-rc-pot` = amount,
    shiny::tags$span(class = "rc-chip-stack", `aria-hidden` = "true"),
    shiny::tags$span(class = "rc-pot-amount", paste(ui("pot", language), amount)))
}

table_dealer_tag <- function(language) {
  label <- ui("Dealer button", language)
  shiny::tags$span(class = "rc-dealer-marker", role = "img",
    `aria-label` = label, title = label, "D")
}

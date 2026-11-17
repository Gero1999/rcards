test_that("help selects the current game and every supported language", {
  skip_if_not_installed("shiny")
  labels <- vapply(table_help_catalog, function(text) text$help, "")
  games <- c("holdem","mus","blackjack","scopa","liar")
  for(language in supported_languages()) {
    text <- table_help_text(language)
    expect_named(text,names(table_help_text("en")))
    expect_equal(text$help,unname(labels[language]))
    expect_setequal(names(text$games),games)
    for(type in games) {
      html <- as.character(table_help_dialog(list(game_type=type),language))
      expect_match(html,text$help,fixed=TRUE)
      expect_match(html,paste0('data-rc-help-game="',type,'"'),fixed=TRUE)
      expect_match(html,'aria-labelledby="rcards-help-title"',fixed=TRUE)
      expect_match(html,'role="dialog"',fixed=TRUE)
      expect_match(html,'rel="noopener noreferrer"',fixed=TRUE)
    }
  }
})

test_that("help describes important rule and continuation distinctions", {
  skip_if_not_installed("shiny")
  render <- function(type) as.character(table_help_dialog(list(game_type=type)))
  expect_match(render("holdem"),"TOTAL",fixed=TRUE)
  expect_match(render("holdem"),"retains stacks",fixed=TRUE)
  expect_match(render("blackjack"),"fresh complete shoe",fixed=TRUE)
  expect_match(render("blackjack"),"Next round",fixed=TRUE)
  expect_match(render("mus"),"30 or 40",fixed=TRUE)
  expect_match(render("mus"),"New match",fixed=TRUE)
  expect_match(render("scopa"),"numbered capture",fixed=TRUE)
  expect_match(render("liar"),"Passing here does not skip",fixed=TRUE)
  expect_match(render("liar"),"Next deal",fixed=TRUE)
})

test_that("help ignores private state and uses only fixed repository links", {
  skip_if_not_installed("shiny")
  view <- list(game_type="holdem",secret="<script>rcards-private-sentinel</script>",
    players=list(list(name="<img src=x onerror=alert(1)>")))
  html <- as.character(table_help_dialog(view))
  expect_false(grepl("rcards-private-sentinel|onerror|alert\\(1\\)|<img",html))
  expect_match(html,"https://github.com/Gero1999/rcards/issues/new?title=Bug%3A%20",fixed=TRUE)
  expect_match(html,"https://github.com/Gero1999/rcards/issues/new?title=Game%20suggestion%3A%20",fixed=TRUE)
  expect_match(html,"/blob/main/docs/rules/holdem.md",fixed=TRUE)
  expect_error(table_help_dialog(list(game_type="<script>")),"supported game")
  expect_error(table_help_text("xx"),"Supported languages")
})

test_that("the app brand embeds the original logo without network or font dependencies", {
  skip_if_not_installed("shiny")
  brand <- table_brand_tag()
  image <- brand$children[[1]]
  expect_equal(image$attribs$alt,"rcards")
  expect_match(image$attribs$src,"^data:image/svg\\+xml;charset=UTF-8,")
  svg <- utils::URLdecode(sub("^data:image/svg\\+xml;charset=UTF-8,","",image$attribs$src))
  expect_match(svg,"rcards logo",fixed=TRUE)
  expect_false(grepl("<script|<image|<text|font-family|href=|src=|onload=",svg))
  source <- testthat::test_path("..","..","man","figures","logo.svg")
  if(file.exists(source)) expect_equal(svg,paste(readLines(source),collapse="\n"))
})

test_that("opening help preserves the live game and follows actual table selection", {
  skip_if_not_installed("shiny")
  initial <- table_session(new_holdem(seed=1))
  shiny::testServer(table_app_server(initial,"en"),{
    session$flushReact()
    session$setInputs(show_help=1)
    expect_identical(state(),initial)
    session$setInputs(game_kind="mus",language="it",show_help=2)
    expect_equal(view()$game_type,"holdem")
    expect_equal(view()$language,"it")
    expect_identical(state(),initial)
    expect_match(output$help_control$html,"Aiuto",fixed=TRUE)
  })
})

test_that("Blackjack continuation requires an explicit wager and accepts fractions", {
  skip_if_not_installed("shiny")
  initial <- table_session(blackjack_step(new_bj("Ts 9h 8d 8c"),"stand"))
  shiny::testServer(table_app_server(initial,"en"),{
    session$flushReact()
    expect_match(output$footer$html,'step="any"',fixed=TRUE)
    session$setInputs(table_continue=list(version=0,wager=NULL))
    expect_identical(state()$game,initial$game)
    expect_match(state()$error,"positive wager")
    session$setInputs(table_continue=list(version=0,wager=5.5))
    expect_equal(state()$game$wagers,5.5)
    expect_equal(state()$game$round_number,2)
  })
})

test_that("clean_full_heading_names replaces ... with ' - '", {
  raw <- paste0(
    "Gibt es Kinderbetreuung in Kindertageseinrichtungen für Kinder ",
    "über 3 Jahren in Ihrer Kommune...Kommunale Einrichtung(en)"
  )
  expect_equal(
    clean_full_heading_names(raw),
    paste0(
      "Gibt es Kinderbetreuung in Kindertageseinrichtungen für Kinder ",
      "über 3 Jahren in Ihrer Kommune - Kommunale Einrichtung(en)"
    )
  )
})

test_that("clean_full_heading_names preserves (en) and collapses whitespace", {
  raw <- c(
    "Datum Abgeschickt",
    "Frage...Antwort(en)  ",
    "A....B"
  )
  expect_equal(
    clean_full_heading_names(raw),
    c(
      "Datum Abgeschickt",
      "Frage - Antwort(en)",
      "A - B"
    )
  )
})

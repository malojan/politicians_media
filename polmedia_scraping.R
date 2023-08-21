

# Import packages

needs(tidyverse, rvest)

# Get url for the page of each politician

pages_politicians <-
  read_html("https://www.politiquemedia.com/personnalites.html") |>
  html_nodes(".personnalites-nom > a") |>
  html_attr("href") %>%
  str_c("https://www.politiquemedia.com/", .)

# Get content of each page


month_mapping <- c(
  " janvier " = "/01/",
  " février " = "/02/",
  " mars " = "/03/",
  " avril " = "/04/",
  " mai " = "/05/",
  " juin " = "/06/",
  " juillet " = "/07/",
  " août " = "/08/",
  " septembre " = "/09/",
  " octobre " = "/10/",
  " novembre " = "/11/",
  " décembre " = "/12/"
)

extract_content <- function(x) {

  page <- read_html(x, encoding = "utf-8")

  get_text <- function(x) {html_element(page, x) |> html_text2()}

  media <- tibble(
    name = get_text(".nom-personnalite") |> str_replace("\n", " "),
    birth_date = get_text(".naissance-personnalite") |> str_extract("\\d{2}/\\d{2}/\\d{4}") |> dmy(),
    party = get_text(".parti-personnalite"),
    fonction = get_text(".presentation-personnalite .presentation-personnalite"),
    date_start = get_text(".icon-informations-speciales+ p") |> str_extract("(?<=depuis ).+"),
    history = html_element(page, ".historique-complet") |> html_table()
  ) |>
    unnest(cols = history) |>
    rename(date = X1,
           time = X2,
           channel = X4,
           emission = X5) |>
    select(- c(X3, X6)) |>
    mutate(
      day = str_extract(date, ".+?(?=\\d+)"),
      date_parsed = str_remove(date |> str_squish(), ".+?(?=\\d+)") |> str_replace_all(month_mapping) |> dmy()
    )
  cat(glue::glue("Retrieved {media$name[1]}"), "\n")

  return(media)
}

media_politicians <- map(pages_politicians[1:10], safely(extract_content), .progress = TRUE) |>
  map('result') |>
  compact() |>
  bind_rows()





# Importing the required libraries
# `tidyverse` provides a collection of R packages for data science.
# `rvest` helps in web scraping tasks.

needs(tidyverse, rvest)

# The following code block is aimed at retrieving URLs for individual politician pages
# from the main directory of politicians.

# Starting with the main directory page
pages_politicians <-
  read_html("https://www.politiquemedia.com/personnalites.html") %>%
  # Extracting the anchor links for each politician
  html_nodes(".personnalites-nom > a") %>%
  # Fetching the 'href' attribute which has the URL
  html_attr("href") %>%
  # Prepending the base URL to each extracted link
  str_c("https://www.politiquemedia.com/", .)

# Mapping French month names to numerical month representation for date conversion

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

# Function to extract content from each politician's individual page

extract_content <- function(x) {
  page <- read_html(x, encoding = "utf-8")

  # Function that faciliate the text extraction from a given selector
  get_text <- function(x) {
    html_element(page, x) |> html_text2()
  }

  media <- tibble(
    name = get_text(".nom-personnalite") |> str_replace("\n", " "),
    birth_date = get_text(".naissance-personnalite") |> str_extract("\\d{2}/\\d{2}/\\d{4}") |> dmy(),
    party = get_text(".parti-personnalite"),
    fonction = get_text(".presentation-personnalite .presentation-personnalite"),
    date_start = get_text(".icon-informations-speciales+ p") |> str_extract("(?<=depuis ).+"),
    history = html_element(page, ".historique-complet") |> html_table()
  ) |>
    unnest(cols = history) |>
    rename(
      date = X1,
      time = X2,
      channel = X4,
      emission = X5
    ) |>
    select(-c(X3, X6)) |>
    mutate(
      day = str_extract(date, ".+?(?=\\d+)"),
      date_parsed = str_remove(date |> str_squish(), ".+?(?=\\d+)") |> str_replace_all(month_mapping) |> dmy()
    )
  cat(glue::glue("Retrieved {media$name[1]}"), "\n")

  return(media)
}

# Going through each politician's page to extract their media appearances

media_politicians <-
  map(pages_politicians, safely(extract_content), .progress = TRUE) %>%
  # Mapping results and handling potential errors from `safely`
  map('result') %>%
  # Removing NULL results if any
  compact() %>%
  # Binding all individual results into a single data frame
  bind_rows()

write_csv(media_politicians, "polmedia.csv")


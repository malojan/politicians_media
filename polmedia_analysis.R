
# Import packages

needs(tidyverse)

# Import data

polmedia <- read_csv("polmedia.csv")

#
polmedia |>
  filter(!is.na(date_parsed), channel %in% c("France Inter")) |>
  mutate(month = str_sub(date_parsed, 1, 7) |> ym(),
         rn = if_else(party %in% c("Rassemblement national", "Reconquête"), TRUE, FALSE)) |>
  group_by(channel, month) |>
  count(rn) |>
  mutate(share = n/sum(n)*100) |>
  filter(rn == TRUE) |>
  ggplot(aes(month, share, color = rn)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  facet_wrap(~ channel)

# Share of parties by media

polmedia |>
  filter(!is.na(date_parsed)) |>
  mutate(month = str_sub(date_parsed, 1, 7) |> ym()) |>
  group_by(channel, month) |>
  count(party) |>
  mutate(share = n/sum(n)*100) |>
  filter( party %in% c("PCF")) |>
  ggplot(aes(month, share, color = party)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

party_plot <- polmedia |>
  filter(!is.na(date_parsed)) |>
  mutate(month = str_sub(date_parsed, 1, 7) |> ym()) |>
  group_by(month) |>
  count(party) |>
  mutate(share = n/sum(n)*100) |>
  filter( party %in% c("PCF", "Les Républicains", "Rassemblement national", "Europe écologie - Les Verts", "La France insoumise", "Reconquête", "Parti socialiste")) |>
  ggplot(aes(month, share, color = party)) +
  scale_color_viridis_d(labels = NULL) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  labs(caption = "@malojan") +
  facet_wrap(~ party, scales = "free")

ggsave(party_plot, filename = "figures/party_plot.jpeg")

# wide version

polwide <- polmedia |>
  select(1:5, 8) |>
  group_by(name, party) |>
  count(channel) |>
  pivot_wider(names_from = channel, values_from = n, values_fill = 0) |>
  janitor::clean_names()

umap <- polwide |>
  uwot::umap()

umap |>
  as.matrix() |>
  as_tibble() |>
  bind_cols(polwide) |>
  ggplot(aes(V1, V2)) +
  geom_point()

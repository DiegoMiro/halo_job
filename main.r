library(tidyverse)
library(patchwork)

fun_extract_text <- function(x) {
  x %>%
    rvest::minimal_html() %>%
    rvest::html_text()
}

fun_parse_ul <- function(x) {
  y <- x %>%
    rvest::minimal_html() %>%
    rvest::html_elements(xpath = "//li") %>%
    rvest::html_text()
  
  y <- y %>%
    stringr::str_remove_all("\n")
  
  if (length(y) == 0) y <- NA_character_
  
  return(y)
  
}

fun_starts <- function(
    gt_object, column, max_rating = 5, ..., color = "orange", icon = "star"
) {
  
  stopifnot(
    `Table must be of class 'gt_tbl'` = "gt_tbl" %in% class(gt_object)
  )
  
  gt::text_transform(
    gt_object,
    locations = gt::cells_body(
      columns = {{column}}
    ),
    fn = function(x) {
      num_x <- suppressWarnings(as.numeric(x))
      lapply(
        num_x,
        FUN = function(rating) {
          if (xfun::is_blank(rating) || rating %in% c(NA, "NA", "")) {
            return(gt::html("&nbsp;"))
          }
          rounded_rating <- floor(rating + 0.5)
          stars <- lapply(
            seq_len(max_rating),
            function(i) {
              if (i <= rounded_rating) {
                fontawesome::fa(
                  icon, fill = color, height = "20px", a11y = "sem",
                  prefer_type = "solid"
                )
              }
              else {
                fontawesome::fa(
                  icon, fill = "lightgrey", height = "20px", a11y = "sem"
                )
              }
            }
          )
          label <- sprintf("%s out of %s", rating, max_rating)
          div_out <- htmltools::div(
            title = label, `aria-label` = label, role = "img", stars,
            style = "padding:0px"
          )
          
          div_out %>%
            as.character() %>%
            gt::html()
        })
    }) %>%
    gt::cols_align(
      align = "left",
      columns = {{column}}
    )
}




url_job <- "https://www.linkedin.com/jobs/view/4315834641"
read_url_job <- url_job |>
  xml2::read_html()



class_description <- read_url_job %>%
  rvest::html_nodes("*") %>% 
  rvest::html_attr("class") %>% 
  unique() %>%
  stringr::str_subset("description__text")



description_content <- read_url_job %>%
  rvest::html_nodes("body") %>%
  rvest::html_nodes(
    xpath = glue::glue("//div[@class='{class_description}']")
  )  %>%
  as.character() %>%
  rvest::minimal_html() %>%
  rvest::html_elements(xpath = "//p | //ul") %>%
  as.character() %>%
  stringr::str_remove_all("<p>|</p>") %>%
  tibble::as_tibble() %>%
  dplyr::filter(
    stringr::str_detect(
      purrr::map_chr(value, fun_extract_text),
      "[:alnum:]"
    )
  ) %>%
  dplyr::mutate(
    topic_flg = stringr::str_detect(value, "^<strong>(.*?)</strong>$"),
    topic = dplyr::case_when(
      topic_flg ~ stringr::str_extract(value, "(?<=<strong>).*?(?=</strong>)")
    )
  ) %>%
  tidyr::fill(topic) %>%
  dplyr::filter(!topic_flg) %>%
  dplyr::select(-topic_flg) %>%
  dplyr::mutate(
    ul = dplyr::case_when(
      stringr::str_detect(value, "<ul>") ~ purrr::map(value, fun_parse_ul),
      TRUE ~ list(NA_character_)
    )
  ) %>%
  tidyr::unnest(ul) %>%
  dplyr::mutate(
    subtopic = dplyr::case_when(
      stringr::str_detect(ul, ":") ~ stringr::word(ul, 1, sep = ":"),
      TRUE ~ NA_character_
    ),
    text = dplyr::case_when(
      stringr::str_detect(ul, ":") ~ stringr::word(ul, 2, sep = ":"),
      TRUE ~ ul
    ) %>%
      stringr::str_trim()
  ) %>%
  dplyr::group_by(topic) %>%
  tidyr::fill(subtopic) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    topic = topic %>% stringr::str_remove(":$"),
    text = dplyr::coalesce(text, value)
  ) %>%
  dplyr::filter(text != "") %>%
  dplyr::select(topic, subtopic, text) %>%
  tibble::rowid_to_column()

description_content <- description_content %>%
  dplyr::mutate(
    answer = dplyr::case_when(
      rowid ==  1L ~ "I visited the website and learned more about the company. I saw the meticulous delivery and the high aesthetic quality of the materials, which made me even more attracted to this opportunity. I learned to appreciate design while working at TV Globo (Brazil's largest media company).",
      rowid ==  2L ~ "This is basically what I enjoy doing most professionally.<br>I've been programming in R for 15 years. When I started, there was no RStudio (an IDE and company, currently Posit), Tidyverse, or other features.<br>In addition, I've worked in various industries applying data analysis and data science, and I've led teams for six years.",
      rowid ==  3L ~ "I've been doing this since 2017 and it's very comfortable for me.",
      rowid ==  4L ~ "I believe that the level of complexity/sophistication of the method is a function of the complexity/sophistication of the problem. Too much complexity can generate excessive work and make it difficult to convey information to non-technical people.",
      rowid ==  5L ~ "The same of line 3.",
      rowid ==  6L ~ "The same of line 3.",
      rowid ==  7L ~ "I like to do this and I believe I do it well.",
      rowid ==  8L ~ stringr::str_c("Data scientists and analysts are my friends.", emojifont::emoji("nerd_face")),
      rowid ==  9L ~ "I'm not a software engineer originally, but I've learned a lot, especially within the R ecosystem, about how to balance functionality, usability, maintainability, and performance.",
      rowid == 10L ~ stringr::str_c("I love documentation. And future Diego thanks past Diego for documenting everything.", emojifont::emoji("sweat_smile")),
      rowid == 11L ~ "I like to follow Posit blogs and follow some people who talk about certain trends, like Hadley himself, who is always posting on LinkedIn.",
      rowid == 12L ~ "I have 15 years of professional experience in R development.",
      rowid == 13L ~ "I have a degree in Statistics and a master's degree in Engineering with an emphasis on Machine Learning.",
      rowid == 14L ~ "I feel extremely comfortable with the Tidyverse packages and other commonly used ones, such as:
- **Tables**: gt (static) and reactable (dynamic).
- **Charts**: in addition to ggplot2 (which is usually my first tool for finding the right visualization), I use Highcharter a lot. When Highcharter irritates me for some reason, Apexcharter or eCharts usually solve the problem.
- **Dashboard**: I’ve used bslib and bs4dash for dashboards, but my favorite has long been Flexdashboard for its simplicity and because I really like the one-page concept.",
      rowid == 15L ~ "I have experience with shiny since 2013. I have created dashboards in production, reading and writing data in the database, with complex filters, loading images and videos, highly customizable via CSS, with group access control, among other features.",
      rowid == 16L ~ "I know enough HTML and CSS to customize tables, charts, and dashboards. I also use it to scrape data from web pages.",
      rowid == 17L ~ "This is where I like to be. My skill lies in tapping into data, organizing it, and producing knowledge that helps produce useful insights. And, of course, then measuring the impact of decisions generated from these insights.",
      rowid == 18L ~ "My experience with Git is that of a basic user. I've never needed to do anything other than init, status, add, commit, push, or pull.",
      rowid == 19L ~ "Again, this is the environment that I like and that motivates me.",
      rowid == 20L ~ "No problem here.",
      rowid == 21L ~ "Looks very interesting.",
      rowid == 22L ~ "I have a good ability to work on different fronts at the same time, as long as there is quality time to carry out all tasks as expected.",
      rowid == 23L ~ "I use Python when I need to but I'm not as fast and deep as I am in R. I've never needed to use hadoop and I've used spark within Python with pyspark.",
      rowid == 24L ~ "I believe I have a good understanding of this. But there's always more to learn in Statistical Modeling and Machine Learning.",
      rowid == 25L ~ "I believe I have excellent communication skills, including translating from business to technical and technical to business. However, this ability is less effective when I need to do this in English, as it's not my native language. I recently took an assessment of my English level and was rated at B2. The text in the answer column has been revised several times to ensure accuracy.",
      rowid == 26L ~ "I enjoy interacting with more junior professionals. Everyone is at a different professional stage, and this should be respected and encouraged. I've been fortunate to have great leaders throughout my career, and this has propelled me forward in many ways.",
      rowid == 27L ~ "This is very good.",
      rowid == 28L ~ stringr::str_c("This is really good.", emojifont::emoji("money_mouth_face")),
      rowid == 29L ~ "For me this is the best benefit.",
      TRUE ~ NA_character_
    )
  )

description_content <- description_content %>%
  dplyr::mutate(
    stars = dplyr::case_when(
      rowid ==  3L ~ 5,
      rowid ==  4L ~ 5,
      rowid ==  5L ~ 5,
      rowid ==  6L ~ 5,
      rowid ==  7L ~ 5,
      rowid ==  8L ~ 5,
      rowid ==  9L ~ 4,
      rowid == 10L ~ 5,
      rowid == 11L ~ 5,
      rowid == 12L ~ 5,
      rowid == 13L ~ 5,
      rowid == 14L ~ 5,
      rowid == 15L ~ 4,
      rowid == 16L ~ 3,
      rowid == 17L ~ 5,
      rowid == 18L ~ 3,
      rowid == 19L ~ 4,
      rowid == 20L ~ 5,
      rowid == 21L ~ 5,
      rowid == 22L ~ 4,
      rowid == 23L ~ 3,
      rowid == 24L ~ 4,
      rowid == 25L ~ 3,
      rowid == 26L ~ 5,
      TRUE ~ NA_real_
    )#,
    # rating = dplyr::case_when(
    #   stars %% 1 == 0 ~ strrep("star,", stars),
    #   stars %% 1 != 0 ~ paste0(strrep("star,", floor(stars)), "star-half")
    # )
  )


# description_content %>% readr::write_csv("data/clean/description_content.csv")

description_content %>%
  gt::gt() %>%
  gt::sub_missing(missing_text = "") %>%
  gt::fmt_markdown(columns = c(text, answer)) %>%
  # gt::fmt_icon(
  #   columns = rating,
  #   fill_color = "orange"
  # ) %>%
  fun_starts(stars, icon = "star") %>%
  gt::cols_width(stars ~ gt::px(150)) %>%
  gt::cols_label(
    topic = "Topic",
    subtopic = "Subtopic",
    text = "Text",
    answer = "Answer",
    stars = "Rating"
  ) %>%
  gt::tab_footnote(
    footnote = gt::md(glue::glue("Processed data from the published [job vacancy]({url_job})")),
    locations = gt::cells_column_labels(columns = c(topic, subtopic, text))
  ) %>%
  gt::tab_footnote(
    footnote = "Self-assessment",
    locations = gt::cells_column_labels(columns = c(answer, stars))
  )


topics_analyze <- c(
  "Key Responsibilities",
  "Required Qualifications",
  "Preferred Qualifications",
  "Technical Skills"
)

description_content %>%
  dplyr::filter(
    topic %in% topics_analyze
  ) %>%
  dplyr::mutate(topic = topic %>% forcats::fct(topics_analyze)) %>%
  dplyr::group_by(topic) %>%
  dplyr::summarise(
    n = n(),
    avg_rating = mean(stars)
  ) %>%
  dplyr::ungroup()





url_image <- "https://framerusercontent.com/images/5C6qdSWkJqTgArbdg5hhP92qE.png"

# read the image
image_theme <- url_image %>%
  imager::load.image()

# extract the color of each pixel
df_image_theme <- image_theme %>%
  as.data.frame() %>%
  tibble::as_tibble() %>%
  tidyr::pivot_wider(
    id_cols = c(x, y),
    names_from = cc,
    values_from = value
  ) %>%
  dplyr::rename(
    "red"   = "1",
    "green" = "2",
    "blue"  = "3",
    "alpha" = "4"
  ) %>%
  dplyr::count(red, green, blue, alpha, sort = TRUE) %>%
  dplyr::mutate(
    p = n / sum(n),
    a = cumsum(p),
    color = grDevices::rgb(red, green, blue, alpha)
  ) %>%
  dplyr::filter(a < 0.95) %>%
  tibble::rowid_to_column()

# prepare data to clustering
df_clust <- df_image_theme %>%
  dplyr::select(red, green, blue, alpha) %>%
  as.data.frame()

rownames(df_clust) <- df_image_theme$color


# hierarchical clustering
hc <- df_clust %>%
  scale() %>%
  dist() %>%
  hclust()


# dendrogram
dendrogram_data <- ggdendro::dendro_data(hc)

color_objects <- dendrogram_data$labels$label
names(color_objects) <- dendrogram_data$labels$label


sysfonts::font_add_google("Roboto Mono")
showtext::showtext_auto()

plot_dendrogram <- dendrogram_data$segments %>%
  ggplot2::ggplot() +
  ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
  ggplot2::geom_label(
    data = dendrogram_data$labels,
    ggplot2::aes(
      x = x,
      y = y,
      label = label,
      hjust = 1,
      fill = label,
      color = ggplot2::after_scale(
        prismatic::best_contrast(fill, c("white", "black"))
      )
    ),
    angle = 0,
    size = 2,
    family = "Roboto Mono"
  ) +
  ggplot2::geom_hline(yintercept = 1, linetype = 2) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(
    breaks = scales::pretty_breaks(),
    expand = ggplot2::expansion(mult = 0.1)
  ) +
  ggplot2::scale_fill_manual(values = color_objects) +
  ggplot2::xlab("Colors") +
  ggplot2::ylab("Distance") +
  ggplot2::ggtitle(
    label = "Hierarchical Clustering of Colors",
    subtitle = gt::md(stringr::str_c("Font: ", url_image))
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    axis.text.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.grid.major.y = element_blank()
  )

plot_dendrogram

# colors selected
df_color_theme <- hc %>%
  cutree(h = 1) %>%
  tibble::tibble(
    color = names(.),
    cluster = .
  ) %>%
  dplyr::left_join(
    df_image_theme %>%
      dplyr::select(color, n)
  ) %>%
  dplyr::arrange(dplyr::desc(n)) %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarise(color = dplyr::first(color)) %>%
  dplyr::ungroup()


color_theme <- df_color_theme$color
names(color_theme) <- df_color_theme$color

df_color_theme %>%
  ggplot2::ggplot(ggplot2::aes(x = color, y = 1, fill = color)) +
  ggplot2::geom_col(color = "black") +
  ggplot2::geom_label(
    aes(
      label = color,
      color = ggplot2::after_scale(
        prismatic::best_contrast(fill, c("white", "black"))
      )
    ),
    hjust = 1
  ) +
  ggplot2::scale_fill_manual(
    values = color_theme
  ) +
  ggplot2::coord_flip() +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none",
    aspect.ratio = 1
  ) 



# ---- FIBA ----

list.files("src", full.names = TRUE) %>%
  purrr::walk(source, encoding = "UTF-8")
# match_data_full <- fun_read_match_data("data/match_2145647.json")

# match_data_full <- fun_read_match_data()
# match_data_full <- fun_read_match_data(match_id = "1754688")
match_data_full <- fun_read_match_data(match_id = "2145647")


df_first_level_description <- match_data_full %>%
  names() %>%
  tibble::as_tibble() %>%
  dplyr::rename(first_level = value)

# ---- to feed my laziness to write lol ----
# stringr::str_c(
#   "first_level == ",
#   stringr::str_c("\"", df_first_level_description$first_level, "\""),
#   " ~ "
# ) %>%
#   stringr::str_c(collapse = "\n") %>%
#   cat()



df_first_level_description <- df_first_level_description %>%
  dplyr::mutate(
    description = dplyr::case_when(
      first_level == "clock" ~ "time (value: 00:00)",
      first_level == "period" ~ "number of periods (value: 4)",
      first_level == "periodLength" ~ "length of period (value: 12)",
      first_level == "periodType" ~ "value: REGULAR",
      first_level == "inOT" ~ "value: 0",
      first_level == "tm" ~ "a lot of data",
      first_level == "pbp" ~ "a lot of data, tabular",
      first_level == "leaddata" ~ "matrix",
      first_level == "disableMatch" ~ "value: 0",
      first_level == "attendance" ~ "value: 0",
      first_level == "periodsMax" ~ "value: 4",
      first_level == "periodLengthREGULAR" ~ "value: 12",
      first_level == "periodLengthOVERTIME" ~ "value: 5",
      first_level == "timeline" ~ "empty",
      first_level == "scorers" ~ "a lot of data",
      first_level == "totalTimeAdded" ~ "value: 0",
      first_level == "totallds" ~ "a lot of data",
      first_level == "officials" ~ "a lot of data",
      first_level == "othermatches" ~ "empty"
    )
  )

df_first_level_description <- df_first_level_description %>%
  filter(
    first_level %in% c(
      "tm", "pbp", "leaddata", "scorers", "totallds", "officials"
    )
  )

vars_with_content <- c(
  "tm", "pbp", "leaddata", "scorers", "totallds", "officials"
)

df_match_data <- match_data_full[vars_with_content] %>%
  fun_parse_match_data(
    dir_write_csv = "data/clean"
  )

df_match_data <- match_data_full[vars_with_content] %>%
  fun_parse_match_data(
    dir_write_csv = "report/data"
  )

df_match_data <- match_data_full[vars_with_content] %>%
  fun_parse_match_data(
    dir_write_csv = "dashboard/data"
  )


df_name <- df_match_data$general_stats %>%
  dplyr::mutate(tno = dplyr::row_number()) %>%
  dplyr::select(tno, code) %>%
  mutate(
    code = code %>% forcats::fct()
  )


df_match_data$totallds %>%
  dplyr::mutate(
    tno = as.character(tno),
    lds = lds %>%
      stringr::str_sub(2) %>%
      stringr::str_replace("ReboundsTotal", "Rebounds")
  ) %>%
  tidyr::pivot_wider(
    id_cols = c(tno, name),
    names_from = lds,
    values_from = tot,
    values_fill = 0
  ) %>%
  dplyr::arrange(tno, name) %>%
  dplyr::mutate(
    across(
      where(is.numeric),
      function(x) x / sum(x)
    )
  ) %>%
  gt::gt(
    id = "one",
    rowname_col = "name"
  ) %>%
  gt::sub_missing(
    columns = tidyselect::everything(),
    missing_text = ""
  ) %>%
  gt::fmt_percent(
    columns = is.double,
    decimals = 1
  ) %>%
  gt::data_color(
    columns = is.double,
    palette = "BuGn",
    na_color = "white",
    domain = c(0, 1)
  ) %>%
  gt::tab_style(
    style = gt::cell_borders(
      sides = c("left", "right"),
      weight = gt::px(1),
      color = "lightgray"
    ),
    locations = gt::cells_body(
      columns = is.numeric
    )
  ) %>%
  gt::cols_align(
    columns = is.double,
    align = "center"
  ) %>%
  gt::cols_width(
    is.double ~ gt::px(80)
  ) %>%
  gt::tab_options(
    table.align = "left"
  )

# interessante
df_match_data$pl %>%
  dplyr::mutate(
    sMinutes = lubridate::ms(sMinutes)
  ) %>%
  filter(sMinutes > lubridate::ms("0:00")) %>%
  select(
    name, shirtNumber, active, starter, playingPosition, 
    sMinutes, sPoints, sFieldGoalsMade,
    sTwoPointersMade, sThreePointersMade,
    sReboundsTotal, sAssists
  )


df_match_data$shot %>%
  count(tno, per, actionType) %>%
  arrange(per, actionType, tno) %>%
  pivot_wider(
    id_cols = c(per),
    names_from = c(actionType, tno),
    values_from = n,
    values_fill = 0
  ) %>%
  gt::gt() %>%
  gt::tab_spanner(
    label = "2pt",
    columns = gt::starts_with("2pt")
  ) %>%
  gt::tab_spanner(
    label = "3pt",
    columns = gt::starts_with("3pt")
  ) %>%
  gt::cols_label(
    gt::ends_with("_1") ~ "1",
    gt::ends_with("_2") ~ "2",
    per ~ "Period"
  )



df_match_data$pbp %>%
  count(actionType, sort = TRUE)

df_match_data$pbp %>%
  filter(stringr::str_detect(actionType, "\\d{1,}pt")) %>%
  group_by(tno, actionType, player) %>%
  summarise(
    n = n(),
    success_count = sum(success),
    success_rate = mean(success)
  ) %>%
  ungroup() %>%
  arrange(desc(success_count))





  





df_general_stats %>%
  select(code, p1_score, p2_score, p3_score, p4_score) %>%
  gt::gt() %>%
  gt::cols_label(
    code ~ "",
    p1_score ~ 1,
    p2_score ~ 2,
    p3_score ~ 3,
    p4_score ~ 4
  ) %>%
  gt::cols_align(
    align = "center",
    columns = gt::ends_with("score")
  )


df_shot %>%
  left_join(df_name) %>%
  ggplot(aes(x = x, y = y, color = code)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  ggplot2::scale_color_manual(
    values = c(
      "GIN" = "orange",
      "ROS" = "blue"
      # "TTC" = "#1DB9C3",
      # "FLA" = "#C3271D"
    )
  ) +
  theme_minimal() +
  ggplot2::theme(panel.grid = element_blank())


df_general_stats %>%
  dplyr::select(
    code, full_score,
    tot_sFieldGoalsMade, tot_sFieldGoalsPercentage,
    tot_sThreePointersMade, tot_sThreePointersPercentage,
    tot_sTwoPointersMade, tot_sTwoPointersPercentage,
    tot_sFreeThrowsMade, tot_sFreeThrowsPercentage,
    tot_sReboundsTotal,
    tot_sAssists,
    tot_sTurnovers,
    tot_sSteals,
    tot_sBlocks,
    tot_sFoulsTotal
  ) %>%
  tidyr::pivot_longer(cols = -code) %>%
  tidyr::pivot_wider(id_cols = name, names_from = code, values_from = value) %>%
  dplyr::select(GIN, name, ROS) %>%
  dplyr::mutate(
    name = name %>%
      stringr::str_remove("tot_s") %>%
      stringr::str_replace("_", " ") %>%
      stringr::str_replace_all(
        pattern = "([[:upper:]])",
        replacement = " \\1"
      ) %>% 
      stringr::str_trim() %>%
      stringr::str_split(" ") %>%
      map_chr(
        \(x) x %>%
          stringr::str_to_title() %>%
          stringr::str_c(collapse = " ")
      )
  ) %>%
  gt::gt() %>%
  gt::cols_align(align = "center")


  
  

df_match_data$scorers %>%
  dplyr::count(subType, sort = TRUE) %>%
  dplyr::mutate(p = n / sum(n))



df_match_data$scorers %>%
  select(per, gt, everything()) %>%
  arrange(per, desc(gt)) %>%
  select(subType)
  filter(player == "G. Norwood")
    
    
    

  
  
  
  
  
  
  
####
inconn <- file("w.bin","rb")
outconn <- file("img.svg","wb")
base64decode(what=inconn, output=outconn)
close(outconn)

a <- "data:image/svg+xml,<svg display=\"block\" role=\"presentation\" viewBox=\"0 0 124 124\" xmlns=\"http://www.w3.org/2000/svg\"><path d=\"M 62.042 0 C 27.764 -0.025 -0.025 27.772 0 62.042 C 0.025 95.746 28.254 123.975 61.958 124 C 96.236 124.025 124.025 96.236 124 61.958 C 123.975 28.254 95.746 0.025 62.042 0 Z M 69.258 103.874 L 68.971 106.595 C 68.955 106.694 68.989 106.794 69.061 106.864 C 69.134 106.933 69.236 106.962 69.334 106.941 L 71.336 106.561 C 71.97 106.443 72.426 107.152 72.055 107.685 L 62.287 120.603 C 62 121.001 61.375 120.756 61.434 120.274 L 62.338 113.886 C 62.353 113.763 62.309 113.64 62.219 113.555 C 62.129 113.47 62.004 113.433 61.882 113.455 L 58.08 114.157 C 57.269 114.309 56.55 113.608 56.669 112.788 L 57.784 104.634 C 57.822 104.383 57.756 104.128 57.601 103.927 C 57.446 103.726 57.216 103.597 56.964 103.57 C 35.951 101.052 19.72 82.98 20.126 61.189 C 20.556 38.334 39.516 19.923 62.372 20.126 C 85.32 20.329 103.866 39 103.866 62 C 103.866 82.396 89.291 99.38 69.984 103.114 C 69.603 103.186 69.308 103.49 69.249 103.874 Z\" height=\"124.00003370507419px\" width=\"124.00003370991963px\"/></svg>"

JS(
  'url("data:image/svg+xml,<svg display=\"block\" role=\"presentation\" viewBox=\"0 0 124 124\" xmlns=\"http://www.w3.org/2000/svg\"><path d=\"M 62.042 0 C 27.764 -0.025 -0.025 27.772 0 62.042 C 0.025 95.746 28.254 123.975 61.958 124 C 96.236 124.025 124.025 96.236 124 61.958 C 123.975 28.254 95.746 0.025 62.042 0 Z M 69.258 103.874 L 68.971 106.595 C 68.955 106.694 68.989 106.794 69.061 106.864 C 69.134 106.933 69.236 106.962 69.334 106.941 L 71.336 106.561 C 71.97 106.443 72.426 107.152 72.055 107.685 L 62.287 120.603 C 62 121.001 61.375 120.756 61.434 120.274 L 62.338 113.886 C 62.353 113.763 62.309 113.64 62.219 113.555 C 62.129 113.47 62.004 113.433 61.882 113.455 L 58.08 114.157 C 57.269 114.309 56.55 113.608 56.669 112.788 L 57.784 104.634 C 57.822 104.383 57.756 104.128 57.601 103.927 C 57.446 103.726 57.216 103.597 56.964 103.57 C 35.951 101.052 19.72 82.98 20.126 61.189 C 20.556 38.334 39.516 19.923 62.372 20.126 C 85.32 20.329 103.866 39 103.866 62 C 103.866 82.396 89.291 99.38 69.984 103.114 C 69.603 103.186 69.308 103.49 69.249 103.874 Z\" height=\"124.00003370507419px\" width=\"124.00003370991963px\"/></svg>")'
)


h <- 12.400003370507419
w <- 12.400003370991963

h/10


######


# brazil_match_url <- "https://www.fiba.basketball/en/history/201-fiba-basketball-world-cup/208182/games/100735-BRA-ESP"
brazil_match_url <- "https://www.fiba.basketball/en/history/201-fiba-basketball-world-cup/208182/games/100757-IRI-BRA"
brazil_match_html <- xml2::read_html(brazil_match_url)

elements_size <- brazil_match_html %>%
  rvest::html_elements("script") %>%
  as.character() %>%
  stringr::str_length() %>%
  tibble::as_tibble() %>%
  tibble::rowid_to_column() %>%
  dplyr::arrange(desc(value))
# [120]: str_length=120020

brazil_match_data <- brazil_match_html %>%
  rvest::html_elements("script") %>%
  .[elements_size$rowid[1]] %>%
  rvest::html_text() %>%
  stringr::str_remove("self.__next_f.push") %>%
  stringr::str_sub(2, -2) %>%
  jsonlite::fromJSON() %>%
  magrittr::extract2(2) %>%
  stringr::str_sub(4) %>%
  jsonlite::fromJSON()

brazil_match_data[[4]]







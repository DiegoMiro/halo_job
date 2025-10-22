library(tidyverse)
library(shiny)
library(htmltools)
library(bs4Dash)
library(shinyWidgets)
library(shinycssloaders)
library(fresh)

library(patchwork)
library(gt)

list.files("src", full.names = TRUE) %>%
  purrr::walk(source, encoding = "UTF-8")

df_stats <- "data/general_stats.csv" %>%
  readr::read_csv()

df_name <- df_stats %>%
  dplyr::mutate(tno = dplyr::row_number()) %>%
  dplyr::select(tno, code) %>%
  mutate(
    code = code %>% forcats::fct()
  )

df_pl <- "data/pl.csv" %>%
  readr::read_csv()

df_shot <- "data/shot.csv" %>%
  readr::read_csv()


# score ----
df_scoring <- "data/scoring.csv" %>%
  readr::read_csv()

df_scorers <- "data/scorers.csv" %>%
  readr::read_csv()


# pbp ----
df_pbp <- "data/pbp.csv" %>%
  readr::read_csv(
    col_types = list(
      col_character(),
      col_character()
    )
  )

df_chart_pbp_full <- df_pbp %>%
  dplyr::left_join(df_name) %>%
  dplyr::mutate(
    gt = lubridate::seconds(
      ((period - 1)*12*60) + ((12*60) - as.integer(lubridate::ms(gt)))
    ),
    gt_date = lubridate::add_with_rollback(
      # lubridate::ymd_hm("197001011745"),
      lubridate::ymd("19700101"),
      gt
    ),
    pt = actionType %>%
      stringr::str_extract("\\d") %>%
      as.integer(),
    success = dplyr::case_when(
      success == 1 ~ "yes",
      success == 0 ~ "no"
    )
  ) %>%
  dplyr::select(
    period, periodType,
    actionNumber, gt, gt_date, clock, s1, s2, lead,
    actionType, subType, success, qualifier,
    code, player, shirtNumber
  ) %>%
  dplyr::arrange(period, actionNumber)


df_chart_pbp <- df_chart_pbp_full %>%
  dplyr::filter(
    s1 != lag(s1) | s2 != lag(s2)
  )


# lds ----
df_lds <- "data/lds.csv" %>%
  readr::read_csv()

df_totallds <- "data/totallds.csv" %>%
  readr::read_csv()


shinyApp(
  ui = bs4Dash::dashboardPage(
    title = "Basic Dashboard",
    fullscreen = FALSE,
    dark = NULL,
    help = NULL,
    header = bs4Dash::dashboardHeader(
      skin = "light",
      status = "white",
      border = TRUE,
      sidebarIcon = shiny::icon("bars"),
      controlbarIcon = icon("sliders"),
      fixed = FALSE,
      bs4Dash::column(
        width = 2,
        shinyWidgets::virtualSelectInput(
          inputId = "id1",
          label = "Filter 1", 
          choices = 1:10,
          multiple = TRUE,
          width = "100%", 
          dropboxWrapper = "body"
        )
      ),
      bs4Dash::column(
        width = 2,
        shinyWidgets::virtualSelectInput(
          inputId = "id2",
          label = "Filter 2", 
          choices = 1:10,
          multiple = TRUE,
          width = "100%", 
          dropboxWrapper = "body"
        )
      ),
      bs4Dash::column(
        width = 2,
        shinyWidgets::virtualSelectInput(
          inputId = "id3",
          label = "Filter 3", 
          choices = 1:10,
          multiple = TRUE,
          width = "100%", 
          dropboxWrapper = "body"
        )
      ),
      bs4Dash::column(
        width = 2,
        shinyWidgets::virtualSelectInput(
          inputId = "id4",
          label = "Filter 4", 
          choices = 1:10,
          multiple = TRUE,
          width = "100%", 
          dropboxWrapper = "body"
        )
      ),
      bs4Dash::column(
        width = 2,
        shinyWidgets::airDatepickerInput(
          inputId = "id5",
          label = "Filter 5",
          # placeholder = "You can pick 5 dates",
          range = TRUE,
          addon = "none",
          dateFormat = "MMM d, yyyy",
          clearButton = TRUE
        )
      )
    ),
    
    
    sidebar = bs4Dash::dashboardSidebar(
      skin = "light",
      status = "primary",
      elevation = 3,
      collapsed = TRUE,
      
      bs4Dash::sidebarMenu(
        
        bs4Dash::sidebarHeader("ANALYTICAL"),
        
        bs4Dash::menuItem(
          text = "Overview",
          tabName = "overview",
          icon = shiny::icon("grip")
        ),
        
        
        bs4Dash::sidebarHeader("PERFORMANCE"),
        
        bs4Dash::menuItem(
          text = "Player",
          tabName = "player_performance",
          icon = shiny::icon("user")
        )
      )
    ),
    
    footer = bs4Dash::dashboardFooter(
      # left = a(
      #   href = "https://twitter.com/divadnojnarg",
      #   target = "_blank", "@DivadNojnarg"
      # ),
      # right = tags$img(
      #   src = "lifemed_logo.svg"
      # )
    ),
    
    body = bs4Dash::dashboardBody(
      bs4Dash::tabItems(
        
        bs4Dash::tabItem(
          tabName = "overview",
          bs4Dash::tabBox(
            id = "overview_tab",
            collapsible = FALSE,
            maximizable = TRUE,
            width = 12,
            title = "Overview",
            shiny::tabPanel(
              title = "Match Summary",
              bslib::layout_columns(
                bslib::card_body(
                  height = "100%",
                  gt::gt_output("general_stats")
                ),
                bslib::card_body(
                  height = "100%",
                  shiny::plotOutput("chart_shot")
                )
              )
            ),
            shiny::tabPanel(
              title = "Points by time",
              bslib::card_body(
                height = "75vh",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("chart_points_by_time")
                )
              )
            )
          )
        ),
        
        bs4Dash::tabItem(
          tabName = "player_performance",
          bs4Dash::tabBox(
            id = "player_performance_tab",
            collapsible = FALSE,
            maximizable = TRUE,
            width = 12,
            title = "Player Performance",
            shiny::tabPanel(
              title = "Total LDS",
              bslib::card_body(
                height = "100%",
                gt::gt_output("totallds")
              )
            ),
            shiny::tabPanel(
              title = "Points by Player by Time",
              bslib::card_body(
                height = "75vh",
                shiny::plotOutput("chart_points_by_player_by_time")
              )
            )
          )
        )
      )
    )
  ),
  
  server <- function(input, output, session) {
    
    output$general_stats <- gt::render_gt({
      
      general_stats <- df_stats %>%
        dplyr::select(
          code, full_score, p1_score, p2_score, p3_score, p4_score,
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
        gt::cols_align(align = "center") %>%
        gt::cols_label(name = "")
      
      general_stats
    })
    
    
    
    output$chart_shot <- shiny::renderPlot({
      chart_shot <- df_shot %>%
        dplyr::left_join(df_name) %>%
        ggplot2::ggplot(ggplot2::aes(x = x, y = y, color = code)) +
        ggplot2::geom_point() +
        ggplot2::scale_x_continuous(limits = c(0, 100)) +
        ggplot2::scale_y_continuous(limits = c(0, 100)) +
        ggplot2::scale_color_manual(
          values = c(
            "GIN" = "orange",
            "ROS" = "blue"
            # "TTC" = "#1DB9C3",
            # "FLA" = "#C3271D"
          )
        ) +
        ggplot2::theme_minimal() +
        ggplot2::coord_flip()
        ggplot2::theme(panel.grid = ggplot2::element_blank())
        
        chart_shot
    })
    
    
    
    output$chart_points_by_time <- shiny::renderPlot({
      
      df_zero_sequence_s1 <- df_chart_pbp %>%
        dplyr::select(s1) %>%
        mutate(d = c(0L, diff(s1))) %>%
        select(d) %>%
        fun_find_zero_sequence()
      
      df_chart_pbp_zero_sequence_s1 <- df_zero_sequence_s1 %>%
        dplyr::filter(
          flg == 0,
          sequence_size == sequence_count_zero
        ) %>%
        dplyr::select(sequence) %>%
        tidyr::unnest(cols = sequence) %>%
        dplyr::arrange(sequence) %>%
        dplyr::left_join(
          df_chart_pbp %>%
            tibble::rowid_to_column(var = "sequence")
        ) %>%
        dplyr::mutate(tno = 1L) %>%
        dplyr::select(tno, gt_date, value = s1)
      
      
      
      df_zero_sequence_s2 <- df_chart_pbp %>%
        dplyr::select(s2) %>%
        mutate(d = c(0L, diff(s2))) %>%
        select(d) %>%
        fun_find_zero_sequence()
      
      df_chart_pbp_zero_sequence_s2 <- df_zero_sequence_s2 %>%
        dplyr::filter(
          flg == 0,
          sequence_size == sequence_count_zero
        ) %>%
        dplyr::select(sequence) %>%
        tidyr::unnest(cols = sequence) %>%
        dplyr::arrange(sequence) %>%
        dplyr::left_join(
          df_chart_pbp %>%
            tibble::rowid_to_column(var = "sequence")
        ) %>%
        dplyr::mutate(tno = 2L) %>%
        dplyr::select(tno, gt_date, value = s2)
      
      
      
      df_chart_pbp_zero_sequence <- dplyr::bind_rows(
        df_chart_pbp_zero_sequence_s1, df_chart_pbp_zero_sequence_s2
      ) %>%
        dplyr::left_join(df_name)
      
      
      df_chart_pbp_1 <- df_chart_pbp %>%
        dplyr::select(period, gt_date, s1, s2) %>%
        tidyr::pivot_longer(cols = c(s1, s2)) %>%
        dplyr::mutate(
          tno = name %>%
            stringr::str_extract("\\d") %>%
            as.integer()
        ) %>%
        dplyr::left_join(df_name) %>%
        dplyr::select(period, gt_date, code, value)
      
      chart_points_by_time_1 <- df_chart_pbp_1 %>%
        ggplot2::ggplot(ggplot2::aes(x = gt_date, y = value, colour = code)) +
        ggplot2::geom_line() +
        ggplot2::geom_point(
          data = dplyr::anti_join(df_chart_pbp_1, df_chart_pbp_zero_sequence),
          size = 2,
          show.legend = FALSE
        ) +
        ggplot2::geom_point(
          data = df_chart_pbp_zero_sequence,
          shape = 21,
          size = 2,
          fill = "white",
          show.legend = FALSE
        ) +
        ggplot2::scale_x_datetime(
          date_labels = "%M",
          date_breaks = "12 min"
        ) +
        ggplot2::scale_color_manual(
          values = c(
            "GIN" = "orange",
            "ROS" = "blue"
            # "TTC" = "#1DB9C3",
            # "FLA" = "#C3271D"
          )
        ) +
        # ggplot2::facet_wrap(~period, nrow = 1, scales = "free_x") +
        ggplot2::ggtitle(label = "") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "top",
          legend.title = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          panel.spacing = grid::unit(0, "lines")
        )
      
      
      chart_points_by_time_2 <- df_chart_pbp %>%
        select(period, gt_date, gt, lead) %>%
        mutate(
          win = case_when(
            lead  > 0 ~ "1",
            lead == 0 ~ "tie",
            lead  < 0 ~ "2",
          )
        )  %>%
        ggplot(aes(x = gt_date)) +
        geom_area(aes(y = lead, fill = win)) +
        geom_hline(yintercept = 0, color = "white") +
        scale_y_continuous(
          limits = c(-1, 1) * max(abs(df_pbp$lead))
        ) +
        ggplot2::scale_fill_manual(
          values = c(
            "tie" = "lightgray",
            "1" = "orange",
            "2" = "blue"
          )
        ) +
        # ggplot2::facet_wrap(~period, nrow = 1, scales = "free_x") +
        ggplot2::theme_void() +
        ggplot2::theme(
          legend.position = "none",
          panel.spacing = grid::unit(0, "lines")
        )
      
      
      chart_points_by_time <- chart_points_by_time_1 / chart_points_by_time_2 +
        patchwork::plot_layout(heights = c(3, 1))
      
      chart_points_by_time
      
    })
    
    
    
    
    
    output$chart_points_by_player_by_time <- shiny::renderPlot({
      
      df_chart_pbp3 <- df_chart_pbp_full %>%
        dplyr::filter(
          actionType %in% c("freethrow", "2pt", "3pt")
        ) %>%
        mutate(
          pts = case_when(
            stringr::str_detect(actionType, "\\d{1}pt") ~ actionType %>% stringr::str_extract("\\d") %>% as.integer(),
            actionType == "freethrow" ~ 1L,
            TRUE ~ NA_integer_
          )
        ) %>%
        select(period, gt_date, gt, code, player, success, pts)
      
      df_count_by_player <- df_chart_pbp3 %>%
        group_by(code, player) %>%
        summarise(pts_sum = sum(pts)) %>%
        ungroup() %>%
        arrange(desc(pts_sum)) %>%
        mutate(player_order = player %>% forcats::fct())
      
      
      df_chart_pbp3 <- df_count_by_player %>%
        left_join(df_chart_pbp3)
      
      df_substitution_long <- df_chart_pbp_full %>%
        filter(actionType == "substitution", success == "yes") %>%
        select(code, player, period, gt_date, subType) %>%
        mutate(subType = stringr::str_to_title(subType)) %>%
        arrange(code, player)
      
      df_players <- df_pl %>%
        filter(sMinutes != "0:00") %>%
        
        select(tno, name, captain, active, starter, sMinutes)
      
      df_substitution <- df_players %>%
        filter(starter == 1) %>%
        left_join(df_name) %>%
        select(code, player = name) %>%
        mutate(
          period = 1L,
          gt_date = lubridate::ymd_hms("19700101000000"),
          subType = "In"
        ) %>%
        bind_rows(df_substitution_long) %>%
        arrange(code, player, period, gt_date) %>%
        nest(.by = c(code, player)) %>%
        mutate(
          data = data %>%
            map(
              function(x) {
                if (last(x$subType) == "In") {
                  output <- x %>%
                    bind_rows(
                      tibble(
                        period = 1L,
                        gt_date = lubridate::ymd_hms("19700101004800"),
                        subType = "Out"
                      )
                    )
                } else {
                  output <- x
                }
                
                output <- output %>%
                  mutate(
                    moviment = if_else(subType == "In", 1, 0) %>%
                      cumsum()
                  )
                
                return(output)
              }
            )
        ) %>%
        unnest(cols = data) %>%
        tidyr::pivot_wider(
          id_cols = c(code, player, moviment),
          names_from = subType,
          values_from = gt_date
        ) %>%
        left_join(df_count_by_player, .)
      
      
      df_stripe <- df_count_by_player %>%
        arrange(code, desc(pts_sum)) %>%
        group_by(code) %>%
        dplyr::mutate(
          y_value = dplyr::row_number(),
          stripe = case_when(
            y_value %% 2 == 1 ~ "white",
            TRUE ~ "gray"
          )
        )
      
      
      chart_points_by_player_by_time <- df_chart_pbp3 %>%
        dplyr::mutate(
          face = dplyr::if_else(success == "yes", "bold", "plain")
        ) %>%
        ggplot(aes(y = reorder(player, pts_sum))) +
        
        ggplot2::geom_rect(
          ggplot2::aes(
            ymin = y_value - 0.5,
            ymax = y_value + 0.5,
            xmin = min(df_chart_pbp_full$gt_date),
            xmax = max(df_chart_pbp_full$gt_date),
            fill = stripe
          ),
          data = df_stripe,
          alpha = 0.5,
          colour = "gray",
          linewidth = 0.1
        ) +
        
        geom_point(aes(x = gt_date, colour = code), size = 3) +
        geom_point(
          aes(x = gt_date),
          size = 2,
          data = df_chart_pbp3 %>% filter(success == "no"),
          color = "white"
        ) +
        
        geom_text(
          aes(x = gt_date, colour = code, label = pts, fontface = face),
          size = 3,
          vjust = -1
        ) +
        
        geom_segment(
          aes(
            x = In,
            xend = Out,
            yend = reorder(player, pts_sum),
            colour = code
          ),
          alpha = 0.5,
          data = df_substitution
        ) +
        geom_point(
          aes(
            x = In,
            y = reorder(player, pts_sum),
            color = code
          ),
          shape = "l",
          size = 4,
          data = df_substitution
        ) +
        geom_point(
          aes(
            x = Out,
            y = reorder(player, pts_sum),
            color = code
          ),
          shape = "l",
          size = 4,
          data = df_substitution
        ) +
        
        ggplot2::scale_x_datetime(
          expand = expansion(mult = 0.01),
          date_labels = "%M min",
          date_breaks = "12 min"
        ) +
        ggplot2::scale_y_discrete(expand = expansion(mult = 0.1)) +
        ggplot2::scale_color_manual(
          values = c(
            "GIN" = "orange",
            "ROS" = "blue"
            # "TTC" = "#1DB9C3",
            # "FLA" = "#C3271D"
          )
        ) +
        ggplot2::scale_fill_manual(values = c("white", "#F9F9F9")) +
        ggplot2::facet_wrap(~code, ncol = 1, scales = "free_y") +
        xlab("Time") +
        ylab("Player") +
        ggplot2::ggtitle(label = "Points by player with playing time") +
        theme_minimal() +
        ggplot2::theme(
          legend.position = "none",
          axis.title = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(margin = margin(r = 0))
        )
      
      chart_points_by_player_by_time
      
    })
    
    
    
    
    output$totallds <- gt::render_gt({
      df_totallds %>%
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
    })
    
    
    output$table_demo <- gt::render_gt(
      expr = {
        iris %>%
          gt::gt()
      },
      height = gt::px(550)
    )
    
  }
)

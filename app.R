# Required Libraries for application
library(shiny)
library(shinydashboard)
library(apexcharter)
library(connectapi)
library(dplyr)
library(shinycssloaders)
library(bslib)
library(stringr)
library(tidyr)
library(lubridate)

# Establishes connection to the Connect server using address & API key in .Renviron file
client <- connect()

# Dates for ranging data; only requesting last 90 days of usage data
days_back <- as.numeric(Sys.getenv("DAYSBACK", 90))
report_from <- today() - lubridate::ddays(days_back)
report_to <- today()

# Data Fetch using connectapi package functions - Pull all data from time frame.

data_usage <- get_usage_shiny(src = client, from = report_from, to=report_to, limit = Inf) %>%
  subset(select = -c(ended)) %>%
  rbind(
    get_usage_static(src = client, from = report_from, to=report_to, limit = Inf) %>%
      subset(select = -c(variant_key, rendering_id, bundle_id, path)) %>%
      rename(started = time)
  )

data_content <- get_content(src = client)

data_users <- get_users(src = client)

# ui of dashboard
ui <- 
  dashboardPage(
    
    dashboardHeader(
      title = "Posit Connect Usage",
      titleWidth = "40%"
    ),
    
    dashboardSidebar(
      disable = TRUE, 
      collapsed = TRUE
    ),
    
    dashboardBody(
      
      tags$style(HTML("
          #timeline {
            padding: 25px;
          }
          
          #buttons {
            padding: 10px;
          }
        ")
      ),
      
      layout_column_wrap(
        id="buttons",
        value_box(
          title = "Total Users on Connect:",
          value = data_users %>% tally() %>% pull(),
          showcase = shiny::icon("user")
        ),
        
        value_box(
          title = "Total Artifacts on Connect:",
          value = data_content %>% tally() %>% pull(),
          showcase = shiny::icon("file")
        )
      ),
      
      tabsetPanel(
        
        tabPanel(
          title = "Posit Connect Artifact Access Metrics",
          fluidRow(id = "timeline",
            shinycssloaders::withSpinner(
              apexchartOutput("use_over_time")
            )
          ),
          fluidRow(
            box(
              apexchartOutput("use_content"),
              width = 4
            ),
            box(
              apexchartOutput("use_viewer"),
              width = 4
            ),
            box(
              apexchartOutput("use_owner"),
              width = 4
            )
          ),
          fluidRow(
            verbatimTextOutput("verbatim")
          )
        ),
        tabPanel(
          title = "Posit Connect Artifact Upload Metrics",
          fluidRow(id="timeline",
            shinycssloaders::withSpinner(
              apexchartOutput("upload_over_time")
            )
          ),
          fluidRow(
            box(
              apexchartOutput("upload_content"),
              width = 6
            ),
            box(
              apexchartOutput("upload_owner"),
              width = 6
            )
          ),
          fluidRow(
            verbatimTextOutput("verbatim")
          )
        )
      )
    )
  )

server <- function(input, output, session) {
  
  # Data Prep -------------------------------------------------------------
  
  delay_duration <- 500
  
  use_content <- debounce(reactive(
    data_usage %>%
      group_by(content_guid) %>%
      tally() %>%
      left_join(
        data_content %>% select(guid, name, title),
        by = c(content_guid = "guid")
      ) %>%
      filter(!is.na(title)) %>%
      arrange(desc(n)) %>%
      head(20) %>%
      mutate(
        plot_values = if_else(n > 2.5*median(n), log10(n)*(2.5*median(n)/log10(n[1])) + min(n), n),
        is_transformed = if_else(n > 2.5*median(n), TRUE, FALSE),
        tooltip = str_c(
          '<div style="padding:5px 10px;;border:1px solid #FFF;border-radius:5px;">
          ',name,'
          <br/>
          <span>
          Count:
          <b>
          ',n,'
          </b>
          <br/>
          Normalized:
          <b>
          ',is_transformed,'
          </b>
          <br/>
          Graphed Value:
          <b>
          ',plot_values,'
          </b>
          </span>
          </div>'
        )
      )
  ), delay_duration)
  
  
  use_viewers <- debounce(reactive(
    data_usage %>%
      group_by(user_guid) %>%
      tally() %>%
      left_join(
        data_users %>% select(guid, username),
        by = c(user_guid = "guid")
      ) %>%
      arrange(desc(n)) %>%
      head(20)%>%
      replace_na(list(username = "anonymous")) %>%
      mutate(
        plot_values = if_else(n > 2.5*median(n), log10(n)*(2.5*median(n)/log10(n[1])) + min(n), n),
        is_transformed = if_else(n > 2.5*median(n), TRUE, FALSE),
        tooltip = str_c(
          '<div style="padding:5px 10px;;border:1px solid #FFF;border-radius:5px;">
            ',username,'
            <br/>
            <span>
            Count: 
            <b>
            ',n,'
            </b>
            <br/>
            Normalized:
            <b>
            ',is_transformed,'
            </b>
            <br/>
            Graphed Value:
            <b>
            ',plot_values,'
            </b>
            </span>
            </div>'
        )
      )
  ), delay_duration)
  
  
  use_owners <- debounce(reactive(
    data_usage %>%
      left_join(
        data_content %>% select(guid, owner_guid),
        by = c(content_guid = "guid")
      ) %>%
      filter(!is.na(owner_guid)) %>% # remove content that was deleted
      group_by(owner_guid) %>%
      tally() %>%
      left_join(
        data_users %>% select(guid, username),
        by = c(owner_guid = "guid")
      ) %>% 
      arrange(desc(n)) %>%
      head(20)%>%
      mutate(
        plot_values = if_else((n > 2.5*median(n)), log10(n)*(2.5*median(n)/log10(n[1])) + min(n), n),
        is_transformed = if_else(n > 2.5*median(n), TRUE, FALSE),
        tooltip = str_c(
          '<div style="padding:5px 10px;;border:1px solid #FFF;border-radius:5px;">
            ',username,'
            <br/>
            <span>
            Count: 
            <b>
            ',n,'
            </b>
            <br/>
            Normalized:
            <b>
            ',is_transformed,'
            </b>
            <br/>
            Graphed Value:
            <b>
            ',plot_values,'
            </b>
            </span>
            </div>'
        )
      )
  ), delay_duration)
  
  
  use_over_time <- debounce(reactive(
    data_usage %>%
      mutate(
        date = lubridate::as_date(lubridate::floor_date(started, "day"))
      ) %>%
      group_by(date) %>%
      tally() %>%
      mutate(
        date_disp = format(date, format="%a %b %d %Y")
      ) %>%
      arrange(date)
  ), delay_duration)
  
  
  upload_content <- debounce(reactive(
    data_content %>%
      mutate(
        date = lubridate::as_date(lubridate::floor_date(created_time, "day"))
      ) %>%
      filter(date >= report_from & date <= report_to) %>%
      group_by(app_mode) %>%
      tally() %>%
      arrange(desc(n))
  ), delay_duration)
  
  
  upload_owners <- debounce(reactive(
    data_content %>%
      mutate(
        date = lubridate::as_date(lubridate::floor_date(created_time, "day"))
      ) %>%
      filter(!is.na(owner_guid) & date >= report_from & date <= report_to) %>% # remove content that was deleted & out of date range
      group_by(owner_guid) %>%
      tally() %>%
      left_join(
        data_users %>% select(guid, username),
        by = c(owner_guid = "guid")
      ) %>% 
      arrange(desc(n))
  ), delay_duration)
  
  
  upload_over_time <- debounce(reactive(
    data_content %>%
      mutate(
        date = lubridate::as_date(lubridate::floor_date(created_time, "day"))
      ) %>%
      filter(date >= report_from & date <= report_to) %>%
      group_by(date) %>%
      tally() %>%
      mutate(
        date_disp = format(date, format="%a %b %d %Y")
      ) %>%
      arrange(date)
  ), delay_duration)
  
  
  # Graph output ----------------------------------------------------------
  
  output$use_over_time <- renderApexchart(
    apexchart(auto_update = FALSE) %>%
      ax_chart(type = "line") %>%
      ax_title("Artifact Access By Date") %>%
      ax_plotOptions() %>%
      ax_series(list(
        name = "Count",
        data = purrr::map2(use_over_time()$date_disp, use_over_time()$n, ~ list(.x,.y))
      )) %>%
      ax_xaxis(
        type = "datetime"
      ) %>%
      set_input_selection("time")
  )
  
  output$use_content <- renderApexchart(
    apexchart(ax_opts = list(
      chart = list(
        type="bar"
      ),
      plotOptions = list(
        bar = list(horizontal = TRUE)  # Set horizontal bars
      ),
      stroke = list(
        curve = "smooth"
      ),
      dataLabels = list(
        enabled = FALSE
      ),
      series = list(
        list(
          name = "data",
          data = lapply(1:nrow(use_content()), function(i) {
            list(
              x = use_content()$title[i],
              y = use_content()$plot_values[i],
              fillColor = ifelse(use_content()$is_transformed[i], "#FB6C00", "#008FFB"),
              tooltip = use_content()$tooltip[i]
            )
          })
        )
      ),
      title = list(
        text = "Access Per Artifact (Top 20)"
      ),
      tooltip = list(
        enabled = TRUE,
        custom = JS(
          "function({series, seriesIndex, dataPointIndex, w}) {
          var tooltip = w.config.series[seriesIndex].data[dataPointIndex].tooltip;
          return typeof tooltip == 'undefined' ? null : tooltip;
          }"
        )
      )
    ))
  )
  
  output$use_viewer <- renderApexchart(
    apexchart(ax_opts = list(
      chart = list(
        type="bar"
      ),
      plotOptions = list(
        bar = list(horizontal = TRUE)  # Set horizontal bars
      ),
      stroke = list(
        curve = "smooth"
      ),
      dataLabels = list(
        enabled = FALSE
      ),
      series = list(
        list(
          name = "data",
          data = lapply(1:nrow(use_viewers()), function(i) {
            list(
              x = use_viewers()$username[i],
              y = use_viewers()$plot_values[i],
              fillColor = ifelse(use_viewers()$is_transformed[i], "#FB6C00", "#008FFB"),
              tooltip = use_viewers()$tooltip[i]
            )
          })
        )
      ),
      title = list(
        text = "Access Per Viewer (Top 20)"
      ),
      tooltip = list(
        enabled = TRUE,
        custom = JS(
          "function({series, seriesIndex, dataPointIndex, w}) {
          var tooltip = w.config.series[seriesIndex].data[dataPointIndex].tooltip;
          return typeof tooltip == 'undefined' ? null : tooltip;
          }"
        )
      )
    ))
  )
  
  output$use_owner <- renderApexchart(
    apexchart(ax_opts = list(
      chart = list(
        type="bar"
      ),
      plotOptions = list(
        bar = list(horizontal = TRUE)  # Set horizontal bars
      ),
      stroke = list(
        curve = "smooth"
      ),
      dataLabels = list(
        enabled = FALSE
      ),
      series = list(
        list(
          name = "data",
          data = lapply(1:nrow(use_owners()), function(i) {
            list(
              x = use_owners()$username[i],
              y = use_owners()$plot_values[i],
              fillColor = ifelse(use_owners()$is_transformed[i], "#FB6C00", "#008FFB"),
              tooltip = use_owners()$tooltip[i]
            )
          })
        )
      ),
      title = list(
        text = "Access Per Owner (Top 20)"
      ),
      tooltip = list(
        enabled = TRUE,
        custom = JS(
          "function({series, seriesIndex, dataPointIndex, w}) {
          var tooltip = w.config.series[seriesIndex].data[dataPointIndex].tooltip;
          return typeof tooltip == 'undefined' ? null : tooltip;
          }"
        )
      )
    ))
  )
  
  output$upload_over_time <- renderApexchart(
    apexchart(auto_update = FALSE) %>%
      ax_chart(type = "line") %>%
      ax_title("Artifact Uploads By Date") %>%
      ax_series(list(
        name = "Count",
        data = purrr::map2(upload_over_time()$date_disp, upload_over_time()$n, ~ list(.x,.y))
      )) %>%
      ax_xaxis(
        type = "datetime"
      ) %>%
      set_input_selection("time")
  )
  
  output$upload_content <- renderApexchart(
    apex(
      data = upload_content() %>% head(20), 
      type = "bar", 
      mapping = aes(app_mode, n)
    ) %>%
      ax_title("Artifact Content Type") %>%
      set_input_click("content")
  )
  
  output$upload_owner <- renderApexchart(
    apex(
      data = upload_owners() %>% head(20), 
      type = "bar", 
      mapping = aes(username, n)
    ) %>%
      ax_title("Artifacts Per Publisher") %>%
      set_input_click("owner")
  )
  
}

shinyApp(ui = ui, server = server)

# load package
library(bslib)
library(bsicons)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(thematic)
library(DT)
library(tidyverse)
library(slider)
library(tidyverse)
library(janitor)
library(skimr)
library(slider)
library(tsibble)
library(plotly)
library(scales)
library(formattable)
library(feasts)
library(zoo)
# none scientific notation

options(scipen=999)


# load quarterly nominal GDP
nva_sua_data_raw <- read_csv("https://www.datazoa.com/publish/export.asp?hash=9F80C8B0CD&dzuuid=1835&a=exportcsv&startdate=1%2F1%2F1996&enddate=7%2F1%2F2030&transpose=yes")

# load quarterly real GDP (rva_sua)
rva_sua_data_raw <- read_csv("https://www.datazoa.com/publish/export.asp?hash=37FB8CB3A8&dzuuid=1835&a=exportcsv&startdate=1%2F1%2F1996&enddate=7%2F1%2F2030&transpose=yes")

# load quarterly real GDP seasonally adjusted (rva_sa)

rva_sa_data_raw <- read_csv("https://www.datazoa.com/publish/export.asp?hash=F436DCD4DC&dzuuid=1835&a=exportcsv&startdate=1%2F1%2F1996&enddate=7%2F1%2F2030&transpose=yes")

goods_industry <- c("Agriculture Forestry & Fishing", "Mining & Quarrying" ,
                    "Manufacture", "Construction"   )



# create function to apply to other data set
clean_raw_va <- function(x, name){
  x %>% 
    rename(industry=1) %>% 
    # create a long-data variable - stacked
    pivot_longer(-industry, names_to = "date", values_to = name) %>% 
    # clean variables
    mutate(date = mdy(date), # convert date column to date variable
           industry = str_remove(industry, "\\*") # remove * from GDP
    )  %>% 
    pivot_wider(names_from = industry, values_from = {{name}}) %>% 
    mutate(`Goods Producing` = `Agriculture Forestry & Fishing` + `Mining & Quarrying` + Manufacture  + Construction,
           `Services` = `Total Value Added at Basic Prices` + `Less Financial Intermediation Services Indirectly Measured (FISIM)` - `Goods Producing` 
           
    ) %>% 
    pivot_longer(cols = 2:last_col(), names_to = "industry", values_to = name)
}

# use function 

nva_sua_data_clean <- clean_raw_va(nva_sua_data_raw, "nva_sua_3m")
rva_sua_data_clean <- clean_raw_va(rva_sua_data_raw, "rva_sua_3m")
rva_sa_data_clean <- clean_raw_va(rva_sa_data_raw, "rva_sa_3m")


va_data_all <- nva_sua_data_clean %>% 
  left_join(rva_sua_data_clean,
            by = join_by(industry, date)
  ) %>% 
  left_join(rva_sa_data_clean,
            by = join_by(industry, date)
  ) %>%
  mutate(date = rollforward(date) %m+% months(2), # convert to end of quarter and date of month
         across(where(is.numeric), ~.*1000000)  # coverting from millions to dollars
  ) %>% 
  mutate(.by = industry,
         # sum six months
         across(.col=c("nva_sua_3m", "rva_sua_3m", "rva_sa_3m"), ~slide_dbl(., sum, .before = 1, .complete = TRUE), .names = "{str_sub(.col, start=1, end=-3)}6m"),
         # sum nine months
         across(.col=c("nva_sua_3m", "rva_sua_3m", "rva_sa_3m"), ~slide_dbl(., sum, .before=2, .complete=TRUE), .names = "{str_sub(.col, start=1, end=-3)}9m"),
         # sum 12 months
         across(.col=c("nva_sua_3m", "rva_sua_3m", "rva_sa_3m"), ~slide_dbl(., sum, .before=3, .complete=TRUE), .names = "{str_sub(.col, start=1, end=-3)}12m"),
         # create deflator and times by 100
         va_deflator_3m = nva_sua_3m/rva_sua_3m*100,
         va_deflator_6m = nva_sua_6m/rva_sua_6m*100,
         va_deflator_9m = nva_sua_9m/rva_sua_9m*100,
         va_deflator_12m = nva_sua_12m/rva_sua_12m*100) %>% 
  mutate(quarter = factor(paste0("Q",quarter(date)) ))



va_data_all_long <- va_data_all %>% 
  relocate(quarter, .after = industry) %>% 
  # convert to long-formaat
  pivot_longer(col=-c("industry", "quarter", "date" ), names_to = "indicator", values_to = "value") %>% 
  # contribution to total value added
  mutate(
    .by = c(indicator, date),
    # note that "Gross Domestic Product at Market Prices"  and "Taxes Less Subsidies on Products"
    # does not contribute to Total Value Added at Basic Prices
    contribution_tva_value = value/value[industry=="Total Value Added at Basic Prices"]
    
  ) %>% 
  
  # create growth
  mutate(
    .by =c(industry, indicator),
    growth_3m = value/lag(value,1)-1,
    growth_6m = value/lag(value,2)-1,
    growth_9m = value/lag(value,3)-1,
    growth_12m = value/lag(value,4)-1,
  ) %>% 
  pivot_longer(cols = growth_3m:growth_12m, names_to = "growth_period", values_to ="growth_rate" ) %>% 
  mutate(.by = c(indicator, industry, growth_period),
         # remove Gross Domestic Product at Market Prices and Taxes Less Subsidies on Products
         # since it is not a component of Total Value Added at Basic Prices
         
         contribution_growth = if_else(industry %in% c("Gross Domestic Product at Market Prices", "Taxes Less Subsidies on Products" ),
                                       NA, growth_rate*lag(contribution_tva_value,4))
         
  ) %>% 
  mutate(
    # create labels for each indicator
    
    indicator_lb =  if_else( str_detect(indicator, "nva_sua_"), str_replace(indicator, "nva_sua_", "Value Added at Current Prices (Seasonally Unadjusted) "), indicator ),
    indicator_lb = if_else( str_detect(indicator_lb, "rva_sua_"), str_replace(indicator_lb, "rva_sua_", "Value Added at Constant (2007) Prices (Seasonally Unadjusted) "), indicator_lb ),
    indicator_lb = if_else( str_detect(indicator_lb, "rva_sa_"), str_replace(indicator_lb, "rva_sa_", "Value Added at Constant (2007) Prices (Seasonally Adjusted) "), indicator_lb ),
    indicator_lb = if_else( str_detect(indicator_lb, "va_deflator_"), str_replace(indicator_lb, "va_deflator_", "Value Added Deflator (Seasonally Unadjusted) "), indicator_lb ),
    # change m to months
    #indicator_lb = if_else( str_detect(indicator_lb, "m"), str_replace(indicator_lb, "m", " month"), indicator_lb ),
    indicator_lb = str_trim(indicator_lb)
    
  ) %>% 
  mutate(
    growth_period_lb = glue::glue("{parse_number(growth_period)}-month growth rate"),
    qyear = yearquarter(date),
    month_lb = lubridate::month(date, label=TRUE)
  ) %>% 
  
  mutate(.by = c(industry,indicator, growth_period),
         sign = sign(growth_rate),
         # create group base on growth, decline or flat trends
         grp = if_else(coalesce(sign,0) !=lag(coalesce(sign,0)),1,0),
         grp = cumsum(coalesce(grp,0))
         
  ) %>% 
  mutate(.by = c(industry, indicator, growth_period, grp),
         growth_trend = if_else(is.na(growth_rate),NA,1:n()),
         sign_lb = case_when(
           sign == -1 ~ "Decline",
           sign==0 ~ "Flat",
           sign==1 ~ "Growth"
         )
  ) %>% 
  mutate(level_lb = str_extract(indicator_lb, "..m$"),
         level_lb = str_trim(level_lb),
         
         # remove period from indicator_lb
         indicator_lb = str_remove(indicator_lb, level_lb),
         indicator_lb  = str_trim( indicator_lb),
         
         # rename level_lb
         level_lb = str_replace(level_lb, "m", "-month")
  ) 


###


options(shiny.useragg = TRUE)

thematic_on(bg = "#222229", fg = "gold", accent = "red")



ui <- page_sidebar(
  
  title = "JAMAICA's INDUSTRY VALUE ADDED & IMPLICIT DEFLATOR",
  
  theme= bs_theme(
    
    navbar_bg="#00a1af",
    preset = "vapor",
    version=5
  ),
  
  sidebar = sidebar(
    #title = "Industry Control Panel",
    title = tooltip(
      span(
        "Industry Control Panel",
        bs_icon("info-circle")
      ),
      "This Sections allows the user to control the output on the right,
      recommend to use the following steps to understand the output
      Step 1: Select Indicator
      Step 2: Select Period of Analysis, quarterly, 6-month, 9-month, 12-month
      Step 3: Select Indicator Change Period - 3-month (quarterly), 6-month, 9-month, 12-month
      Step 4: Select Start Date
      Step 5: End Date: Note the end date is what is shown on the value box on the right
      Step 6: Select industry/ies
      Step 7: Select end of quarter period, Mar, Jun, Sep, Dec
      Step 8: Multiple Graphs - switch from multiple graphs to single graph. 
           ",
      placement = "bottom"
    ),
    
    # create input widgets
    
    
    
    # Select Indicator
    selectInput(inputId = "indicator_lb",
                label = tooltip(
                  span(
                    "Select Indicator",
                    bs_icon("info-circle")
                  ),
                  "There are four indicators to track the performance of the real sector
               a) Value Added at Current Prices (Seasonally Unadjusted) - captures changes in volume and prices for each industry value added
               b) Value Added at Constant (2007) Prices (Seasonally Unadjusted) - captures changes in volume for each industry value added (Real Value)
               c) Value Added at Constant (2007) Prices (Seasonally Adjusted) - The data is seasonally adjusted to ensure that you can compare current period with previous period
                                                                                captures changes in volume
               d) Value Added Deflator (Seasonally Unadjusted) - Captures changes in prices for each industry
           ",
           placement = "bottom"
                )
           
           
           
           
           ,
           choices = unique(va_data_all_long$indicator_lb),
           selected = "Value Added at Constant (2007) Prices (Seasonally Unadjusted)" 
    ), 
    
    
    
    # Select Perid of of Analysis
    selectInput(inputId = "level_lb",
                label = tooltip(
                  span(
                    "Select Period of Analysis",
                    bs_icon("info-circle")
                  ),
                  "There are four period for which analysis can be done.
                   The default period is 3-month (quarterly), then aggregated to
                  6-months, 9 months and 12 months.
                       ",
                  placement = "bottom"
                ),
                choices = unique(va_data_all_long$level_lb),
                selected = "3-month" 
    ),
    # Select Growth Period
    selectInput(inputId = "growth_period_lb",
                label = tooltip(
                  span(
                    "Select Period of Change",
                    bs_icon("info-circle")
                  ),
                  "This represent the period for which percentage change will be calculated
                   They are four periods: The default period is 3-month (quarterly), then 
                  6-months, 9 months and 12 months.
                  Please Note. Because of Seasonality, it is recommended to use the 12-month growth rate, unless you are
                  looking on Vale Added at Constant (2007) Prices (Seasonally Adjusted)
                       ",
                  placement = "bottom"
                ),
                choices = unique(va_data_all_long$growth_period_lb),
                selected = "12-month growth rate"
    ), 
    
    # start date
    selectInput(inputId = "start_date",
                label = tooltip(
                  span(
                    "Select Start Date",
                    bs_icon("info-circle")
                  ),
                  "The Start Date represents the period for which graphs and tables will start showing results
                       ",
                  placement = "bottom"
                ),
                choices = unique(va_data_all_long$qyear),
                selected = head(unique(va_data_all_long$qyear),1)
    ),
    
    # end date
    selectInput(inputId = "end_date",
                label = tooltip(
                  span(
                    "Select End Date",
                    bs_icon("info-circle")
                  ),
                  "The End Date determines when the analysis will end.
                  It will also influence the period of anlaysis for the Value Box performance indicators,
                       ",
                  placement = "bottom"
                ),
                choices = unique(va_data_all_long$qyear),
                selected = tail(unique(va_data_all_long$qyear),1)
    ),
    
    
    # Select Industry/ies
    shinyWidgets::pickerInput(inputId = "industry",
                              label =  tooltip(
                                span(
                                  "Select Industry/ies",
                                  bs_icon("info-circle")
                                ),
                                "All industry in Natioal accounts are represented based on Jamaica
                                Industrial Classification 2005. Additionally, information is presented on
                                Goods Producing Industry (Agriculture, Mining & Quarrying, Manufacture and Construction).
                                Services Industry - Everying excluding Goods Producing Industry and FISIM
                                Taxes Less Subsides on Products and Gross Domestic Product at market Prices are only available for
                                Value Added at Current Prices (Seasonally Unadjusted)
                                         ",
                                placement = "bottom"
                              ),
                              choices = unique(va_data_all_long$industry),
                              multiple = TRUE,
                              
                              options = list(
                                style = "btn-danger"
                                
                              )),
    
    
    
    # Select Quarter
    pickerInput(inputId = "month_lb",
                label =  tooltip(
                  span(
                    "Select Period Ending",
                    bs_icon("info-circle")
                  ),
                  "They are 4 period ending March, June, September and December.
                   All are selected by default, however, you can select select any combination.
                   To look on Calendar Year data only, you can select Dec (December) and Select Period of Analysis: 12-months
                   To look on Fiscal Year data only, you can select Mar (March) and Selection Period of Analysis: 12-month
                       ",
                  placement = "bottom"
                )        ,
                choices = unique(va_data_all_long$month_lb),
                selected = levels(va_data_all_long$month_lb),
                
                multiple = TRUE,
                
    ),
    
    
    
    hr(), # Add a horizontal rule
    prettySwitch(inputId = "multiple", 
                 
                 label=  tooltip(
                   span(
                     "Select Multiple Graph",
                     bs_icon("info-circle")
                   ),
                   "Allows you to switch between single and multiple graphs
                  The dault is multiple graphs. Some graphics looks better with single while other with multiple
                  Decomposition plot should only be view in mulitple graph
                       ",
                  placement = "bottom"
                 ),     
                 TRUE),
  ),
  
  # value box
  layout_columns(
    
    # tags$h1("Growth {input$end_date}"),
    value_box(
      title =  tooltip(
        span(
          "Period Ending",
          bs_icon("info-circle")
        ),
        " This represent the period for which the indicators on the right represent.
          It is influence by End Date and Quarter Ending
                       ",
        placement = "bottom"
      ),
      value = textOutput("period"),
      id ="vb_period",
    ),
    
    
    
    value_box(
      title =  tooltip(
        span(
          "Overall Performance",
          bs_icon("info-circle")
        ),
        " This capture the primary industry for all indicators, which is Total VAlue Added at Basic
        Prices for every indicator except Value Added at Current Prices (Seasonally Unajusted), 
        which Uses Gross Domestic Product at Market Prices as its indicator (GDP).
        GDP is not available for the other indicators
             ",
        placement = "bottom"
      )
      
      ,
      value = textOutput("tva"),
      id = "vb_tva"                        
    ),
    
    value_box(
      title = tooltip(
        span(
          "Top Performer",
          bs_icon("info-circle")
        ),
        " This represent the industry with the highest growth rate based on:
          Indicator selected, Period of analysis, Period of Change and Period ending (Mar, Jun, Sep, Oct) 
         ",
        placement = "bottom"
      )   ,
      value = textOutput("tp"),
      id = "vb_tp",                       
    ),
    
    value_box(
      title = tooltip(
        span(
          "Top Performer",
          bs_icon("info-circle")
        ),
        " This represent the industry with the Worst growth rate based on:
          Indicator selected, Period of analysis, Period of Change and Period ending (Mar, Jun, Sep, Oct) 
         ",
        placement = "bottom"
      ) ,
      value = textOutput("wp"),
      id = "vb_wp"                       
    ),
    
    
  ),
  
  navset_card_underline(
    title = tooltip(
      span(
        "Performance Results",
        bs_icon("info-circle")
      ),
      " This represent the different results for the combined selections:
        Plot Levels: Graphically represent the Value of the selection in Levels 
        Plot Growth: Graphically represent the value of the Plot level based on growth rate selected
        Plot Levels Contribution: Graphically represents the Contribution of selection to Total Value Added at Basic Prices for all industry selected 
        Plot Growth Contribution: Graphically represents the contribution of selection to % Change in Total Value Added at Basic Prices for all industry selected
        Plot Levels Decomposition: Graphically represents the decomposition of each industry selected based on selection into Trend, Seasonality, Remainder
        Table Levels: Tabularly represent the Value of the selection in Levels 
        Table Growth: Tabularly represent the value of the Plot level based on growth rate selected
        Table Levels Contribution: Tabularly represents the Contribution of selection to Total Value Added at Basic Prices for all industry selected 
        Table Growth Contribution: Tabularly represents the contribution of selection to % Change in Total Value Added at Basic Prices for all industry selected
        Table Levels Decomposition: Tabularly represents the decomposition of each industry selected based on selection into Trend, Seasonality, Remainder
      
         ",
      placement = "bottom"
    )   ,
    
    # graphs
    nav_panel(title = "Plot Levels", plotlyOutput("plot_l")),
    nav_panel("Plot Growth", plotlyOutput("plot_g")),
    nav_panel("Plot Levels Contribution", plotlyOutput("plot_lc")),
    nav_panel("Plot Growth Contribution", plotlyOutput("plot_lg")),
    nav_panel("Plot Levels Decomposition", plotlyOutput("plot_ld")),
    
    # Tables
    nav_panel("Table Levels", DTOutput("table_l")),
    nav_panel("Table Growth", DTOutput("table_g")),
    nav_panel("Table Levels Contribution", DTOutput("table_lc")),
    nav_panel("Table Growth Contribution", DTOutput("table_lg")),
    nav_panel("Table Levels Decomposition", DTOutput("table_ld")),
  )
)

server_va <- function(input, output, session){
  
  data <- reactive({
    
    
    va_data_all_long %>% 
      filter(qyear>=yearquarter(input$start_date) & qyear<=yearquarter(input$end_date)) %>% 
      filter(indicator_lb==input$indicator_lb) %>% 
      filter(level_lb==input$level_lb) %>% 
      filter(growth_period_lb==input$growth_period_lb) %>%
      filter(month_lb %in% c(input$month_lb)) %>% 
      filter(industry %in% c(input$industry)) 
  })
  
  
  
  
  # create data for table
  table_data_l <- reactive({
    
    
    if (input$indicator_lb== "Value Added Deflator (Seasonally Unadjusted)") {
      data() %>% 
        select( `Period Ending`=qyear, industry, value) %>%
        mutate(`Period Ending` = as_date(`Period Ending`),
               
               `Period Ending` =  rollforward(`Period Ending`) %m+% months(2),
               `Period Ending` = as.yearmon(`Period Ending`,format="%b-%y"),
               `Period Ending` = as.character(`Period Ending`)
        )%>% 
        pivot_wider(names_from = `Period Ending`, values_from = c(value))
      
    } else{
      
      data() %>% 
        
        select( `Period Ending`=qyear, industry, value) %>%
        
        mutate(`Period Ending` = as_date(`Period Ending`),
               
               `Period Ending` =  rollforward(`Period Ending`) %m+% months(2),
               `Period Ending` = as.yearmon(`Period Ending`,format="%b-%y"),
               `Period Ending` = as.character(`Period Ending`)
        )%>% 
        pivot_wider(names_from = `Period Ending`, values_from = c(value))
    }
    
    
    
    
  })
  
  table_data_g <- reactive({
    data() %>% 
      select( `Period Ending`=qyear, industry, growth_rate) %>% 
      mutate(`Period Ending` = as_date(`Period Ending`),
             
             `Period Ending` =  rollforward(`Period Ending`) %m+% months(2),
             `Period Ending` = as.yearmon(`Period Ending`,format="%b-%y"),
             `Period Ending` = as.character(`Period Ending`)
      )%>% 
      pivot_wider(names_from = `Period Ending`, values_from = c(growth_rate))
    
    
  }) 
  
  table_data_lc <- reactive({
    data() %>% 
      select( `Period Ending`=qyear, industry, contribution_tva_value) %>% 
      mutate(`Period Ending` = as_date(`Period Ending`),
             
             `Period Ending` =  rollforward(`Period Ending`) %m+% months(2),
             `Period Ending` = as.yearmon(`Period Ending`,format="%b-%y"),
             `Period Ending` = as.character(`Period Ending`)
      )%>% 
      pivot_wider(names_from = `Period Ending`, values_from = c(contribution_tva_value))
  }) 
  
  table_data_lg <- reactive({
    data() %>% 
      select( `Period Ending`=qyear, industry, contribution_growth) %>% 
      
      mutate(`Period Ending` = as_date(`Period Ending`),
             
             `Period Ending` =  rollforward(`Period Ending`) %m+% months(2),
             `Period Ending` = as.yearmon(`Period Ending`,format="%b-%y"),
             `Period Ending` = as.character(`Period Ending`)
      )%>% 
      pivot_wider(names_from = `Period Ending`, values_from = c(contribution_growth))
    
    
  }) 
  
  table_data_ld <- reactive({
    data() %>% 
      select(`Period Ending`=qyear, industry, value) %>%
      as_tsibble(index = `Period Ending`, key=industry) %>% 
      drop_na() %>% 
      model(STL(value ~ season())) %>% 
      components()
  }) 
  
  
  ### Value Book
  
  data_vb <- reactive({
    
    va_data_all_long %>% 
      filter(qyear>=yearquarter(input$start_date) & qyear<=yearquarter(input$end_date)) %>% 
      filter(month_lb %in% c(input$month_lb)) %>% 
      slice_max(date,n=1) %>% 
      filter(indicator_lb==input$indicator_lb) %>% 
      filter(level_lb==input$level_lb) %>% 
      filter(growth_period_lb==input$growth_period_lb) %>%
      filter(!industry %in% c( "Food Beverages & Tobacco" , "Other Manufacturing" , 
                               "Services", "Goods Producing", "Less Financial Intermediation Services Indirectly Measured (FISIM)",
                               "Taxes Less Subsidies on Products" 
                               
      )) 
  })
  
  
  
  check <- reactive({      validate(
    need(input$industry !="", "Please select Industry/ies"),
    
  )
  })
  
  # Level graph
  output$plot_l <- plotly::renderPlotly({
    
    check()
    
    data() %>% 
      mutate(
        `Period Ending`=qyear
      ) %>% 
      ggplot(aes(x= `Period Ending`, y=value, colour=industry,
                 
      ))+
      geom_line()+
      list(
        if (input$indicator_lb== "Value Added Deflator (Seasonally Unadjusted)")
          scale_y_continuous(labels = label_number(accuracy = 0.1)),
        
        if (input$indicator_lb!= "Value Added Deflator (Seasonally Unadjusted)")
          scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))
        
      ) + 
      
      labs(
        title = glue::glue(" {input$level_lb} {input$indicator_lb} for Selected Industries for the Period {input$start_date} to {input$end_date}"),
        y = input$indicator,
        caption = "STATIN"
      )+
      # theme_bw()+
      list(
        if (input$multiple)
          facet_wrap(vars(industry),
                     
                     scales = "free_y",
                     labeller = label_wrap_gen(50)
                     
          )
      )
    
    
  })
  
  # growth graph
  
  output$plot_g <- plotly::renderPlotly({
    
    check()
    
    data() %>% 
      mutate(growth_rate = formattable::percent(growth_rate, digits =1),
             `Period Ending`=qyear
      ) %>% 
      ggplot(aes(x=`Period Ending`, y=growth_rate, colour=industry ))+
      geom_line()+
      scale_y_continuous(labels = scales::percent)+
      
      labs(
        title = glue::glue(" {input$growth_period_lb} for {input$level_lb} {input$indicator_lb}  for Selected Industries for the Period {input$start_date} to {input$end_date}"),
        y = input$indicator,
        caption = "STATIN"
      )+
      # theme_bw()+
      list(
        if (input$multiple)
          facet_wrap(vars(industry),
                     
                     scales = "free_y",
                     labeller = label_wrap_gen(50)
                     
          )
      )
    
  })
  
  
  # Contribution to Levels
  
  output$plot_lc <- plotly::renderPlotly({
    
    check()
    
    data() %>% 
      mutate(contribution_tva_value = formattable::percent(contribution_tva_value, digits =1),
             `Period Ending`=qyear
             
      ) %>% 
      ggplot(aes(x=`Period Ending`, y=contribution_tva_value,  fill=industry  ),
             colour="white",alpha=0.05
      )+
      geom_col(position = "stack")+
      scale_y_continuous(labels = scales::percent)+
      
      labs(
        title = glue::glue("Contribution to Total Value Added {input$level_lb} {input$indicator_lb} for Selected Industries for the Period {input$start_date} to {input$end_date}"),
        y = input$indicator,
        caption = "STATIN"
      )+
      # theme_bw()+
      list(
        if (input$multiple)
          facet_wrap(vars(industry),
                     
                     scales = "free_y",
                     labeller = label_wrap_gen(50)
                     
          )
      )
    
  })
  
  
  # Contribution to growth rate
  
  output$plot_lg <- plotly::renderPlotly({
    
    check()
    
    data() %>% 
      mutate(contribution_growth = formattable::percent(contribution_growth, digits =1),
             `Period Ending`=qyear
      ) %>% 
      ggplot(aes(x=`Period Ending`, y=contribution_growth, fill=industry),
             colour="white", alpha=0.05
      )+
      geom_col(position = "stack"   )+
      scale_y_continuous(labels = scales::percent)+
      labs(
        title = glue::glue("Contribution to {input$growth_period_lb} for Total Value Added {input$level_lb}  {input$indicator_lb} for Selected Industries for the Period {input$start_date} to {input$end_date}"),
        y = input$indicator,
        caption = "STATIN"
      )+
      # theme_bw()+
      list(
        if (input$multiple)
          facet_wrap(vars(industry),
                     
                     scales = "free_y",
                     labeller = label_wrap_gen(50)
                     
          )
      )
    
  })
  
  
  # Decomposition
  
  output$plot_ld <- plotly::renderPlotly({
    
    check()
    
    table_data_ld() %>% 
      pivot_longer(4:7) %>% 
      
      ggplot(aes(x=`Period Ending`, y=value, colour=industry))+
      geom_line()+
      
      list(
        if (input$indicator_lb== "Value Added Deflator (Seasonally Unadjusted)")
          scale_y_continuous(labels = label_number(accuracy = 0.1)),
        
        if (input$indicator_lb!= "Value Added Deflator (Seasonally Unadjusted)")
          scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))
        
        
      )+ 
      labs(
        title = glue::glue(" Decomposition of {input$level_lb} {input$indicator_lb} for Selected Industries for the Period {input$start_date} to {input$end_date}"),
        y = input$indicator,
        caption = "STATIN"
      )+
      
      list(
        if (input$multiple)
          facet_wrap(vars(name),
                     
                     scales = "free_y",
                     labeller = label_wrap_gen(50)
                     
          )
      )
    
  })
  
  
  
  
  
  
  
  
  
  # Tables Levels
  
  output$table_l <- DT::renderDataTable({
    check()
    
    
    if (input$indicator_lb== "Value Added Deflator (Seasonally Unadjusted)") {
      
      datatable( 
        
        table_data_l(),
        
        filter = 'top', 
        
        
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center;',
          htmltools::em(glue::glue('Decomposition of {input$level_lb} {input$indicator_lb} for Selected Industries
                                     for the period {input$start_date} to {input$end_date} ')),
          
        ),
        
        
        extensions = 'Buttons',
        options = list(
          pageLength = 15, autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
        
        
      ) %>% 
        formatRound(2:ncol(table_data_l()), digits = 1) } else{
          datatable( 
            
            table_data_l(),
            
            
            filter = 'top', 
            
            
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: center;',
              htmltools::em(glue::glue('{input$level_lb} {input$indicator_lb} for Selected Industries
                                     for the period {input$start_date} to {input$end_date} ')),
              
            ),
            
            
            extensions = 'Buttons',
            options = list(
              pageLength = 15, autoWidth = TRUE,
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            )
            
            
          )  %>%
            
            formatCurrency(2:ncol(table_data_ld()), digits =1)  
          
        }
    
    
    
    
  })
  
  # Tables Growth
  
  
  output$table_g <- DT::renderDataTable({
    check()
    
    
    datatable( 
      table_data_g(),
      
      filter = 'top', 
      
      
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center;',
        htmltools::em(glue::glue(" {input$growth_period_lb} for {input$level_lb} {input$indicator_lb}  for Selected Industries for the Period {input$start_date} to {input$end_date}")),
      ),
      
      
      extensions = 'Buttons', 
      options = list(
        pageLength = 15, autoWidth = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
      
      
    )  %>% 
      formatPercentage(2:ncol(table_data_g()), digits =1) 
    
    
  })
  
  # Tables Contribution Levels
  
  
  output$table_lc <- DT::renderDataTable({
    check()
    
    
    datatable( 
      table_data_lc(), 
      
      
      filter = 'top', 
      
      
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center;',
        htmltools::em(glue::glue('Contribution to Total Value Added for {input$level_lb} {input$indicator_lb}  for Selected Industries for the period {input$start_date} to {input$end_date}
                                                                                                            ')),
      ),
      
      
      extensions = 'Buttons', 
      options = list(
        pageLength = 15, autoWidth = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
      
      
    )  %>% 
      formatPercentage(2:ncol(table_data_lc()), digits =1) 
    
    
  })
  
  # Contribution growth
  output$table_lg <- DT::renderDataTable({
    check()
    
    
    datatable( 
      table_data_lg()
      ,
      
      filter = 'top', 
      
      
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center;',
        htmltools::em(glue::glue('Contribution to {input$growth_period_lb} for Total Value Added for {input$level_lb} {input$indicator_lb} for Selected Industries for the period {input$start_date} to {input$end_date}
                                                                                                            ')),
      ),
      
      
      extensions = 'Buttons', 
      options = list(
        pageLength = 15, autoWidth = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
      
      
    )  %>% 
      formatPercentage(2:ncol(table_data_lg()), digits =1) 
    
    
  })
  
  output$table_ld <- DT::renderDataTable({
    check()
    
    if (input$indicator_lb== "Value Added Deflator (Seasonally Unadjusted)") {
      
      datatable(
        table_data_ld() %>% 
          as_tibble() %>% 
          mutate(`Period Ending` = as_date(`Period Ending`),
                 
                 `Period Ending` =  rollforward(`Period Ending`) %m+% months(2),
                 `Period Ending` = as.yearmon(`Period Ending`,format="%b-%y"),
                 `Period Ending` = as.character(`Period Ending`)
          )
        ,
        
        filter = 'top',
        
        
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center;',
          htmltools::em(glue::glue('Decomposition of {input$level_lb} {input$indicator_lb} for Selected Industries for the period {input$start_date} to {input$end_date}
                                                                                                            ')),
        ),
        
        
        extensions = 'Buttons',
        options = list(
          pageLength = 15, autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
        
        
      )  %>%
        
        formatRound(4:ncol(table_data_ld()), digits =1) } else{
          datatable(
            table_data_ld() %>% 
              as_tibble() %>% 
              mutate(`Period Ending` = as_date(`Period Ending`),
                     
                     `Period Ending` =  rollforward(`Period Ending`) %m+% months(2),
                     `Period Ending` = as.yearmon(`Period Ending`,format="%b-%y"),
                     `Period Ending` = as.character(`Period Ending`)
              )
            ,
            
            filter = 'top',
            
            
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: center;',
              htmltools::em(glue::glue('Decomposition of {input$level_lb} {input$indicator_lb} for Selected Industries
                                     for the period {input$start_date} to {input$end_date}')),
              
            ),
            
            
            extensions = 'Buttons',
            options = list(
              pageLength = 15, autoWidth = TRUE,
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            )
            
            
          )  %>%
            
            formatCurrency(4:ncol(table_data_ld()), digits =1)  
          
          
          
          
        }
    
    
  })
  
  # Value Book
  
  # Period
  
  output$period <- renderText({
    unique(  data_vb() %>% 
               mutate(#`Period Ending` = as_date(`Period Ending`),
                 
                 date =  rollforward(date) ,
                 date = as.yearmon(date,format="%b-%y"),
                 date = as.character(date)
               ) %>% 
               pull(date)
    )
    
    
  })
  
  
  # TVA
  output$tva <- renderText({
    
    
    if (input$indicator_lb=="Value Added at Current Prices (Seasonally Unadjusted)" ) {  
      
      
      data_vb() %>% 
        filter(industry== "Gross Domestic Product at Market Prices" ) %>% 
        mutate(text = glue::glue("{industry}:
                               {formattable::percent(growth_rate,digits=1)}")) %>% 
        pull(text) } else
        {
          data_vb() %>% 
            filter(industry=="Total Value Added at Basic Prices") %>% 
            mutate(text = glue::glue("{industry}:
                               {formattable::percent(growth_rate,digits=1)}")) %>% 
            pull(text)
          
          
        }
    
    
  })
  
  # Top Performing Industry
  output$tp <- renderText({
    
    data_vb() %>% 
      slice_max(growth_rate, n=1) %>% 
      mutate(text = glue::glue("{industry}:
                               {formattable::percent(growth_rate, digits=1)}")) %>% 
      pull(text)
    
    
    
    
    
  })
  
  #Worse Performing Industry
  output$wp <- renderText({
    
    data_vb() %>% 
      slice_min(growth_rate, n=1) %>% 
      mutate(text = glue::glue("{industry}:
                                {formattable::percent(growth_rate, digits=1)}")) %>% 
      pull(text)
    
    
    
  })
  
}

shinyApp(ui, server_va)

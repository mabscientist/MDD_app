
# setup -------------------------------------------------------------------

# load required packages:

library(shiny)
library(shinydashboard)

# load data from source script:

source('data/MDD_source_script.R')

# UI ----------------------------------------------------------------------


# Define UI:

ui <- fluidPage(
  
  dashboardPage(
    dashboardHeader(title = 'Mammal Diversity App'),
    dashboardSidebar(
      sidebarMenu(
        menuItem(text = 'Home', 
                 icon = icon('book'),
                 tabName = 'home'),
        
        menuItem(text = 'Richness Map by Order',
                 icon = icon('paw'),
                 tabName = 'order'),
        
        menuItem(text = 'Taxonomy Over Time Map',
                 icon = icon('clock'),
                 tabName = 'year'),
        
        menuItem(text = 'IUCN Status Map',
                 icon = icon('circle-exclamation'),
                 tabName = 'iucn'),
        
        menuItem(text = 'Explore the Data',
                 icon = icon('table'),
                 tabName = 'mdd'))),
    
    dashboardBody(
      tags$head(
        tags$link(
          rel = 'stylesheet',
          type = 'text/css',
          href = 'dashboard_styles.css')),
      
      tabItems(
        
        #Homepage:
        
        tabItem(tabName = 'home',
                h2('Home'),
                p('Welcome to the Mammal Diversity Database Shiny app. We will use this app to explore maps and data describing the spatiotemporal distribution of mammal taxonomy.'),
                p('Functions of this app:'),
                tags$ul(
                  tags$li('Investigate how the geographic distribution of species richness and endemism difers by taxonomic order.'),
                  tags$li('Witness how the distribution of mammals recognized by the taxonomy has changed over time.'),
                  tags$li('Examine how different metrics of conservation concern are distributed across the globe and how well they sync up with the taxonomy.'),
                  tags$li('Zoom in on the type localities for all mammals with latitude and longitude points in their species descriptions.'),
                  tags$li('Explore the data on your own for species richness, endemism, and threatened mammals per country/territory.')),
                p('Warning: patience required to load maps!')),
        
        # Static ggplot page + plot:
        
        tabItem(tabName = 'order',
                h2('Richness Map by Order'),
                radioButtons(
                  inputId = 'dataset',
                  label = 'View:',
                  choiceNames = c('All species', 'Endemics only'),
                  choiceValues = c('all', 'endemic')),
                selectInput(
                  inputId = 'order_select',
                  label = 'Choose order:',
                  choices = c('Show all',
                              MDD_lenient %>% 
                                pull(order) %>% 
                                unique() %>% 
                                str_to_title())),
                plotOutput(outputId = 'order_map'),
                p(''),
                p('See how the top 10 countries change with your parameter input:'),
                plotOutput(outputId = 'top_10_plot_1')),
        
        # Static tmap page + plot:
        
        tabItem(tabName = 'year',
                h2('Taxonomy Over Time Map'),
                sliderInput(
                  inputId = 'year_select',
                  label = 'Choose year(s) of species description:',
                  min = 1758,
                  max = 2022,
                  value = c(1758, 2022),
                  sep = "",
                  ticks = FALSE,
                  animate = FALSE,
                  dragRange = TRUE),
                plotOutput(outputId = 'year_map'),
                p(''),
                p('See how the top 10 countries change with your parameter input:'),
                plotOutput(outputId = 'top_10_plot_2')),
        
        # Interactive tmap page:
        
        tabItem(tabName = 'iucn',
                h2('IUCN Status Map and Type Locality Coordinates'),
                p('Toggle between different layers within the interactive map:'),
                tmapOutput(outputId = 'interactive_map'),
                p('Note: distinct biogeographic territories are not listed separately from the nation they are owned by in this interactive map. For a complete list of geographic entities tracked by the MDD (following IUCN), please go to the Explore the Data page')),
        
        tabItem(tabName = 'mdd',
                h2('Explore the Data'),
                dataTableOutput(outputId = 'summary_table'))
      )
    )
  )
)

server <- function(input, output) {
  
  
  # Server: Inputs ----------------------------------------------------------
  
  # filtering for static ggplot:
  
  ggfilter <-
    reactive({
      
      # this is annoying but I couldn't think of a more parsimonious way
      # (without defining another function)
      # first if statement: chooses tibble based on radio button input
      # nested if statements: filter order based on dropdown input
      
      if(input$dataset == 'endemic'){
        
        # default: show all automatically selected; doesn't filter
        
        if(input$order_select == 'Show all'){
          endemics
        }
        else{
          endemics %>% 
            
            # filter order based on input; match case
            
            filter(order == 
                     str_to_upper(input$order_select))
        }
      }
      else{
        
        # default: show all automatically selected; doesn't filter
        
        if(input$order_select == 'Show all'){
          MDD_lenient
        }
        else{
          MDD_lenient %>% 
            
            # filter order based on input; match case
            
            filter(order == 
                     str_to_upper(input$order_select))}}})
  
  # filtering for static tmap:
  
  authyear_filter <-
    reactive({
      
      # no dataset choice this time
      
      MDD_lenient %>% 
        
        # filter authyear based on 2 input years
        
        filter(
          between(authyear,
                  input$year_select[1],
                  input$year_select[2]))})
  
# The following are filters for the interactive tmap:
  
  # IUCN threatened filter
  
  threatened_filter <-
    reactive({
      MDD_lenient %>%
        
        # filter IUCN status to critically endangerd, endangered, vulnerable
        
        filter(iucn %in% c('CR', 'EN', 'VU'))})
  
  # IUCN extinct filter
  
  extinct_filter <-
    reactive({
      MDD_lenient %>%
        
        # filter IUCN status to extinct, extinct in the wild
        
        filter(iucn %in% c('EX', 'EW'))})
  
  # IUCN DD filter
  
  dd_filter <-
    reactive({
      MDD_lenient %>%
        
        # filter IUCN status to data deficient
        
        filter(iucn == 'DD')})
  
  # IUCN NA filter
  
  na_filter <-
    reactive({
      MDD_lenient %>%
        
        # filter IUCN status to NA's (automatically DD in a way)
        
        filter(is.na(iucn))})
  
  # make summary table w/ totals, endemics, and % threatened:
  
  country_richness_summarized <-
    reactive({
      
      # calculate # of endemics by country:
      
      endemics %>% 
        group_by(country) %>% 
        summarize(endemic = n()) %>% 
        
        # join to existing total # species/country:
        
        left_join(totals,
                  .,
                  by = 'country') %>% 
        
        # make additional column calculating proportion of endemics/total
        
        mutate(percent_endemic = 
                 
                 # use round to not have annoying extra digits
                 
                 round(endemic/total*100,
                       digits = 1)) %>% 
        
        # add a new column using IUCN data for a % threatened column too
        
        left_join(
          threatened_filter() %>%
            
            # calculate # threatened/country
            
            group_by(country) %>%
            summarize(percent_threatened = n()),
          by = 'country') %>%
        
        # calculate % threatened/country 
        
        mutate(percent_threatened = 
                 
                 # restrict to 1 decimal place again
                 
                 round(percent_threatened/total*100,
                       digits = 1))})
  
  # Server: outputs ---------------------------------------------------------
  
  # render static ggplot
  
  output$order_map <-
    renderPlot(
      
      # call relevant filtered dataset
      
      ggfilter() %>% 
        
        # calculate # of species per country
        
        group_by(country) %>% 
        summarize(n = n()) %>%
        
        # join spatial data
        
        left_join(countries_sf,
                  .,
                  by = 'country') %>%
        
        #plot:
        
        ggplot() +
        geom_sf(
          aes(fill = n))  +
        scale_fill_viridis_c(
          na.value = '#440154')  +
        labs(fill = 'Species') +
        theme_bw())
  
  # make top 10 plot to accompany static ggplot from same data
  
  output$top_10_plot_1 <-
    renderPlot(
      ggfilter() %>% 
        make_top_10())
  
  # render static tmap
  
  output$year_map <-
    renderPlot(
      
      # call relevant filtered dataset
      
      authyear_filter() %>%
        
        # calculate #species/country
        
        group_by(country) %>%
        summarize(n = n()) %>%
        
        # join to country shapefile
        
        left_join(countries_sf,
                  .,
                  by = 'country') %>%
        
        # plot:
        
        tm_shape() +
        tm_polygons(col = 'n',
                    title = "Species",
                    style = 'fixed',
                    breaks =
                      c(0,
                        5,
                        20,
                        50,
                        100,
                        200,
                        300,
                        400,
                        500,
                        600,
                        700,
                        800),) +
        tm_layout(legend.outside = TRUE))
  
  # make top 10 plot to accompany static tmap from same data
  
  output$top_10_plot_2 <-
    renderPlot(
      authyear_filter() %>% 
        make_top_10())
  
  # render interactive map
  
  output$interactive_map <-
    renderTmap(
      
      #basemap
      
      tm_basemap(
        c('Esri.WorldTopoMap',
          'OpenStreetMap',
          'Esri.WorldImagery')) +
        
        # threatened (CR, EN, VU)
        
        threatened_filter() %>%
        
        # calculate # species/country
        
        group_by(country) %>%
        summarize(n = n()) %>%
        
        # inner join to total species summary (so there aren't blobs of gray)
      
        inner_join(
          totals,
          by = 'country') %>%
        
        # calculate proportion of threatened species not raw 
        
        mutate(proportion = n/total*100,
               .keep = 'unused') %>%
        
        # join to shapefile
        
        left_join(countries_tmap,
                  .,
                  by = 'country') %>%
        
        # plot:
        
        tm_shape(name = 'IUCN Threatened') +
        tm_polygons(col = 'proportion',
                    title = '% Threatened',
                    palette = 'Reds',
                    colorNA = NULL,
                    alpha = 0.75,
                    breaks =
                      c(0,
                        5,
                        10,
                        20,
                        30,
                        40,
                        50,
                        60)) +
        
        # extinct/extinct in wild (EX, EW)
        # intermediate steps are exact same so see above for thorough comments
        
        extinct_filter() %>%
        group_by(country) %>%
        summarize(n = n()) %>%
        inner_join(
          totals,
          by = 'country') %>%
        mutate(proportion = n/total*100,
               .keep = 'unused') %>%
        left_join(countries_tmap,
                  .,
                  by = 'country') %>%
        tm_shape(name = 'IUCN Extinct/Extinct in Wild') +
        tm_polygons(col = 'proportion',
                    title = "% Extinct",
                    palette = 'Reds',
                    colorNA = NULL,
                    alpha = 0.75,
                    breaks =
                      c(0.1,
                        0.5,
                        1,
                        1.5)) +
        
        # data deficient (DD)
        # intermediate steps are exact same so see above for thorough comments
        
        dd_filter() %>%
        group_by(country) %>%
        summarize(n = n()) %>%
        inner_join(
          totals,
          by = 'country') %>%
        mutate(proportion = n/total*100,
               .keep = 'unused') %>%
        left_join(countries_tmap,
                  .,
                  by = 'country') %>%
        tm_shape(name = 'IUCN Data Deficient') +
        tm_polygons(col = 'proportion',
                    title = "% Data Deficient",
                    palette = 'Reds',
                    colorNA = NULL,
                    alpha = 0.75,
                    breaks =
                      c(0,
                        5,
                        10,
                        15,
                        20)) +
        
        # NA (taxonomy makes inherently DD)
        # intermediate steps are exact same so see above for thorough comments
        
        na_filter() %>%
        group_by(country) %>%
        summarize(n = n()) %>%
        inner_join(
          totals,
          by = 'country') %>%
        mutate(proportion = n/total*100,
               .keep = 'unused') %>%
        left_join(countries_tmap,
                  .,
                  by = 'country') %>%
        tm_shape(name = 'Not IUCN-Listed') +
        tm_polygons(col = 'proportion',
                    colorNA = NULL,
                    title = '% Not in IUCN',
                    palette = 'Reds',
                    alpha = 0.75,
                    breaks =
                      c(0,
                        5,
                        10,
                        15,
                        20)) +
        
        # type localities
        
        tm_shape(points,
                 name = 'Type Localities') +
        tm_dots(clustering = TRUE))
  
  
  # Render summary table:
  
  output$summary_table <-
    renderDataTable(
      country_richness_summarized())
  
}

# Knit & run app ----------------------------------------------------------


# Run the application 
shinyApp(ui = ui, server = server)

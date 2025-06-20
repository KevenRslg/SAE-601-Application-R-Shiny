  #
  # This is a Shiny web application. You can run the application by clicking
  # the 'Run App' button above.
  #
  # Find out more about building applications with Shiny here:
  #
  #    http://shiny.rstudio.com/
  #


  library(xml2)
  library(sf)
  library(plotly)
  library(dplyr)
  library(shinydashboard)
  library(shiny)
  library(rhandsontable)
  library(ggplot2)
  library(tidyr)
  library(stringr)
  library(leaflet)
  library(httr)
  library(shinydashboardPlus)
  library(bslib)
  #setwd("H:/SAE R Shiny Big Data/App")
  data_communes <- read.csv2("data_communes.csv", skip = 2)
  data_offices_tourisme <- read.csv2("data_offices_tourisme_2.csv", encoding = "latin1")
  
  
  data_offices_tourisme <- data_offices_tourisme %>%
    mutate(
      Latitude = as.numeric(Latitude),
      Longitude = as.numeric(Longitude)
    ) %>%
    # Filtrage : France métropolitaine uniquement
    filter(
      !is.na(Latitude), !is.na(Longitude),
      Latitude >= 41, Latitude <= 51.5,
      Longitude >= -5, Longitude <= 10
    ) %>%
    # Arrondi pour simplification visuelle
    mutate(
      Latitude = round(Latitude, 6),
      Longitude = round(Longitude, 6)
    )
  
  
  client_id <- "VOTRE_CLIENT_ID"
  client_secret <- "VOTRE_CLIENT_SECRET"
  
  # Requête de token
  res <- POST("https://data.datatourisme.fr/oauth/token",
              body = list(
                grant_type = "client_credentials",
                client_id = client_id,
                client_secret = client_secret
              ),
              encode = "form")
  
  token <- content(res)$access_token
  
  names(data_communes) <- c("Code_dpt","Dpt","Nb_nuitees_camping_2024","Nb_chambres_hotels_2025", "Nb_hotels_2025",
                            "Part_hotel_haut_de_gamme_2025","Part_hotel_moyenne_gamme_2025", "Part_hotel_entree_gamme_2025",
                            "Nb_nuitees_hôtels_tourisme", "Nombre_emplacements_campings_2025", "Part_emplacement_camping_entree_gamme_2025",
                            "Part_emplacement_camping_haut_gamme_2025", "Part_emplacement_camping_moyenne_gamme_2025", "Part_logements_secondaires", "part_logement_vacants")
  
  data_communes_Bretagne <- data_communes %>%
    filter(Code_dpt %in% c(22, 29, 35, 56))
  
  
  
  
  # Transformation variables numériques avec as.numeric
  
  data_offices_tourisme$Latitude <- as.numeric(data_offices_tourisme$Latitude)
  
  data_offices_tourisme$Longitude <- as.numeric(data_offices_tourisme$Longitude)
    
  data_communes$Nb_nuitees_camping_2024 <- as.numeric(data_communes$Nb_nuitees_camping_2024)
  
  data_communes$Nb_chambres_hotels_2025 <- as.numeric(data_communes$Nb_chambres_hotels_2025)
  
  data_communes$Nb_hotels_2025<- as.numeric(data_communes$Nb_hotels_2025)
  
  data_communes$Part_hotel_haut_de_gamme_2025<- as.numeric(data_communes$Part_hotel_haut_de_gamme_2025)
  
  data_communes$Part_hotel_moyenne_gamme_2025 <- as.numeric(data_communes$Part_hotel_moyenne_gamme_2025)
  
  data_communes$Part_hotel_entree_gamme_2025 <- as.numeric(data_communes$Part_hotel_entree_gamme_2025)
  
  data_communes$Part_hotel_entree_gamme_2025 <- as.numeric(data_communes$Part_hotel_entree_gamme_2025)
  
  data_communes$Nb_nuitees_hôtels_tourisme <- as.numeric(data_communes$Nb_nuitees_hôtels_tourisme)
  
  data_communes$Nombre_emplacements_campings_2025 <- as.numeric(data_communes$Nombre_emplacements_campings_2025)
  
  data_communes$Part_emplacement_camping_entree_gamme_2025 <- as.numeric(data_communes$Part_emplacement_camping_entree_gamme_2025)
  
  data_communes$Part_emplacement_camping_haut_gamme_2025 <- as.numeric(data_communes$Part_emplacement_camping_haut_gamme_2025)
  
  data_communes$Part_emplacement_camping_moyenne_gamme_2025 <- as.numeric(data_communes$Part_emplacement_camping_moyenne_gamme_2025)
  
  
  
  
  # Conversion code departement de 1 chiffre à 2 
  data_communes$Code_dpt <- str_pad(data_communes$Code_dpt, width = 2, pad = "0")
  
  departements_sf <- st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements-version-simplifiee.geojson")
  
  head(departements_sf)
  
  # Exemple de jeu de données (à adapter avec votre `data_communes`)
  # Assurez-vous que Code_dpt est bien du type character
  data_communes$Code_dpt <- as.character(data_communes$Code_dpt)
  
  # Jointure données + géométries
  departements_geo <- departements_sf %>%
    left_join(data_communes, by = c("code" = "Code_dpt"))
  
  
  # Vérification des colonnes disponibles
  head(departements_sf)
  
  
  
  
  # Define UI for application that draws a histogram
  ui <- dashboardPage(
    dashboardHeader(
      title = span(HTML("Analyse des dynamiques touristiques en France"), style = "font-size: 18px")
      ,
      titleWidth = 450 # ou ajustez la largeur selon votre besoin
    )
    
    ,
    dashboardSidebar( width = 450,
      sidebarMenu(
        menuItem("Accueil", tabName = "Accueil"),
        menuItem("Extrait de données", tabName = "extrait_donnees", icon = icon("info-circle"), badgeLabel = "🔍", badgeColor = "blue"),
        menuItem("Statistiques", tabName = "hotels", icon = icon("chart-pie"),badgeLabel = "📊", badgeColor = "red"),
        menuItem("Cartographies", tabName = "Cartes", icon = icon("map"),  badgeLabel = "🗺️", badgeColor = "green")
        
      )
    ),
    dashboardBody(
      theme = bs_theme(version = 5, bootswatch = "darkly", primary = "#1abc9c"),
      tabItems(
        tabItem(tabName = "hotels",
                tabsetPanel(
                  tabPanel("Répartition des types de logement touristique par département",
                           fluidRow(
                             box(width = 3, 
                                 selectInput("departement", "Choisissez un département :",
                                             choices = setNames(data_communes$Code_dpt, data_communes$Dpt),
                                             selected = 22)
                             ),
                             box(width = 9,
                                 fluidRow(
                                   box(title = "Répartition des types d'hôtels ", width = 12, solidHeader = TRUE, status = "primary",
                                       plotOutput("pieChart", height = "300px"))
                                 ),
                                 fluidRow(
                                   box(title = "Répartition des types d'emplacements de camping", width = 12, solidHeader = TRUE, status = "success",
                                       plotOutput("pieChartCamping", height = "300px"))
                                 )
                             )
                           )
                  ),
                  
                  ## NOUVEL ONGLET INTERCALÉ
                  tabPanel("TOP 5 des départements pour chaque variable",
                           fluidRow(
                             box(width = 3,
                                 selectInput("comparative_var", "Variable :",
                                             choices = names(data_communes)[sapply(data_communes, is.numeric)])
                             ),
                             box(width = 9,
                                 plotOutput("comparative_barplot"))
                           )
                  ),
                  
                  tabPanel("KPI",
                           fluidRow(
                             box(width = 3, 
                                 selectInput("kpi_departement", "Choisissez un département :",
                                             choices = setNames(data_communes$Code_dpt, data_communes$Dpt),
                                             selected = 1)
                             )
                           ),
                           uiOutput("kpi_boxes")
                  )
                )
        )
        ,
        tabItem(tabName = "extrait_donnees",
                accordion(
                  accordion_panel("Filtrage et affichage",
                                  layout_sidebar(
                                    sidebar = list(
                                      sliderInput("nb_lignes", "Nombre de lignes :", 1, nrow(data_communes), 4),
                                      selectInput("filtre_departement", "Département :", c("Tous", unique(data_communes$Dpt)))
                                    ),
                                    rhandsontable::rHandsontableOutput("table_extrait")
                                  )
                  )
                )
        ),
        tabItem(tabName = "Cartes",
                tabsetPanel(
                  tabPanel("Cartographie des hôtels et des emplacements de camping",
                           fluidRow(
                             box(width = 3,
                                 selectInput("colorBy", "Choisir une variable :", 
                                             choices = names(data_communes)[sapply(data_communes, is.numeric)])),
                             box(width = 9,
                                 leafletOutput("map"))
                           )
                  ), 
                  tabPanel("Emplacement des offices de tourisme",
                           fluidRow(
                             box(width = 12, 
                                 leafletOutput("map1", height = 600))
                           )
                  
                  )
                )
        ),
        tabItem(tabName = "Accueil",
                fluidRow(
                  box(
                    title = tagList(icon("bullseye"), "Objectif de l'analyse"), width = 6,
                    solidHeader = TRUE, status = "primary",
                    HTML("<div style='font-size: 15px;'>
                        <p>Cette application a pour but d'explorer les dynamiques touristiques en France, notamment en ce qui concerne l'hébergement hôtelier et l'offre de camping.</p>
                        <p>Les indicateurs clés, les répartitions et les cartes interactives permettent de donner un état des lieux des potentielles disparités territoriales en termes de tourisme.</p>
                      </div>")
                  ),
                  box(
                    title = tagList(icon("database"), "Données"), width = 6,
                    solidHeader = TRUE, status = "info",
                    HTML("<div style='font-size: 15px;'>
                        <p>Les données utilisées proviennent de sources publiques, incluant :</p>
                        <ul>
                          <li>Les offices de tourisme</li>
                          <li>Des statistiques sur les nuitées et les capacités d'accueil (campings & hôtels) en 2024 et 2025 </li>
                          <li>Des données géographiques sur les départements français</li>
                        </ul>
                      </div>")
                  ),                box(
                    title = tagList(icon("info-circle"), "Détails des données"), width = 6,
                    solidHeader = TRUE, status = "warning",
                    HTML("<div style='font-size: 15px;'>
                        <p>Les différentes variables du jeu de données sont les suivantes, elles concernent les départements :</p>
                        <ul>
                          
                          <li>Nombre d'hôtels par département en 2025 </li>
                          <li>Nombre de chambres dans les hôtels par département en 2025</li>
                          <li> Proportion d'hôtels haut de gamme, moyenne gamme et haut de gamme en 2025 </li>
                          <li>Nombre de nuitées dans les hôtels en 2025  </li>
                          <li> Nombre d'emplacements de camping en 2025  </li>
                          <li> Nombre de nuitées dans les campings en 2024  </li>
                          <li> Proportion d'emplacements de camping haut de gamme, moyenne gamme et haut de gamme en 2025 </li>
                          <li> Part de logements secondaires en 2021 </li>
                          <li> Part de logements vacants en 2021 </li>
                        </ul>
                      </div>")
                  ),
                  box(
                    title = tagList(icon("book"), "Références"), width = 6,
                    solidHeader = TRUE, status = "success",
                    HTML("<div style='font-size: 15px;'>
                        <ul>
                          <li><a href='https://www.data.gouv.fr/fr/datasets/'>data.gouv.fr</a> - Portail de données publiques françaises</li>
                          <li><a href='https://www.datatourisme.fr'>datatourisme.fr</a> - Référentiel des données touristiques</li>
                          <li>Documentation INSEE sur l’hébergement touristique</li>
                        </ul>
                      </div>")
                  )
                )
        )
        )
      )
    )
  
  
  
  
  
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
  
    
    # Données au format long pour ggplot
    df_long <- data_communes %>%
      pivot_longer(
        cols = c("Part_hotel_haut_de_gamme_2025", 
                 "Part_hotel_moyenne_gamme_2025", 
                 "Part_hotel_entree_gamme_2025"),
        names_to = "categorie",
        values_to = "part"
      ) %>%
      mutate(
        categorie = case_when(
          categorie == "Part_hotel_haut_de_gamme_2025" ~ "Haut de gamme",
          categorie == "Part_hotel_moyenne_gamme_2025" ~ "Moyenne gamme",
          categorie == "Part_hotel_entree_gamme_2025" ~ "Entrée de gamme",
          TRUE ~ categorie
        )
      )
    
    df_long_camping <- data_communes %>%
      pivot_longer(
        cols = c("Part_emplacement_camping_entree_gamme_2025",
                 "Part_emplacement_camping_haut_gamme_2025",
                 "Part_emplacement_camping_moyenne_gamme_2025"),
        names_to = "categorie",
        values_to = "part"
      ) %>%
      mutate(
        categorie = case_when(
          categorie == "Part_emplacement_camping_haut_gamme_2025" ~ "Haut de gamme",
          categorie == "Part_emplacement_camping_moyenne_gamme_2025" ~ "Moyenne gamme",
          categorie == "Part_emplacement_camping_entree_gamme_2025" ~ "Entrée de gamme",
          TRUE ~ categorie
        )
      )
    
    output$pieChart <- renderPlot({
      data <- df_long %>% filter(Code_dpt == input$departement)
      
      ggplot(data, aes(x = "", y = part, fill = categorie)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y") +
        geom_text(aes(label = paste0(round(part, 1), "%")),
                  position = position_stack(vjust = 0.5), size = 5, color = "black") +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 10)
        ) +
        labs(
          title = paste0("Répartition des types d'hôtels : ", unique(data$Dpt)),
          fill = "Catégorie d'hôtel"
        )
    })
    
    output$pieChartCamping <- renderPlot({
      data <- df_long_camping %>% filter(Code_dpt == input$departement)
      
      ggplot(data, aes(x = "", y = part, fill = categorie)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y") +
        geom_text(aes(label = paste0(round(part, 1), "%")),
                  position = position_stack(vjust = 0.5), size = 5, color = "black") +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 10)
        ) +
        labs(
          title = paste0("Répartition des types d'emplacements de camping : ", unique(data$Dpt)),
          fill = "Catégorie de camping"
        )
    })
    
    output$table_extrait <- rhandsontable::renderRHandsontable({
      data_filtrée <- if (input$filtre_departement == "Tous") {
        data_communes
      } else {
        data_communes %>% filter(Dpt == input$filtre_departement)
      }
      rhandsontable::rhandsontable(head(data_filtrée, input$nb_lignes))
    })
    
output$kpi_boxes <- renderUI({
  dept_selection <- data_communes %>% 
    filter(Code_dpt == input$kpi_departement)
  
  if (nrow(dept_selection) == 0) return(NULL)
  
  fluidRow(
    valueBox(
      value = dept_selection$Nb_nuitees_hôtels_tourisme,
      subtitle = "Nombre de nuitées hôtelières (en milliers) 🛏️",
      icon = icon("bed"),
      color = "maroon",
      width = 6
    ),
    valueBox(
      value = dept_selection$Nb_hotels_2025,
      subtitle = "Nombre d'hôtels 🏨",
      icon = icon("hotel"),
      color = "yellow",
      width = 6
    ),
    
    valueBox(
      value = dept_selection$Nb_chambres_hotels_2025,
      subtitle = "Nombre de chambres d'hôtels 🛎️",
      icon = icon("concierge-bell"),
      color = "orange",
      width = 6
    ),
    valueBox(
      value = dept_selection$Nombre_emplacements_campings_2025,
      subtitle = "Nombre d'emplacements de camping 🌲",
      icon = icon("tree"),
      color = "blue",
      width = 6
    ),
    
    valueBox(
      value = dept_selection$Nb_nuitees_camping_2024,
      subtitle = "Nombre de nuitées en camping (en milliers) ⛺",
      icon = icon("campground"),
      color = "green",
      width = 6
    )
  )
})

output$comparative_barplot <- renderPlot({
  req(input$comparative_var)
  var <- input$comparative_var
  
  # Calcul des moyennes par département
  top5 <- data_communes %>%
    group_by(Dpt) %>%
    summarise(valeur = mean(.data[[var]], na.rm = TRUE)) %>%
    arrange(desc(valeur)) %>%
    slice_head(n = 5)
  
  ggplot(top5, aes(x = reorder(Dpt, valeur), y = valeur, fill = Dpt)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = paste("Top 5 départements pour :", var),
      x = "Département",
      y = var
    ) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
})
    output$map <- renderLeaflet({
      var <- input$colorBy
      
  
      
      
      # Palette
      pal <- colorNumeric("YlOrRd", domain = departements_geo[[var]], na.color = "#D3D3D3")
      
      leaflet(departements_geo) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(get(var)),
          color = "white", weight = 1, fillOpacity = 0.8,
          label = ~paste0(
            "<strong>", nom, "</strong><br/>",
            var, " : ", round(get(var), 1), "<br/>",
            "Part logements secondaires : ", Part_logements_secondaires, " %", "<br/>",
            "Part logements vacants : ", part_logement_vacants, " %"
            
          ) %>% lapply(htmltools::HTML),
          highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
        ) %>%
        addLegend(pal = pal, values = departements_geo[[var]], title = var, position = "bottomright", )
    })
    
    output$map1 <- renderLeaflet({
      leaflet(data_offices_tourisme) %>%
        addTiles() %>%
        addMarkers(~Longitude, ~Latitude, popup = ~Ville)
    })
    
  }
  # Run the application 
  shinyApp(ui = ui, server = server)

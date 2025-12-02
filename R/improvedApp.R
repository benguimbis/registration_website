library(shiny)
library(bslib)
library(fontawesome)
library(shinyjs)
library(shinycssloaders)
library(here)
library(sf)
library(methods)
# library(mapview)
library(googlesheets4)
library(Microsoft365R)
library(pins)
library(qs)
library(waiter)
library(pointblank)
library(glue)
library(shinyWidgets)
library(blastula)
library(crew)
library(tidyverse)
library(shinyvalidate)
library(bsicons)

# Add at the start of your app
options(shiny.maxRequestSize = 30*1024^2)  # Increase max upload size if needed
options(future.globals.maxSize = 500*1024^2)  # If using future package

# Pins authentication

#token <- qs::qread("data/token.qs")

#od <- get_personal_onedrive(token = token)

#board <- board_ms365(od, "istbulletin")

# Email Token

#tok <- qs::qread("data/email.qs")

#outl <- get_personal_outlook(token = tok)

# Email Validation

is_valid_email <- function(email) {
  # Basic regex pattern for email validation
  pattern <- "^[[:alnum:].-_]+@[[:alnum:].-]+$"
  grepl(pattern, email)
}



gs4_auth(cache = ".secrets", email = "benguimbis@gmail.com")


id <- gs4_create("registrations", sheets = c("enregistrements"))

data = data.frame(participant = character(0), email = character(0), telephone = character(0), 
                  province = character(0), provenance = character(0), organisation = character(0), jour = Date(0), timestamp = POSIXct(0))
write_sheet(data, id, "enregistrements")

sites <- c(
  "Kinshasa",
  "Tshikapa",
  "Mweka",
  "Bulape",
  "Kindu",
  "Kasongo",
  "Lubutu",
  "Lisala",
  "Bumba",
  "Kisangani",
  "Lokutu",
  "Lubumbashi",
  "Likasi",
  "Mbujiayi",
  "Gbadolite",
  "Buta",
  "Kananga",
  "Lwiza",
  "Matadi",
  "Mbanza Ngunngu",
  "Boma",
  "Kenge",
  "Boende",
  "Bokungu",
  "Mbandaka",
  "Lodja",
  "Tshumbe",
  "Lusambo",
  "Bandundu",
  "Kikwit",
  "Kolwezi",
  "Kalemie",
  "Kabalo",
  "Kamina",
  "Kabondo Dianda",
  "Kabinda",
  "Muene Ditu",
  "Isiro",
  "Watsa",
  "Bunia",
  "Aru",
  "Inongo",
  "Nioki",
  "Gemena",
  "Uvira",
  "Bukavu",
  "Goma",
  "Butembo"
)

provinces <- c(
  "Kinshasa",
  "Kasai",
  "Maniema",
  "Mongala",
  "Tshopo",
  "Haut Katanga",
  "Kasai Oriental",
  "Nord Ubangi",
  "Bas Uele",
  "Kasai Central",
  "Kongo Central",
  "Kwango",
  "Tshuapa",
  "Equateur",
  "Sankuru",
  "Kwilu",
  "Lualaba",
  "Tanganyika",
  "Haut Lomami",
  "Lomami",
  "Haut Uele",
  "Ituri",
  "Maindombe",
  "Sud Ubangi",
  "Sud Kivu",
  "Nord Kivu"
)
immunisation_partners_rdc <- c(
  "Ministère de la Santé Publique, Hygiène et Prévoyance Sociale",
  "Programme Élargi de Vaccination (PEV)",
  "Organisation mondiale de la Santé",
  "UNICEF",
  "Gavi, l’Alliance du Vaccin",
  "Fondation Bill & Melinda Gates",
  "PATH",
  "Clinton Health Access Initiative",
  "SANRU",
  "Breakthrough ACTION",
  "PMI (President’s Malaria Initiative)",
  "Banque mondiale"
)


# Setting views

th <- bs_theme(
  version = 5,
  bootswatch = "lux",
  
  # Color scheme
  bg = "#FFFFFF",
  fg = "#2C3E50",
  primary = "#2980B9", 
  secondary = "#7F8C8D",
  success = "#27AE60",
  
  # Typography
  heading_font = font_google("Poppins"),
  base_font = font_google("Inter"),
  
  # Components
  "card-border-radius" = "1rem",
  "card-border-color" = "rgba(0,0,0,0.08)",
  "card-box-shadow" = "0 10px 15px -3px rgba(0,0,0,0.1), 0 4px 6px -2px rgba(0,0,0,0.05)",
  
  # Inputs
  "input-border-radius" = "0.5rem",
  "input-focus-border-color" = "#2980B9",
  "btn-border-radius" = "0.5rem",
  "btn-font-weight" = "600"
)

# --- UI ---
ui <- page_fillable(
  useShinyjs(),
  theme = th,
  
  # CSS to center vertically and add a background nuance
  tags$style(HTML("
    body { background-color: #f8f9fa; }
    .center-container {
      display: flex;
      justify-content: center;
      align-items: center;
      min-height: 100vh;
      padding: 20px;
    }
    .form-card {
      width: 100%;
      max-width: 600px; /* Limits width on big screens */
    }
  ")),
  
  div(
    class = "center-container",
    
    div(
      class = "form-card",
      card(
        card_header(
          class = "bg-primary text-white text-center py-3",
          h3(class = "m-0", "Présence Journalière"),
          div(class = "small opacity-75", "Révue annuelle du PEV")
        ),
        
        card_body(
          gap = "1rem",
          
          # Section: Identification
          h5("Identification", class = "text-primary border-bottom pb-2 mb-3"),
          
          textInput(
            "nom", 
            tags$span("Nom complet", class = "fw-medium"), 
            placeholder = "Ex: Jean Mutombo",
            width = "100%"
          ),
          
          # Use layout_columns to put contact info side-by-side
          layout_columns(
            col_widths = c(6, 6),
            textInput(
              "contact1", 
              tags$span(bs_icon("envelope"), " Email"), 
              placeholder = "nom@exemple.com"
            ),
            textInput(
              "contact2", 
              tags$span(bs_icon("phone"), " Tél (Mpésa)"), 
              placeholder = "08X-XXX-XXX"
            )
          ),
          
          # Section: Localisation
          h5("Localisation & Structure", class = "text-primary border-bottom pb-2 mb-3 mt-2"),
          
          layout_columns(
            col_widths = c(6, 6),
            selectInput(
              "provenance", 
              tags$span("Lieu de provenance"), 
              choices = c("", sites), 
              selected = ""
            ),
            selectInput(
              "province", 
              tags$span("Province"), 
              choices = c("", provinces), 
              selected = ""
            )
          ),
          
          selectInput(
            "organisation", 
            tags$span("Organisation"), 
            choices = c("", immunisation_partners_rdc), 
            selected = "",
            width = "100%"
          ),
          
          br(),
          
          # Submit Button
          div(
            class = "d-grid gap-2", # Bootstrap class to make button full width
            input_task_button(
              "load", 
              label = "Enregistrer la présence",
              icon = icon("check"),
              class = "btn-primary btn-lg" 
            )
          )
        ),
        
        card_footer(
          class = "text-center text-muted small",
          paste("Mise à jour:", format(Sys.Date(), "%d %B %Y"))
        )
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # 1. Initialize Validation Rules
  iv <- InputValidator$new()
  
  # Add rules
  iv$add_rule("nom", sv_required(message = "Le nom est obligatoire"))
  iv$add_rule("contact1", sv_required(message = "L'email est requis"))
  iv$add_rule("contact1", sv_email(message = "Veuillez entrer une adresse email valide"))
  
  iv$add_rule("contact2", sv_required(message = "Le numéro est requis"))
  # Regex for general DRC format (10 digits) or starting with 0
  iv$add_rule("contact2", sv_regex("^0[0-9]{9}$", "Le numéro doit comporter 10 chiffres (ex: 081...)"))
  
  iv$add_rule("provenance", sv_required(message = "Choisissez une provenance"))
  iv$add_rule("province", sv_required(message = "Choisissez une province"))
  iv$add_rule("organisation", sv_required(message = "Choisissez une organisation"))
  
  # Enable validation
  iv$enable()
  
  observeEvent(input$load, {
    
    # Check if validation passes. If not, stop here.
    if (!iv$is_valid()) {
      showNotification("Veuillez corriger les erreurs dans le formulaire.", type = "error")
      return() 
    }
    
    # Prepare Data
    data <- data.frame(
      participant = input$nom, 
      email = input$contact1, 
      telephone = input$contact2, 
      province = input$province, 
      provenance = input$provenance, 
      organisation = input$organisation, 
      jour = Sys.Date(),
      timestamp = Sys.time()
    )
    
    tryCatch({
      
      # --- Google Sheet Logic ---
      sheet_append(id, data, "enregistrements")
         
      
      # Simulation delay for the UI to show 'processing'
      Sys.sleep(1) 
      
      # Success Feedback
      showNotification("Enregistrement effectué avec succès!", type = "message", duration = 5)
      
      # Reset Form
      reset("nom")
      reset("contact1")
      reset("contact2")
      updateSelectInput(session, "provenance", selected = "")
      updateSelectInput(session, "province", selected = "")
      updateSelectInput(session, "organisation", selected = "")
      
      # Reset validation state so red text disappears until they type again
      iv$disable()
      iv$enable()
      
    }, error = function(e) {
      showNotification(paste("Erreur de connexion:", e$message), type = "error")
    })
    
  })
}

shinyApp(ui, server)
























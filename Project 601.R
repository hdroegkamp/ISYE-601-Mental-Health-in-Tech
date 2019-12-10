#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggrepel) # offset labels
rm(list = ls()) #Clears all variables
OSMI = read.csv("OSMI Mental Health in Tech Survey Combined.csv")

OSMI <- OSMI %>% 
    rename(Mental_Disorder = Do.you.currently.have.a.mental.health.disorder.,
       Age = What.is.your.age.,
       Gender = What.is.your.gender.,
       State = What.US.state.or.territory.do.you.work.in.,
       Employees = How.many.employees.does.your.company.or.organization.have.,
       Health_Coverage = Does.your.employer.provide.mental.health.benefits.as.part.of.healthcare.coverage.,
       Communication = Has.your.employer.ever.formally.discussed.mental.health..for.example..as.part.of.a.wellness.campaign.or.other.official.communication..,
       Resources = Does.your.employer.offer.resources.to.learn.more.about.mental.health.disorders.and.options.for.seeking.help.,
       Comfort_support = Would.you.feel.comfortable.discussing.a.mental.health.issue.with.your.direct.supervisor.s..,
       Comfort_coworkers = Would.you.feel.comfortable.discussing.a.mental.health.issue.with.your.coworkers.,
       Mental_Physical = Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health.)

# Keep only Yes and No answers for having a mental health disorder, mental as seriously as physical
test_1 = data.frame(OSMI[, c(1, 7:15)])
test_2 = data.frame(OSMI[,c(2:6, 16)])

test = cbind(test_2, test_1)
rm(test_1, test_2)
long = gather(test, question, value, c(7:16))

long = long %>% ungroup() %>% filter(value == "Yes" | value == "No") %>% mutate(value = ifelse(value  == "Yes", 1, 0)) 


OSMI <- long
rm(test, long)

names(OSMI)[5]="size"
OSMI$size = as.factor(OSMI$size)
OSMI = OSMI %>% filter(!is.na(size))


company_prop <- OSMI %>% group_by(size, Year, question)%>%
    summarise(percent = mean(value, na.rm=TRUE))
company_prop$size <- factor(company_prop$size, levels(company_prop$size)[c(1,5,3,2,4,6)])

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Companies of any size have not improved mental health support to match increase in mental health disorders"),

   
    sidebarLayout(
        sidebarPanel(
            selectInput("question", "Types of question",
                        choices = unique(OSMI$question))
        ),
        mainPanel = (
            plotOutput("graph", width = "80%", height = "750px")

            )
    )
)

server <- function(input, output) {

    output$graph <- renderPlot({
        
        if(input$question == "Mental_Disorder" ) {
            title = "Do you currently have a mental health disorder ?"  }
        else if (input$question == "Health_Coverage") { title = "Does your employer provide mental health benefits as part of healthcare coverage ?"}
        else if (input$question == "Communication") {title = "Has your employer ever formally discussed mental health for example as part of a wellness campaign or other official communication ?"}
        else if (input$question == "Resources") {title = "Does your employer offer resources to learn more about mental health disorders and options for seeking help ?"}
        else if (input$question == "Comfort_support") {title = "Would you feel comfortable discussing a mental health issue with your direct supervisors ?"}
        else if (input$question == "Comfort_coworkers") {title = "Would you feel comfortable discussing a mental health issue with your coworkers ?"}
        else if (input$question == "Mental_Physical") {title = "Do you feel that your employer takes mental health as seriously as physical health ?"}
        
        # color gradient for company size
        color_scale <- c("#66B2FF", "#3399FF", "#0080FF", "#0066CC", "#004C99", "#003366")
      
        # Plot it
        ggplot((data = company_prop %>% filter(question == input$question)), aes(x = as.factor(Year), y = percent)) +
            geom_line(aes(group = size, color = as.factor(size)), size = 1.5) +
            geom_label_repel(data = company_prop %>% filter(Year == 2016 & question == input$question), aes(label = size, color = size), nudge_x = -0.15) +
            geom_label_repel(data = company_prop %>% filter(Year == 2018 & question == input$question), aes(label = size, color = size), nudge_x = 0.12) +
            theme(legend.position = "none") +
            scale_color_manual(values = color_scale) +
            xlab("Year") +
            ylab("Percent answered 'yes'") +
            scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
            theme_bw() +
            theme(
                 # Hide panel borders and remove grid lines
                 panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 # Change axis line
                 axis.line = element_line(colour = "black"),
                 legend.position = "none",
                 # Increase title and axes size
                 plot.title = element_text(size = 28),
                 axis.text = element_text(size = 18),
                 axis.title = element_text(size = 16)) +
            ggtitle(title)
         
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

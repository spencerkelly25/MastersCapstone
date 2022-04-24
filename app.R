library(shinydashboard)
library(shiny)
library(dplyr)
library(lubridate)
library(hms)
library(tidyr)
library(purrr)
library(rlang)
library(stringr)
library(DT)
library(r2d3)
library(webshot)
library(htmlwidgets)
library(memor)
library(shinyjs)
library(readxl)
library(shinythemes)
library(shinyjs)
library(devtools)
library(exploratory)                       
library(ggplot2)
library(ggiraph)
library(cachem)
library(shinyWidgets)
library(plotly)

jscode <- "shinyjs.closeWindow = function() { window.close(); }"


#create a list of all the excel files in that folder in order to read them in
file.list = list.files(all.files=TRUE, pattern='*.xlsx')
#read them all in
df.list = lapply(file.list, read_excel)
# stack them all together
main.df = bind_rows(df.list)
main.df <- main.df[!duplicated(main.df),]

# run the cleaning on the dataframe
data1 <- main.df %>% 
    mutate(`Work Date` = as_date(`Work Date`), `Punch In Time` = as_hms(`Punch In Time`), `Punch Out Time` = as_hms(`Punch Out Time`)) %>%
    mutate(is_work = ifelse(`Punch In Type`=="Non-Work", 0, 1), MCO = ifelse(`Punch Out Time` == 0, 1, 0), MCO = replace_na(MCO, 1)) %>% 
    mutate(supervisorApproval = ifelse(`Supervisor Approval`  == "Yes", TRUE, FALSE)) %>% unite(`Employee Name`, `Last Name`, `First Name`, sep = ", ", remove = FALSE, na.rm = FALSE) %>% 
    select(c(`Employee Name`, `First Name`, `Middle Name`, `Last Name`, `Employee Id`, `Company Code Name`, `Position Description`,
             `Location Name`, `Department Name`, `Work Date`, `Pay Type Description`, `Punch In Time`, `Punch Out Time`, `Punch In Type`,
             `Punch Out Type`, `Regular Duration (hours)`, `OT1 Duration (hours)`, `OT2 Duration (hours)`, `Unpaid Duration (hours)`,
             `Supervisor Approval`, `Employee Approval`, `Supervisor Approval By`, is_work, MCO, supervisorApproval, `Supervisor's Name`))

# subsetted data filtering out the job descriptions that we don't want!
data2 <- data1 %>% filter(`Position Description` %in% c("Accounting Supervisor", "Bodyshop Manager", "Bodyshop Technician", "Detailer", "Dispatcher", 
                                                        "Estimator", "Finance Director", "Finance Manager", "General Sales Manager", "HR Administrator", "Internet Sales Manager", 
                                                        "Painter", "Parts Advisor", "Parts Assistant Manager", "Parts Manager", "Roadside Driver", "Sales Associate Used", "Sales Associate New", 
                                                        "Sales Associate", "Security Engineer", "Service Advisor", "Service Advisor Manager", "Shop Foreman", "Shop Manager", "Technician", "Technician Recon"))

###### Overtime Dataframes        
# Create overtime dataframe and classify if overtime
weekly_ot = summarize_group(.data = data1 %>% filter(`Pay Type Description`== 'Lunch' | `Pay Type Description`== 'Work'), group_cols = c(`Week Of` = "Work Date",  `Employee Id` = "Employee Id", `Employee Name` = "Employee Name", 
                                                                                                                                         `Supervisor Approval By` = "Supervisor Approval By", `Supervisor's Name` = "Supervisor's Name", `Approved` = "Supervisor Approval"),
                            group_funs = c("rtoweek",  "aschar",  "none", "none", "none", "none"),`Hours` = round(sum(`Regular Duration (hours)`, na.rm = TRUE),2))
daily_ot =  summarize_group(.data = data1 %>% filter(`Pay Type Description`== 'Lunch' | `Pay Type Description`== 'Work'), group_cols = c(`Work Date` = "Work Date",  `Employee Id` = "Employee Id", `Employee Name` = "Employee Name", 
                                                                                                                                         `Supervisor Approval By` = "Supervisor Approval By", `Supervisor's Name` = "Supervisor's Name", `Approved` = "Supervisor Approval"),
                            group_funs = c("rtoday",  "aschar",  "none", "none", "none", "none"),`Hours` = round(sum(`Regular Duration (hours)`, na.rm = TRUE),2))

# is it overtime?
weekly_ot$overtime = ifelse(weekly_ot$Hours > 40, 1, 0)
daily_ot$overtime = ifelse(daily_ot$Hours > 8, 1, 0)

# but did it get approved and is it a violation?
weekly_ot$violation = ifelse((weekly_ot$overtime == 1 & weekly_ot$Approved == "Yes"), 1, 0)
daily_ot$violation = ifelse((daily_ot$overtime == 1 & daily_ot$Approved == "Yes"), 1, 0)

# groups weekly and daily ot by employee ID to see aggregates
w_ot_by_emp = weekly_ot %>% summarize_group(group_cols = c(`Employee Name` = "Employee Name", `Employee Id` = "Employee Id", `Supervisor Approval By` = "Supervisor Approval By", `Supervisor's Name` = "Supervisor's Name"), group_funs = c("aschar", "aschar", "aschar", "none"), Count = sum(violation, na.rm = TRUE))
d_ot_by_emp = daily_ot %>% summarize_group(group_cols = c(`Employee Name` = "Employee Name", `Employee Id` = "Employee Id", `Supervisor Approval By` = "Supervisor Approval By", `Supervisor's Name` = "Supervisor's Name"), group_funs = c("aschar", "none", "none", "none"), Count = sum(violation, na.rm = TRUE))


# lists out each violation, day, supervisor, employee name and ID
full_weekly_viol = weekly_ot %>% select(`Employee Name`, `Employee Id`, `Supervisor Approval By`, `Supervisor's Name`, `Week Of`, Hours, violation) %>% filter(violation == 1) %>% select(!violation)
full_daily_viol = daily_ot %>% select(`Employee Name`, `Employee Id`, `Supervisor Approval By`, `Supervisor's Name`, `Work Date`, Hours, violation) %>% filter(violation == 1) %>% select(!violation)

# groups overtime violations approved by supervisor, necessary for computing metric
w_ot_by_sup = weekly_ot %>%  summarize_group(group_cols = c(`Supervisor Approval By` = "Supervisor Approval By"),group_funs = c("aschar"), Total = n(), Count = sum(violation, na.rm = TRUE)) %>% 
    mutate(performance = (1-(Count/Total))*100) %>% arrange(desc(performance)) 
d_ot_by_sup = daily_ot %>%  summarize_group(group_cols = c(`Supervisor Approval By` = "Supervisor Approval By"),group_funs = c("aschar"), Total = n(), Count = sum(violation, na.rm = TRUE)) %>% 
    mutate(performance = (1-(Count/Total))*100) %>% arrange(desc(performance))


###### Lunch Dataframes
no_lunch = summarize_group(.data = data1 %>% filter(`Pay Type Description`== 'Lunch' | `Pay Type Description`== 'Work'), 
                           group_cols = c(`Work Date` = "Work Date", `Employee Name` = "Employee Name", `Employee Id` = "Employee Id", 
                                          `Supervisor Approval By` = "Supervisor Approval By", `Supervisor's Name` = "Supervisor's Name", 
                                          supervisorApproval = "supervisorApproval"),
                           group_funs = c("rtoday",  "aschar", "aschar", "aschar", "aschar", "aschar"),`Pay Type Description_unq` = n_distinct(`Pay Type Description`),
                           lunch_type = ifelse(`Pay Type Description` == "Lunch", 1, 0)) %>%
    summarize_group(group_cols = c(`Work Date` = "Work Date", `Employee Name` = "Employee Name", `Employee Id` = "Employee Id", 
                                   `Supervisor Approval By` = "Supervisor Approval By", supervisorApproval = "supervisorApproval", 
                                   `Supervisor's Name` = "Supervisor's Name"),
                    group_funs = c("rtoday", "aschar", "aschar", "aschar", "aschar", "aschar"), took_lunch = sum(lunch_type, na.rm = TRUE)) %>%
    arrange(`Employee Id`) %>%
    mutate(lunch_viol = ifelse((supervisorApproval == TRUE & took_lunch != 0), 1, 0))

# aggregates lunch violations by employee ID to see how many
lunch_viol_by_emp = no_lunch %>% summarize_group(group_cols = c(`Employee Name` = "Employee Name", `Employee Id` = "Employee Id", `Supervisor Approval By` = "Supervisor Approval By", `Supervisor's Name` = "Supervisor's Name"),group_funs = c("aschar", "aschar", "aschar", "aschar"),
                                                 Count = sum(lunch_viol, na.rm = TRUE))
# full list
full_lunch_viol = no_lunch %>% select(`Employee Name`, `Employee Id`, `Work Date`, `Supervisor Approval By`,`Supervisor's Name`, lunch_viol) %>% filter(lunch_viol == 1) %>% select(!lunch_viol)

# lunch violations by supervisor
lunch_by_sup = no_lunch %>% summarize_group(group_cols = c(`Supervisor Approval By` = "Supervisor Approval By"),group_funs = c("aschar"),
                                            Total = n(),Violations = sum(lunch_viol, na.rm = TRUE)) %>% 
    mutate(perf_metric = (1-(Violations/Total))*100) %>% arrange(desc(perf_metric))


#### MCO dataframes
# MCO violation df, not super helpful bc MCOs are never approved
MCOviolation = data1 %>% filter(`Pay Type Description`== 'Lunch' | `Pay Type Description`== 'Work') %>% select(`Work Date`, `Supervisor Approval By`, `Employee Name`, `Employee Id`, `Supervisor's Name`, supervisorApproval, MCO, is_work) %>% mutate('MCOviolation' = ifelse((supervisorApproval == TRUE & MCO == 1 & is_work == 1), 1, 0))

# groups MCO by employee, for this one, there are no MCO violations, so this is just for their records if they want to count the MCO
MCO_by_emp = MCOviolation %>% summarize_group(group_cols = c(`Employee Name` = "Employee Name",`Employee Id` = "Employee Id", `Supervisor Approval By` = "Supervisor Approval By", `Supervisor's Name` = "Supervisor's Name"),
                                              group_funs = c("aschar"),`MCOs` = sum(MCO, na.rm = TRUE))

# MCO violations by employee to compute metric
MCO_by_sup = MCOviolation %>% summarize_group(group_cols = c(`Supervisor Approval By` = "Supervisor Approval By"),group_funs = c("aschar"),
                                              Total = n(),Violations = sum(MCOviolation, na.rm = TRUE)) %>% 
    mutate(perf_metric = ((Total-Violations)/Total)*100)  %>% arrange(desc(perf_metric))

#detailed list
full_MCO = MCOviolation %>% select(`Work Date`, `Supervisor Approval By`, `Employee Name`, `Employee Id`, `Supervisor's Name`, MCO, MCOviolation) %>% 
    filter(MCO == 1) %>% select(!c(MCO, MCOviolation))


avg_metrics = lunch_by_sup %>% select('Supervisor Approval By',lunch_metric = perf_metric) %>% left_join((d_ot_by_sup %>% select(`Supervisor Approval By`, d_ot = performance)), by="Supervisor Approval By") %>% 
    left_join((w_ot_by_sup %>% select(`Supervisor Approval By`, w_ot = performance)), by="Supervisor Approval By") %>% left_join((MCO_by_sup %>% select(`Supervisor Approval By`, mco_metric = perf_metric)), by="Supervisor Approval By") %>% 
    mutate(avg = (lunch_metric+d_ot+w_ot+mco_metric)/4) %>% mutate(avg = round(avg,2)) %>% arrange(desc(avg))

top_5 = avg_metrics %>% drop_na() %>% select(`Supervisor Approval By`, avg) %>% top_n(5, avg) %>% rename("Average Metric" = avg)
bottom_5 =  avg_metrics %>% arrange(desc(avg)) %>% drop_na() %>% tail(5) %>% select(`Supervisor Approval By`, avg) %>% arrange(avg) %>% rename("Average Metric" = avg)


main_tab_data <- data1[,c(1,5,7,8,10,12,13,22,26)]
main_tab_data = main_tab_data %>% rename( Employee.Name = `Employee Name`,
                                          Employee.Id = `Employee Id`,
                                          Position.Description = `Position Description`,
                                          Location.Name = `Location Name`,
                                          Work.Date = `Work Date`,
                                          Punch.In.Time = `Punch In Time`,
                                          Punch.Out.Time = `Punch Out Time`,
                                          Supervisor.Approval.By = `Supervisor Approval By`,
                                          Supervisor.Name = `Supervisor's Name`
)



ui <- function(req) { tagList(
    dashboardPage(
        # Dashboard Page Setup ----------------------------------------------------
        skin = "black",
        #theme = shinytheme("simplex"),
        title = "Hennessy Auto Payroll Report",
        
        dashboardHeader(
            title = "Hennessy Auto Payroll Report"
            
        )
        ,
        
        # Dashboard Sidebar -------------------------------------------------------
        dashboardSidebar(
            sidebarMenu(
                menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
                menuItem("Performance Score", tabName = "tab_performance_score", icon = icon("trophy")),
                menuItem("Violation Detail", tabName = "tab_violation_detail", icon = icon("stream")),
                menuItem("Employee Detail", tabName = "tab_employee_detail", icon = icon("user-circle")),
                menuItem("Explore", tabName = "tab_explore", icon = icon("compass")),
                menuItem("About", tabName = "tab_about", icon = icon("info"))
            ),
            actionButton('close', "Close App", class="butt" ),br(),
            tags$head(tags$style(".butt{background-color:#cd0000;} .butt{color: #1D1B1C;}"))
        ),
        
        # Dashboard Body ----------------------------------------------------------
        dashboardBody(
            tabItems(
                
                # Frontpage - tab_dashboard -----------------------------------------------
                tabItem(
                    "tab_dashboard",
                    tags$head(
                        # Lets put in a nice name and maybe a graphic or two to represeent the school and the client
                    ),
                    fluidRow(
                        uiOutput("supervisor_list"),
                        # Frontpage - boxes - start -----------------------------------------------
                        valueBox(
                            value = prettyNum(sum(no_lunch$lunch_viol == 1), big.mark=","),
                            subtitle = "Lunch Violations",
                            color = "orange",
                            icon = icon("utensils"),
                            width = 3),
                        valueBox(
                            value = prettyNum(sum(daily_ot$violation == 1), big.mark = ","),
                            subtitle = "Daily Overtime Violations",
                            color = "purple",
                            icon = icon("calendar-day"),
                            width = 3),
                        valueBox(
                            value = prettyNum(sum(weekly_ot$violation == 1), big.mark = ","),
                            subtitle = "Weekly Overtime Violations",
                            color = "red",
                            icon = icon("calendar-week"),
                            width = 3),
                        valueBox(
                            value = prettyNum(sum(MCOviolation$MCOviolation == 1), big.mark = ","),
                            subtitle = "MCO Violations",
                            color = "green",
                            icon = icon("clock"),
                            width = 3)
                        # Frontpage - boxes - end -------------------------------------------------
                    ),
                    fluidRow(
                        # Frontpage - violation volume plots - start ----------------------------------
                        tags$style('.nav-tabs-custom .nav-tabs li.active {border-top-color: #cd0000;}"'),
                        tabBox(
                            width = 12,
                            tabPanel(
                                selectInput("start1", "Start Date:", 
                                            choices= c(sort(unique(as.character(full_lunch_viol$`Work Date`)))),
                                            selected = max(full_lunch_viol$`Work Date`)-30),
                                selectInput("end1", "End Date:", 
                                            choices= c(sort(unique(as.character(full_lunch_viol$`Work Date`)))),
                                            selected = max(full_lunch_viol$`Work Date`)),
                                status = "danger",
                                title = "Lunch Violation Volume",
                                plotlyOutput("lunch_volume")
                            ),
                            tabPanel(
                                selectInput("start2", "Start Date:", 
                                            choices= c(sort(unique(as.character(full_daily_viol$`Work Date`)))),
                                            selected = max(full_daily_viol$`Work Date`)-30),
                                selectInput("end2", "End Date:", 
                                            choices= c(sort(unique(as.character(full_daily_viol$`Work Date`)))),
                                            selected = max(full_daily_viol$`Work Date`)),
                                status = "danger",
                                title = "Daily Overtime Violation Volume",
                                plotlyOutput("daily_ot_volume")
                            ),
                            tabPanel(
                                selectInput("start3", "Start Date:", 
                                            choices= c(sort(unique(as.character(full_weekly_viol$`Week Of`)))),
                                            selected = max(full_weekly_viol$`Week Of`)-4),
                                selectInput("end3", "End Date:", 
                                            choices= c(sort(unique(as.character(full_weekly_viol$`Week Of`)))),
                                            selected=max(full_weekly_viol$`Week Of`)),
                                status = "danger",
                                title = "Weekly Overtime Violation Volume",
                                plotlyOutput("weekly_ot_volume")
                            ),
                            tabPanel(
                                selectInput("start4", "Start Date:", 
                                            choices= c(sort(unique(as.character(full_MCO$`Work Date`)))),
                                            selected = max(full_MCO$`Work Date`)-30),
                                selectInput("end4", "End Date:", 
                                            choices= c(sort(unique(as.character(full_MCO$`Work Date`)))),
                                            selected = max(full_MCO$`Work Date`)),
                                status = "danger",
                                title = "Missed Clock Out Volume",
                                plotlyOutput("mco_volume")
                            )
                        ))
                    # Frontpage - violation volume plots - end ------------------------------------
                ),
                
                # Performance Score - tab_performance_score ---------------------------------------------
                tabItem(
                    "tab_performance_score",
                    fluidRow(
                        box(
                            width = "6 col-lg-12",
                            height = "30%",
                            status = "danger",
                            title = "Supervisor Approval Performance",
                            tags$div(
                                class = "scroll-overflow-x",
                                selectInput("sup2",
                                            "Approving Supervisor:",
                                            choices = c("Select", sort(unique(as.character(data1$`Supervisor Approval By`)))))
                            ),
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            ), 
                            helpText("The metric serves to explain the performance \n of the supervisor's ability to detect payroll violations of \n
                         their employees. The legend shows the \n
                         count of each of these categories, \n
                         with black representing violations unapproved and red representing violations approved.")
                        ),
                        box(
                            width = "6 col-lg-3",
                            height = 350,
                            status = "danger",
                            title = "Lunch Violations",
                            girafeOutput("plot_lunch", width="100%")
                        ),
                        box(
                            width = "6 col-lg-3",
                            height = 350,
                            status = "danger",
                            title = "Weekly Overtime Violations",
                            girafeOutput("plot_weeklyot", width="100%")
                        ),
                        box(
                            width = "6 col-lg-3",
                            height = 350,
                            status = "danger",
                            title = "Daily Overtime Violations",
                            girafeOutput("plot_dailyot", width="100%")
                        ),
                        box(
                            width = "6 col-lg-3",
                            height = 350,
                            status = "danger",
                            title = "Missed Clock Out Violations",
                            girafeOutput("plot_mco", width="100%")
                        )
                    ),
                    fluidRow(
                        box(
                            width = "6 col-lg-6",
                            status = "danger",
                            solidHeader = T,
                            title = h3("Top 5 Supervisors"),
                            column(12, align="center", tableOutput("top_5"))
                        ),
                        box(
                            width = "6 col-lg-6",
                            status = "danger",
                            solidHeader = T,
                            title = h3("Bottom 5 Supervisors"),
                            column(12, align="center", tableOutput("bottom_5"))
                        )
                    )
                ),
                
                # Violation Detail - tab_violation_detail ---------------------------------------------
                tabItem(
                    "tab_violation_detail",
                    class = "text-center",
                    tags$style('.nav-tabs-custom .nav-tabs li.active {border-top-color: #cd0000;}"'),
                    fluidRow(
                    h3(textOutput("sup_title")),
                    helpText("These tables allow you to see the violations of each supervisor in more detail by type."),
                    tabBox(
                        width=12,
                        tabPanel("Lunch Violations",
                                 h3("Lunch Violations by Employee"),
                                 helpText("This table shows aggregated counts."),
                                 dataTableOutput("lunch_viol_by_emp"),
                                 h3("Full List of Lunch Violations"),
                                 helpText("This table shows each violation with additional information including date."),
                                 dataTableOutput("full_lunch_viol")
                        ),
                        tabPanel( "Weekly Overtime Violations",
                                  h3("Weekly Overtime Violations by Employee"),
                                  helpText("This table shows aggregated counts."),
                                  dataTableOutput("w_ot_by_emp"),
                                  h3("Full List of Weekly Overtime Violations"),
                                  helpText("This table shows each violation with additional information including date and hours."),
                                  dataTableOutput("full_weekly_viol")
                        ),
                        tabPanel( "Daily Overtime Violations",
                                  h3("Daily Overtime Violations by Employee"),
                                  helpText("This table shows aggregated counts."),
                                  dataTableOutput("d_ot_by_emp"),
                                  h3("Full List of Daily Overtime Violations"),
                                  helpText("This table shows each violations with additional information including date and hours."),
                                  dataTableOutput("full_daily_viol")
                        ),
                        tabPanel( "Missed Clock Out Violations",
                                  h3("Missed Clock Outs by Employee"),
                                  h6("Missed clock outs never get approved, so this list is different in that instead it shows the employees who had
                                     a MCO, regardless of supervisor approval."),
                                  helpText("This table shows aggregated counts."),
                                  dataTableOutput("MCO_by_emp"),
                                  h3("Full List of Missed Clock Outs"),                                 
                                  helpText("This table shows each violations with additional information including date."),
                                  dataTableOutput("full_MCO")
                        )
                        # Violations Detail - Controls - end ---------------------------------------------
                    ))
                    ),
                
                # Employees - tab_employee_detail -----------------------------------------------
                tabItem(
                    "tab_employee_detail",
                    class = "text-center",
                    tags$style('.nav-tabs-custom .nav-tabs li.active {border-top-color: #cd0000;}"'),
                    fluidRow(
                    h3(textOutput("sup_plots")),
                    helpText("These plots represent the violations by employee of the selected supervisor. Hover over each bar to see details."),
                    tabBox(
                        width=12,
                        tabPanel("Lunch Violations",
                                 h3("Lunch Violations"),
                                 plotlyOutput("lunch")
                        ),
                        tabPanel( "Weekly Overtime Violations",
                                  h3("Weekly Overtime Violations"),
                                  plotlyOutput("weeklyovertime")
                        ),
                        tabPanel( "Daily Overtime Violations",
                                  h3("Daily Overtime Violations"),
                                  plotlyOutput("dailyovertime")
                        ),
                        tabPanel( "Missed Clock Out Violations",
                                  h3("Missed Clock Outs"),
                                  plotlyOutput("mco")
                        )
                    ),
                    box(
                        status = "danger",
                        helpText("Select or type an employee name to see details of their overtime violations. Hover over the points to see the details"),
                        width=12,
                        htmlOutput("emp_given_sup")
                    ),
                    tabBox(
                        width=12,
                        
                        tabPanel(
                            "Lunch Violations",
                            h3("Lunch Violation Frequency by Day of the Week"),
                            plotlyOutput("lunch_freq")
                        ),
                        tabPanel( 
                            "Weekly Overtime (Hours)",
                            h3("Weekly Overtime Hours"),
                            plotlyOutput("emp_weekly")
                        ),
                        
                        tabPanel( 
                            "Weekly Overtime (Frequency)",
                            h3("Weekly Overtime Frequency by Month"),
                            plotlyOutput("weekly_freq")
                        ),
                        tabPanel( 
                            "Daily Overtime (Hours)",
                             h3("Daily Overtime Hours"),
                             plotlyOutput("emp_daily")
                                  
                        ),
                        tabPanel( 
                            "Daily Overtime (Frequency)",
                            h3("Daily Overtime Frequency by Day of the Week"),
                            plotlyOutput("daily_freq")
                        ),
                        tabPanel(
                            "Missed Clock Outs",
                            h3("Missed Clock Out Frequency by Day"),
                            plotlyOutput("mco_freq")
                        )
                    )
                    
                    )
                    
                ),
                
                
                
                
                # Explore - tab_explore ---------------------------------------------------
                tabItem(
                    "tab_explore",
                    h3("Explore"),
                    helpText("Use this tab to explore the data within the application."),
                    fluidRow(
                        selectizeGroupUI(
                            id = "filters",
                            inline = FALSE,
                            params = list(
                                var_store = list(inputId = "Location.Name", title = "Location:", placeholder = 'select'),
                                var_sup = list(inputId = "Supervisor.Approval.By", title = "Approving Supervisor:", placeholder = 'select'),
                                var_emp = list(inputId = "Employee.Name", title = "Employee:", placeholder = 'select'),
                                var_pos = list(inputId = "Position.Description", title = "Position Description:", placeholder = 'select')
                            )
                        )
                    ),
                    DT::dataTableOutput("table")
                ),
                
                # About - tab_about -------------------------------------------------------
                tabItem(
                    "tab_about",
                    fluidRow(
                        # About - start ------------------------------------------------
                        box(
                            title = "About this dashboard",
                            status = "danger",
                            width = "6 col-lg-6",
                            tags$p(
                                "This application was developed by students from the Terry College of Business Masters of Science in Buisness Analytics Program in the Spring Semester of 2021.",
                                "This application was developed for Hennessy Auto for the purpose of pin-pointing and visualizing payroll violations in their system.", 
                                "This project made use of Excel, Exploratory, RStudio, Shiny, and ShinyApps.io." #maybe add more instructions or details here
                            ),
                            tags$p(
                                "Faculty supervision was handled by Nikhil Shrinivasan.", 
                                "Application code development was done by Cora Tucker.",
                                "Cleaning code developement was done by Jordyn Gulle.",
                                "Graphics were designed by Billy Rowe.", 
                                "Project was managed by Spencer Kelly."
                            )
                        ),
                        box(
                            title = "Instructional Document",
                            status = "danger",
                            width = "6 col-lg-6",
                            tags$p(
                                class = "text-center",
                               tags$a("Click here for instructions.",target="_blank",href="Instructional Document.pdf")))
                    )
                ))
        )))}


# Define server logic required to draw a histogram
server <- function(session, input, output) {
    
    # Global Reactives --------------------------------------------------------
    
    
    # Dashboard Boxes ---------------------------------------------------------
    
    
    # Dashboard Plots ---------------------------------------------------------
    #table for violation volumne
    output$lunch_volume <- renderPlotly(
        ggplotly(ggplot(data=full_lunch_viol %>% filter(`Work Date` >= input$start1 & `Work Date` <= input$end1), aes(x=factor(`Work Date`)))+
                     geom_bar(stat="count", fill="red3") + labs(title="Volume of Lunch Violations", x="Date", y="Count") + theme_classic() + theme(axis.text.x = element_text(angle = 90))) %>%
            layout(hoverlabel=list(bgcolor="white"))
    )
    
    output$daily_ot_volume <- renderPlotly(
        ggplotly(ggplot(data=full_daily_viol %>% filter(`Work Date` >= input$start2 & `Work Date` <= input$end2), aes(x=factor(`Work Date`)))+
                     geom_bar(stat="count", fill="red3") + labs(title="Volume of Daily Overtime Violations", x="Date", y="Count") + theme_classic() + theme(axis.text.x = element_text(angle = 90))) %>% 
            layout(hoverlabel=list(bgcolor="white")) 
    )
    
    output$weekly_ot_volume <- renderPlotly(
        ggplotly(ggplot(data=full_weekly_viol %>% filter(`Week Of` >= input$start3 & `Week Of` <= input$end3), aes(x=factor(`Week Of`)))+
                     geom_bar(stat="count", fill="red3") + labs(title="Volume of Weekly Overtime Violations", x="Week", y="Count") + theme_classic() +theme(axis.text.x = element_text(angle = 90)))%>% 
            layout(hoverlabel=list(bgcolor="white"))
    )
    
    output$mco_volume <- renderPlotly(
        ggplotly(ggplot(data=full_MCO %>% filter(`Work Date` >= input$start4 & `Work Date` <= input$end4), aes(x=factor(`Work Date`)))+
                     geom_bar(stat="count", fill="red3") + labs(title="Volume of MCO Violations", x="Date", y="Count")  + theme_classic() +theme(axis.text.x = element_text(angle = 90))) %>%
            layout(hoverlabel=list(bgcolor="white"))
    )
    
    
    # Performance Score --------------------------------------------------------------
    output$plot_lunch <- renderGirafe({
        #donut chart for the performance metric
        donut_data <- data.frame(type = c("Black", "Red"), value = c(lunch_by_sup$perf_metric[which(lunch_by_sup$`Supervisor Approval By`== input$sup2)], 100-lunch_by_sup$perf_metric[which(lunch_by_sup$`Supervisor Approval By`== input$sup2)])) %>%
            mutate(hover_text = paste0(type, ": ", round(value,3))) %>%
            mutate(percentage_label = paste0(round(value, 1), "%"))
        
        donut_plot <- ggplot(donut_data, aes(y = value, fill = type)) +
            geom_bar_interactive(
                aes(x = 1, tooltip = hover_text),
                width = 0.1,
                stat = "identity",
                show.legend = TRUE
            ) +
            annotate(
                geom = "text",
                x = 0,
                y = 0,
                label = donut_data[["percentage_label"]][donut_data[["type"]] == "Black"],
                size = 20,
                color = "black"
            ) +
            scale_fill_manual(values = c(Red = "red3", Black = "black"),
                              labels = c(lunch_by_sup$Total[which(lunch_by_sup$`Supervisor Approval By` ==input$sup2)]-lunch_by_sup$Violations[which(lunch_by_sup$`Supervisor Approval By` ==input$sup2)], 
                                         lunch_by_sup$Violations[which(lunch_by_sup$`Supervisor Approval By` ==input$sup2)])
            ) +
            coord_polar(theta = "y") + theme_void() +
            theme(panel.background = element_rect(fill = "transparent", color="NA"),
                  legend.position = "bottom", plot.background = element_rect(fill = "transparent", color="NA"),
                  legend.text=element_text(size=15), legend.title = element_text(size=15)) + labs(title = "Performance Metric for Lunch Violations of:", 
                                                                                                  subtitle = lunch_by_sup$`Supervisor Approval By`[which(lunch_by_sup$`Supervisor Approval By`== input$sup2)],
                                                                                                  fill = "Count of Violations \n Unapproved and Approved")
        
        girafe(ggobj = donut_plot)
    })
    
    output$plot_weeklyot <- renderGirafe({
        #donut chart for the performance metric, will need to adjust values based on reactive filter
        donut_data_wot <- data.frame(type = c("Black", "Red"), value = c(w_ot_by_sup$performance[which(w_ot_by_sup$`Supervisor Approval By`== input$sup2)], 100-w_ot_by_sup$performance[which(w_ot_by_sup$`Supervisor Approval By`== input$sup2)])) %>%
            mutate(hover_text = paste0(type, ": ", round(value,3))) %>%
            mutate(percentage_label = paste0(round(value, 1), "%"))
        
        donut_plot_wot <- ggplot(donut_data_wot, aes(y = value, fill = type)) +
            geom_bar_interactive(
                aes(x = 1, tooltip = hover_text),
                width = 0.1,
                stat = "identity",
                show.legend = TRUE
            ) +
            annotate(
                geom = "text",
                x = 0,
                y = 0,
                label = donut_data_wot[["percentage_label"]][donut_data_wot[["type"]] == "Black"],
                size = 20,
                color = "black"
            ) +
            scale_fill_manual(values = c(Red = "red3", Black = "black"),
                              labels = c(w_ot_by_sup$Total[which(w_ot_by_sup$`Supervisor Approval By`== input$sup2)]-w_ot_by_sup$Count[which(w_ot_by_sup$`Supervisor Approval By`== input$sup2)], 
                                         w_ot_by_sup$Count[which(w_ot_by_sup$`Supervisor Approval By`== input$sup2)])
            ) +
            coord_polar(theta = "y") + theme_void() +
            theme(panel.background = element_rect(fill = "transparent", color="NA"),
                  legend.position = "bottom", plot.background = element_rect(fill = "transparent", color="NA"),
                  legend.text=element_text(size=15), legend.title = element_text(size=15)) + 
            labs(title = "Performance Metric for Weekly Overtime Violations of:", 
                 subtitle = w_ot_by_sup$`Supervisor Approval By`[which(w_ot_by_sup$`Supervisor Approval By`== input$sup2)],
                 fill = "Count of Violations \n Unapproved and Approved")
        
        girafe(ggobj = donut_plot_wot)
    })
    
    output$plot_dailyot <- renderGirafe({
        #donut chart for the performance metric, will need to adjust values based on reactive filter
        donut_data_dot <- data.frame(type = c("Black", "Red"), value = c(d_ot_by_sup$performance[which(d_ot_by_sup$`Supervisor Approval By`== input$sup2)], 100-d_ot_by_sup$performance[which(d_ot_by_sup$`Supervisor Approval By`== input$sup2)])) %>%
            mutate(hover_text = paste0(type, ": ", round(value,3))) %>%
            mutate(percentage_label = paste0(round(value, 1), "%"))
        
        donut_plot_dot <- ggplot(donut_data_dot, aes(y = value, fill = type)) +
            geom_bar_interactive(
                aes(x = 1, tooltip = hover_text),
                width = 0.1,
                stat = "identity",
                show.legend = TRUE
            ) +
            annotate(
                geom = "text",
                x = 0,
                y = 0,
                label = donut_data_dot[["percentage_label"]][donut_data_dot[["type"]] == "Black"],
                size = 20,
                color = "black"
            ) +
            scale_fill_manual(values = c(Red = "red3", Black = "black"),
                              labels = c(d_ot_by_sup$Total[which(d_ot_by_sup$`Supervisor Approval By`== input$sup2)]-d_ot_by_sup$Count[which(d_ot_by_sup$`Supervisor Approval By`== input$sup2)], 
                                         d_ot_by_sup$Count[which(d_ot_by_sup$`Supervisor Approval By`== input$sup2)])
            ) +
            coord_polar(theta = "y") + theme_void() +
            theme(panel.background = element_rect(fill = "transparent", color="NA"),
                  legend.position = "bottom", plot.background = element_rect(fill = "transparent", color="NA"),
                  legend.text=element_text(size=15), legend.title = element_text(size=15)) + labs(title = "Performance Metric for Daily Overtime Violations of:", 
                                                                                                  subtitle = d_ot_by_sup$`Supervisor Approval By`[which(d_ot_by_sup$`Supervisor Approval By`== input$sup2)],
                                                                                                  fill = "Count of Violations \n Unapproved and Approved")
        
        
        girafe(ggobj = donut_plot_dot)
    })
    
    output$plot_mco <- renderGirafe({
        #donut chart for the performance metric, will need to adjust values based on reactive filter
        donut_data_mco <- data.frame(type = c("Black", "Red"), value = c(MCO_by_sup$perf_metric[which(MCO_by_sup$`Supervisor Approval By`== input$sup2)], 100-MCO_by_sup$perf_metric[which(MCO_by_sup$`Supervisor Approval By`== input$sup2)])) %>%
            mutate(hover_text = paste0(type, ": ", round(value,3))) %>%
            mutate(percentage_label = paste0(round(value, 1), "%"))
        
        donut_plot_mco <- ggplot(donut_data_mco, aes(y = value, fill = type)) +
            geom_bar_interactive(
                aes(x = 1, tooltip = hover_text),
                width = 0.1,
                stat = "identity",
                show.legend = TRUE
            ) +
            annotate(
                geom = "text",
                x = 0,
                y = 0,
                label = donut_data_mco[["percentage_label"]][donut_data_mco[["type"]] == "Black"],
                size = 20,
                color = "black"
            ) +
            scale_fill_manual(values = c(Red = "red3", Black = "black"),
                              labels = c(MCO_by_sup$Total[which(MCO_by_sup$`Supervisor Approval By`== input$sup2)]-MCO_by_sup$Violations[which(MCO_by_sup$`Supervisor Approval By`== input$sup2)], 
                                         MCO_by_sup$Violations[which(MCO_by_sup$`Supervisor Approval By`== input$sup2)])
            ) +
            coord_polar(theta = "y") + theme_void() +
            theme(panel.background = element_rect(fill = "transparent", color="NA"),
                  legend.position = "bottom", plot.background = element_rect(fill = "transparent", color="NA"),
                  legend.text=element_text(size=15), legend.title = element_text(size=15)) + labs(title = "Performance Metric for Missed Clock Out Violations of:", 
                                                                                                  subtitle = MCO_by_sup$`Supervisor Approval By`[which(MCO_by_sup$`Supervisor Approval By`== input$sup2)],
                                                                                                  fill = "Count of Violations \n Unapproved and Approved")
        girafe(ggobj = donut_plot_mco)
    })
    
    avg_metrics = lunch_by_sup %>% select('Supervisor Approval By',lunch_metric = perf_metric) %>% left_join((d_ot_by_sup %>% select(`Supervisor Approval By`, d_ot = performance)), by="Supervisor Approval By") %>% 
        left_join((w_ot_by_sup %>% select(`Supervisor Approval By`, w_ot = performance)), by="Supervisor Approval By") %>% left_join((MCO_by_sup %>% select(`Supervisor Approval By`, mco_metric = perf_metric)), by="Supervisor Approval By") %>% 
        mutate(avg = (lunch_metric+d_ot+w_ot+mco_metric)/4) %>% mutate(avg = round(avg,2)) %>% arrange(desc(avg))
    
    output$top_5 = renderTable(avg_metrics %>% drop_na() %>% select(`Supervisor Approval By`, avg) %>% top_n(5, avg) %>% rename("Average Metric" = avg))
    output$bottom_5 = renderTable(avg_metrics %>% arrange(desc(avg)) %>% drop_na() %>% tail(5) %>% select(`Supervisor Approval By`, avg) %>% arrange(avg) %>% rename("Average Metric" = avg))
    
    
    # Violation Detail --------------------------------------------------------------
    #reactivesup <- title("Approving Supervisor" + input$sup2)
    # lunch violation tables
    #Subtitle <- input$sup2
    
    output$sup_title <- renderText({ 
        paste("Supervisor Approval By: ", input$sup2)
    })
    
    output$lunch_viol_by_emp <- DT::renderDataTable(DT::datatable({
        lunch_viol_by_emp %>% filter(`Supervisor Approval By` == input$sup2) %>% select(!(`Supervisor Approval By`)) %>% arrange(desc(Count))
    }, options=list(columnDefs = list(list(className = 'dt-center', targets = "_all")))))
    
    output$full_lunch_viol <- DT::renderDataTable(DT::datatable({
        full_lunch_viol %>% filter(`Supervisor Approval By` == input$sup2) %>% select(!(`Supervisor Approval By`))
    }, options=list(columnDefs = list(list(className = 'dt-center', targets = "_all")))))
    
    #weekly overtime tables
    output$w_ot_by_emp <- DT::renderDataTable(DT::datatable({
        w_ot_by_emp %>% filter(`Supervisor Approval By` == input$sup2) %>% select(!(`Supervisor Approval By`)) %>% arrange(desc(Count))
    }, options=list(columnDefs = list(list(className = 'dt-center', targets = "_all")))))
    
    output$full_weekly_viol <- DT::renderDataTable(DT::datatable({
        full_weekly_viol %>% filter(`Supervisor Approval By` == input$sup2) %>% select(!(`Supervisor Approval By`))
    }, options=list(columnDefs = list(list(className = 'dt-center', targets = "_all")))))
    
    # daily overtime tables
    output$d_ot_by_emp <- DT::renderDataTable(DT::datatable({
        d_ot_by_emp %>% filter(`Supervisor Approval By` == input$sup2) %>% select(!(`Supervisor Approval By`)) %>% arrange(desc(Count))
    }, options=list(columnDefs = list(list(className = 'dt-center', targets = "_all")))))
    
    output$full_daily_viol <- DT::renderDataTable(DT::datatable({
        full_daily_viol %>% filter(`Supervisor Approval By` == input$sup2) %>% select(!(`Supervisor Approval By`))
    }, options=list(columnDefs = list(list(className = 'dt-center', targets = "_all")))))
    
    # MCO tables
    output$MCO_by_emp <- DT::renderDataTable(DT::datatable({
        MCO_by_emp %>% filter(`Supervisor Approval By` == input$sup2) %>% select(!(`Supervisor Approval By`)) %>% arrange(desc(MCOs))
    }, options=list(columnDefs = list(list(className = 'dt-center', targets = "_all")))))
    
    output$full_MCO <- DT::renderDataTable(DT::datatable({
        full_MCO %>% filter(`Supervisor Approval By` == input$sup2) %>% select(!(`Supervisor Approval By`))
    }, options=list(columnDefs = list(list(className = 'dt-center', targets = "_all")))))
    
    # Employee Detail ------------------------------------------------------
    output$sup_plots <- renderText({ 
        paste("Supervisor: ", input$sup2)
    })
    
    output$emp_given_sup <- renderUI({
        employee_list = lunch_viol_by_emp %>% filter(`Supervisor Approval By` == input$sup2) %>% select(`Employee Name`)
        selectInput(inputId = "emp",
                    label = "Employee:",
                    choices = unique(employee_list))
        
    })
    
    output$lunch <- renderPlotly(
        ggplotly(ggplot(data=lunch_viol_by_emp %>% filter(`Supervisor Approval By` == input$sup2 & Count > 0), aes(x =`Employee Name`, y=Count)) + 
                     geom_bar(stat="identity", fill="red3") + geom_text(aes(label=Count), hjust=-0.3) + coord_flip() + 
                     labs(title=paste("Lunch Violations by Employee Approved by: ",input$sup2),  y="Count of Lunch Violations") +theme_classic()) %>% 
            layout(hoverlabel=list(bgcolor="white"))
    )  
    
    output$dailyovertime <- renderPlotly(
        ggplotly(ggplot(data=d_ot_by_emp %>% filter(`Supervisor Approval By` == input$sup2 & Count > 0), aes(x =`Employee Name`, y=Count)) + 
                     geom_bar(stat="identity", fill="red3") + geom_text(aes(label=Count), hjust=-0.3) + coord_flip() + 
                     labs(title=paste("Daily Overtime by Employee Approved by: ",input$sup2), y="Count of Daily Overtime Violations") +theme_classic())%>% 
            layout(hoverlabel=list(bgcolor="white"))
    )
    
    output$weeklyovertime <- renderPlotly(
        ggplotly(ggplot(data=w_ot_by_emp %>% filter(`Supervisor Approval By` == input$sup2 & Count > 0), aes(x =`Employee Name`, y=Count)) + 
                     geom_bar(stat="identity", fill="red3") + geom_text(aes(label=Count), hjust=-0.3) + coord_flip() + 
                     labs(title=paste("Weekly Overtime by Employee Approved by: ",input$sup2), y="Count of Weekly Overtime Violations")+theme_classic())%>% 
            layout(hoverlabel=list(bgcolor="white"))
    )
    
    output$mco <- renderPlotly(
        ggplotly(ggplot(data=MCO_by_emp %>% filter(`Supervisor Approval By` == input$sup2 & MCOs > 0), aes(x =`Employee Name`, y=MCOs)) + 
                     geom_bar(stat="identity", fill="red3") + geom_text(aes(label=MCOs), hjust=-0.3) + coord_flip() + 
                     labs(title=paste("Missed Clock Outs by Employee Approved by: ",input$sup2), y="Count of MCOs")+theme_classic())%>% 
            layout(hoverlabel=list(bgcolor="white"))
    )
    
    output$emp_daily <- renderPlotly(
        ggplotly(ggplot(full_daily_viol %>% filter(`Employee Name` == input$emp)) +
                     aes(x = `Work Date`, y = Hours, group = `Supervisor Approval By`) +
                     geom_line(size = 1L, colour = "red3") + geom_point(colour = "red3") +
                     geom_hline(yintercept = 8.0, color="black") + theme_classic() +
                     labs(title = paste("Hours Worked Per Daily Overtime Violation for: ",input$emp))) %>% 
            layout(hoverlabel=list(bgcolor="white"))
        
    )
    
    output$emp_weekly <- renderPlotly(
        ggplotly(ggplot(full_weekly_viol %>% filter(`Employee Name` == input$emp)) +
                     aes(x = `Week Of`, y = Hours, group = `Supervisor Approval By`) +
                     geom_line(size = 1L, colour = "red3") + geom_point(colour = "red3") +
                     geom_hline(yintercept = 40.0, color="black") + theme_classic() +
                     labs(title = paste("Hours Worked Per Weekly Overtime Violation for: ",input$emp))) %>% 
            layout(hoverlabel=list(bgcolor="white"))
    ) 
    
    output$lunch_freq <- renderPlotly(
        ggplotly(ggplot(full_lunch_viol %>%  filter(`Employee Name` == input$emp) %>%  group_by(Day = wday(`Work Date`,label=TRUE, abbr=FALSE))) +
                     aes(x = `Day`) + geom_bar(stat = "count", fill="red3") + theme_classic() +labs(title=paste("Frequency of Lunch Violations by day for: ", input$emp))+
                                                                                       ylab("Count")) %>% layout(hoverlabel=list(bgcolor="white"))
    )
    
    output$mco_freq <- renderPlotly(
        ggplotly(ggplot(full_MCO %>%  filter(`Employee Name` == input$emp) %>%  group_by(Day = wday(`Work Date`,label=TRUE, abbr=FALSE))) +
                     aes(x = `Day`) + geom_bar(stat = "count", fill="red3") + theme_classic() +labs(title=paste("Frequency of Missed Clock Outs by day for: ", input$emp))+ 
                                                                                                    ylab("Count")) %>% layout(hoverlabel=list(bgcolor="white"))
    )
    
    output$daily_freq <- renderPlotly(
        ggplotly(ggplot(full_daily_viol %>%  filter(`Employee Name` == input$emp) %>%  group_by(Day = wday(`Work Date`,label=TRUE, abbr=FALSE))) +
                     aes(x = `Day`) + geom_bar(stat = "count", fill="red3") + theme_classic() +labs(title=paste("Frequency of Daily Overtime Violations by day for: ", input$emp))+ 
                                                                                                    ylab("Count")) %>% layout(hoverlabel=list(bgcolor="white"))
    )
    
    output$weekly_freq <- renderPlotly(
        ggplotly(ggplot(full_weekly_viol %>%  filter(`Employee Name` == input$emp) %>%  group_by(Month= month(`Week Of`,label=TRUE, abbr=FALSE))) +
                     aes(x = `Month`) + geom_bar(stat = "count", fill="red3") + theme_classic() +labs(title=paste("Frequency of Weekly Overtime Violations by month for: ", input$emp))+ 
                                                                                                    ylab("Count")) %>% layout(hoverlabel=list(bgcolor="white"))
    )
    
    
    
    
    # DataExplorer -----------------------------------------------------------
    res_mod <- callModule(
        module = selectizeGroupServer,
        id = "filters",
        data = main_tab_data,
        vars = c("Location.Name", "Supervisor.Approval.By", "Employee.Name", "Position.Description")
    )
    output$table <- DT::renderDataTable(DT::datatable(res_mod(), colnames = c("Employee Name", "Employee Id", "Position Description", "Location Name", "Work Date",
                                                                              "Punch In Time", "Punch Out Time", "Supervisor Approval By", "Supervisor's Name"),
                                                      options=list(columnDefs = list(list(className = 'dt-center', targets = "_all")))))
    
    observeEvent(input$close, {
        js$closeWindow()
        stopApp()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


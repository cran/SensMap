# SensMapUI shiny application

SensMapUI<- function ()
{
  shiny::runApp(system.file('SensMapUI', package='SensMap'),
                launch.browser = TRUE)
}


# install.packages("shinylive")

shiny::runApp()

shinylive::export(getwd(), "site")

httpuv::runStaticServer("site")

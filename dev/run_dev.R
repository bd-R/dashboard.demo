
# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
golem::document_and_reload()
dashboard.demo::run_app()
# rm(list=ls(all.names = TRUE))
# Document and reload your package
# Run the application


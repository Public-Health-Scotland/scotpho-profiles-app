###########################################
# Cicerone Guided Tour for Trend Tab 
###########################################

guide_summary <- Cicerone$
  new()$
  step(
   "summary_download_pdf_wrapper",
   "Download PDF",
   "Click here to download a PDF containing all available indicators for the selected area.",
   position = "below"
  )$
  step(
    "summary_download_data_wrapper",
    "Download Data Button",
    "Click here to download the selected data as a CSV, RDS or JSON file.",
    position = "below"
  )
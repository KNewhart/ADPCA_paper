as.Powerpoint <- function(codeblock) {
  library(tidyverse)
  library(officer)
  library(rvg)
  
  read_pptx() %>% 
    add_slide(layout = "Title and Content", master = "Office Theme") %>% 
    ph_with(dml(code = {
      codeblock
    }), location = ph_location_type(type = "body")) %>% 
    print(target = "demo_rvg.pptx")
}

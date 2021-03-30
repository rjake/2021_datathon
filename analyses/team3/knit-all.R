local_path <- '.' # with docker

# get all rmd files
rnotebooks <- list.files(path = local_path, 
                         pattern = '.Rmd',
                         full.names = TRUE)

for (notebooki in rnotebooks){
  print(notebooki)
  rmarkdown::render(notebooki, 
                    # output_dir = file.path(local_path, 'reports'),
                    output_format = 'github_document')
}

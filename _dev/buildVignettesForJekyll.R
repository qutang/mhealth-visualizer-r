KnitPost <- function(input, base.url = "/") {
  require(rmarkdown)
  output = file.path(base.url, sub(".Rmd$", ".md", basename(input)))
  output_html = file.path(base.url, sub(".Rmd$", ".html", basename(input)))

  cssFiles = paste0("../assets/css/", c("doc.css"))

  if(basename(input) == "index.Rmd"){
    output_file = sub(".Rmd$", ".html", basename(input))
  }else{
    output_file = sub(".Rmd$", ".html", basename(input))
  }
  dir.create(dirname(output))
  render(input = input,
         output_file = output_file,
         output_dir = base.url,
         output_format = html_document(toc = TRUE,
                                       toc_depth = 5,
                                       self_contained = FALSE,
                                       css = cssFiles,
                                       theme = "default", lib_dir="gh-pages/docs/assets"),
         envir = parent.frame())
}

current = file.path(getSrcDirectory(KnitPost), "..")
original = getwd()
setwd(current)
filenames = list.files("../", pattern = "*.Rmd", all.files = TRUE, full.names = TRUE, recursive = FALSE)
filenames = filenames[!grepl(pattern = "template.Rmd", x = filenames)]
unlink("docs/", recursive = TRUE, force = TRUE)
docFiles = list.files("docs/", pattern = "*.html", all.files = TRUE, full.names = TRUE, recursive = FALSE)
for(filename in filenames){
  KnitPost(filename, "docs/")
}
setwd(original)

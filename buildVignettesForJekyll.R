KnitPost <- function(input, base.url = "/") {
  require(rmarkdown)
  output = file.path(base.url, sub(".Rmd$", ".md", basename(input)))
  output_html = file.path(base.url, sub(".Rmd$", ".html", basename(input)))

  dir.create(dirname(output))
  render(input = input,
         output_file = sub(".Rmd$", ".html", basename(input)),
         output_dir = base.url,
         output_format = html_document(toc = TRUE, self_contained = FALSE),
         envir = parent.frame())
}

current = getSrcDirectory(KnitPost)
original = getwd()
setwd(current)
filenames = list.files("../", pattern = "*.Rmd", all.files = TRUE, full.names = TRUE, recursive = FALSE)
for(filename in filenames){
  KnitPost(filename, "docs/")
}
setwd(original)

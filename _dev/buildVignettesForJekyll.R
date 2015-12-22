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
#   render(input = input,
#          output_file = output_file,
#          output_dir = base.url,
#          output_format = html_document(toc = TRUE,
#                                        toc_depth = 5,
#                                        self_contained = FALSE,
#                                        css = cssFiles,
#                                        theme = "default", lib_dir="gh-pages/docs/assets"),
#          envir = parent.frame())

  render(input = input, output_file = output, output_dir = base.url, output_format = md_document(
    variant = "markdown_github", preserve_yaml = TRUE
  ))
}

KnitDoc <- function(base.url = "/") {
#   require(rmarkdown)
#   output = file.path(base.url, sub(".R$", ".md", basename(input)))
#   output_html = file.path(base.url, sub(".R$", ".html", basename(input)))
#
#   cssFiles = paste0("../assets/css/", c("doc.css"))
#
#   if(basename(input) == "index.R"){
#     output_file = sub(".R$", ".html", basename(input))
#   }else{
#     output_file = sub(".R$", ".html", basename(input))
#   }
#   dir.create(dirname(output))
#
#   render(input = input, output_file = output, output_dir = base.url, output_format = md_document(
#     variant = "markdown_github", preserve_yaml = TRUE
#   ))
  require(mhealthformatsupportr)
  require(Rd2markdown)
  Rd2markdown("mhealthformatsupportr", outdir = base.url, verbose = TRUE, file.ext="md",
              front.matter = "---\nlayout: doc\ntitle: API Document\n---")
}

current = file.path(getSrcDirectory(KnitPost), "..")
original = getwd()
setwd(current)

unlink("docs/*", recursive = TRUE, force = TRUE)

KnitDoc("docs/")
file.rename(from = "docs/index.md", to = "docs/api.md")

filenames = list.files("../", pattern = "*.Rmd", all.files = TRUE, full.names = TRUE, recursive = FALSE)
filenames = filenames[!grepl(pattern = "template.Rmd", x = filenames)]
for(filename in filenames){
  KnitPost(filename, "docs/")
}
setwd(original)

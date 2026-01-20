if (!requireNamespace("quarto", quietly = TRUE)) {
  stop("Package 'quarto' is required. Install it with install.packages('quarto').")
}

inputs <- c("L1.qmd",  "L2.qmd", "L3.qmd", "L4.qmd", "E1.qmd", "E2.qmd", "E3.qmd" , "E4.qmd")

for (input in inputs) {
  output_file <- sub("\\.qmd$", ".html", input)
  quarto::quarto_render(
    input = input,
    output_file = output_file
  )

  if (!dir.exists("docs")) {
    dir.create("docs", recursive = TRUE)
  }
  file.rename(output_file, file.path("docs", output_file))
}

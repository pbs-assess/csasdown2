test_that("bibliography DOI check catches unescaped angle brackets", {
  tmp_dir <- tempfile("bib-validation-")
  dir.create(tmp_dir)

  index_file <- file.path(tmp_dir, "index.Rmd")
  bib_file <- file.path(tmp_dir, "refs.bib")

  writeLines(c(
    "---",
    "bibliography: refs.bib",
    "output:",
    "  csasdown::resdoc_docx:",
    "    french: false",
    "---",
    ""
  ), index_file)

  writeLines(c(
    "@article{foo,",
    "  title = {Example},",
    "  doi = {10.1577/1548-8675(2002)022<0251:FRTYL>2.0.CO;2}",
    "}"
  ), bib_file)

  expect_error(
    check_bibliography_for_unescaped_doi_angles(index_file),
    "Replace `<` and `>` with `&lt;` and `&gt;`"
  )
  expect_error(
    check_bibliography_for_unescaped_doi_angles(index_file),
    "refs.bib"
  )
})

test_that("bibliography DOI check allows escaped angle brackets", {
  tmp_dir <- tempfile("bib-validation-")
  dir.create(tmp_dir)

  index_file <- file.path(tmp_dir, "index.Rmd")
  bib_file <- file.path(tmp_dir, "refs.bib")

  writeLines(c(
    "---",
    "bibliography: refs.bib",
    "output:",
    "  csasdown::resdoc_docx:",
    "    french: false",
    "---",
    ""
  ), index_file)

  writeLines(c(
    "@article{foo,",
    "  title = {Example},",
    "  doi = {10.1577/1548-8675(2002)022&lt;0251:FRTYL&gt;2.0.CO;2}",
    "}"
  ), bib_file)

  expect_no_error(
    csasdown:::check_bibliography_for_unescaped_doi_angles(index_file)
  )
})

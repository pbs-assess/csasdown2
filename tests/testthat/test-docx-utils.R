count_fixed_matches <- function(x, pattern) {
  matches <- gregexpr(pattern, x, fixed = TRUE)[[1]]
  if (identical(matches[1], -1L)) {
    return(0L)
  }
  length(matches)
}

test_that("fix_table_cell_styles_xml applies expected table cell styles", {
  xml <- paste0(
    '<w:document><w:body><w:tbl>',
    '<w:tr><w:tc><w:tcPr></w:tcPr><w:p><w:pPr><w:sz w:val="20"/><w:szCs w:val="20"/></w:pPr>',
    '<w:r><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/>',
    '</w:rPr><w:t>Header</w:t></w:r></w:p></w:tc></w:tr>',
    '<w:tr><w:tc><w:tcPr></w:tcPr><w:p><w:pPr><w:sz w:val="20"/><w:szCs w:val="20"/></w:pPr>',
    '<w:r><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/>',
    '</w:rPr><w:t>Body</w:t></w:r></w:p></w:tc></w:tr>',
    '</w:tbl></w:body></w:document>'
  )

  out <- csasdown:::fix_table_cell_styles_xml(xml)
  rows <- regmatches(out, gregexpr("<w:tr[^>]*>.*?</w:tr>", out, perl = TRUE))[[1]]

  expect_length(rows, 2L)
  expect_true(grepl('<w:pStyle w:val="Caption-Table"/>', rows[1], fixed = TRUE))
  expect_true(grepl('<w:pStyle w:val="BodyText"/>', rows[2], fixed = TRUE))
  expect_false(grepl('<w:sz w:val="20"/>', rows[1], fixed = TRUE))
  expect_false(grepl('<w:szCs w:val="20"/>', rows[1], fixed = TRUE))
  expect_true(grepl('<w:sz w:val="20"/>', rows[2], fixed = TRUE))
  expect_true(grepl('<w:szCs w:val="20"/>', rows[2], fixed = TRUE))
  expect_false(grepl('<w:rFonts w:ascii="Helvetica"', out, fixed = TRUE))
  expect_equal(count_fixed_matches(out, "<w:rFonts/>"), 2L)
  expect_false(grepl("\\\\1<w:pStyle", out, fixed = TRUE))
})

test_that("fix_table_cell_styles_xml is idempotent and de-duplicates styles", {
  xml <- paste0(
    '<w:document><w:body><w:tbl>',
    '<w:tr><w:tc><w:tcPr></w:tcPr><w:p><w:pPr>',
    '<w:pStyle w:val="Caption-Table"/><w:pStyle w:val="Caption-Table"/>',
    '</w:pPr><w:r><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" ',
    'w:eastAsia="Helvetica" w:cs="Helvetica"/></w:rPr><w:t>Header</w:t></w:r></w:p></w:tc></w:tr>',
    '</w:tbl></w:body></w:document>'
  )

  once <- csasdown:::fix_table_cell_styles_xml(xml)
  twice <- csasdown:::fix_table_cell_styles_xml(once)

  expect_identical(once, twice)
  expect_equal(count_fixed_matches(once, '<w:pStyle w:val="Caption-Table"/>'), 1L)
})

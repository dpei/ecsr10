# parse_ahrq_sas.R
# Tokenizer for the AHRQ/HCUP CMR SAS format programs.
#
# `CMR_Format_Program_v<rel>.sas` holds every lookup table the methodology needs,
# as SAS `Proc format` value blocks that all share one shape:
#
#     Value $COMFMT
#        "B20",
#        "O98711",
#        "Z21" = "AIDS"
#
#        "F1010", ... = "ALCOHOL"
#        other = " "
#        ;
#
# `$COMFMT` maps diagnosis code -> comorbidity target; each `$POAXMPT_V<nn>FMT`
# maps POA-exempt code -> "1". Same grammar, so one tokenizer parses both.
#
# Four things bite anyone writing this parser, and all four are handled below:
#
#   1. Encoding is per FILE, not per release. 2026.1's Format and Index programs
#      carry a UTF-8 BOM; its Mapping program does not, and 2022.1-2025.1 are
#      plain ASCII. So probe for the BOM rather than assuming either way.
#   2. `$POAXMPT_V41FMT` and `$POAXMPT_V43FMT` quote their codes with SINGLE
#      quotes; every other block, including `$COMFMT`, uses double. A
#      double-quote-only tokenizer silently yields ZERO codes for those two
#      blocks - no error, just a POA-exempt list that is empty at ICD versions
#      41 and 43.
#   3. Every block ends with an `other = " "` catch-all. Left in, it parses as a
#      code named " " mapped to the previous target. Each block is truncated
#      there instead.
#   4. The file header is a large `/* */` banner containing prose. Comments are
#      stripped before block detection so nothing in the banner can be mistaken
#      for a value block.
#
# CRLF needs no special handling: `\r` is whitespace outside quoted strings, and
# no token spans a line break.

# ---- reading -----------------------------------------------------------------

#' Read a SAS program as text, BOM- and encoding-safe.
#' Returns a length-1 character vector.
read_sas_text <- function(path) {
  raw <- readBin(path, "raw", file.size(path))
  if (length(raw) >= 3L && identical(raw[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) {
    raw <- raw[-(1:3)]
  }
  # latin-1, not strict UTF-8: these files are ASCII in practice, but latin-1
  # cannot fail on a stray high byte the way UTF-8 validation can.
  txt <- iconv(rawToChar(raw), from = "latin1", to = "UTF-8")
  if (is.na(txt)) stop("could not decode ", path, call. = FALSE)
  txt
}

#' Strip /* ... */ comments.
strip_sas_comments <- function(txt) {
  gsub("/\\*.*?\\*/", " ", txt, perl = TRUE)
}

# ---- block splitting ---------------------------------------------------------

#' Split a format program into its `Value $NAME` blocks.
#'
#' Each block runs to the next `Value $` or end of text, and is truncated at its
#' `other =` clause. Returns a named character vector: names are the block names
#' without the `$`, values are the block bodies (header line removed).
sas_value_blocks <- function(txt) {
  txt <- strip_sas_comments(txt)
  pat <- "Value\\s+\\$([A-Za-z0-9_]+)"
  m <- gregexpr(pat, txt, ignore.case = TRUE, perl = TRUE)[[1]]
  if (m[1] == -1L) stop("no `Value $NAME` blocks found", call. = FALSE)

  hits   <- regmatches(txt, gregexpr(pat, txt, ignore.case = TRUE, perl = TRUE))[[1]]
  names_ <- toupper(sub(pat, "\\1", hits, ignore.case = TRUE, perl = TRUE))

  starts <- as.integer(m) + attr(m, "match.length")   # just past the header
  ends   <- c(as.integer(m)[-1] - 1L, nchar(txt))

  out <- vapply(seq_along(starts), function(i) {
    blk <- substr(txt, starts[i], ends[i])
    # Truncate at the `other =` catch-all (gotcha 3).
    cut <- regexpr("\\bother\\s*=", blk, ignore.case = TRUE, perl = TRUE)
    if (cut > 0L) blk <- substr(blk, 1L, cut - 1L)
    blk
  }, character(1))

  stats::setNames(out, names_)
}

# ---- tokenising --------------------------------------------------------------

#' Extract quoted string literals, single- or double-quoted (gotcha 2).
#'
#' One alternation over both quote styles, so a block's quoting convention never
#' has to be known in advance.
sas_tokens <- function(x) {
  pat <- "\"([^\"]*)\"|'([^']*)'"
  hits <- regmatches(x, gregexpr(pat, x, perl = TRUE))[[1]]
  if (!length(hits)) return(character(0))
  substr(hits, 2L, nchar(hits) - 1L)
}

#' Parse one value block into its (code, target) pairs.
#'
#' The grammar is `code, code, ... code = target` repeated. Splitting the block
#' on `=` puts each target at the head of the following segment, with the rest of
#' that segment being the next group's codes:
#'
#'   segment 0        -> codes of group 1
#'   segment i (i>0)  -> [target of group i, codes of group i+1 ...]
#'
#' so one pass pairs them all. A block whose final segment carries trailing codes
#' is malformed (codes with no target) and errors rather than dropping them.
sas_pairs <- function(block, block_name = "<block>") {
  segs <- strsplit(block, "=", fixed = TRUE)[[1]]
  if (length(segs) < 2L) {
    return(data.frame(code = character(0), target = character(0),
                      stringsAsFactors = FALSE))
  }
  toks <- lapply(segs, sas_tokens)

  pending <- toks[[1]]
  codes   <- vector("list", length(toks) - 1L)
  targets <- character(length(toks) - 1L)

  for (i in seq.int(2L, length(toks))) {
    tk <- toks[[i]]
    if (!length(tk)) {
      stop("malformed block ", block_name, ": `=` with no target token after it",
           call. = FALSE)
    }
    codes[[i - 1L]] <- pending
    targets[i - 1L] <- tk[[1]]
    pending <- tk[-1]
  }
  if (length(pending)) {
    stop("malformed block ", block_name, ": ", length(pending),
         " trailing code(s) with no target, e.g. ", pending[[1]], call. = FALSE)
  }

  n <- lengths(codes)
  data.frame(code   = unlist(codes, use.names = FALSE),
             target = rep(targets, n),
             stringsAsFactors = FALSE)
}

# ---- the two datasets --------------------------------------------------------

#' Parse `$COMFMT` from a format program.
#' Returns a data.frame(code, comorbidity), sorted by code - the order the
#' shipped `comfmt_lookup` uses.
parse_comfmt <- function(path) {
  blocks <- sas_value_blocks(read_sas_text(path))
  if (!"COMFMT" %in% names(blocks)) {
    stop("no $COMFMT block in ", path, call. = FALSE)
  }
  df <- sas_pairs(blocks[["COMFMT"]], "COMFMT")
  names(df) <- c("code", "comorbidity")

  dup <- df$code[duplicated(df$code)]
  if (length(dup)) {
    stop("duplicate codes in $COMFMT of ", path, ": ",
         paste(utils::head(unique(dup), 5), collapse = ", "), call. = FALSE)
  }

  df <- df[order(df$code), , drop = FALSE]
  rownames(df) <- NULL
  df
}

#' Parse every `$POAXMPT_V<nn>FMT` block from a format program.
#'
#' Returns a data.frame(version, code) in block order, versions ascending.
#'
#' `other_sentinel = TRUE` appends one `**OTHER**` row per version. That is what
#' the shipped `poaxmpt_codes_long` carries - an artifact of the original parse
#' having captured the `other = " "` catch-all - and reproducing it is what makes
#' the regenerated object `identical()` to the shipped one. It is harmless at
#' runtime, since `**OTHER**` can never match a normalised ICD code.
parse_poaxmpt <- function(path, other_sentinel = TRUE) {
  blocks <- sas_value_blocks(read_sas_text(path))
  nm <- grep("^POAXMPT_V[0-9]+FMT$", names(blocks), value = TRUE)
  if (!length(nm)) stop("no $POAXMPT_V<nn>FMT blocks in ", path, call. = FALSE)

  vers <- as.integer(sub("^POAXMPT_V([0-9]+)FMT$", "\\1", nm))
  nm   <- nm[order(vers)]
  vers <- sort(vers)

  parts <- lapply(seq_along(nm), function(i) {
    df <- sas_pairs(blocks[[nm[i]]], nm[i])
    bad <- unique(df$target[df$target != "1"])
    if (length(bad)) {
      stop("unexpected target(s) in ", nm[i], " of ", path, ": ",
           paste(utils::head(bad, 5), collapse = ", "), call. = FALSE)
    }
    codes <- df$code
    if (other_sentinel) codes <- c(codes, "**OTHER**")
    data.frame(version = rep(vers[i], length(codes)), code = codes,
               stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, parts)
  out$version <- as.integer(out$version)
  rownames(out) <- NULL
  out
}

#' The AHRQ releases this directory knows how to parse, newest last.
#'
#' v2021.1 is deliberately absent: it is structurally different software (two
#' SAS programs rather than three, measures named ARTH/CHF rather than
#' AUTOIMMUNE/HF, no `CMR_` output prefix, and no index program at all - AHRQ
#' states the Indices "are not available until v2022.1"), and no independent
#' reference exists to validate a translation of it against.
AHRQ_RELEASES <- c("2022.1", "2023.1", "2024.1", "2025.1", "2026.1")

#' Locate a release's format program under SAS_software/.
format_program_path <- function(release, sas_dir) {
  d <- file.path(sas_dir, release)
  if (!dir.exists(d)) stop("no such release directory: ", d, call. = FALSE)
  f <- list.files(d, pattern = "^CMR_Format_Program_.*\\.sas$", full.names = TRUE)
  if (length(f) != 1L) {
    stop("expected exactly one CMR_Format_Program_*.sas in ", d,
         ", found ", length(f), call. = FALSE)
  }
  f
}

#' The newest ICD-10-CM version a release covers.
#'
#' A release labelled v<Y>.1 covers codes through September of fiscal year Y, and
#' ICD-10-CM version numbering makes that Y - 1983 (v33 = FY2016 ... v43 = FY2026).
#' Verified against all five mapping programs: each one's ICDVER ladder is
#' truncated here and its final `ELSE` assigns exactly this value.
release_max_icd_version <- function(release) {
  as.integer(substr(release, 1L, 4L)) - 1983L
}

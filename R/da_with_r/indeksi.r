# ========================================================================
# Indeksi 
#
# Do komponent vektorjev dostopamo z indeksi tako, da v oglatih
# oklepajih za vektorjem lahko naštejemo vektor želenih indeksov.
# 
#     x <- 20 : 30
#     x[c(1, 2, 7)] # 1., 2. in 7. element
#     x[c(1, 2, 7, 5, 1)] # 1., 2., 7., 5. in 1. element
#     
# Funkcija `order` vrne vektor mest, na katera spadajo posamezne
# komponente v vektorju, urejenem po velikosti. Po korakih izvedite
# spodnje tri ukaze. Kaj je rezultat zadnjega?
# 
#     y <- c(3, 7, 1, 5, 9)
#     order(y)
#     y[order(y)]
#     
# Če so indeksi negativni, naštejemo neželene elemente.
# 
#     x[c(-1, -2, -7)] # Vsi elementi razen 1., 2. in 7. elementa
#     
# Če podamo vektor logičnih vrednosti, se izberejo tisti elementi, kjer je
# pripadajoča vrednost enaka `TRUE`.
# 
#     x[c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)]
#     
# Namen logičnih vektorjev v indeksih je izbor elementov, ki zadoščajo
# danemu pogoju.
# 
#     x %% 3 # vektor ostankov pri deljenju s 3 
#     x %% 3 == 0 # vektor, ki je TRUE tam, kjer je ostanek pri deljenju s 3 enak 0
#     x[x %% 3 == 0] # števila, ki so deljiva s 3
#     x[x %% 5 == 1] # števila, ki dajo pri deljenju s 5 ostanek 1
#     x[x %% 3 == 0 | x %% 5 == 1] # števila, ki zadoščajo vsaj enemu pogoju
# 
# Vrednosti komponent spreminjamo s prireditvenim stavkom.
# 
#     x <- c(1, -3, 5, -7, 9, -11)
#     x[3] <- -5
# 
# Spreminjamo lahko tudi več vrednosti naenkrat.
# 
#     x[c(1, 5)] <- 2 # 1. in 5. komponento nastavimo na 2
#     x[c(2, 4)] <- c(60, 90) # 2. komponento nastavimo na 60, 4. pa na 90
#     x[x %% 3 == 0] <- 10 # vse komponente, deljive z 3, nastavimo na 10
#     x[x > 1] <- 100 * x[x > 1] # komponente, večje od 1, pomnožimo s 100
# ================================================================@021589=
# 1. podnaloga
# Sestavite funkcijo `sodi(v)`, ki vrne vse komponente vektorja `v` na
# sodih mestih.
# ========================================================================
sodi <- function(v) {
  tst <- 1:length(v)
  
  return(v[tst %% 2 == 0])
}
# ================================================================@021590=
# 2. podnaloga
# Sestavite funkcijo `pozitivni(v)`, ki vrne vektor pozitivnih komponent
# vektorja `v`.
# ========================================================================
pozitivni <- function(v) {
  return(v[v > 0])
}
# ================================================================@021591=
# 3. podnaloga
# Sestavite funkcijo `obrestuj(stanja)`, ki sprejme vektor stanj na
# posameznih računih in vrne vektor stanj po obračunanih obrestih.
# Na pozitivna stanja obračunajte 5%, na negativna pa 10% obresti.
# ========================================================================
obrestuj <- function(stanja) {
    stanja[stanja < 0] <- stanja[stanja < 0] + stanja[stanja < 0] * 0.1
    stanja[stanja > 0] <- stanja[stanja > 0] + stanja[stanja > 0] * 0.05
    
    return(stanja)
}








































































































# =======================================================================@
# Kode pod to črto nikakor ne spreminjajte.
# ========================================================================

"TA VRSTICA JE PRAVILNA."
"ČE VAM R SPOROČI, DA JE V NJEJ NAPAKA, SE MOTI."
"NAPAKA JE NAJVERJETNEJE V ZADNJI VRSTICI VAŠE KODE."
"ČE JE NE NAJDETE, VPRAŠAJTE ASISTENTA."




























































if (length(showConnections()) > 1) {
  .filename <- showConnections()[1, "description"]
} else {
  .filename <- Find(Negate(is.null), Map(function(f) { f$ofile }, sys.frames()), right=TRUE)
}


.check <- function() {
  .error <- FALSE
.errfun <- function(e) {
    warning(e)
    .error <<- TRUE
}
tryCatch({
    library(rjson)
}, error = .errfun)
tryCatch({
    library(httr)
}, error = .errfun)

if (.error) {
    stop("Required libraries are unavailable. Please make sure that rjson and httr are available.")
}

regex_break <- function(whole_regex, regexes, source) {
    whole_matches <- gregexpr(paste("(?sm)", whole_regex, sep=""), source, perl=TRUE)[[1]]
    n <- length(regexes)
    if (whole_matches[1] > 0) {
      whole_matches <- mapply(
          function(start, end) substr(source, start, end),
          whole_matches,
          whole_matches + attr(whole_matches, "match.length") - 1
      )
      m <- length(whole_matches)
    } else {
      return(matrix("", nrow=0, ncol=n))
    }
    matches <- matrix("", nrow=m, ncol=n)
    for (i in 1:m) {
        whole <- whole_matches[i]
        for (j in 1:n) {
            rest_regex <- paste(regexes[-(1 : j)], collapse="")
            part_regex <- paste("(?sm)\\A", regexes[j], "(?=", rest_regex, "\\Z)", sep="")
            match <- regexpr(part_regex, whole, perl=TRUE)
            end <- attr(match, "match.length")
            matches[i, j] <- substr(whole, 1, end)
            whole <- substr(whole, end + 1, nchar(whole))
        }
    }
    matches
}

strip <- function(str) gsub("^\\s+|\\s+$", "", str)
rstrip <- function(str) gsub("\\s+$", "", str)

super_strip <- function(str) {
    str <- gsub("(^|\n)\\s*# ?", "\n", str)
    gsub("\\A\\s+|\\s+\\Z", "", str, perl=TRUE)
}

pretty.print <- function(x) {
  output <- capture.output(print(x))
  if(length(output) == 0) {
    return("NULL")
  } else if(length(output) == 1) {
    return(output)
  } else {
    return(paste("    ", c("", output, ""), collapse = "\n"))
  }
}


  check <- list()

check$initialize <- function(parts) {
  init.part <- function(part) {
    part$valid <- TRUE
    part$feedback <- list()
    part$secret <- list()
    if (part$part) part$id <- part$part
    return(part)
  }
  check$parts <<- lapply(parts, init.part)
  check$current.part <<- NA
  check$part.counter <<- NA
}

check$part <- function() {
  if(is.na(check$part.counter)) {
    check$part.counter <<- 1
  } else {
    check$part.counter <<- check$part.counter + 1
  }
  check$current.part <<- check$parts[[check$part.counter]]
  return(strip(check$current.part$solution) != "")
}

check$feedback <- function(msg, ...) {
  check$parts[[check$part.counter]]$feedback <<-
    c(check$parts[[check$part.counter]]$feedback, sprintf(msg, ...))
}

check$error <- function(msg, ...) {
  check$parts[[check$part.counter]]$valid <<- FALSE
  check$feedback(msg, ...)
}

check$secret <- function(x, hint = "") {
  pair <- c(toString(check$canonize(x)), toString(hint))
  check$parts[[check$part.counter]]$secret<<-
    c(check$parts[[check$part.counter]]$secret, list(pair))
}

check$run <- function(statements, expected.state) {
  code <- substitute(statements)
  statements <- paste0("  > ", paste(unlist(strsplit(deparse(code), "\n")),
                                     collapse = "\n  > "))
  env <- new.env(parent = parent.frame())
  eval(code, env)
  errors <- character(0)
  for (x in names(expected.state)) {
    if (! x %in% names(env)) {
      errors <- c(errors, sprintf("morajo nastaviti spremenljivko %s, vendar je ne", x))
    } else if (check$canonize(env[[x]]) != check$canonize(expected.state[[x]])) {
      errors <- c(errors, sprintf("nastavijo %s na %s namesto na %s",
                                  x, env[[x]], expected.state[[x]]))
    }
  }
  if (length(errors) > 0) {
    check$error("Ukazi\n%s\n%s.", statements, paste(errors, collapse = ";\n"))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

check$canonize <- function(x, digits = 6) {
  if(typeof(x) == "double" || typeof(x) == "complex") {
    return(round(x, digits))
  } else if(typeof(x) == "list") {
    return(lapply(x, function(y) check$canonize(y, digits)))
  } else {
    return(x)
  }
}

check$equal <- function(example, value = NA, exception = NA,
                        clean = function(x) x,
                        precision = 1.0e-6, strict.float = FALSE, check.attributes = FALSE) {
  difference <- function(x, y) {
    if(identical(x, y)) return(NA)
    else if(isTRUE(all.equal(x, y, check.attributes = check.attributes))) return(NA)
    else if(typeof(x) != typeof(y) && (strict.float || !(mode(x) != mode(y))))
      return("različna tipa")
    else if(length(x) != length(y))
      return("različno število komponent")
    else if(mode(x) == 'numeric' && mode(y) == 'numeric') {
      if(any(abs(x - y) > precision))
        return("numerična napaka")
      else
        return(NA)
    }
    else return("različni vrednosti")
  }
  example <- substitute(example)

  if(!is.na(exception)) {
    tryCatch({
      returned <- eval(example, parent.frame())
      check$error("Izraz %s vrne %s namesto da bi sprožil izjemo '%s'.",
                  deparse(example), pretty.print(returned), exception)
      return(FALSE)
    }, error = function(e) {
      if(e$message != exception)
        check$error("Izraz %s sproži izjemo '%s' namesto '%s'.",
                    deparse(example), e$message, exception)
        return(FALSE)
    })
  } else {
    returned <- eval(example, parent.frame())
    reason <- difference(clean(returned), clean(value))
    if(!is.na(reason)) {
      check$error("Izraz %s vrne %s namesto %s (%s)",
                  deparse(example), pretty.print(returned), pretty.print(value), reason)
      return(FALSE)
    }
  }
  return(TRUE)
}

check$random <- function(example, period = 10, sample = 100, uniqueness = 0.9) {
  example <- substitute(example)
  results <- replicate(sample, toString(check$canonize(replicate(period, eval(example, parent.frame())))))
  if (length(unique(results)) < uniqueness * sample) {
    check$error("Izraz %s ne vrača naključnih rezultatov.", deparse(example))
  }
}

check$probability <- function(example, interval, sample = 100) {
  example <- substitute(example)
  results <- replicate(sample, isTRUE(eval(example, parent.frame())))
  prob <- sum(results) / sample
  if (!(interval[1] < prob && prob <= interval[2])) {
    check$error("Izraz %s velja z verjetnostjo %.2f, ki je izven pričakovanega intervala [%.2f, %.2f].", deparse(example), prob, interval[1], interval[2])
  }
}

check$expected <- function(example, interval, sample = 100) {
  example <- substitute(example)
  results <- replicate(sample, eval(example, parent.frame()))
  prob <- sum(results) / sample
  if (!(interval[1] < prob && prob <= interval[2])) {
    check$error("Povprečna vrednost izraza %s je %.2f, kar je izven pričakovanega intervala [%.2f, %.2f].", deparse(example), prob, interval[1], interval[2])
  }
}

check$in.file <- function(filename, content, statements) {
  code <- substitute(statements)
  cat(paste0(content, "\n", collapse = ""), file = filename)
  old.feedback <- check$parts[[check$part.counter]]$feedback
  eval(code, parent.frame())
  new.feedback <- check$parts[[check$part.counter]]$feedback
  if (length(new.feedback) > length(old.feedback)) {
    check$parts[[check$part.counter]]$feedback <<- old.feedback
    check$error("Pri vhodni datoteki %s z vsebino\n  %s\nso se pojavile naslednje napake:\n- %s",
                filename, paste(content, collapse = "\n  "),
                paste(gsub("\n", "\n    ",
                           new.feedback[(length(old.feedback) + 1) : length(new.feedback)]),
                      collapse = "\n- "))
  }
}

check$out.file <- function(filename, content) {
  tryCatch({
    f <- file(filename)
    out.lines <- readLines(f)
    diff <- check$difflines(out.lines, content)
    if (diff$equal) {
      return(TRUE)
    } else {
      check$error('Izhodna datoteka %s\n  je enaka%s  namesto:\n  %s',
                  filename, paste(rep(" ", diff$line.width - 7), collapse = ""),
                  paste(diff$diff, collapse = "\n  "))
    }
  }, finally = {
    close(f)
  })
  return(FALSE)
}

check$output <- function(statements, content) {
  output <- capture.output(statements)
  diff <- check$difflines(output, content)
  if (diff$equal) {
    return(TRUE)
  } else {
    check$error('Program izpiše%s  namesto:\n  %s',
                paste(rep(" ", diff$line.width - 13), collapse = ""),
                paste(diff$diff, collapse = "\n  "))
    return(FALSE)
  }
}

check$difflines <- function(actual.lines, expected.lines) {
  actual.len <- length(actual.lines)
  expected.len <- length(expected.lines)
  if (actual.len < expected.len) {
    actual.lines <- c(actual.lines, rep("\n", expected.len - actual.len))
  } else {
    expected.lines <- c(expected.lines, rep("\n", actual.len - expected.len))
  }
  equal <- TRUE
  out <- trimws(actual.lines, "right")
  given <- trimws(expected.lines, "right")
  line.width <- max(sapply(c(out, "Program izpiše"), nchar))
  format <- paste0("%-", line.width, "s %s %s")
  diff <- sprintf(format, out, ifelse(out == given, "|", "*"), given)
  return(list(equal = all(out == given), diff = diff, line.width = line.width))
}

check$summarize <- function() {
  if (length(check$parts) == 0) return()
  for(i in 1:length(check$parts)) {
    if(strip(check$parts[[i]]$solution) == "") {
      cat("Podnaloga", i, "je brez rešitve.\n")
    } else if (! check$parts[[i]]$valid) {
      cat("Podnaloga", i, "nima veljavne rešitve.\n")
    } else {
      cat("Podnaloga", i, "ima veljavno rešitev.\n")
    }
    for (message in check$parts[[i]]$feedback) {
        cat("- ", message, "\n", sep = "")
    }
  }
}

  check$challenge <- check$secret

  .source <- paste(readLines(.filename), collapse="\n")

  matches <- regex_break(paste(
      '# =+@(\\d+)=\n',        # beginning of header
      '(\\s*#( [^\n]*)?\n)+?', # description
      '\\s*# =+\n',            # end of header
      '.*?',                   # solution
      '(?=\n\\s*# =+@)',       # beginning of next part
      sep=""
  ), c(
      '# =+@',                 # beginning of header
      '(\\d+)',                # beginning of header (?P<part>)
      '=\n',                   # beginning of header
      '(\\s*#( [^\n]*)?\n)+?', # description
      '\\s*# =+\n',            # end of header
      '.*?'                    # solution
  ), .source)

  check$initialize(
    apply(matches, 1, function(match) list(
        part = as.numeric(match[2]),
        solution = match[6]
      )
    )
  )
  check$parts[[length(check$parts)]]$solution <- rstrip(check$parts[[length(check$parts)]]$solution)

  body <- list()
  indices <- c()
  
  if (check$part()) {
    check$parts[[check$part.counter]]$token <- 'eyJ1c2VyIjoyNjkwLCJwYXJ0IjoyMTU4OX0:1j6DBf:pbhEXgWZpLyxfnNI1Z3RMKyAQHk'
    tryCatch({
      check$equal(sodi(c(1, 5, 2, 4, 1, 3, 10, 7)), c(5, 4, 3, 7))
      check$equal(sodi(c(1, 5, -2, -4, -1)), c(5, -4))
      check$secret(paste(sodi(round(seq(1, 1238, length.out=40))), collapse=" "))
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
    body[[length(body) + 1]] <- check$parts[[check$part.counter]]
    indices <- c(indices, check$part.counter)
  }
  
  if (check$part()) {
    check$parts[[check$part.counter]]$token <- 'eyJ1c2VyIjoyNjkwLCJwYXJ0IjoyMTU5MH0:1j6DBf:Z3boRdjPDxKEotwcSKMURgZHkQU'
    tryCatch({
      check$equal(pozitivni(c(2, 5, 1, 7)), c(2, 5, 1, 7))
      check$equal(pozitivni(c(2, 5, -1, 7, -1, -5)), c(2, 5, 7))
      check$secret(paste(pozitivni(-50 : 51), collapse=" "))
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
    body[[length(body) + 1]] <- check$parts[[check$part.counter]]
    indices <- c(indices, check$part.counter)
  }
  
  if (check$part()) {
    check$parts[[check$part.counter]]$token <- 'eyJ1c2VyIjoyNjkwLCJwYXJ0IjoyMTU5MX0:1j6DBf:4LOd5cd66_hgnojVu0lnzLLwIOU'
    tryCatch({
      check$equal(obrestuj(c(200, -500, 10, 70)), c(210, -550, 10.5, 73.5))
      check$equal(obrestuj(c(200, -50, 10, 700)), c(210, -55, 10.5, 735))
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
    body[[length(body) + 1]] <- check$parts[[check$part.counter]]
    indices <- c(indices, check$part.counter)
  }
  

  cat('Shranjujem rešitve na strežnik... ')
  tryCatch({
    r <- POST(
      'https://www.projekt-tomo.si/api/attempts/submit/',
      body = lapply(body, function(part) {
        part$secret <- lapply(part$secret, function(x) x[1])
        part
      }),
      encode = "json",
      add_headers(Authorization = 'Token 439ce124b913fcefaa60611efbc171480a756c77')
    )
    response <- content(r)
    cat('Rešitve so shranjene.\n')
    updates <- list()
    for (part in response$attempts) {
      updates[[part$part]] <- part
    }
    if (length(body) > 0) {
      for(i in 1:length(body)) {
        valid.before <- body[[i]]$valid
        if (!is.null(updates[[body[[i]]$part]])) {
          for (field in names(updates[[body[[i]]$part]])) {
            body[[i]][[field]] <- updates[[body[[i]]$part]][[field]]
          }
        }
        valid.after <- body[[i]]$valid
        if (valid.before && ! valid.after) {
          wrong.index <- response$wrong_indices[[as.character(body[[i]]$part)]]
          if (! is.null(wrong.index)) {
            hint <- body[[i]]$secret[[wrong.index+1]][2]
            if (nchar(hint) > 0) {
              body[[i]]$feedback <- c(body[[i]]$feedback, paste("Namig:", hint))
            }
          }
        }
        check$parts[[indices[i]]] <- body[[i]]
      }
    }
    if("update" %in% names(response)) {
      cat("Updating file... ")
      index <- 1
      while(file.exists(paste(.filename, ".", index, sep = "")))
        index <- index + 1
      backup.filename = paste(.filename, ".", index, sep = "")
      file.copy(.filename, backup.filename)
      r <- readLines(response$update, encoding="UTF-8", warn=FALSE)
      f <- file(.filename, encoding="UTF-8")
      writeLines(r, f)
      close.connection(f)
      cat("Previous file has been renamed to ", basename(backup.filename), ".\n", sep = "")
      cat("If the file did not refresh in your editor, close and reopen it.\n")
    }
    check$summarize()
  },
  error = function(r) {
    cat('Pri shranjevanju je prišlo do napake.\n')
    check$summarize()
    cat('Pri shranjevanju je prišlo do napake. Poskusite znova.\n')
  })
}

.check()

# ========================================================================
# Branje in obdelava podatkov 
#
# Podatke najpogosteje dobimo v razpredelnici kot tekstovno datoteko, kjer
# je vsaka vrstica razpredelnice v eni vrstici datoteke, celice pa so med
# seboj ločene z nekim ločilom. Formatu datoteke, kjer je to ločilo
# vejica, pravimo CSV (comma separated values). Ker pa v Evropi vejico
# uporabljamo za ločevanje celega dela števila od decimalk, za ta format
# namesto vejic uporabljamo tudi podpičje - na računalniku s slovenskimi
# nastavitvami bo tako Excel pri shranjevanju v CSV za ločilo uporabil
# prav podpičje.
# 
# Take datoteke bomo brali s funkcijami `read_*` iz knjižnice `readr`.
# Vse delujejo na enak način, razlikujejo se le v privzetih nastavitvah:
# 
# * `read_csv` prebere datoteko CSV, kjer je ločilo med celicami vejica;
# * `read_csv2` prebere datoteko CSV, kjer je ločilo med celicami podpičje;
# * `read_tsv` prebere datoteko, kjer je ločilo med celicami tabulator;
# * `read_delim` prebere datoteko, ločilo pa moramo podati s parametrom
#   `delim`.
# 
# Vse funkcije kot argument sprejmejo ime vhodne datoteke, vrnejo pa
# razpredelnico s prebranimi podatki. Poleg imena datoteke funkcije
# sprejmejo še ostale neobvezne argumente, kot so:
# 
# * `col_names`: `TRUE`, če naj se prva vrstica razume kot glava, torej
#   vsebuje imena stolpcev, sicer pa `FALSE` ali vektor z imeni stolpcev;
# * `na`: vektor nizov, ki naj se interpretirajo kot manjkajoč podatek (`NA`);
# * `comment`: niz, ki predstavlja začetek komentarja - podatki za tem nizom
#   bodo ignorirani;
# * `n_max`: največje število vrstic, ki naj se prebere (če ni podan, se
#   preberejo vse vrstice);
# * `skip`: število vrstic na začetku datoteke, ki naj se izpustijo;
# * `locale`: nastavitev lokalizacije (decimalna vejica, kodiranje znakov itd.)
#   z uporabo funkcije `locale`.
# 
# S spletne učilnice poberite datoteki
# [imena-moski.csv](http://ucilnica.fmf.uni-lj.si/mod/resource/view.php?id=8316)
# in
# [imena-zenske.csv](http://ucilnica.fmf.uni-lj.si/mod/resource/view.php?id=8317),
# ki vsebujeta podatke o pojavnosti moških oziroma ženskih imen v letih od
# 2008 do 2019
# ([vir](https://pxweb.stat.si/SiStatDb/pxweb/sl/10_Dem_soc/10_Dem_soc__05_prebivalstvo__46_Imena_priimki__06_05X10_imena_priimki/?tablelist=true)).
# Pri obeh se za ločilo med celicami uporablja vejica, prav tako imata
# obe na začetku tri vrstice, ki jih bomo izpustili. Pri obeh so imena
# vrstic v prvem stolpcu. Izpuščeni podatki (zaradi premajhne pojavnosti)
# so označeni z znakom `"-"`. Uporabljena kodna tabela je `Windows-1250`.
# Ker bomo želeli tabeli združiti, bomo posebej podali tudi imena
# stolpcev.
# 
#     library(readr)
#     M <- read_csv("imena-moski.csv", col_names=c("ime", 2008:2019),
#                   skip=3, na="-", locale=locale(encoding="Windows-1250"))
#     Z <- read_csv("imena-zenske.csv", col_names=c("ime", 2008:2019),
#                   skip=3, na="-", locale=locale(encoding="Windows-1250"))
# 
# Če želimo neko funkcijo uporabiti na vrsticah ali stolpcih razpredelnice,
# lahko to storimo s pomočjo funkcije `apply`. Če želimo dobiti število
# moških glede na leto, lahko to dobimo z
# 
#     apply(M[-1], 2, sum, na.rm=TRUE)
# 
# Poglejmo si, kaj smo tukaj naredili. Funkciji `apply` smo najprej podali
# razpredelnico `M` brez prvega stolpca (indeks -1). Nato smo podali
# številko dimenzije, po kateri bi radi delali - če bi želeli seštevati po
# vrsticah, bi tukaj napisali `1`, dimenzija `2` pa ustreza stolpcem
# razpredelnice. Nato smo podali funkcijo, ki naj se uporabi - v našem primeru
# na vsakem stolpcu razpredelnice. Ker pa funkcija `sum` naleti na manjkajoče
# vrednosti, je te potrebno izločiti (sicer bo vsaka vsota enaka `NA`).
# To storimo tako, da funkciji `sum` podamo parameter `na.rm=TRUE`, ki ga
# pa lahko podamo kar kot parameter funkcije `apply`. Podobno bi lahko
# naredili tako - tokrat za ženske:
# 
#     apply(Z[-1], 2, function(x) {sum(x, na.rm=TRUE)})
# 
# Za lažje delo s podatki želimo te imeti v obliki *tidy data*. Osnovna ideja
# te oblike je ta, da imamo za vsako "meritev" po eno vrstico v tabeli.
# Tako bomo navadno imeli enega ali več stolpcev, ki identificirajo meritev,
# poleg tega pa še en stolpec (izjemoma lahko tudi več) s samo vrednostjo
# meritve. Če bi v našem primeru npr. dodajali podatke o pogostosti imen v
# prihodnjih letih, bi torej morali dodati nove vrstice, medtem ko naj bi
# stolpci ostali vedno enaki.
# 
# Za pretvorbo v obliko *tidy data* bomo uporabili funkcijo `gather` iz
# knjižnice `tidyr`. Funkciji poleg razpredelnice podamo imena stolpcev, ki
# (delno) identificirajo meritve (ti se bodo ohranili), ali pa podamo negirana
# imena stolpcev, ki podajajo meritve pri različnih pogojih. S parametrom `key`
# podamo ime novega stolpca s pogoji, s parametrom `value` pa podamo ime novega
# stolpca z vrednostmi. Parameter `na.rm=TRUE` določa, naj se izpustijo
# vrstice, pri katerih je vrednost enaka `NA`.
# 
#     library(tidyr)
#     imena.moski <- gather(M, -ime, key=leto, value=stevilo, na.rm=TRUE)
#     imena.zenske <- gather(Z, -ime, key=leto, value=stevilo, na.rm=TRUE)
# 
# Dobili smo razpredelnici s stolpci `ime`, `leto` in `stevilo`, pri čemer so
# vrednosti v stolpcu `leto` kar enake imenom stolpcev v razpredelnicah `M` in
# `Z`. Te bomo tako pretvorili v števila, dodali pa bomo še stolpec za spol.
# 
#     imena.moski$leto <- parse_integer(imena.moski$leto)
#     imena.zenske$leto <- parse_integer(imena.zenske$leto)
#     imena.moski$spol <- factor("moski", levels=c("moski", "zenske"))
#     imena.zenske$spol <- factor("zenske", levels=c("moski", "zenske"))
# 
# Sedaj lahko združimo podatke iz obeh tabel v eno tako, da enostavno združimo
# vrstice obeh tabel. To storimo s funkcijo `rbind`:
# 
#     imena <- rbind(imena.moski, imena.zenske)
# 
# Za delo s podatki v obliki *tidy data* si lahko pomagamo s funkcijami iz
# knjižnice `dplyr`. Tako lahko filtriramo z uporabo funkcije `filter`:
# 
#     library(dplyr)
#     filter(imena, ime == "Bojan")
# 
# Če ne želimo izpisovati vseh stolpcev, lahko želene izberemo s funkcijo
# `select`. Pomagamo si lahko tudi z operatorjem `%>%`: če pišemo
# `x %>% f(y, z, ...)`, je to enako kot `f(x, y, z, ...)`.
# 
#     # vsa ženska imena, ki so se v letu 2013 pojavila največ petkrat
#     imena %>% filter(leto == 2013, spol == "zenske", stevilo <= 5) %>%
#       select(ime, stevilo)
# 
# Podatke lahko tudi grupiramo s funkcijama `group_by` in `summarise`, npr.
# 
#     # prebivalstvo po letih in spolu
#     imena %>% group_by(leto, spol) %>% summarise(prebivalstvo=sum(stevilo))
# 
# S funkcijo `arrange` lahko razvrstimo vrstice razpredelnice po določenem kriteriju.
# 
#     imena %>% filter(leto == 2019) %>% group_by(ime) %>%
#       summarise(stevilo=sum(stevilo)) %>% arrange(desc(stevilo)) %>%
#       head(20) # Kaj vrne ta poizvedba?
# ================================================================@021597=
# 1. podnaloga
# Napišite funkcijo `povprecje`, ki bo za dani vektor imen vrnila
# razpredelnico s stolpci `ime`, `spol` in `povprecje`, ki naj vsebuje
# povprečne pojavnosti imen skozi leta za moške in ženske posebej.
# Pomagajte si z operatorjem `%in%` za preverjanje vsebovanosti v vektorju.
# ========================================================================
library(readr)
library(tidyr)
library(dplyr)
M <- read_csv("imena-moski.csv", col_names=c("ime", 2008:2019),
              skip=3, na="-", locale=locale(encoding="Windows-1250"))
Z <- read_csv("imena-zenske.csv", col_names=c("ime", 2008:2019),
              skip=3, na="-", locale=locale(encoding="Windows-1250"))
imena.moski <- gather(M, -ime, key=leto, value=stevilo, na.rm=TRUE)
imena.zenske <- gather(Z, -ime, key=leto, value=stevilo, na.rm=TRUE)
imena.moski$leto <- parse_integer(imena.moski$leto)
imena.zenske$leto <- parse_integer(imena.zenske$leto)
imena.moski$spol <- factor("moski", levels=c("moski", "zenske"))
imena.zenske$spol <- factor("zenske", levels=c("moski", "zenske"))
imena <- rbind(imena.moski, imena.zenske)

povprecje <- function(names) {
  
  N <- length(names)
  averages <- rep(0, length = N)
  gends <- levels(imena)
  
  for (i in 1:N) {
    name <- names[i]
    records <- imena$stevilo[imena$ime == name]
    name.avg <- round(mean(records), digits = 5)
    
    averages[i] <- name.avg
    }
  
  return(averages)
}

# ================================================================@021598=
# 2. podnaloga
# Napišite funkcijo `prebivalstvo`, ki na podlagi podatkov v razpredelnici
# `imena` za dani vektor let izračuna skupno prebivalstvo Slovenije
# (ne glede na spol) za podana leta (privzeta vrednost naj bodo vsa leta,
# za katera imamo podatke, torej od 2008 do 2019).
# Funkcija naj vrne razpredelnico s stolpcema `leto` in `prebivalstvo`.
# ========================================================================

# ================================================================@021599=
# 3. podnaloga
# Ustvari razpredelnico `MZ` z enim samim stolpcem `ime`, ki vsebuje tista
# imena iz razpredelnice `imena`, ki se pojavijo tako za moške kot za ženske.
# Pomagaj si s funkcijo `n_distinct`, ki prešteje število različnih vrednosti
# v stolpcu.
# ========================================================================









































































































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
    check$parts[[check$part.counter]]$token <- 'eyJ1c2VyIjoyNjkwLCJwYXJ0IjoyMTU5N30:1j6IGb:y-BChFp24-G6xk1YY-9o_lkLw-w'
    tryCatch({
      check$equal(povprecje("Slovenko")$povprecje, 11.58333, precision=3)
      check$equal(povprecje("Alojzija Stanislava")$povprecje, 5)
      check$equal(sum(povprecje("Vanja")$povprecje), 2209.167, precision=3)
      check$equal("zenske" %in% povprecje("Jane")$spol, TRUE)
      check$equal("moski" %in% povprecje("Tilka")$spol, FALSE)
      check$equal(sum(povprecje(c("Tine", "Tone", "Jure", "Meta"))$povprecje),
                  9785.333, precision=3)
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
    body[[length(body) + 1]] <- check$parts[[check$part.counter]]
    indices <- c(indices, check$part.counter)
  }
  
  if (check$part()) {
    check$parts[[check$part.counter]]$token <- 'eyJ1c2VyIjoyNjkwLCJwYXJ0IjoyMTU5OH0:1j6IGb:dvPr4FV1cfKC1xqK9e3l8rPTLA8'
    tryCatch({
      check$equal(ncol(prebivalstvo()), 2)
      check$equal(nrow(prebivalstvo()), 12)
      check$equal(prebivalstvo("2019")$prebivalstvo, 2017391)
      check$equal(prebivalstvo("2013")$prebivalstvo, 2001129)
      check$equal(prebivalstvo("2008")$prebivalstvo, 1975621)
      check$equal(sum(prebivalstvo(seq(2009, 2018, 3))$prebivalstvo), 7987823)
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
    body[[length(body) + 1]] <- check$parts[[check$part.counter]]
    indices <- c(indices, check$part.counter)
  }
  
  if (check$part()) {
    check$parts[[check$part.counter]]$token <- 'eyJ1c2VyIjoyNjkwLCJwYXJ0IjoyMTU5OX0:1j6IGb:uv71NPqKcLNyT-jCmAOSn4k9fn8'
    tryCatch({
      check$equal(ncol(MZ), 1)
      check$equal(nrow(MZ), 117)
      check$equal("Nastja" %in% MZ$ime, TRUE)
      check$equal("Denis" %in% MZ$ime, TRUE)
      check$equal("Tilka" %in% MZ$ime, FALSE)
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

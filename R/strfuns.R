Unicode_alpha_tokenizer <-
function(x)
{
    x <- .str_to_u_char(paste(x, collapse = "\n"))
    gc <- u_char_property(x, "General_Category")
    x[substring(gc, 1L, 1L) != "L"] <- 32L
    unlist(strsplit(intToUtf8(x), " +"))
}

u_tolower <-
function(x)
{
    y <- lapply(x,
                function(s)
                intToUtf8(u_char_tolower(.str_to_u_char(s))))
    as.character(unlist(y))
}

u_toupper <-
function(x)
{
    y <- lapply(x,
                function(s)
                intToUtf8(u_char_toupper(.str_to_u_char(s))))
    as.character(unlist(y))
}

.str_to_u_char <-
function(x)
{
    x <- as.character(x)
    if(length(x) > 1L) {
        if(any(nchar(x) != 1L))
            stop("Invalid 'x'.")
        x <- paste(x, collapse = "")
    }
    if(Encoding(x) == "latin1")
        x <- iconv(x, from = "latin1", to = "UTF-8")
    x <- if(is.na(x))
        NA_integer_
    else
        utf8ToInt(x)
    as.u_char(x)
}

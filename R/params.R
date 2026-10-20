# Loads params.yaml (the single parameter register). Use par_get("section", "name").
PARAMS <- yaml::read_yaml("params.yaml")
par_get <- function(...) {
  x <- PARAMS
  for (k in c(...)) x <- x[[k]]
  if (is.list(x) && !is.null(x$value)) x$value else x
}

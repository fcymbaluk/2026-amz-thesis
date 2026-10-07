# interpolate_census.R
# Purpose  Fill annual series between census anchors. interp_linear() is the
#          linear interpolation inside the anchor range (NA outside);
#          extrapolate_linear() extends a filled series beyond its first and
#          last non-missing years with the slope of the adjacent year pair.
# Inputs   A numeric vector `x` and an integer vector `year` of equal length,
#          already sorted by year within one municipality.
# Outputs  A numeric vector of the same length. Definitions only; sourced by
#          scripts 02 (census) and 03 (population).
# Status   Phase 1, script 02 (2026-10-07). Numerically identical to the
#          zoo::na.approx(x, year, na.rm = FALSE) call of the lost scripts.

interp_linear <- function(x, year) {
  ok <- !is.na(x)
  if (sum(ok) < 2L) return(x)
  stats::approx(year[ok], x[ok], xout = year, rule = 1)$y
}

extrapolate_linear <- function(x, year) {
  ok <- which(!is.na(x))
  if (length(ok) < 2L) return(x)
  first <- ok[1]
  last  <- ok[length(ok)]
  slope_start <- (x[ok[2]] - x[first]) / (year[ok[2]] - year[first])
  slope_end   <- (x[last] - x[ok[length(ok) - 1]]) /
    (year[last] - year[ok[length(ok) - 1]])
  out <- x
  before <- year < year[first]
  after  <- year > year[last]
  out[before] <- x[first] + slope_start * (year[before] - year[first])
  out[after]  <- x[last] + slope_end * (year[after] - year[last])
  out
}

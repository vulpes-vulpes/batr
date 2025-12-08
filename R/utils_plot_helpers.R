#' Adaptive date breaks and label cleaning helpers
#' @keywords internal
.adaptive_date_breaks <- function(min_date, max_date, date_label = "%b") {
    span_days <- as.integer(max_date - min_date)

    breaks <- if (span_days <= 14) {
        seq(min_date, max_date, by = "2 days")
    } else if (span_days <= 90) {
        seq(min_date, max_date, by = "1 week")
    } else if (span_days <= 540) {
        seq(min_date, max_date, by = "1 month")
    } else if (span_days <= 1095) {
        seq(min_date, max_date, by = "3 months")
    } else {
        seq(min_date, max_date, by = "6 months")
    }

    if (length(breaks) == 0) {
        breaks <- c(min_date, max_date)
    }

    label_format <- if (identical(date_label, "%b")) {
        if (span_days <= 90) {
            "%d %b"
        } else {
            "%b %Y"
        }
    } else {
        date_label
    }

    list(
        breaks = breaks,
        labels = scales::label_date(label_format)
    )
}

#' Clean location labels for display
#' @keywords internal
.clean_location_label <- function(x) {
    gsub("_", " ", x)
}

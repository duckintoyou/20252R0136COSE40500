str(books)
head(books)
autoplot(books)

ses_p <- ses(books[, "Paperback"], h = 4)
ses_h <- ses(books[, "Hardcover"], h = 4)

autoplot(books[, "Paperback"], series = "Paperback") +
  autolayer(ses_p, series = "Paperback") +
  autolayer(books[, "Hardcover"], series = "Hardcover") +
  autolayer(ses_h, series = "Hardcover", PI = FALSE) +
  ylab("amount")

sqrt(mean(ses_p$residuals^2))
sqrt(mean(ses_h$residuals^2))


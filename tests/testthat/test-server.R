# test_that("server calls appropriate server modules and outputs what it should", {
  
  testServer(
    app = server,
    args = list(),
    exp = {
     browser()
    }
  )
# })

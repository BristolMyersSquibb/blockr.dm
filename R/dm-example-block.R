#' DM Example Block
#'
#' A data block that provides ready-to-use dm objects from a built-in catalog
#' of example datasets. The dropdown shows only datasets whose required
#' packages are installed. A built-in BI star schema is always available.
#'
#' To add a new dataset, add an expression builder function (e.g.,
#' `my_data_expr()`) and wire it into [dm_example_choices()] and
#' [dm_example_expr()].
#'
#' @param dataset Character, the ID of the dm example to load. Defaults to
#'   `"bi_star_schema"` (always available). See [dm_example_choices()].
#' @param ... Forwarded to [blockr.core::new_data_block()]
#'
#' @return A data block of class `c("dm_example_block", "dm_block")`.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dm)
#'   serve(new_dm_example_block())
#' }
#'
#' @export
new_dm_example_block <- function(dataset = "bi_star_schema", ...) {

  blockr.core::new_data_block(
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        dat <- shiny::reactiveVal(dataset)

        shiny::observeEvent(
          shiny::req(input$dataset),
          dat(input$dataset)
        )

        shiny::observeEvent(
          shiny::req(dat()),
          {
            if (!identical(dat(), input$dataset)) {
              shiny::updateSelectInput(
                session, "dataset",
                choices = dm_example_choices(),
                selected = dat()
              )
            }
          }
        )

        list(
          expr = shiny::reactive({
            selected <- dat()
            shiny::req(nzchar(selected))
            dm_example_expr(selected)
          }),
          state = list(
            dataset = dat
          )
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::selectInput(
        inputId = ns("dataset"),
        label = "DM Dataset",
        choices = dm_example_choices(),
        selected = dataset
      )
    },
    allow_empty_state = TRUE,
    class = c("dm_example_block", "dm_block"),
    external_ctrl = "dataset",
    ...
  )
}

#' @method block_output dm_example_block
#' @export
block_output.dm_example_block <- function(x, result, session) {
  block_output.dm_block(x, result, session)
}

#' @method block_ui dm_example_block
#' @export
block_ui.dm_example_block <- function(id, x, ...) {
  block_ui.dm_block(id, x, ...)
}

#' @method block_render_trigger dm_example_block
#' @export
block_render_trigger.dm_example_block <- function(x, session = blockr.core::get_session()) {
  NULL
}

# -- Choices & dispatch -------------------------------------------------------

#' List available dm example datasets
#'
#' Returns a named character vector (id -> display name) of dm example datasets
#' whose required packages are installed.
#'
#' @return Named character vector.
#' @keywords internal
dm_example_choices <- function() {
  # selectInput uses names as display labels, values as what's sent to server
  choices <- c("BI Star Schema" = "bi_star_schema")

  if (requireNamespace("safetyData", quietly = TRUE)) {
    choices[["Safety ADaM (safetyData)"]] <- "safetydata_adam"
  }
  if (requireNamespace("pharmaverseadam", quietly = TRUE)) {
    choices[["Pharmaverse ADaM (pharmaverseadam)"]] <- "pharmaverseadam"
  }
  if (requireNamespace("nycflights13", quietly = TRUE)) {
    choices[["NYC Flights (nycflights13)"]] <- "nycflights13"
  }
  if (requireNamespace("insuranceData", quietly = TRUE)) {
    choices[["Insurance (insuranceData)"]] <- "insurancedata"
  }

  choices
}

#' Get a quoted expression for a dm example
#'
#' @param id Character, the dataset ID.
#' @return A quoted R expression that evaluates to a `dm` object.
#' @keywords internal
dm_example_expr <- function(id) {
  switch(id,
    bi_star_schema = bi_star_schema_expr(),
    safetydata_adam = safetydata_adam_expr(),
    pharmaverseadam = pharmaverseadam_expr(),
    nycflights13 = nycflights13_expr(),
    insurancedata = insurancedata_expr(),
    stop("Unknown dm example: ", id)
  )
}

# -- Expression builders ------------------------------------------------------

bi_star_schema_expr <- function() {
  quote(local({
    categories <- data.frame(
      category_id = 1:5,
      category_name = c("Electronics", "Clothing", "Food", "Books", "Sports"),
      stringsAsFactors = FALSE
    )

    products <- data.frame(
      product_id = 1:12,
      product_name = c(
        "Laptop", "Phone", "Tablet",
        "T-Shirt", "Jacket", "Sneakers",
        "Coffee", "Bread", "Cheese",
        "Novel", "Textbook",
        "Basketball"
      ),
      category_id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 5L),
      unit_price = c(999, 699, 449, 25, 89, 120, 12, 4, 8, 15, 65, 30),
      stringsAsFactors = FALSE
    )

    customers <- data.frame(
      customer_id = 1:8,
      name = c("Alice", "Bob", "Carol", "David",
               "Eve", "Frank", "Grace", "Hank"),
      city = c("New York", "Chicago", "Boston", "Denver",
               "Seattle", "Miami", "Portland", "Austin"),
      segment = c("Consumer", "Business", "Consumer", "Enterprise",
                   "Consumer", "Business", "Enterprise", "Consumer"),
      stringsAsFactors = FALSE
    )

    set.seed(42)
    n_orders <- 45L
    orders <- data.frame(
      order_id = seq_len(n_orders),
      customer_id = sample(1:8, n_orders, replace = TRUE),
      product_id = sample(1:12, n_orders, replace = TRUE),
      quantity = sample(1:5, n_orders, replace = TRUE),
      order_date = as.Date("2024-01-01") + sample(0:364, n_orders, replace = TRUE),
      stringsAsFactors = FALSE
    )
    orders$amount <- orders$quantity * products$unit_price[orders$product_id]

    result <- dm::dm(
      categories = categories,
      products = products,
      customers = customers,
      orders = orders
    )
    result <- dm::dm_add_pk(result, categories, category_id)
    result <- dm::dm_add_pk(result, products, product_id)
    result <- dm::dm_add_pk(result, customers, customer_id)
    result <- dm::dm_add_pk(result, orders, order_id)
    result <- dm::dm_add_fk(result, products, category_id, categories)
    result <- dm::dm_add_fk(result, orders, customer_id, customers)
    result <- dm::dm_add_fk(result, orders, product_id, products)
    result
  }))
}

safetydata_adam_expr <- function() {
  quote(local({
    adsl     <- safetyData::adam_adsl
    adae     <- safetyData::adam_adae
    adlbc    <- safetyData::adam_adlbc
    adlbh    <- safetyData::adam_adlbh
    adlbhy   <- safetyData::adam_adlbhy
    adqsadas <- safetyData::adam_adqsadas
    adqscibc <- safetyData::adam_adqscibc
    adqsnpix <- safetyData::adam_adqsnpix
    adtte    <- safetyData::adam_adtte
    advs     <- safetyData::adam_advs

    result <- dm::dm(
      adsl = adsl, adae = adae, adlbc = adlbc, adlbh = adlbh,
      adlbhy = adlbhy, adqsadas = adqsadas, adqscibc = adqscibc,
      adqsnpix = adqsnpix, adtte = adtte, advs = advs
    )
    result <- dm::dm_add_pk(result, adsl, USUBJID)
    result <- dm::dm_add_fk(result, adae, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adlbc, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adlbh, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adlbhy, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adqsadas, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adqscibc, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adqsnpix, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adtte, USUBJID, adsl)
    result <- dm::dm_add_fk(result, advs, USUBJID, adsl)
    result
  }))
}

pharmaverseadam_expr <- function() {
  quote(local({
    adsl <- pharmaverseadam::adsl
    adae <- pharmaverseadam::adae
    adlb <- pharmaverseadam::adlb
    advs <- pharmaverseadam::advs
    adcm <- pharmaverseadam::adcm

    result <- dm::dm(
      adsl = adsl, adae = adae, adlb = adlb, advs = advs, adcm = adcm
    )
    result <- dm::dm_add_pk(result, adsl, USUBJID)
    result <- dm::dm_add_fk(result, adae, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adlb, USUBJID, adsl)
    result <- dm::dm_add_fk(result, advs, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adcm, USUBJID, adsl)
    result
  }))
}

nycflights13_expr <- function() {
  quote(dm::dm_nycflights13(cycle = FALSE))
}

insurancedata_expr <- function() {
  quote(local({
    load_ins <- function(name) {
      env <- new.env(parent = emptyenv())
      utils::data(list = name, package = "insuranceData", envir = env)
      env[[name]]
    }
    dm::dm(
      dataCar = load_ins("dataCar"),
      dataOhlsson = load_ins("dataOhlsson"),
      AutoClaims = load_ins("AutoClaims"),
      AutoBi = load_ins("AutoBi"),
      AutoCollision = load_ins("AutoCollision"),
      SingaporeAuto = load_ins("SingaporeAuto"),
      ClaimsLong = load_ins("ClaimsLong"),
      IndustryAuto = load_ins("IndustryAuto"),
      WorkersComp = load_ins("WorkersComp")
    )
  }))
}

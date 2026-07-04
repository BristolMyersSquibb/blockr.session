# Workflow pins on a board, excluding the internal blockr-workflow-index pin, so
# assertions can count workflows without tripping over the index artifact.
workflow_pins <- function(backend) {
  setdiff(pins::pin_list(backend), index_pin_name())
}

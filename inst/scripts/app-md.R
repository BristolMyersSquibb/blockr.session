library(blockr.md)
library(blockr.core)
library(blockr.ai)

options(
  blockr.session_mgmt_backend = pins::board_connect()
)

serve(
  new_md_board(
    blocks = c(
      a = new_dataset_block("mtcars"),
      b = new_scatter_block("disp", "hp"),
      c = new_llm_insights_block("List the 5 most powerful cars.")
    ),
    links = list(from = c("a", "a"), to = c("b", "c"), input = rep("data", 2)),
    document = c(
      "# My title",
      "",
      "## Slide with table",
      "",
      "![](blockr://a)",
      "",
      "## Slide with plot",
      "",
      "![Displacement (cu.in.) vs. gross horsepower](blockr://b)",
      "",
      "## Slide with AI text",
      "",
      "![](blockr://c)",
      "",
      "## Slide with MD text",
      "",
      "Some paragraph text.",
      "",
      "- bullet 1",
      "- bullet 2",
      "",
      "That's it, that's all."
    )
  )
)

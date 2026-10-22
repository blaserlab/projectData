# Manual reproduction: does RStudio force projectData's deferred bindings?
#
# Run this script line by line in an interactive RStudio session. It registers
# the always-installed 'datasets' package (no data are read at registration),
# attaches the facade, and logs every time projectData actually opens a
# resource. Compare the log after each IDE action.
#
# projectData makes NO claim to prevent IDE-induced loading. Active bindings
# and promises can both be evaluated by completion, the Environment pane, or
# object previews. If you see log lines you did not trigger, that is the IDE.

library(projectData)

log_file <- file.path(tempdir(), "projectData-open.log")
unlink(log_file)
data_config(
  messages = FALSE,
  on_open = function(info) {
    cat(format(Sys.time(), "%H:%M:%OS3"), info$collection, info$resource, "\n",
        file = log_file, append = TRUE)
  }
)

data_register("datasets", include = c("mtcars", "iris"))
data_attach()

show_log <- function() {
  if (file.exists(log_file)) cat(readLines(log_file), sep = "\n") else cat("(no opens logged)\n")
}

# Step 1: expect "(no opens logged)". Registration and attachment read nothing.
show_log()

# Step 2: in the console type `mtc` and press Tab (completion), then `mtcars$`
# and press Tab. Then run show_log() again and note whether mtcars was opened.
# show_log()

# Step 3: open the Environment pane and select "projectData:data" in its
# environment drop-down. Run show_log() again.
# show_log()

# Step 4: explicit access never depends on the IDE. This is the robust option
# for scripts: no attachment, no active binding on the search path.
x <- data_get("mtcars")
show_log()

# Clean up
data_detach()
data_unregister("datasets")

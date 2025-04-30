tempconfig <- tempfile()
withr::local_envvar(
  GIT_CONFIG_GLOBAL = tempconfig,
  .local_envir = teardown_env()
)
withr::defer(unlink(tempconfig), envir = teardown_env())

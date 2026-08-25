import Config

config :manual_web,
  port: 4001,
  start_server: true

import_config "#{config_env()}.exs"

defmodule ManualWeb.Application do
  @moduledoc false

  use Application

  @impl Application
  def start(_type, _args) do
    children =
      case Application.fetch_env!(:manual_web, :start_server) do
        true ->
          [
            {Bandit,
             plug: ManualWeb.Router,
             scheme: :http,
             ip: {127, 0, 0, 1},
             port: Application.fetch_env!(:manual_web, :port)}
          ]

        false ->
          []
      end

    Supervisor.start_link(children, strategy: :one_for_one, name: ManualWeb.Supervisor)
  end
end

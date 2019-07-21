defmodule MyApp do
  use Application

  def start(_Type, _Args) do
    children = [
      {MyClient, [name: MyClient]}
    ]
    Supervisor.start_link(children, [strategy: :one_for_one, name: __MODULE__])
  end
  
  def stop(_State) do
    :ok
  end

end
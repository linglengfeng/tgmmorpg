defmodule MyClient do
  use GenServer
  require Logger

  def start_link(opts) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def init(_) do
    connect_socket = connect()
    {:ok, %{connect_socket: connect_socket}}
  end

  def cast(request) do
    GenServer.cast(__MODULE__, request)
  end

  def call(request) do
    GenServer.call(__MODULE__, request)
  end

  def req_info(request, delay \\ 0) do
    Process.send_after(__MODULE__, request, delay)
  end

  def handle_cast({:send, msg}, %{connect_socket: connect_socket} = state) do
    case :gen_tcp.send(connect_socket, msg) do
      :ok ->
        {:noreply, state}

      err ->
        Logger.debug(fn -> "gen_tcp send err:#{inspect err}\n, 
          tcp_connect, try again" end)
        tcp_connect()
        {:noreply, state |> Map.delete(:connect_socket)}
    end
  end

  def handle_cast(msg, state) do
    Logger.debug(fn -> "handle_cast msg:#{inspect msg}\n" end)
    {:noreply, state}
  end

  def handle_call(msg, _from, state) do
    Logger.debug(fn -> "handle_call msg:#{inspect msg}\n" end)
    {:noreply, state}
  end

  def handle_info({:tcp_connect}, state) do
    {:noreply, state |> Map.merge(%{connect_socket: connect()})}
  end

  def handle_info({:tcp_connect, ip, port}, state) do
    {:noreply, state |> Map.merge(%{connect_socket: connect(ip, port)})}
  end

  def handle_info({:tcp, _socket, msg}, %{connect_socket: _connect_socket} = state) do
    msg = decode(msg)
    Logger.debug(fn -> "receive msg:\n #{inspect msg}\n" end)
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, state) do
    :gen_tcp.close(socket)
    {:noreply, state}
  end

  def handle_info({:error, socket, reason}, state) do
    :gen_tcp.close(socket)
    Logger.debug(fn -> "receive msg error reason:#{inspect reason}\n" end)
    {:noreply, state}
  end

  def handle_info({:closed}, %{connect_socket: connect_socket} = state) do
    :gen_tcp.close(connect_socket)
    {:noreply, state}
  end

  def handle_info(msg, state) do
    Logger.debug(fn -> "handle_info msg:#{inspect msg}\n" end)
    {:noreply, state}
  end

  def terminate(state) do# 终止
      state
  end

  # MyClient.send(Test, :test0, [1,2,3])
  def send(mod, func, args \\ []) do
    msg = encode(mod, func, args)
    cast({:send, msg})
  end

  def tcp_connect() do
    # close()
    req_info({:tcp_connect}, 0)
  end

  def tcp_connect(ip, port) do
    # close()
    req_info({:tcp_connect, ip, port}, 0)
  end

  def close(delay \\ 0) do
    req_info({:closed}, delay)
  end

  defp encode(mod, func, args) do
    [mod, func, args] |> :erlang.term_to_binary()
  end

  defp decode(msg) do
    :erlang.binary_to_term(msg)
  end

  defp connect() do
    ip = Application.get_env(:my_client, :ip, {0, 0, 0, 0})
    port = Application.get_env(:my_client, :port, 1111)
    {:ok, connect_socket} = :gen_tcp.connect(ip, port, [:binary, {:packet, 0}])
    Logger.debug(fn -> "connect_socket:#{inspect connect_socket}" end)
    connect_socket
  end

  defp connect(ip, port) do
    {:ok, connect_socket} = :gen_tcp.connect(ip, port, [:binary, {:packet, 0}])
    Logger.debug(fn -> "connect_socket:#{inspect connect_socket}" end)
    connect_socket
  end

end

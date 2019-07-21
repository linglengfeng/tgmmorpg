defmodule Msg do

  # Msg.send(Test, :positive_add, [1,2])
  def send(mod, func, args \\ []) do
    is_list(args) && args || List.wrap(args)
    MyClient.send(mod, func, args)
  end

  # MyClient.tcp_connect()
  # Msg.send(:login, :player, [123]) 
  # Msg.send(Chat, :send, ["你好 世界"]) 
  # Msg.send(Chat, :join, [1]) 
  # Msg.send(Chat, :send, [{:hello, :world}])
end
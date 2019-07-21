目录结构
  my_client(mix构建)
  my_server | config(其他配置文件) | sys.config(outside_service配置， 现易变动的env在此处)
            | include(头文件) | common.hrl(公共头文件) (DEBUG待改)
            | src(源文件) | lib | db(数据库) | db_mysql
                                                      | db_mysql.erl(mysql接口)
                                                      | db_role.erl(人物数据存储接口)
                                            | db_mongo
                                                      | mongo_agent.erl(mongo代理)
                                                      | mongo_sup.erl(mongo监督者)
                                                      | mongo_test.erl(测试模块)
                                                      | mongo.erl(人物数据存储接口)
                                | roles(角色相关) | role_data.erl(角色数据)
                                                  | role.erl(角色进程)
                                                  | roles_sup(角色进程监督者)
                                | account.erl(账户)
                                | date.erl(日期时间处理)
                                | guid.erl(唯一id处理)
                                | login_pool.erl(注册连接的角色进程和接收进程) --- (待改,考虑写不写注册上限和登录排队)
                                | reloader.erl(重载文件)
                                | router.erl(消息路由)
                                | session.erl(socket连接) --- (用的ranch1.4可以改成ranch1.7)
                                | test.erl(一些测试函数)
                                | utils(小工具函数)
                          | my_server_app.erl
                          | my_server_sup.erl
                          | my_server.app.src(app cfg)



my_client 说明
ex官网 https://elixir-lang.org/docs.html
mix文档(项目构建) https://hexdocs.pm/mix/Mix.html
ex函数文档 https://hexdocs.pm/elixir/Kernel.html
my_client command
改config.exs的ip为你的ip
启动iex -S mix
shell里面
  Msg.send(mod, func, args)发送消息,mod接收模块，func方法名，args参数（list）
example
  Msg.send(:login, :role, [123])
  Msg.send(:test, :test, [1, 2]) 

my_server 说明
返回值有3种
  ok，不做任何操作
  {notify, Msg, Changed}, 通知客户端Msg消息，Changed会被merge进玩家数据
  {notify, Msg}，仅仅通知客户端Msg消息
  Msg格式：。。。。。具体格式不固定，可与客户端自行约定
  Changed：是一个maps就行，不是则当次操作不做任何操作,相当于返回ok


mysql 说明
  创建数据库：create DATABASE tgmmorpg_1; (tgmmorpg_1是temmorpg_serverid格式，serverid为env中的server_id)
  查看所有数据库：show databases;
  使用数据库： use tgmmorpg_1;
  查看当前连接数据库：select database();
  查看所有tmmmorpg的表：show tables;
  创建表: create table test;
  查看表结构：desc test;
  ** exception throw: {auth_fail,
                        {error_packet,2,1251,<<"08004">>,
                            "Client does not support authentication protocol requested by server; consider upgrading MySQL client"}}
     in function  emysql:add_pool/1 (f:/erlang/erl/tg/tgmmorpg/my_server/_build/default/lib/emysql/src/emysql.erl, line 317)
  解决 ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY '123456';
  没创建数据库的话去创建下 create DATABASE tgmmorpg;
  删除数据库 drop database tgmmorpg;
  删除表 drop table role;
  or     drop table tgmmorpg.role;
  mysql -u root -p123456

mongodb 说明
  菜鸟：https://www.runoob.com/mongodb/mongodb-databases-documents-collections.html
  git下载安装adminmongo: https://github.com/mrvautin/adminMongo
  启动MongoDB服务net start MongoDB
  关闭MongoDB服务net stop MongoDB
  cd D:\mongodb-win32-x86_64-2008plus-ssl-4.0.10\mongodb-win32-x86_64-2008plus-ssl-4.0.10\bin
  在你自己的mondb的bin目录下打开shell输入：mongo
  mongodb可视化界面：https://robomongo.org/download

  show dbs 查看所有数据库
  use runoob



遇到过的神奇问题
  % {emysql, "0.5.0", {pkg, inaka_emysql}}
  {emysql, ".*", {git, "https://github.com/inaka/Emysql", {tag, "0.5.0"}}}
  依赖添加emysql按照上面的方式编译失败，拉下来的东西不全，改为git地址就对了


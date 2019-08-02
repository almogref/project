%%%-------------------------------------------------------------------
%%% @author almog
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Aug 2019 11:09
%%%-------------------------------------------------------------------
-module(graphic_server).
-author("almog").

-define(OfekIp,ofek).

-export([start/0,loop/0,sendData/0]).



start() ->
  % Initiallizing the main node of the processes

  %rpc:call(?OfekIp, ofek_server, start, []),
  spawn(graphic_server,loop,[]).



loop() ->
  %% Call Ofek and get the data
  List1 = [{{6,6},0,1},{{16,16},0,2},{{26,26},0,3}],
  gen_server:call(landscape,{data,List1}),
  gen_server:call(landscape,{frame}),
  timer:sleep(40),
  List2 = [{{26,26},0,1},{{36,36},0,2},{{46,46},0,3}],
  gen_server:call(landscape,{data,List2}),
  gen_server:call(landscape,{frame}),
  timer:sleep(40),
  List3 = [{{46,46},0,1},{{56,56},0,2},{{66,66},0,3}],
  gen_server:call(landscape,{data,List3}),
  gen_server:call(landscape,{frame}),
  List4 = [{{66,66},0,1},{{76,76},0,2},{{86,86},0,3}],
  gen_server:call(landscape,{data,List4}),
  gen_server:call(landscape,{frame}),
  timer:sleep(40),
  List5 = [{{86,86},0,1},{{96,96},0,2},{{106,106},0,3}],
  gen_server:call(landscape,{data,List5}),
  gen_server:call(landscape,{frame}),
  timer:sleep(40),
  List6 = [{{96,96},0,1},{{106,106},0,2},{{116,116},0,3}],
  gen_server:call(landscape,{data,List6}),
  gen_server:call(landscape,{frame}),
  List7 = [{{106,106},0,1},{{116,116},0,2},{{126,126},0,3}],
  gen_server:call(landscape,{data,List7}),
  gen_server:call(landscape,{frame}),
  timer:sleep(40),
  List8 = [{{116,116},0,1},{{126,126},0,2},{{136,136},0,3}],
  gen_server:call(landscape,{data,List8}),
  gen_server:call(landscape,{frame}),
  timer:sleep(40),
  List9 = [{{126,126},0,1},{{136,136},0,2},{{146,146},0,3}],
  gen_server:call(landscape,{data,List9}),
  gen_server:call(landscape,{frame}).



%%sendData() ->
%%  rpc:call(?OfekIp, ofek_server, call, [data]).
%%
%%
%%



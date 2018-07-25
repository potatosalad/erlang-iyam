%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2013-2018, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  24 Jul 2018 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(zcsd_event_manager).

%% API
-export([start_link/0]).
-export([add_handler/2]).
-export([sync_add_handler/2]).
-export([sync_add_handler/3]).
% -export([node_add/2]).
% -export([node_del/2]).
% -export([node_set/2]).
% -export([ring_add/1]).
% -export([ring_del/1]).
% -export([ring_refresh/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Pid) ->
	gen_event:add_handler(?MODULE, Handler, Pid).

sync_add_handler(Handler, Pid) ->
	sync_add_handler(Handler, Pid, 5000).

sync_add_handler(Handler, Pid, Timeout) when Timeout =:= infinity orelse (is_integer(Timeout) andalso Timeout >= 0) ->
	To = erlang:self(),
	Tag = erlang:make_ref(),
	From = {To, Tag},
	ok = add_handler(Handler, {From, Pid}),
	receive
		{Tag, Result} ->
			Result
	after
		Timeout ->
			{error, timeout}
	end.

% node_add(RingName, Node) ->
% 	notify({node, add, RingName, Node}).

% node_del(RingName, NodeObject) ->
% 	notify({node, del, RingName, NodeObject}).

% node_set(RingName, Node) ->
% 	notify({ring, set, RingName, Node}).

% ring_add(Ring) ->
% 	notify({ring, add, Ring}).

% ring_del(RingName) ->
% 	notify({ring, del, RingName}).

% ring_refresh(Ring) ->
% 	notify({ring, refresh, Ring}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

% %% @private
% notify(Message) ->
% 	gen_event:notify(?MODULE, Message).

%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2013-2018, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  23 Jul 2018 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(zcsd_event_forwarder).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1]).
-export([handle_call/2]).
-export([handle_event/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%% @private
init({{To, Tag}, Pid}) when is_pid(To) andalso is_reference(Tag) andalso is_pid(Pid) ->
	catch To ! {Tag, {ok, erlang:self()}},
	init(Pid);
init(Pid) when is_pid(Pid) ->
	Ref = erlang:monitor(process, Pid),
	{ok, {Ref, Pid}}.

%% @private
handle_call(_Request, State) ->
	{ok, ok, State}.

%% @private
handle_event(Event, State = {_Ref, Pid}) ->
	catch Pid ! {'$zcsd', Event},
	{ok, State}.

%% @private
handle_info({'EXIT', _Parent, shutdown}, _State = {Ref, _Pid}) ->
	_ = erlang:demonitor(Ref, [flush]),
	remove_handler;
handle_info({'DOWN', Ref, process, Pid, _Reason}, _State = {Ref, Pid}) ->
	remove_handler;
handle_info(_Info, State) ->
	{ok, State}.

%% @private
terminate(_Reason, _Pid) ->
	ok.

%% @private
code_change(_OldVsn, Pid, _Extra) ->
	{ok, Pid}.

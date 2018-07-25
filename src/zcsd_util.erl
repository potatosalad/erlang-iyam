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
-module(zcsd_util).

%% API
-export([any_to_bitstring/1]).
-export([any_to_charlist/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Converts many common terms to a binary.
-spec any_to_bitstring(integer() | binary() | atom() | iodata()) -> binary().
any_to_bitstring(B) when is_binary(B) ->
	B;
any_to_bitstring(T) ->
	erlang:iolist_to_binary(any_to_charlist(T)).

%% @doc Converts many common terms to a charlist.
-spec any_to_charlist(integer() | binary() | atom() | iodata()) -> string().
any_to_charlist(I) when is_integer(I) ->
	erlang:integer_to_list(I);
any_to_charlist(B) when is_binary(B) ->
	erlang:binary_to_list(B);
any_to_charlist(A) when is_atom(A) ->
	erlang:atom_to_list(A);
any_to_charlist(L) when is_list(L) ->
	any_to_charlist(erlang:iolist_to_binary(L)).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

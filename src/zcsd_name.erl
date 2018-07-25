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
-module(zcsd_name).

%% API
-export([key_decode/1]).
-export([key_encode/1]).
-export([is_valid_label/1]).
-export([join/1]).
-export([reverse/1]).
-export([split/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Converts PTR, SRV, and KEY into {Domain, Type, Service, Instance}.
-spec key_decode(#{'PTR' := binary(), 'SRV' := binary(), 'KEY' := binary()})
	-> {binary(), binary(), binary(), binary()}.
key_decode(Arg = #{'PTR' := PTR, 'SRV' := SRV, 'KEY' := KEY})
		when is_binary(PTR)
		andalso is_binary(SRV)
		andalso is_binary(KEY)
		andalso byte_size(KEY) =:= byte_size(SRV)
		andalso byte_size(PTR) > 0
		andalso byte_size(PTR) < byte_size(SRV)
		andalso << $., PTR/binary >> =:= binary_part(SRV, byte_size(SRV) - byte_size(PTR) - 1, byte_size(PTR) + 1) ->
	Encoded = binary:part(SRV, 0, byte_size(SRV) - byte_size(PTR) - 1),
	Instance = zcsd_rfc3986:urldecode(Encoded),
	case split(PTR) of
		[<< $_, Service/binary >>, << $_, Type/binary >> | DomainLabels] ->
			Domain = join(DomainLabels),
			{Domain, Type, Service, Instance};
		_ ->
			erlang:error(badarg, [Arg])
	end.

%% @doc Converts {Domain, Type, Service, Instance} into PTR, SRV, and KEY.
-spec key_encode({binary(), binary(), binary(), binary()})
	-> #{'PTR' := binary(), 'SRV' := binary(), 'KEY' := binary()}.
key_encode({Domain, Type, Service, Instance})
		when is_binary(Domain)
		andalso is_binary(Type)
		andalso is_binary(Service)
		andalso is_binary(Instance) ->
	case lists:all(fun is_valid_label/1, [Service, Type | split(Domain)]) of
		true ->
			Encoded = zcsd_rfc3986:urlencode(Instance),
			PTR = join([<< $_, Service/binary >>, << $_, Type/binary >>, Domain]),
			SRV = << Encoded/binary, $., PTR/binary >>,
			KEY = << (reverse(PTR))/binary, $., Encoded/binary >>,
			#{
				'PTR' => PTR,
				'SRV' => SRV,
				'KEY' => KEY
			};
		false ->
			erlang:error(badarg, [{Domain, Type, Service, Instance}])
	end.

%% @doc Validates whether label is a valid hostname label string().
%% Must contain: lowercase a-z, 0-9, and hyphen (-).
%% Must NOT start or end with hyphen (-).
%% Must be at least 1 byte in length.
%% Must NOT be longer than 63 bytes.
%% [RFC 952](http://tools.ietf.org/html/rfc952)
%% [RFC 1123](http://tools.ietf.org/html/rfc1123)
-spec is_valid_label(iodata()) -> boolean().
is_valid_label(<<>>) ->
	false;
is_valid_label(<<$-, _/binary>>) ->
	false;
is_valid_label(Binary) when is_binary(Binary) andalso byte_size(Binary) =< 63 ->
	validate_label(Binary);
is_valid_label(List) when is_list(List) ->
	is_valid_label(erlang:iolist_to_binary(List));
is_valid_label(_) ->
	false.

%% @doc Join labels with dots (.)
-spec join(binary() | [iodata()]) -> binary().
join(Binary) when is_binary(Binary) ->
	Binary;
join([]) ->
	<<>>;
join(List) when is_list(List) ->
	[[_, Head] | Tail] = [[$., Label] || Label <- List],
	erlang:iolist_to_binary([Head | Tail]).

%% @doc Reverse the parts of a name.
%% For example: <<"_service._type.domain">> becomes <<"domain._type._service">>
-spec reverse(iodata()) -> binary().
reverse(Binary) when is_binary(Binary) ->
	join(lists:reverse(split(Binary)));
reverse(List) when is_list(List) ->
	reverse(erlang:iolist_to_binary(List)).

%% @doc Split a name by dots (.)
-spec split(iodata()) -> binary().
split(Binary) when is_binary(Binary) ->
	binary:split(Binary, <<$.>>, [global]);
split(List) when is_list(List) ->
	split(erlang:iolist_to_binary(List)).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
validate_label(<<>>) ->
	true;
validate_label(<<$->>) ->
	false;
validate_label(<<$-, Rest/binary>>) ->
	validate_label(Rest);
validate_label(<<C, Rest/binary>>) when (C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9) ->
	validate_label(Rest);
validate_label(_) ->
	false.

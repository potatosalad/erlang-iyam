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
-module(zcsd_data_service).

%% Types
-type domain() :: iodata().
-type type() :: iodata().
-type service() :: iodata().
-type instance() :: iodata().
-type ttl() :: non_neg_integer().

-export_type([domain/0]).
-export_type([type/0]).
-export_type([service/0]).
-export_type([instance/0]).
-export_type([ttl/0]).

-type srv_priority() :: non_neg_integer().
-type srv_weight() :: non_neg_integer().
-type srv_port() :: inet:port_number().
-type srv_target() :: iodata().

-export_type([srv_priority/0]).
-export_type([srv_weight/0]).
-export_type([srv_port/0]).
-export_type([srv_target/0]).

-type txt_data() :: [{iodata(), iodata()}].

-export_type([txt_data/0]).

-type t() :: #{
	'__struct__' := ?MODULE,
	%% Domain
	domain := domain(),
	type := type(),
	service := service(),
	instance := instance(),
	ttl := ttl(),
	%% SRV
	priority := srv_priority(),
	weight := srv_weight(),
	port := srv_port(),
	target := srv_target(),
	%% TXT
	data := txt_data()
}.

-export_type([t/0]).

%% Elixir API
-export(['__struct__'/0]).
-export(['__struct__'/1]).
%% API
-export([new/10]).
-export([to_dns_record/1]).
-export([from_dns_record/1]).
-export([encode/1]).
-export([decode/1]).

%%%===================================================================
%%% Elixir API functions
%%%===================================================================

-spec '__struct__'() -> t().
'__struct__'() ->
	#{
		'__struct__' => ?MODULE,
		domain => nil,
		type => nil,
		service => nil,
		instance => nil,
		ttl => nil,
		priority => nil,
		weight => nil,
		port => nil,
		target => nil,
		data => nil
	}.

-spec '__struct__'(list() | map()) -> t().
'__struct__'(List) when is_list(List) ->
	'__struct__'(maps:from_list(List));
'__struct__'(Map) when is_map(Map) ->
	maps:fold(fun maps:update/3, '__struct__'(), Map).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec new(Domain::domain(), Type::type(), Service::service(),
		Instance::instance(), TTL::ttl(), Priority::srv_priority(),
		Weight::srv_weight(), Port::srv_port(), Target::srv_target(),
		Data::txt_data())
	-> t().
new(Domain, Type, Service, Instance, TTL, Priority, Weight, Port, Target, Data) ->
	'__struct__'(#{
		domain => Domain,
		type => Type,
		service => Service,
		instance => Instance,
		ttl => TTL,
		priority => Priority,
		weight => Weight,
		port => Port,
		target => Target,
		data => Data
	}).

-spec to_dns_record(t()) -> term().
to_dns_record(DNS = #{'__struct__' := ?MODULE, domain := D, type := T, service := S, instance := I}) ->
	#{'PTR' := PTR, 'SRV' := SRV} = zcsd_name:key_encode({D, T, S, I}),
	inet_dns:make_msg([
		{header, inet_dns_header()},
		{anlist, inet_dns_anlist(PTR, SRV, DNS)},
		{arlist, inet_dns_arlist(SRV, DNS)}
	]).

-spec from_dns_record(Record::term())
	-> t().
from_dns_record(Record) ->
	Header = inet_dns:header(inet_dns:msg(Record, header)),
	Type = inet_dns:record_type(Record),
	Questions = [inet_dns:dns_query(Query) || Query <- inet_dns:msg(Record, qdlist)],
	Answers = [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, anlist)],
	Authorities = [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, nslist)],
	Resources = [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, arlist)],
	QR = proplists:get_value(qr, Header),
	Opcode = proplists:get_value(opcode, Header),
	case {Header, Type, Questions, Answers, Authorities, Resources, QR, Opcode} of
		{Header, msg, [], [Answer], [], [Service, Text], true, 'query'} ->
			ptr = proplists:get_value(type, Answer),
			PTR = proplists:get_value(domain, Answer),
			TTL = proplists:get_value(ttl, Answer),
			SRV = proplists:get_value(data, Answer),
			srv = proplists:get_value(type, Service),
			SRV = proplists:get_value(domain, Service),
			TTL = proplists:get_value(ttl, Service),
			{Priority, Weight, Port, Target} = proplists:get_value(data, Service),
			txt = proplists:get_value(type, Text),
			SRV = proplists:get_value(domain, Text),
			TTL = proplists:get_value(ttl, Text),
			TXTData = inet_dns_txt_data_decode(proplists:get_value(data, Text)),
			PTRx = zcsd_util:any_to_bitstring(PTR),
			SRVx = zcsd_util:any_to_bitstring(SRV),
			Encoded = binary:part(SRVx, 0, byte_size(SRVx) - byte_size(PTRx) - 1),
			KEYx = << (zcsd_name:reverse(PTRx))/binary, $., Encoded/binary >>,
			Arg = #{
				'PTR' => PTRx,
				'SRV' => SRVx,
				'KEY' => KEYx
			},
			{RDomain, RType, RService, RInstance} = zcsd_name:key_decode(Arg),
			new(RDomain, RType, RService, RInstance, TTL, Priority, Weight, Port, zcsd_util:any_to_bitstring(Target), TXTData);
		_ ->
			erlang:error(badarg, [Record])
	end.

-spec encode(DNS::t())
	-> binary().
encode(DNS = #{'__struct__' := ?MODULE }) ->
	inet_dns:encode(to_dns_record(DNS)).

-spec decode(Binary::binary())
	-> {ok, t()} | {error, term()}.
decode(Binary) when is_binary(Binary) ->
	case inet_dns:decode(Binary) of
		{ok, Record} ->
			try from_dns_record(Record) of
				DNS = #{ '__struct__' := ?MODULE } ->
					{ok, DNS}
			catch
				error:Reason:Stacktrace ->
					{error, {Reason, Stacktrace}}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
inet_dns_header() ->
	inet_dns:make_header([
		{id, 0},
		{qr, true},
		{opcode, 'query'},
		{aa, true},
		{tc, false},
		{rd, false},
		{ra, false},
		{pr, false},
		{rcode, 0}
	]).

%% @private
inet_dns_anlist(PTR, SRV, DNS) ->
	[inet_dns_ptr(PTR, SRV, DNS)].

%% @private
inet_dns_ptr(PTR, SRV, #{'__struct__' := ?MODULE, ttl := TTL}) ->
	inet_dns:make_rr([
		{type, ptr},
		{domain, zcsd_util:any_to_charlist(PTR)},
		{class, in},
		{ttl, TTL},
		{data, zcsd_util:any_to_charlist(SRV)}
	]).

%% @private
inet_dns_arlist(SRV, DNS) ->
	[inet_dns_srv(SRV, DNS), inet_dns_txt(SRV, DNS)].

%% @private
inet_dns_srv(SRV, #{'__struct__' := ?MODULE, ttl := TTL, priority := Priority, weight := Weight, port := Port, target := Target}) ->
	inet_dns:make_rr([
		{type, srv},
		{domain, zcsd_util:any_to_charlist(SRV)},
		{class, in},
		{ttl, TTL},
		{data, {Priority, Weight, Port, zcsd_util:any_to_charlist(Target)}}
	]).

%% @private
inet_dns_txt(SRV, DNS = #{'__struct__' := ?MODULE, ttl := TTL}) ->
	inet_dns:make_rr([
		{type, txt},
		{domain, zcsd_util:any_to_charlist(SRV)},
		{class, in},
		{ttl, TTL},
		{data, inet_dns_txt_data_encode(DNS)}
	]).

%% @private
inet_dns_txt_data_encode(#{'__struct__' := ?MODULE, data := Data}) ->
	[begin
		zcsd_util:any_to_charlist([zcsd_rfc3986:urlencode(Key), $=, zcsd_rfc3986:urlencode(Val)])
	end || {Key, Val} <- Data].

%% @private
inet_dns_txt_data_decode(Data) ->
	[begin
		[Key, Val] = binary:split(zcsd_util:any_to_bitstring(KeyVal), <<"=">>),
		{zcsd_rfc3986:urldecode(Key), zcsd_rfc3986:urldecode(Val)}
	end || KeyVal <- Data].

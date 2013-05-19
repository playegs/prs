-module(prs_SUITE).

-export([all/0]).

-export([comp_0s/1]).
-export([comp_alpha/1]).
-export([comp_empty/1]).
-export([comp_ga/1]).
-export([comp_npc/1]).
-export([comp_repeat/1]).
-export([decomp_ga/1]).
-export([decomp_npc/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
	[comp_0s, comp_alpha, comp_empty, comp_ga, comp_npc, comp_repeat,
		decomp_ga, decomp_npc].

comp_0s(_) ->
	Data = << 0:10000 >>,
	Data = prs:decompress(prs:compress(Data)),
	ok.

comp_alpha(_) ->
	Data = <<"abcdefghijklmnopqrstuvwxyz">>,
	Data = prs:decompress(prs:compress(Data)),
	ok.

comp_empty(_) ->
	Data = <<"">>,
	Data = prs:decompress(prs:compress(Data)),
	ok.

comp_ga(Config) ->
	Dir = ?config(data_dir, Config),
	{ok, Decomp} = file:read_file(Dir ++ "/ga_decomp.bin"),
	Comp = prs:compress(Decomp),
	Decomp = prs:decompress(Comp),
	ct:print("comp_ga: decompressed ~p compressed ~p",
		[byte_size(Decomp), byte_size(Comp)]),
	ok.

comp_npc(Config) ->
	Dir = ?config(data_dir, Config),
	{ok, Decomp} = file:read_file(Dir ++ "/npc_decomp.bin"),
	Comp = prs:compress(Decomp),
	Decomp = prs:decompress(Comp),
	ct:print("comp_npc: decompressed ~p compressed ~p",
		[byte_size(Decomp), byte_size(Comp)]),
	ok.

comp_repeat(_) ->
	Data = <<"1234512345">>,
	Data = prs:decompress(prs:compress(Data)),
	ok.

decomp_ga(Config) ->
	Dir = ?config(data_dir, Config),
	{ok, Comp} = file:read_file(Dir ++ "/ga_comp.bin"),
	{ok, Decomp} = file:read_file(Dir ++ "/ga_decomp.bin"),
	Decomp = prs:decompress(Comp),
	ok.

decomp_npc(Config) ->
	Dir = ?config(data_dir, Config),
	{ok, Comp} = file:read_file(Dir ++ "/npc_comp.bin"),
	{ok, Decomp} = file:read_file(Dir ++ "/npc_decomp.bin"),
	Decomp = prs:decompress(Comp),
	ok.

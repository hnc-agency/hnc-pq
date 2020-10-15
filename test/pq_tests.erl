%% Copyright (c) 2020, Maria Scott <maria-12648430@gmx.net>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(pq_tests).

-include_lib("eunit/include/eunit.hrl").

-record(pq, {p=[], l=undefined, q=#{}}).

new_test() ->
	?assertError(badarg, hnc_pq:new(foo)),
	?assertError(badarg, hnc_pq:new([])),
	?assertError(badarg, hnc_pq:new([a, a])),
	?assertEqual(
		#pq{p=[a, b], l=b, q=#{a => queue:new(), b => queue:new()}},
		hnc_pq:new([a, b])
	),
	ok.

from_list_test() ->
	?assertError(badarg, hnc_pq:from_list(foo)),
	?assertError(badarg, hnc_pq:from_list([])),
	?assertError(badarg, hnc_pq:from_list([foo])),
	?assertError(badarg, hnc_pq:from_list([{foo}])),
	?assertError(badarg, hnc_pq:from_list([{foo, x}])),
	?assertEqual(
		#pq{p=[a, b], l=b, q=#{a => queue:new(), b => queue:new()}},
		hnc_pq:from_list([{a, []}, {b, []}])
	),
	?assertEqual(
		#pq{p=[a, b], l=b, q=#{a => queue:from_list([foo, bar]), b => queue:from_list([baz])}},
		hnc_pq:from_list([{a, [foo, bar]}, {b, [baz]}])
	),
	ok.

to_list_test() ->
	?assertError(badarg, hnc_pq:to_list(foo)),
	?assertError(badarg, hnc_pq:to_list(x, hnc_pq:new([a, b]))),
	List0=[{a, []}, {b, []}],
	?assertEqual([], hnc_pq:to_list(a, hnc_pq:from_list(List0))),
	?assertEqual([], hnc_pq:to_list(b, hnc_pq:from_list(List0))),
	?assertEqual(List0, hnc_pq:to_list(hnc_pq:from_list(List0))),
	List1=[{a, [foo]}, {b, [bar, baz]}],
	?assertEqual([foo], hnc_pq:to_list(a, hnc_pq:from_list(List1))),
	?assertEqual([bar, baz], hnc_pq:to_list(b, hnc_pq:from_list(List1))),
	?assertEqual(List1, hnc_pq:to_list(hnc_pq:from_list(List1))),
	ok.

to_flatlist_test() ->
	?assertError(badarg, hnc_pq:to_flatlist(foo)),
	List0=[{a, []}, {b, []}],
	?assertEqual([], hnc_pq:to_flatlist(hnc_pq:from_list(List0))),
	List1=[{a, [foo]}, {b, [bar, baz]}],
	?assertEqual([foo, bar, baz], hnc_pq:to_flatlist(hnc_pq:from_list(List1))),
	ok.

to_flatqueue_test() ->
	?assertError(badarg, hnc_pq:to_flatqueue(foo)),
	List0=[{a, []}, {b, []}],
	?assertEqual([], queue:to_list(hnc_pq:to_flatqueue(hnc_pq:from_list(List0)))),
	List1=[{a, [foo]}, {b, [bar, baz]}],
	?assertEqual([foo, bar, baz], queue:to_list(hnc_pq:to_flatqueue(hnc_pq:from_list(List1)))),
	ok.

is_empty_test() ->
	?assertError(badarg, hnc_pq:is_empty(foo)),
	?assertError(badarg, hnc_pq:is_empty(x, hnc_pq:new([a, b]))),
	?assert(hnc_pq:is_empty(hnc_pq:new([a, b]))),
	?assert(hnc_pq:is_empty(a, hnc_pq:new([a, b]))),
	?assert(hnc_pq:is_empty(b, hnc_pq:new([a, b]))),
	?assertNot(hnc_pq:is_empty(hnc_pq:from_list([{a, [foo]}, {b, [bar, baz]}]))),
	?assertNot(hnc_pq:is_empty(a, hnc_pq:from_list([{a, [foo]}, {b, [bar, baz]}]))),
	?assertNot(hnc_pq:is_empty(a, hnc_pq:from_list([{a, [foo]}, {b, []}]))),
	?assertNot(hnc_pq:is_empty(b, hnc_pq:from_list([{a, [foo]}, {b, [bar, baz]}]))),
	?assertNot(hnc_pq:is_empty(b, hnc_pq:from_list([{a, []}, {b, [bar, baz]}]))),
	ok.

length_test() ->
	?assertError(badarg, hnc_pq:length(foo)),
	?assertError(badarg, hnc_pq:length(x, hnc_pq:new([a, b]))),
	Q0=hnc_pq:new([a, b]),
	?assertEqual(0, hnc_pq:length(a, Q0)),
	?assertEqual(0, hnc_pq:length(b, Q0)),
	?assertEqual(0, hnc_pq:length(Q0)),
	Q1=hnc_pq:from_list([{a, [foo]}, {b, [bar, baz]}]),
	?assertEqual(1, hnc_pq:length(a, Q1)),
	?assertEqual(2, hnc_pq:length(b, Q1)),
	?assertEqual(3, hnc_pq:length(Q1)),
	ok.

member_test() ->
	?assertError(badarg, hnc_pq:member(bar, foo)),
	?assertError(badarg, hnc_pq:member(x, foo, hnc_pq:new([a, b]))),
	Q0=hnc_pq:new([a, b]),
	?assertNot(hnc_pq:member(a, foo, Q0)),
	?assertNot(hnc_pq:member(b, foo, Q0)),
	?assertNot(hnc_pq:member(foo, Q0)),
	Q1=hnc_pq:from_list([{a, [foo]}, {b, [bar, baz]}]),
	?assert(hnc_pq:member(a, foo, Q1)),
	?assertNot(hnc_pq:member(b, foo, Q1)),
	?assertNot(hnc_pq:member(a, bar, Q1)),
	?assert(hnc_pq:member(b, bar, Q1)),
	?assert(hnc_pq:member(foo, Q1)),
	?assert(hnc_pq:member(bar, Q1)),
	?assertNot(hnc_pq:member(undefined, Q1)),
	ok.

is_priority_test() ->
	?assertError(badarg, hnc_pq:is_priority(bar, foo)),
	Q=hnc_pq:new([a, b]),
	?assert(hnc_pq:is_priority(a, Q)),
	?assert(hnc_pq:is_priority(b, Q)),
	?assertNot(hnc_pq:is_priority(x, Q)),
	ok.

priorities_test() ->
	?assertError(badarg, hnc_pq:priorities(foo)),
	?assertEqual([a, b], hnc_pq:priorities(hnc_pq:new([a, b]))),
	?assertEqual([b, a], hnc_pq:priorities(hnc_pq:new([b, a]))),
	ok.

queue_test() ->
	?assertError(badarg, hnc_pq:queue(bar, foo)),
	?assertError(badarg, hnc_pq:queue(x, hnc_pq:new([a, b]))),
	Q0=hnc_pq:new([a, b]),
	?assertEqual([], queue:to_list(hnc_pq:queue(a, Q0))),
	?assertEqual([], queue:to_list(hnc_pq:queue(b, Q0))),
	Q1=hnc_pq:from_list([{a, [foo]}, {b, [bar, baz]}]),
	?assertEqual([foo], queue:to_list(hnc_pq:queue(a, Q1))),
	?assertEqual([bar, baz], queue:to_list(hnc_pq:queue(b, Q1))),
	ok.

reverse_priorities_test() ->
	?assertError(badarg, hnc_pq:reverse_priorities(foo)),
	Q0=hnc_pq:new([a, b]),
	?assertEqual([b, a], hnc_pq:priorities(hnc_pq:reverse_priorities(Q0))),
	Q1=hnc_pq:from_list([{a, [foo]}, {b, [bar, baz]}]),
	?assertEqual([{b, [bar, baz]}, {a, [foo]}], hnc_pq:to_list(hnc_pq:reverse_priorities(Q1))),
	ok.

reverse_test() ->
	?assertError(badarg, hnc_pq:reverse(foo)),
	?assertError(badarg, hnc_pq:reverse(x, hnc_pq:new([a, b]))),
	Q0=hnc_pq:new([a, b]),
	?assertEqual([b, a], hnc_pq:priorities(hnc_pq:reverse(Q0))),
	Q1=hnc_pq:from_list([{a, [foo]}, {b, [bar, baz]}]),
	?assertEqual([{a, [foo]}, {b, [bar, baz]}], hnc_pq:to_list(hnc_pq:reverse(a, Q1))),
	?assertEqual([{a, [foo]}, {b, [baz, bar]}], hnc_pq:to_list(hnc_pq:reverse(b, Q1))),
	?assertEqual([{b, [baz, bar]}, {a, [foo]}], hnc_pq:to_list(hnc_pq:reverse(Q1))),
	ok.

reprioritize_test() ->
	?assertError(badarg, hnc_pq:reprioritize(bar, foo)),
	?assertError(badarg, hnc_pq:reprioritize([], foo)),
	?assertError(badarg, hnc_pq:reprioritize([], hnc_pq:new([a, b]))),
	Q0=hnc_pq:new([a, b]),
	?assertEqual([a], hnc_pq:priorities(hnc_pq:reprioritize([a], Q0))),
	?assertEqual([b], hnc_pq:priorities(hnc_pq:reprioritize([b], Q0))),
	?assertEqual([c], hnc_pq:priorities(hnc_pq:reprioritize([c], Q0))),
	?assertEqual([a, b], hnc_pq:priorities(hnc_pq:reprioritize([a, b], Q0))),
	?assertEqual([b, a], hnc_pq:priorities(hnc_pq:reprioritize([b, a], Q0))),
	?assertEqual([a, b, c], hnc_pq:priorities(hnc_pq:reprioritize([a, b, c], Q0))),
	?assertEqual([a, c, b], hnc_pq:priorities(hnc_pq:reprioritize([a, c, b], Q0))),
	?assertEqual([c, a, b], hnc_pq:priorities(hnc_pq:reprioritize([c, a, b], Q0))),
	Q1=hnc_pq:from_list([{a, [foo]}, {b, [bar, baz]}]),
	?assertEqual([{a, [foo]}], hnc_pq:to_list(hnc_pq:reprioritize([a], Q1))),
	?assertEqual([{b, [bar, baz]}], hnc_pq:to_list(hnc_pq:reprioritize([b], Q1))),
	?assertEqual([{c, []}], hnc_pq:to_list(hnc_pq:reprioritize([c], Q1))),
	?assertEqual([{a, [foo]}, {b, [bar, baz]}], hnc_pq:to_list(hnc_pq:reprioritize([a, b], Q1))),
	?assertEqual([{b, [bar, baz]}, {a, [foo]}], hnc_pq:to_list(hnc_pq:reprioritize([b, a], Q1))),
	?assertEqual([{a, [foo]}, {b, [bar, baz]}, {c, []}], hnc_pq:to_list(hnc_pq:reprioritize([a, b, c], Q1))),
	?assertEqual([{a, [foo]}, {c, []}, {b, [bar, baz]}], hnc_pq:to_list(hnc_pq:reprioritize([a, c, b], Q1))),
	?assertEqual([{c, []}, {a, [foo]}, {b, [bar, baz]}], hnc_pq:to_list(hnc_pq:reprioritize([c, a, b], Q1))),
	?assertEqual([{c, []}, {b, [bar, baz]}], hnc_pq:to_list(hnc_pq:reprioritize([c, b], Q1))),
	ok.

in_test() ->
	?assertError(badarg, hnc_pq:in(bar, foo)),
	?assertError(badarg, hnc_pq:in(x, foo, hnc_pq:new([a, b]))),
	Q0=hnc_pq:new([a, b]),
	?assertEqual([{a, [foo]}, {b, []}], hnc_pq:to_list(hnc_pq:in(a, foo, Q0))),
	?assertEqual([{a, []}, {b, [bar]}], hnc_pq:to_list(hnc_pq:in(b, bar, Q0))),
	?assertEqual([{a, []}, {b, [baz]}], hnc_pq:to_list(hnc_pq:in(baz, Q0))),
	Q01=hnc_pq:in(a, foo, Q0),
	Q02=hnc_pq:in(b, bar, Q01),
	Q03=hnc_pq:in(baz, Q02),
	?assertEqual([{a, [foo]}, {b, [bar, baz]}], hnc_pq:to_list(Q03)),
	ok.

in_r_test() ->
	?assertError(badarg, hnc_pq:in_r(bar, foo)),
	?assertError(badarg, hnc_pq:in_r(x, foo, hnc_pq:new([a, b]))),
	Q0=hnc_pq:new([a, b]),
	?assertEqual([{a, [foo]}, {b, []}], hnc_pq:to_list(hnc_pq:in_r(a, foo, Q0))),
	?assertEqual([{a, []}, {b, [bar]}], hnc_pq:to_list(hnc_pq:in_r(b, bar, Q0))),
	?assertEqual([{a, [baz]}, {b, []}], hnc_pq:to_list(hnc_pq:in_r(baz, Q0))),
	Q01=hnc_pq:in_r(a, foo, Q0),
	Q02=hnc_pq:in_r(b, bar, Q01),
	Q03=hnc_pq:in_r(baz, Q02),
	?assertEqual([{a, [baz, foo]}, {b, [bar]}], hnc_pq:to_list(Q03)),
	ok.

out_test() ->
	?assertError(badarg, hnc_pq:out(foo)),
	?assertError(badarg, hnc_pq:out(x, hnc_pq:new([a, b]))),
	Q0=hnc_pq:new([a, b]),
	?assertEqual(empty, hnc_pq:out(a, Q0)),
	?assertEqual(empty, hnc_pq:out(b, Q0)),
	?assertEqual(empty, hnc_pq:out(Q0)),
	Q1=hnc_pq:from_list([{a, [foo]}, {b, [bar, baz]}]),
	{V11, Q11}=hnc_pq:out(a, Q1),
	?assertEqual(foo, V11),
	?assertEqual([{a, []}, {b, [bar, baz]}], hnc_pq:to_list(Q11)),
	{V12, Q12}=hnc_pq:out(b, Q1),
	?assertEqual(bar, V12),
	?assertEqual([{a, [foo]}, {b, [baz]}], hnc_pq:to_list(Q12)),
	{V13, Q13}=hnc_pq:out(Q1),
	?assertEqual(foo, V13),
	?assertEqual([{a, []}, {b, [bar, baz]}], hnc_pq:to_list(Q13)),
	ok.

out_r_test() ->
	?assertError(badarg, hnc_pq:out_r(foo)),
	?assertError(badarg, hnc_pq:out_r(x, hnc_pq:new([a, b]))),
	Q0=hnc_pq:new([a, b]),
	?assertEqual(empty, hnc_pq:out_r(a, Q0)),
	?assertEqual(empty, hnc_pq:out_r(b, Q0)),
	?assertEqual(empty, hnc_pq:out_r(Q0)),
	Q1=hnc_pq:from_list([{a, [foo]}, {b, [bar, baz]}]),
	{V11, Q11}=hnc_pq:out_r(a, Q1),
	?assertEqual(foo, V11),
	?assertEqual([{a, []}, {b, [bar, baz]}], hnc_pq:to_list(Q11)),
	{V12, Q12}=hnc_pq:out_r(b, Q1),
	?assertEqual(baz, V12),
	?assertEqual([{a, [foo]}, {b, [bar]}], hnc_pq:to_list(Q12)),
	{V13, Q13}=hnc_pq:out_r(Q1),
	?assertEqual(baz, V13),
	?assertEqual([{a, [foo]}, {b, [bar]}], hnc_pq:to_list(Q13)),
	ok.

join_test() ->
	?assertError(badarg, hnc_pq:join(bar, foo, queue:new())),
	?assertError(badarg, hnc_pq:join(x, hnc_pq:new([a]), queue:new())),
	?assertError(badarg, hnc_pq:join(x, queue:new(), hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:join(a, hnc_pq:new([a]), foo)),
	?assertError(badarg, hnc_pq:join(a, foo, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:join(hnc_pq:new([a]), hnc_pq:new([b]))),
	?assertError(badarg, hnc_pq:join(hnc_pq:new([a, b]), hnc_pq:new([b, a]))),
	Q0=hnc_pq:new([a, b]),
	?assertEqual([{a, [foo, bar]}, {b, []}], hnc_pq:to_list(hnc_pq:join(a, Q0, queue:from_list([foo, bar])))),
	?assertEqual([{a, []}, {b, [foo, bar]}], hnc_pq:to_list(hnc_pq:join(b, Q0, queue:from_list([foo, bar])))),
	?assertEqual([{a, [foo, bar]}, {b, []}], hnc_pq:to_list(hnc_pq:join(a, queue:from_list([foo, bar]), Q0))),
	?assertEqual([{a, []}, {b, [foo, bar]}], hnc_pq:to_list(hnc_pq:join(b, queue:from_list([foo, bar]), Q0))),
	Q1=hnc_pq:from_list([{a, [foo]}, {b, [bar, baz]}]),
	?assertEqual([{a, [foo]}, {b, [bar, baz]}], hnc_pq:to_list(hnc_pq:join(Q0, Q1))),
	?assertEqual([{a, [foo]}, {b, [bar, baz]}], hnc_pq:to_list(hnc_pq:join(Q1, Q0))),
	?assertEqual([{a, [foo, foo2, bar2]}, {b, [bar, baz]}], hnc_pq:to_list(hnc_pq:join(a, Q1, queue:from_list([foo2, bar2])))),
	?assertEqual([{a, [foo]}, {b, [bar, baz, foo2, bar2]}], hnc_pq:to_list(hnc_pq:join(b, Q1, queue:from_list([foo2, bar2])))),
	?assertEqual([{a, [foo2, bar2, foo]}, {b, [bar, baz]}], hnc_pq:to_list(hnc_pq:join(a, queue:from_list([foo2, bar2]), Q1))),
	?assertEqual([{a, [foo]}, {b, [foo2, bar2, bar, baz]}], hnc_pq:to_list(hnc_pq:join(b, queue:from_list([foo2, bar2]), Q1))),
	Q2=hnc_pq:from_list([{a, [foo2]}, {b, [bar2, baz2]}]),
	?assertEqual([{a, [foo, foo2]}, {b, [bar, baz, bar2, baz2]}], hnc_pq:to_list(hnc_pq:join(Q1, Q2))),
	?assertEqual([{a, [foo2, foo]}, {b, [bar2, baz2, bar, baz]}], hnc_pq:to_list(hnc_pq:join(Q2, Q1))),
	ok.

filter_test() ->
	?assertError(badarg, hnc_pq:filter(x, fun (_, _) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:filter(a, fun (_) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:filter(fun (_) -> true end, hnc_pq:new([a]))),
	Q0=hnc_pq:new([a, b]),
	?assertEqual([{a, []}, {b, []}], hnc_pq:to_list(hnc_pq:filter(a, fun (_, _) -> true end, Q0))),
	?assertEqual([{a, []}, {b, []}], hnc_pq:to_list(hnc_pq:filter(a, fun (_, _) -> false end, Q0))),
	?assertEqual([{a, []}, {b, []}], hnc_pq:to_list(hnc_pq:filter(a, fun (_, _) -> [x] end, Q0))),
	?assertEqual([{a, []}, {b, []}], hnc_pq:to_list(hnc_pq:filter(b, fun (_, _) -> true end, Q0))),
	?assertEqual([{a, []}, {b, []}], hnc_pq:to_list(hnc_pq:filter(b, fun (_, _) -> false end, Q0))),
	?assertEqual([{a, []}, {b, []}], hnc_pq:to_list(hnc_pq:filter(b, fun (_, _) -> [x] end, Q0))),
	?assertEqual([{a, []}, {b, []}], hnc_pq:to_list(hnc_pq:filter(fun (_, _) -> true end, Q0))),
	?assertEqual([{a, []}, {b, []}], hnc_pq:to_list(hnc_pq:filter(fun (_, _) -> false end, Q0))),
	?assertEqual([{a, []}, {b, []}], hnc_pq:to_list(hnc_pq:filter(fun (_, _) -> [x] end, Q0))),
	Q1=hnc_pq:from_list(
		[
			{a, [{keep, a}, {drop, a}, {replace, a}, {duplicate, a}, {keep, b}, {drop, b}, {replace, b}, {duplicate, b}, {keep, x}, {drop, x}, {replace, x}, {duplicate, x}]},
			{b, [{keep, a}, {drop, a}, {replace, a}, {duplicate, a}, {keep, b}, {drop, b}, {replace, b}, {duplicate, b}, {keep, x}, {drop, x}, {replace, x}, {duplicate, x}]}
		]
	),
	Fun1=fun
		(P, {keep, P}) -> true;
		(_, {keep, _}) -> false;
		(P, {drop, P}) -> false;
		(_, {drop, _}) -> true;
		(P, {replace, P}) -> [{replaced, P}];
		(_, {replace, _}) -> true;
		(P, {duplicate, P}) -> [{duplicated, P}, {duplicated, P}];
		(_, {duplicate, _}) -> true
	end,
	?assertEqual(
		[
			{a, [{keep, a}, {replaced, a}, {duplicated, a}, {duplicated, a}, {drop, b}, {replace, b}, {duplicate, b}, {drop, x}, {replace, x}, {duplicate, x}]},
			{b, [{keep, a}, {drop, a}, {replace, a}, {duplicate, a}, {keep, b}, {drop, b}, {replace, b}, {duplicate, b}, {keep, x}, {drop, x}, {replace, x}, {duplicate, x}]}
		],
		hnc_pq:to_list(hnc_pq:filter(a, Fun1, Q1))
	),
	?assertEqual(
		[
			{a, [{keep, a}, {drop, a}, {replace, a}, {duplicate, a}, {keep, b}, {drop, b}, {replace, b}, {duplicate, b}, {keep, x}, {drop, x}, {replace, x}, {duplicate, x}]},
			{b, [{drop, a}, {replace, a}, {duplicate, a}, {keep, b}, {replaced, b}, {duplicated, b}, {duplicated, b}, {drop, x}, {replace, x}, {duplicate, x}]}
		],
		hnc_pq:to_list(hnc_pq:filter(b, Fun1, Q1))
	),
	?assertEqual(
		[
			{a, [{keep, a}, {replaced, a}, {duplicated, a}, {duplicated, a}, {drop, b}, {replace, b}, {duplicate, b}, {drop, x}, {replace, x}, {duplicate, x}]},
			{b, [{drop, a}, {replace, a}, {duplicate, a}, {keep, b}, {replaced, b}, {duplicated, b}, {duplicated, b}, {drop, x}, {replace, x}, {duplicate, x}]}
		],
		hnc_pq:to_list(hnc_pq:filter(Fun1, Q1))
	),
	ok.

fold_test() ->
	?assertError(badarg, hnc_pq:fold(x, fun (_, _, _) -> true end, 0, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:fold(a, fun (_, _) -> true end, 0, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:fold(fun (_, _) -> true end, 0, hnc_pq:new([a]))),
	Fun0=fun (_, _, Acc) -> Acc+1 end,
	Q0=hnc_pq:new([a, b]),
	?assertEqual(0, hnc_pq:fold(a, Fun0, 0, Q0)),
	?assertEqual(0, hnc_pq:fold(b, Fun0, 0, Q0)),
	?assertEqual(0, hnc_pq:fold(Fun0, 0, Q0)),
	Q1=hnc_pq:from_list([{a, [foo]}, {b, [bar, baz]}]),
	?assertEqual(1, hnc_pq:fold(a, Fun0, 0, Q1)),
	?assertEqual(2, hnc_pq:fold(b, Fun0, 0, Q1)),
	?assertEqual(3, hnc_pq:fold(Fun0, 0, Q1)),
	Fun1=fun (P, I, Acc) -> [{P, I}|Acc] end,
	?assertEqual([{b, baz}, {b, bar}, {a, foo}], hnc_pq:fold(Fun1, [], Q1)),
	ok.

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

new_test() ->
	?assertError(badarg, hnc_pq:new(foo)),
	?assertError(badarg, hnc_pq:new([])),
	?assertError(badarg, hnc_pq:new([a, a])),

	?assertEqual(
		{pq, [a, b], b, #{a => queue:new(), b => queue:new()}},
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
		{pq, [a, b], b, #{a => queue:new(), b => queue:new()}},
		hnc_pq:from_list([{a, []}, {b, []}])
	),
	?assertEqual(
		{pq, [a, b], b, #{a => queue:from_list([a1]), b => queue:from_list([b1, b2])}},
		hnc_pq:from_list([{a, [a1]}, {b, [b1, b2]}])
	),
	ok.

to_list_test() ->
	?assertError(badarg, hnc_pq:to_list(foo)),
	?assertError(badarg, hnc_pq:to_list(x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:to_list(a, x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:to_list(x, a, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:to_list(x, y, hnc_pq:new([a]))),

	lists:foreach(
		fun ({A, B, C}) ->
			QA={a, A},
			QB={b, B},
			QC={c, C},

			PQ=hnc_pq:from_list([QA, QB, QC]),

			?assertEqual([QA, QB, QC], hnc_pq:to_list(PQ)),
			?assertEqual([QA, QB, QC], hnc_pq:to_list(a, c, PQ)),

			?assertEqual([QA], hnc_pq:to_list(a, PQ)),
			?assertEqual([QA], hnc_pq:to_list(a, a, PQ)),

			?assertEqual([QB], hnc_pq:to_list(b, PQ)),
			?assertEqual([QB], hnc_pq:to_list(b, b, PQ)),

			?assertEqual([QC], hnc_pq:to_list(c, PQ)),
			?assertEqual([QC], hnc_pq:to_list(c, c, PQ)),

			?assertEqual([QA, QB], hnc_pq:to_list(a, b, PQ)),

			?assertEqual([QB, QC], hnc_pq:to_list(b, c, PQ)),

			?assertEqual([], hnc_pq:to_list(c, a, PQ))
		end,
		[
			{T1, T2, T3}
			||
			T1 <- [[], [a1], [a1, a2]],
			T2 <- [[], [b1], [b1, b2]],
			T3 <- [[], [c1], [c1, c2]]
		]
	),

	ok.

to_flatlist_test() ->
	?assertError(badarg, hnc_pq:to_flatlist(foo)),
	?assertError(badarg, hnc_pq:to_flatlist(x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:to_flatlist(a, x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:to_flatlist(x, a, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:to_flatlist(x, y, hnc_pq:new([a]))),

	lists:foreach(
		fun ({A, B, C}) ->
			PQ=hnc_pq:from_list([{a, A}, {b, B}, {c, C}]),

			?assertEqual(A++B++C, hnc_pq:to_flatlist(PQ)),
			?assertEqual(A++B++C, hnc_pq:to_flatlist(a, c, PQ)),

			?assertEqual(A, hnc_pq:to_flatlist(a, PQ)),
			?assertEqual(A, hnc_pq:to_flatlist(a, a, PQ)),

			?assertEqual(B, hnc_pq:to_flatlist(b, PQ)),
			?assertEqual(B, hnc_pq:to_flatlist(b, b, PQ)),

			?assertEqual(C, hnc_pq:to_flatlist(c, PQ)),
			?assertEqual(C, hnc_pq:to_flatlist(c, c, PQ)),

			?assertEqual(A++B, hnc_pq:to_flatlist(a, b, PQ)),

			?assertEqual(B++C, hnc_pq:to_flatlist(b, c, PQ)),

			?assertEqual([], hnc_pq:to_flatlist(c, a, PQ))
		end,
		[
			{T1, T2, T3}
			||
			T1 <- [[], [a1], [a1, a2]],
			T2 <- [[], [b1], [b1, b2]],
			T3 <- [[], [c1], [c1, c2]]
		]
	),

	ok.

to_flatqueue_test() ->
	?assertError(badarg, hnc_pq:to_flatqueue(foo)),
	?assertError(badarg, hnc_pq:to_flatqueue(x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:to_flatqueue(a, x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:to_flatqueue(x, a, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:to_flatqueue(x, y, hnc_pq:new([a]))),

	lists:foreach(
		fun ({A, B, C}) ->
			PQ=hnc_pq:from_list([{a, A}, {b, B}, {c, C}]),

			?assertEqual(A++B++C, queue:to_list(hnc_pq:to_flatqueue(PQ))),
			?assertEqual(A++B++C, queue:to_list(hnc_pq:to_flatqueue(a, c, PQ))),

			?assertEqual(A, queue:to_list(hnc_pq:to_flatqueue(a, PQ))),
			?assertEqual(A, queue:to_list(hnc_pq:to_flatqueue(a, a, PQ))),

			?assertEqual(B, queue:to_list(hnc_pq:to_flatqueue(b, PQ))),
			?assertEqual(B, queue:to_list(hnc_pq:to_flatqueue(b, b, PQ))),

			?assertEqual(C, queue:to_list(hnc_pq:to_flatqueue(c, PQ))),
			?assertEqual(C, queue:to_list(hnc_pq:to_flatqueue(c, c, PQ))),

			?assertEqual(A++B, queue:to_list(hnc_pq:to_flatqueue(a, b, PQ))),

			?assertEqual(B++C, queue:to_list(hnc_pq:to_flatqueue(b, c, PQ))),

			?assertEqual([], queue:to_list(hnc_pq:to_flatqueue(c, a, PQ)))
		end,
		[
			{T1, T2, T3}
			||
			T1 <- [[], [a1], [a1, a2]],
			T2 <- [[], [b1], [b1, b2]],
			T3 <- [[], [c1], [c1, c2]]
		]
	),

	ok.

is_empty_test() ->
	?assertError(badarg, hnc_pq:is_empty(foo)),
	?assertError(badarg, hnc_pq:is_empty(x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:is_empty(a, x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:is_empty(x, a, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:is_empty(x, y, hnc_pq:new([a]))),

	lists:foreach(
		fun ({A, B, C}) ->
			PQ=hnc_pq:from_list([{a, A}, {b, B}, {c, C}]),

			?assertEqual(A==[] andalso B==[] andalso C==[], hnc_pq:is_empty(PQ)),
			?assertEqual(A==[] andalso B==[] andalso C==[], hnc_pq:is_empty(a, c, PQ)),

			?assertEqual(A==[], hnc_pq:is_empty(a, PQ)),
			?assertEqual(A==[], hnc_pq:is_empty(a, a, PQ)),

			?assertEqual(B==[], hnc_pq:is_empty(b, PQ)),
			?assertEqual(B==[], hnc_pq:is_empty(b, b, PQ)),

			?assertEqual(C==[], hnc_pq:is_empty(c, PQ)),
			?assertEqual(C==[], hnc_pq:is_empty(c, c, PQ)),

			?assertEqual(A==[] andalso B==[], hnc_pq:is_empty(a, b, PQ)),

			?assertEqual(B==[] andalso C==[], hnc_pq:is_empty(b, c, PQ)),

			?assert(hnc_pq:is_empty(c, a, PQ))
		end,
		[
			{T1, T2, T3}
			||
			T1 <- [[], [a1], [a1, a2]],
			T2 <- [[], [b1], [b1, b2]],
			T3 <- [[], [c1], [c1, c2]]
		]
	),

	ok.

length_test() ->
	?assertError(badarg, hnc_pq:length(foo)),
	?assertError(badarg, hnc_pq:length(x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:length(a, x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:length(x, a, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:length(x, y, hnc_pq:new([a]))),

	lists:foreach(
		fun ({A, B, C}) ->
			LA=length(A),
			LB=length(B),
			LC=length(C),

			PQ=hnc_pq:from_list([{a, A}, {b, B}, {c, C}]),

			?assertEqual(LA+LB+LC, hnc_pq:length(PQ)),
			?assertEqual(LA+LB+LC, hnc_pq:length(a, c, PQ)),

			?assertEqual(LA, hnc_pq:length(a, PQ)),
			?assertEqual(LA, hnc_pq:length(a, a, PQ)),

			?assertEqual(LB, hnc_pq:length(b, PQ)),
			?assertEqual(LB, hnc_pq:length(b, b, PQ)),

			?assertEqual(LC, hnc_pq:length(c, PQ)),
			?assertEqual(LC, hnc_pq:length(c, c, PQ)),

			?assertEqual(LA+LB, hnc_pq:length(a, b, PQ)),

			?assertEqual(LB+LC, hnc_pq:length(b, c, PQ)),

			?assertEqual(0, hnc_pq:length(c, a, PQ))
		end,
		[
			{T1, T2, T3}
			||
			T1 <- [[], [a1], [a1, a2]],
			T2 <- [[], [b1], [b1, b2]],
			T3 <- [[], [c1], [c1, c2]]
		]
	),

	ok.

member_test() ->
	?assertError(badarg, hnc_pq:member(bar, foo)),
	?assertError(badarg, hnc_pq:member(x, foo, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:member(a, x, foo, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:member(x, a, foo, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:member(x, y, foo, hnc_pq:new([a]))),

	lists:foreach(
		fun ({M, A, B, C}) ->
			MA=lists:member(M, A),
			MB=lists:member(M, B),
			MC=lists:member(M, C),

			PQ=hnc_pq:from_list([{a, A}, {b, B}, {c, C}]),

			?assertEqual(MA orelse MB orelse MC, hnc_pq:member(M, PQ)),
			?assertEqual(MA orelse MB orelse MC, hnc_pq:member(a, c, M, PQ)),

			?assertEqual(MA, hnc_pq:member(a, M, PQ)),
			?assertEqual(MA, hnc_pq:member(a, a, M, PQ)),

			?assertEqual(MB, hnc_pq:member(b, M, PQ)),
			?assertEqual(MB, hnc_pq:member(b, b, M, PQ)),

			?assertEqual(MC, hnc_pq:member(c, M, PQ)),
			?assertEqual(MC, hnc_pq:member(c, c, M, PQ)),

			?assertEqual(MA orelse MB, hnc_pq:member(a, b, M, PQ)),

			?assertEqual(MB orelse MC, hnc_pq:member(b, c, M, PQ)),

			?assertEqual(false, hnc_pq:member(c, a, M, PQ))
		end,
		[
			{T0, T1, T2, T3}
			||
			T0 <- [x, a1, a2, b1, b2, c1, c2],
			T1 <- [[], [a1], [a1, a2]],
			T2 <- [[], [b1], [b1, b2]],
			T3 <- [[], [c1], [c1, c2]]
		]
	),

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
	Q1=hnc_pq:from_list([{a, [a1, a2]}, {b, [b1, b2]}]),
	?assertEqual([a1, a2], queue:to_list(hnc_pq:queue(a, Q1))),
	?assertEqual([b1, b2], queue:to_list(hnc_pq:queue(b, Q1))),
	ok.

reverse_priorities_test() ->
	?assertError(badarg, hnc_pq:reverse_priorities(foo)),
	?assertError(badarg, hnc_pq:reverse_priorities(a, x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:reverse_priorities(x, a, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:reverse_priorities(x, y, hnc_pq:new([a]))),

	lists:foreach(
		fun ({A, B, C}) ->
			QA={a, A},
			QB={b, B},
			QC={c, C},

			PQ=hnc_pq:from_list([QA, QB, QC]),

			?assertEqual([QC, QB, QA], hnc_pq:to_list(hnc_pq:reverse_priorities(PQ))),
			?assertEqual([QC, QB, QA], hnc_pq:to_list(hnc_pq:reverse_priorities(a, c, PQ))),

			?assertEqual([QA, QB, QC], hnc_pq:to_list(hnc_pq:reverse_priorities(a, a, PQ))),

			?assertEqual([QA, QB, QC], hnc_pq:to_list(hnc_pq:reverse_priorities(b, b, PQ))),

			?assertEqual([QA, QB, QC], hnc_pq:to_list(hnc_pq:reverse_priorities(c, c, PQ))),

			?assertEqual([QB, QA, QC], hnc_pq:to_list(hnc_pq:reverse_priorities(a, b, PQ))),

			?assertEqual([QA, QC, QB], hnc_pq:to_list(hnc_pq:reverse_priorities(b, c, PQ))),

			?assertEqual([QA, QB, QC], hnc_pq:to_list(hnc_pq:reverse_priorities(c, a, PQ)))
		end,
		[
			{T1, T2, T3}
			||
			T1 <- [[], [a1], [a1, a2]],
			T2 <- [[], [b1], [b1, b2]],
			T3 <- [[], [c1], [c1, c2]]
		]
	),

	ok.

reverse_test() ->
	?assertError(badarg, hnc_pq:reverse(foo)),
	?assertError(badarg, hnc_pq:reverse(x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:reverse(a, x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:reverse(x, a, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:reverse(x, y, hnc_pq:new([a]))),

	lists:foreach(
		fun ({A, B, C}) ->
			QA={a, A},
			QB={b, B},
			QC={c, C},
			RA={a, lists:reverse(A)},
			RB={b, lists:reverse(B)},
			RC={c, lists:reverse(C)},

			PQ=hnc_pq:from_list([QA, QB, QC]),

			?assertEqual([RC, RB, RA], hnc_pq:to_list(hnc_pq:reverse(PQ))),
			?assertEqual([RC, RB, RA], hnc_pq:to_list(hnc_pq:reverse(a, c, PQ))),

			?assertEqual([RA, QB, QC], hnc_pq:to_list(hnc_pq:reverse(a, PQ))),
			?assertEqual([RA, QB, QC], hnc_pq:to_list(hnc_pq:reverse(a, a, PQ))),

			?assertEqual([QA, RB, QC], hnc_pq:to_list(hnc_pq:reverse(b, PQ))),
			?assertEqual([QA, RB, QC], hnc_pq:to_list(hnc_pq:reverse(b, b, PQ))),

			?assertEqual([QA, QB, RC], hnc_pq:to_list(hnc_pq:reverse(c, PQ))),
			?assertEqual([QA, QB, RC], hnc_pq:to_list(hnc_pq:reverse(c, c, PQ))),

			?assertEqual([RB, RA, QC], hnc_pq:to_list(hnc_pq:reverse(a, b, PQ))),

			?assertEqual([QA, RC, RB], hnc_pq:to_list(hnc_pq:reverse(b, c, PQ))),

			?assertEqual([QA, QB, QC], hnc_pq:to_list(hnc_pq:reverse(c, a, PQ)))
		end,
		[
			{T1, T2, T3}
			||
			T1 <- [[], [a1], [a1, a2]],
			T2 <- [[], [b1], [b1, b2]],
			T3 <- [[], [c1], [c1, c2]]
		]
	),

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
	?assertError(badarg, hnc_pq:in(x, foo, hnc_pq:new([a]))),

	lists:foreach(
		fun ({A, B, C}) ->
			PQ=hnc_pq:from_list([{a, A}, {b, B}, {c, C}]),

			?assertEqual(A++B++C++[x], hnc_pq:to_flatlist(hnc_pq:in(x, PQ))),

			?assertEqual(A++[x]++B++C, hnc_pq:to_flatlist(hnc_pq:in(a, x, PQ))),

			?assertEqual(A++B++[x]++C, hnc_pq:to_flatlist(hnc_pq:in(b, x, PQ))),

			?assertEqual(A++B++C++[x], hnc_pq:to_flatlist(hnc_pq:in(c, x, PQ)))
		end,
		[
			{T1, T2, T3}
			||
			T1 <- [[], [a1], [a1, a2]],
			T2 <- [[], [b1], [b1, b2]],
			T3 <- [[], [c1], [c1, c2]]
		]
	),

	ok.

in_r_test() ->
	?assertError(badarg, hnc_pq:in_r(bar, foo)),
	?assertError(badarg, hnc_pq:in_r(x, foo, hnc_pq:new([a]))),

	lists:foreach(
		fun ({A, B, C}) ->
			PQ=hnc_pq:from_list([{a, A}, {b, B}, {c, C}]),

			?assertEqual([x]++A++B++C, hnc_pq:to_flatlist(hnc_pq:in_r(x, PQ))),

			?assertEqual([x]++A++B++C, hnc_pq:to_flatlist(hnc_pq:in_r(a, x, PQ))),

			?assertEqual(A++[x]++B++C, hnc_pq:to_flatlist(hnc_pq:in_r(b, x, PQ))),

			?assertEqual(A++B++[x]++C, hnc_pq:to_flatlist(hnc_pq:in_r(c, x, PQ)))
		end,
		[
			{T1, T2, T3}
			||
			T1 <- [[], [a1], [a1, a2]],
			T2 <- [[], [b1], [b1, b2]],
			T3 <- [[], [c1], [c1, c2]]
		]
	),

	ok.

out_test() ->
	?assertError(badarg, hnc_pq:out(foo)),
	?assertError(badarg, hnc_pq:out(x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:out(a, x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:out(x, a, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:out(x, y, hnc_pq:new([a]))),

	lists:foreach(
		fun
			({A, B, C}) ->
				PQ=hnc_pq:from_list([{a, A}, {b, B}, {c, C}]),

				R1=hnc_pq:out(PQ),
				?assertEqual(R1, hnc_pq:out(a, c, PQ)),

				R2=hnc_pq:out(a, PQ),
				?assertEqual(R2, hnc_pq:out(a, a, PQ)),

				R3=hnc_pq:out(b, PQ),
				?assertEqual(R3, hnc_pq:out(b, b, PQ)),

				R4=hnc_pq:out(c, PQ),
				?assertEqual(R4, hnc_pq:out(c, c, PQ)),

				R5=hnc_pq:out(a, b, PQ),

				R6=hnc_pq:out(b, c, PQ),

				?assertEqual(empty, hnc_pq:out(c, a, PQ)),

				case R1 of
					empty ->
						?assertEqual([], A++B++C);
					{I1, PQ1} ->
						?assertEqual(hd(A++B++C), I1),
						?assertEqual(tl(A++B++C), hnc_pq:to_flatlist(PQ1))
				end,

				case R2 of
					empty ->
						?assertEqual([], A);
					{I2, PQ2} ->
						?assertEqual(hd(A), I2),
						?assertEqual(tl(A)++B++C, hnc_pq:to_flatlist(PQ2))
				end,

				case R3 of
					empty ->
						?assertEqual([], B);
					{I3, PQ3} ->
						?assertEqual(hd(B), I3),
						?assertEqual(A++tl(B)++C, hnc_pq:to_flatlist(PQ3))
				end,

				case R4 of
					empty ->
						?assertEqual([], C);
					{I4, PQ4} ->
						?assertEqual(hd(C), I4),
						?assertEqual(A++B++tl(C), hnc_pq:to_flatlist(PQ4))
				end,

				case R5 of
					empty ->
						?assertEqual([], A++B);
					{I5, PQ5} ->
						?assertEqual(hd(A++B), I5),
						?assertEqual(tl(A++B)++C, hnc_pq:to_flatlist(PQ5))
				end,

				case R6 of
					empty ->
						?assertEqual([], B++C);
					{I6, PQ6} ->
						?assertEqual(hd(B++C), I6),
						?assertEqual(A++tl(B++C), hnc_pq:to_flatlist(PQ6))
				end
		end,
		[
			{T1, T2, T3}
			||
			T1 <- [[], [a1], [a1, a2]],
			T2 <- [[], [b1], [b1, b2]],
			T3 <- [[], [c1], [c1, c2]]
		]
	),

	ok.

out_r_test() ->
	?assertError(badarg, hnc_pq:out_r(foo)),
	?assertError(badarg, hnc_pq:out_r(x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:out_r(a, x, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:out_r(x, a, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:out_r(x, y, hnc_pq:new([a]))),

	lists:foreach(
		fun
			({A, B, C}) ->
				PQ=hnc_pq:from_list([{a, A}, {b, B}, {c, C}]),

				R1=hnc_pq:out_r(PQ),
				?assertEqual(R1, hnc_pq:out_r(a, c, PQ)),

				R2=hnc_pq:out_r(a, PQ),
				?assertEqual(R2, hnc_pq:out_r(a, a, PQ)),

				R3=hnc_pq:out_r(b, PQ),
				?assertEqual(R3, hnc_pq:out_r(b, b, PQ)),

				R4=hnc_pq:out_r(c, PQ),
				?assertEqual(R4, hnc_pq:out_r(c, c, PQ)),

				R5=hnc_pq:out_r(a, b, PQ),

				R6=hnc_pq:out_r(b, c, PQ),

				?assertEqual(empty, hnc_pq:out_r(c, a, PQ)),

				case R1 of
					empty ->
						?assertEqual([], A++B++C);
					{I1, PQ1} ->
						?assertEqual(lists:last(A++B++C), I1),
						?assertEqual(lists:reverse(tl(lists:reverse(A++B++C))), hnc_pq:to_flatlist(PQ1))
				end,

				case R2 of
					empty ->
						?assertEqual([], A);
					{I2, PQ2} ->
						?assertEqual(lists:last(A), I2),
						?assertEqual(lists:reverse(tl(lists:reverse(A)))++B++C, hnc_pq:to_flatlist(PQ2))
				end,

				case R3 of
					empty ->
						?assertEqual([], B);
					{I3, PQ3} ->
						?assertEqual(lists:last(B), I3),
						?assertEqual(A++lists:reverse(tl(lists:reverse(B)))++C, hnc_pq:to_flatlist(PQ3))
				end,

				case R4 of
					empty ->
						?assertEqual([], C);
					{I4, PQ4} ->
						?assertEqual(lists:last(C), I4),
						?assertEqual(A++B++lists:reverse(tl(lists:reverse(C))), hnc_pq:to_flatlist(PQ4))
				end,

				case R5 of
					empty ->
						?assertEqual([], A++B);
					{I5, PQ5} ->
						?assertEqual(lists:last(A++B), I5),
						?assertEqual(lists:reverse(tl(lists:reverse(A++B)))++C, hnc_pq:to_flatlist(PQ5))
				end,

				case R6 of
					empty ->
						?assertEqual([], B++C);
					{I6, PQ6} ->
						?assertEqual(lists:last(B++C), I6),
						?assertEqual(A++lists:reverse(tl(lists:reverse(B++C))), hnc_pq:to_flatlist(PQ6))
				end
		end,
		[
			{T1, T2, T3}
			||
			T1 <- [[], [a1], [a1, a2]],
			T2 <- [[], [b1], [b1, b2]],
			T3 <- [[], [c1], [c1, c2]]
		]
	),

	ok.

join_test() ->
	?assertError(badarg, hnc_pq:join(bar, foo, queue:new())),
	?assertError(badarg, hnc_pq:join(x, hnc_pq:new([a]), queue:new())),
	?assertError(badarg, hnc_pq:join(x, queue:new(), hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:join(a, hnc_pq:new([a]), foo)),
	?assertError(badarg, hnc_pq:join(a, foo, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:join(hnc_pq:new([a]), hnc_pq:new([b]))),
	?assertError(badarg, hnc_pq:join(hnc_pq:new([a, b]), hnc_pq:new([b, a]))),

	lists:foreach(
		fun ({{A1, B1, C1}, {A2, B2, C2}}) ->
			PQ1=hnc_pq:from_list([{a, A1}, {b, B1}, {c, C1}]),
			PQ2=hnc_pq:from_list([{a, A2}, {b, B2}, {c, C2}]),

			?assertEqual([{a, A1++A2}, {b, B1++B2}, {c, C1++C2}], hnc_pq:to_list(hnc_pq:join(PQ1, PQ2))),
			?assertEqual([{a, A2++A1}, {b, B2++B1}, {c, C2++C1}], hnc_pq:to_list(hnc_pq:join(PQ2, PQ1)))
		end,
		[
			{{T1, T2, T3}, {T4, T5, T6}}
			||
			T1 <- [[], [a1], [a1, a2]],
			T2 <- [[], [b1], [b1, b2]],
			T3 <- [[], [c1], [c1, c2]],
			T4 <- [[], [a3], [a3, a4]],
			T5 <- [[], [b3], [b3, b4]],
			T6 <- [[], [c3], [c3, c4]]
		]
	),

	lists:foreach(
		fun ({A, B, C, X}) ->
			PQ=hnc_pq:from_list([{a, A}, {b, B}, {c, C}]),
			JQ=queue:from_list(X),

			?assertEqual([{a, A++X}, {b, B}, {c, C}], hnc_pq:to_list(hnc_pq:join(a, PQ, JQ))),
			?assertEqual([{a, X++A}, {b, B}, {c, C}], hnc_pq:to_list(hnc_pq:join(a, JQ, PQ))),

			?assertEqual([{a, A}, {b, B++X}, {c, C}], hnc_pq:to_list(hnc_pq:join(b, PQ, JQ))),
			?assertEqual([{a, A}, {b, X++B}, {c, C}], hnc_pq:to_list(hnc_pq:join(b, JQ, PQ))),

			?assertEqual([{a, A}, {b, B}, {c, C++X}], hnc_pq:to_list(hnc_pq:join(c, PQ, JQ))),
			?assertEqual([{a, A}, {b, B}, {c, X++C}], hnc_pq:to_list(hnc_pq:join(c, JQ, PQ)))
		end,
		[
			{T1, T2, T3, T4}
			||
			T1 <- [[], [a1], [a1, a2]],
			T2 <- [[], [b1], [b1, b2]],
			T3 <- [[], [c1], [c1, c2]],
			T4 <- [[], [x1], [x1, x2]]
		]
	),

	ok.

filter_test() ->
	?assertError(badarg, hnc_pq:filter(x, fun (_, _) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:filter(a, x, fun (_, _) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:filter(x, a, fun (_, _) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:filter(x, y, fun (_, _) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:filter(a, fun (_) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:filter(fun (_) -> true end, hnc_pq:new([a]))),

	lists:foreach(
		fun ({F, A, B, C}) ->
			PQ=hnc_pq:from_list([{a, A}, {b, B}, {c, C}]),
			QF=fun (P, L) -> queue:to_list(queue:filter(fun (I) -> F(P, I) end, queue:from_list(L))) end,

			R1=hnc_pq:filter(F, PQ),
			?assertEqual(
				[
					{a, QF(a, A)},
					{b, QF(b, B)},
					{c, QF(c, C)}
				],
				hnc_pq:to_list(R1)
			),
			?assertEqual(R1, hnc_pq:filter(a, c, F, PQ)),

			R2=hnc_pq:filter(a, F, PQ),
			?assertEqual(
				[
					{a, QF(a, A)},
					{b, B},
					{c, C}
				],
				hnc_pq:to_list(R2)
			),
			?assertEqual(R2, hnc_pq:filter(a, a, F, PQ)),

			R3=hnc_pq:filter(b, F, PQ),
			?assertEqual(
				[
					{a, A},
					{b, QF(b, B)},
					{c, C}
				],
				hnc_pq:to_list(R3)
			),
			?assertEqual(R3, hnc_pq:filter(b, b, F, PQ)),

			R4=hnc_pq:filter(c, F, PQ),
			?assertEqual(
				[
					{a, A},
					{b, B},
					{c, QF(c, C)}
				],
				hnc_pq:to_list(R4)
			),
			?assertEqual(R4, hnc_pq:filter(c, c, F, PQ)),

			?assertEqual(
				[
					{a, QF(a, A)},
					{b, QF(b, B)},
					{c, C}
				],
				hnc_pq:to_list(hnc_pq:filter(a, b, F, PQ))
			),

			?assertEqual(
				[
					{a, A},
					{b, QF(b, B)},
					{c, QF(c, C)}
				],
				hnc_pq:to_list(hnc_pq:filter(b, c, F, PQ))
			),

			?assertEqual(PQ, hnc_pq:filter(c, a, F, PQ))
		end,
		[
			{T1, T2, T3, T4}
			||
			T1 <- [
				fun (_, _) -> true end,
				fun (_, _) -> false end,
				fun (P, I) -> [{P, I}] end
			],
			T2 <- [[], [a1], [a1, a2]],
			T3 <- [[], [b1], [b1, b2]],
			T4 <- [[], [c1], [c1, c2]]
		]
	),

	ok.

fold_test() ->
	?assertError(badarg, hnc_pq:fold(x, fun (_, _, _) -> true end, 0, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:fold(a, x, fun (_, _, _) -> true end, 0, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:fold(x, a, fun (_, _, _) -> true end, 0, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:fold(x, y, fun (_, _, _) -> true end, 0, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:fold(a, fun (_, _) -> true end, 0, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:fold(fun (_, _) -> true end, 0, hnc_pq:new([a]))),

	lists:foreach(
		fun ({{F, Acc0}, A, B, C}) ->
			PQ=hnc_pq:from_list([{a, A}, {b, B}, {c, C}]),
			LF=fun (P, L, A0) -> lists:foldl(fun (I, AccIn) -> F(P, I, AccIn) end, A0, L) end,

			R1=hnc_pq:fold(F, Acc0, PQ),
			?assertEqual(LF(c, C, LF(b, B, LF(a, A, Acc0))), R1),
			?assertEqual(R1, hnc_pq:fold(a, c, F, Acc0, PQ)),

			R2=hnc_pq:fold(a, F, Acc0, PQ),
			?assertEqual(LF(a, A, Acc0), R2),
			?assertEqual(R2, hnc_pq:fold(a, a, F, Acc0, PQ)),

			R3=hnc_pq:fold(b, F, Acc0, PQ),
			?assertEqual(LF(b, B, Acc0), R3),
			?assertEqual(R3, hnc_pq:fold(b, b, F, Acc0, PQ)),

			R4=hnc_pq:fold(c, F, Acc0, PQ),
			?assertEqual(LF(c, C, Acc0), R4),
			?assertEqual(R4, hnc_pq:fold(c, c, F, Acc0, PQ)),

			R5=hnc_pq:fold(a, b, F, Acc0, PQ),
			?assertEqual(LF(b, B, LF(a, A, Acc0)), R5),

			R6=hnc_pq:fold(b, c, F, Acc0, PQ),
			?assertEqual(LF(c, C, LF(b, B, Acc0)), R6),

			?assertEqual(Acc0, hnc_pq:fold(c, a, F, Acc0, PQ))
		end,
		[
			{T1, T2, T3, T4}
			||
			T1 <- [
				{fun (_, _, Acc) -> Acc+1 end, 0},
				{fun (P, I, Acc) -> [{P, I}|Acc] end, []}
			],
			T2 <- [[], [a1], [a1, a2]],
			T3 <- [[], [b1], [b1, b2]],
			T4 <- [[], [c1], [c1, c2]]
		]
	),

	ok.

any_test() ->
	?assertError(badarg, hnc_pq:any(a, x, fun (_, _) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:any(x, a, fun (_, _) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:any(x, x, fun (_, _) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:any(a, a, fun (_) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:any(x, fun (_, _) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:any(a, fun (_) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:any(fun (_) -> true end, hnc_pq:new([a]))),

	lists:foreach(
		fun ({F, A, B, C}) ->
			PQ=hnc_pq:from_list([{a, A}, {b, B}, {c, C}]),
			QF=fun (P, L) -> lists:any(fun (I) -> F(P, I) end, L) end,

			R1=hnc_pq:any(F, PQ),
			?assertEqual(QF(a, A) orelse QF(b, B) orelse QF(c, C), R1),
			?assertEqual(R1, hnc_pq:any(a, c, F, PQ)),

			R2=hnc_pq:any(a, F, PQ),
			?assertEqual(QF(a, A), R2),
			?assertEqual(R2, hnc_pq:any(a, a, F, PQ)),

			R3=hnc_pq:any(b, F, PQ),
			?assertEqual(QF(b, B), R3),
			?assertEqual(R3, hnc_pq:any(b, b, F, PQ)),

			R4=hnc_pq:any(c, F, PQ),
			?assertEqual(QF(c, C), R4),
			?assertEqual(R4, hnc_pq:any(c, c, F, PQ)),

			R5=hnc_pq:any(a, b, F, PQ),
			?assertEqual(QF(a, A) orelse QF(b, B), R5),

			R6=hnc_pq:any(b, c, F, PQ),
			?assertEqual(QF(b, B) orelse QF(c, C), R6),

			?assertEqual(false, hnc_pq:any(c, a, F, PQ))
		end,
		[
			{T1, T2, T3, T4}
			||
			T1 <- [
				fun (_, _) -> true end,
				fun (_, _) -> false end,
				fun (_, I) -> I rem 2==0 end,
				fun (_, I) -> I rem 2/=0 end
			],
			T2 <- [[], [1], [1, 2]],
			T3 <- [[], [3], [3, 4]],
			T4 <- [[], [5], [5, 6]]
		]
	),

	ok.

all_test() ->
	?assertError(badarg, hnc_pq:all(a, x, fun (_, _) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:all(x, a, fun (_, _) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:all(x, x, fun (_, _) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:all(a, a, fun (_) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:all(x, fun (_, _) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:all(a, fun (_) -> true end, hnc_pq:new([a]))),
	?assertError(badarg, hnc_pq:all(fun (_) -> true end, hnc_pq:new([a]))),

	lists:foreach(
		fun ({F, A, B, C}) ->
			PQ=hnc_pq:from_list([{a, A}, {b, B}, {c, C}]),
			QF=fun (P, L) -> lists:all(fun (I) -> F(P, I) end, L) end,

			R1=hnc_pq:all(F, PQ),
			?assertEqual(QF(a, A) andalso QF(b, B) andalso QF(c, C), R1),
			?assertEqual(R1, hnc_pq:all(a, c, F, PQ)),

			R2=hnc_pq:all(a, F, PQ),
			?assertEqual(QF(a, A), R2),
			?assertEqual(R2, hnc_pq:all(a, a, F, PQ)),

			R3=hnc_pq:all(b, F, PQ),
			?assertEqual(QF(b, B), R3),
			?assertEqual(R3, hnc_pq:all(b, b, F, PQ)),

			R4=hnc_pq:all(c, F, PQ),
			?assertEqual(QF(c, C), R4),
			?assertEqual(R4, hnc_pq:all(c, c, F, PQ)),

			R5=hnc_pq:all(a, b, F, PQ),
			?assertEqual(QF(a, A) andalso QF(b, B), R5),

			R6=hnc_pq:all(b, c, F, PQ),
			?assertEqual(QF(b, B) andalso QF(c, C), R6),

			?assertEqual(true, hnc_pq:all(c, a, F, PQ))
		end,
		[
			{T1, T2, T3, T4}
			||
			T1 <- [
				fun (_, _) -> true end,
				fun (_, _) -> false end,
				fun (_, I) -> I rem 2==0 end,
				fun (_, I) -> I rem 2/=0 end
			],
			T2 <- [[], [1], [1, 2]],
			T3 <- [[], [3], [3, 4]],
			T4 <- [[], [5], [5, 6]]
		]
	),

	ok.

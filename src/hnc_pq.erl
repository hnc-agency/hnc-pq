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

%% @doc Erlang priority queues.
%%
%% `hnc_pq' bundles a set of queues, ranked by priority.
%%
%% For most functions, three flavors exist:
%% <ul>
%%   <li>Functions addressing the entire priority queue, for example
%%       `out/1', which will return the item at the front of the
%%       highest-priority subqueue which is not empty, if any items
%%       exist in the entire priority queue at all.</li>
%%   <li>Functions addressing a specific subqueue of the priority
%%       queue, for example `out/2', which will return the item at
%%       the front of the given subqueue, if any items exist in that
%%       subqueue at all.</li>
%%   <li>Functions addressing a range of subqueues of the priority
%%       queue, for example `out/3', which will return the item at
%%       the front of the highest-priority subqueue in the given
%%       range which is not empty, if any items exist in the given
%%       range at all.</li>
%% </ul>
%%
%% Functions operate in priority queue order, iterating the subqueues
%% from highest to lowest priority, and processing each subqueue from
%% front to rear (except `out_r/1,2,3' which operate in reverse
%% priority queue order).
%%
%% For this reason, the functions addressing a range of subqueues
%% take the maximum priority as their first and the minimum priority
%% as their second argument, different from what one might otherwise
%% be used to.
-module(hnc_pq).

-export([all/2, all/3, all/4]).
-export([any/2, any/3, any/4]).
-export([filter/2, filter/3, filter/4]).
-export([fold/3, fold/4, fold/5]).
-export([from_list/1]).
-export([in/2, in/3]).
-export([in_r/2, in_r/3]).
-export([is_empty/1, is_empty/2, is_empty/3]).
-export([is_priority/2]).
-export([join/2, join/3]).
-export([length/1, length/2, length/3]).
-export([member/2, member/3, member/4]).
-export([new/1]).
-export([out/1, out/2, out/3]).
-export([out_r/1, out_r/2, out_r/3]).
-export([priorities/1]).
-export([queue/2]).
-export([reprioritize/2]).
-export([reverse/1, reverse/2, reverse/3]).
-export([reverse_priorities/1, reverse_priorities/3]).
-export([to_flatlist/1, to_flatlist/2, to_flatlist/3]).
-export([to_flatqueue/1, to_flatqueue/2, to_flatqueue/3]).
-export([to_list/1, to_list/2, to_list/3]).

-record(pq, {
	p=[]        :: list(Prio),
	l=undefined :: Prio,
	q=#{}       :: #{Prio => queue:queue()}
}).

-opaque prioqueue() :: #pq{}.
%% As returned by `new/1'.
-export_type([prioqueue/0]).

-type priority() :: term().
%% Priority label.

-type item() :: term().
%% Queue item.

%% @doc Create a new priority queue.
%% 
%% The argument given to this function is a list of the priorities
%% the priority queue should have, in descending order.
-spec new(Priorities) -> PrioQueue when
	Priorities :: nonempty_list(Priority),
	Priority :: priority(),
	PrioQueue :: prioqueue().
new(Prios=[_|_]) ->
	lists:foldl(
		fun
			(Prio, #pq{q=Queues}) when is_map_key(Prio, Queues) ->
				error(badarg, [Prios]);
			(Prio, PQ=#pq{q=Queues}) ->
				PQ#pq{l=Prio, q=maps:put(Prio, queue:new(), Queues)}
		end,
		#pq{p=Prios},
		Prios
	);
new(Prios) ->
	error(badarg, [Prios]).

%% @doc Convert a subqueue of a priority queue to a flat queue of items.
%%
%% The items in the result queue are ordered from front to rear.
-spec to_flatqueue(Priority, PrioQueue) -> FlatQueue when
	Priority :: priority(),
	PrioQueue :: prioqueue(),
	FlatQueue :: queue:queue().
to_flatqueue(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_to_flatqueue([Prio], PQ);
to_flatqueue(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Convert a range of subqueues of a priority queue to a flat queue of items.
%%
%% The items in the result queue are ordered from the front
%% element of the subqueue with highest priority to the rear element of
%% the subqueue with the lowest priority, which is the order in which they would
%% be returned by successive calls to `out/3'.
-spec to_flatqueue(MaxPriority, MinPriority, PrioQueue) -> FlatQueue when
	MaxPriority :: priority(),
	MinPriority :: priority(),
	PrioQueue :: prioqueue(),
	FlatQueue :: queue:queue().
to_flatqueue(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_to_flatqueue([Prio], PQ);
to_flatqueue(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_to_flatqueue(Prios, PQ);
to_flatqueue(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_to_flatqueue(Range, PQ);
to_flatqueue(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% @doc Convert a priority queue to a flat queue of items.
%%
%% The items in the result queue are ordered from the front
%% element of the highest priority to the rear element of
%% the lowest priority, which is the order in which they would
%% be returned by successive calls to `out/1'.
-spec to_flatqueue(PrioQueue) -> FlatQueue when
	PrioQueue :: prioqueue(),
	FlatQueue :: queue:queue().
to_flatqueue(PQ=#pq{p=Prios}) ->
	do_to_flatqueue(Prios, PQ);
to_flatqueue(PQ) ->
	error(badarg, [PQ]).

do_to_flatqueue([], _) ->
	queue:new();
do_to_flatqueue(Range, #pq{q=Queues}) ->
	lists:foldr(
		fun (Prio, Acc) ->
			queue:join(maps:get(Prio, Queues), Acc)
		end,
		queue:new(),
		Range
	).

%% @doc Convert a specific subqueue of the priority queue to a list of priorities and items.
%%
%% The result list contains a tuple of the form `{Priority, Items}' for
%% each subqueue, in descending priority order. `Items' is a list of the
%% items in the subqueue, ordered from front to rear.
-spec to_list(Priority, PrioQueue) -> PrioList when
	Priority :: priority(),
	PrioQueue :: prioqueue(),
	PrioList :: list({Priority, ItemList}),
	ItemList :: list(Item),
	Item :: item().
to_list(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_to_list([Prio], PQ);
to_list(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Convert a range of subqueues of the priority queue to a list of items.
%%
%% The result list contains a tuple of the form `{Priority, Items}' for
%% each subqueue, in descending priority order. `Items' is a list of the
%% items in a subqueue, ordered from front to rear.
-spec to_list(MaxPriority, MinPriority, PrioQueue) -> PrioList when
	MaxPriority :: priority(),
	MinPriority :: priority(),
	PrioQueue :: prioqueue(),
	PrioList :: list({Priority, ItemList}),
	Priority :: priority(),
	ItemList :: list(Item),
	Item :: item().
to_list(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_to_list([Prio], PQ);
to_list(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_to_list(Prios, PQ);
to_list(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_to_list(Range, PQ);
to_list(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% @doc Convert a priority queue to a list of priorities and items.
%%
%% The result list contains a tuple of the form `{Priority, Items}' for
%% each subqueue, in descending priority order. `Items' is a list of the
%% items in a subqueue, ordered from front to rear.
-spec to_list(PrioQueue) -> PrioList when
	PrioQueue :: prioqueue(),
	PrioList :: list({Priority, ItemList}),
	Priority :: priority(),
	ItemList :: list(Item),
	Item :: item().
to_list(PQ=#pq{p=Prios}) ->
	do_to_list(Prios, PQ);
to_list(PQ) ->
	error(badarg, [PQ]).

do_to_list([], _) ->
	[];
do_to_list(Range, #pq{q=Queues}) ->
	lists:foldr(
		fun (Prio, Acc) ->
			[{Prio, queue:to_list(maps:get(Prio, Queues))}|Acc]
		end,
		[],
		Range
	).

%% @doc Convert a subqueue of a priority queue to a flat list of items.
%%
%% The items in the result list are ordered from front to rear.
-spec to_flatlist(Priority, PrioQueue) -> ItemList when
	Priority :: priority(),
	PrioQueue :: prioqueue(),
	ItemList :: list(Item),
	Item :: item().
to_flatlist(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_to_flatlist([Prio], PQ);
to_flatlist(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Convert a range of subqueues of a priority queue to a flat list of items.
%%
%% The items in the result list are ordered from the front
%% element of the subqueue with the highest priority to the rear
%% element of the subqueue with the lowest priority, which is the
%% order in which they would be returned by successive calls to `out/3'.
-spec to_flatlist(MaxPriority, MinPriority, PrioQueue) -> ItemList when
	MaxPriority :: priority(),
	MinPriority :: priority(),
	PrioQueue :: prioqueue(),
	ItemList :: list(Item),
	Item :: item().
to_flatlist(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_to_flatlist([Prio], PQ);
to_flatlist(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_to_flatlist(Prios, PQ);
to_flatlist(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_to_flatlist(Range, PQ);
to_flatlist(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% @doc Convert a priority queue to a flat list of items.
%%
%% The items in the result list are ordered from the front
%% element of the highest priority to the rear element of
%% the lowest priority, which is the order in which they would
%% be returned by successive calls to `out/1'.
-spec to_flatlist(PrioQueue) -> ItemList when
	PrioQueue :: prioqueue(),
	ItemList :: list(Item),
	Item :: item().
to_flatlist(PQ=#pq{p=Prios}) ->
	do_to_flatlist(Prios, PQ);
to_flatlist(PQ) ->
	error(badarg, [PQ]).

do_to_flatlist([], _) ->
	[];
do_to_flatlist(Range, #pq{q=Queues}) ->
	lists:foldr(
		fun (Prio, Acc) ->
			queue:to_list(maps:get(Prio, Queues))++Acc
		end,
		[],
		Range
	).

%% @doc Create a priority queue from a list.
%%
%% The argument list must consist of tuples of the form
%% `{Priority, Items}', in descending priority order. `Items'
%% must be a list of the items of a subqueue, in front to
%% rear order.
-spec from_list(PrioList) -> PrioQueue when
	PrioList :: list({Priority, ItemList}),
	Priority :: priority(),
	ItemList :: list(Item),
	Item :: item(),
	PrioQueue :: prioqueue().
from_list(List=[_|_]) ->
	lists:foldr(
		fun
			({Prio, Items}, undefined) when is_list(Items) ->
				#pq{p=[Prio], l=Prio, q=#{Prio => queue:from_list(Items)}};
			({Prio, Items}, PQ=#pq{p=Prios, q=Queues}) when is_list(Items), not is_map_key(Prio, Queues) ->
				PQ#pq{p=[Prio|Prios], q=maps:put(Prio, queue:from_list(Items), Queues)};
			(_, _) ->
				error(badarg, [List])
		end,
		undefined,
		List
	);
from_list(List) ->
	error(badarg, [List]).

%% @doc Change the priorities of a priority queue.
%%
%% The resulting priority queue will have the given priorities
%% in the order specified by the first parameter. Subqueues that
%% exist in both the priorities list and the priority queue will
%% remain. Subqueues that exist in the priorities list but not in
%% the priority queue will be created empty. Subqueues that exist
%% in the priority queue but not in the priorities list will be
%% removed.
-spec reprioritize(Priorities, PrioQueue0) -> PrioQueue1 when
	Priorities :: nonempty_list(Priority),
	Priority :: priority(),
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
reprioritize(Prios, PQ=#pq{p=Prios}) ->
	PQ;
reprioritize(Prios=[_|_], #pq{q=Queues0}) ->
	Queues1=maps:from_list([{Prio, queue:new()} || Prio <- Prios]),
	Queues2=maps:with(Prios, Queues0),
	Queues3=maps:merge(Queues1, Queues2),
	Lowest=lists:last(Prios),
	#pq{p=Prios, l=Lowest, q=Queues3};
reprioritize(Prios, PQ) ->
	error(badarg, [Prios, PQ]).

%% @doc Reverse the items in a subqueue of a priority queue.
-spec reverse(Priority, PrioQueue0) -> PrioQueue1 when
	Priority :: priority(),
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
reverse(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_reverse([Prio], PQ);
reverse(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Reverse a range of subqueues of a priority queue.
%%
%% The resulting priority queue will have the priorities in the range
%% in reverse order, and the items in each subqueue in the range will
%% be in reverse order, also.
-spec reverse(MaxPriority, MinPriority, PrioQueue0) -> PrioQueue1 when
	MaxPriority :: priority(),
	MinPriority :: priority(),
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
reverse(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_reverse([Prio], PQ);
reverse(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_reverse(Prios, PQ);
reverse(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_reverse(Range, PQ);
reverse(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% @doc Reverse a priority queue.
%%
%% The resulting priority queue will have the priorities in reverse
%% order, and the items in each subqueue will be in reverse order, also.
-spec reverse(PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
reverse(PQ=#pq{p=Prios}) ->
	do_reverse(Prios, PQ);
reverse(PQ) ->
	error(badarg, [PQ]).

do_reverse([], PQ) ->
	PQ;
do_reverse([Prio], PQ=#pq{q=Queues}) ->
	PQ#pq{q=maps:update_with(Prio, fun (Queue) -> queue:reverse(Queue) end, Queues)};
do_reverse(Range, #pq{p=Prios, q=Queues}) ->
	do_reverse(Prios, Range, lists:reverse(Range), [], Queues).

do_reverse([], _, _, AccPrios, Queues) ->
	#pq{p=lists:reverse(AccPrios), l=hd(AccPrios), q=Queues};
do_reverse([Prio|Prios], [Prio|Range], [RPrio|RRange], AccPrios, Queues) ->
	Queues1=maps:update_with(RPrio, fun (Queue) -> queue:reverse(Queue) end, Queues),
	do_reverse(Prios, Range, RRange, [RPrio|AccPrios], Queues1);
do_reverse([Prio|Prios], Range, RRange, AccPrios, Queues) ->
	do_reverse(Prios, Range, RRange, [Prio|AccPrios], Queues).

%% @doc Reverse the priorities of a range of subqueues of a priority queue.
%%
%% The resulting priority queue will have the priorities in the given range
%% in reverse order, but the items in each subqueue will remain in the current order.
reverse_priorities(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	PQ;
reverse_priorities(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	PQ#pq{p=lists:reverse(Prios), l=hd(Prios)};
reverse_priorities(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_reverse_priorities(Range, PQ);
reverse_priorities(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% @doc Reverse the priorities of a priority queue.
%%
%% The resulting priority queue will have the priorities in reverse order,
%% but the items in each subqueue will remain in the current order.
-spec reverse_priorities(PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
reverse_priorities(PQ=#pq{p=Prios}) ->
	PQ#pq{p=lists:reverse(Prios), l=hd(Prios)};
reverse_priorities(PQ) ->
	error(badarg, [PQ]).

do_reverse_priorities([], PQ) ->
	PQ;
do_reverse_priorities(Range, PQ=#pq{p=Prios}) ->
	{Prios1, Lowest1}=do_reverse_priorities(Prios, Range, lists:reverse(Range), []),
	PQ#pq{p=Prios1, l=Lowest1}.

do_reverse_priorities([], _, _, AccPrios) ->
	{lists:reverse(AccPrios), hd(AccPrios)};
do_reverse_priorities([Prio|Prios], [Prio|Range], [RPrio|RRange], AccPrios) ->
	do_reverse_priorities(Prios, Range, RRange, [RPrio|AccPrios]);
do_reverse_priorities([Prio|Prios], Range, RRange, AccPrios) ->
	do_reverse_priorities(Prios, Range, RRange, [Prio|AccPrios]).

%% @doc Check if the given priority exists in a priority queue.
-spec is_priority(Priority, PrioQueue) -> boolean() when
	Priority :: priority(),
	PrioQueue :: prioqueue().
is_priority(Prio, #pq{q=Queues}) ->
	is_map_key(Prio, Queues);
is_priority(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Retrieve the priorities of a priority queue.
-spec priorities(PrioQueue) -> Priorities when
	PrioQueue :: prioqueue(),
	Priorities :: nonempty_list(Priority),
	Priority :: priority().
priorities(#pq{p=Prios}) ->
	Prios;
priorities(PQ) ->
	error(badarg, [PQ]).

%% @doc Retrieve a subqueue of a priority queue.
-spec queue(Priority, PrioQueue) -> Queue when
	Priority :: priority(),
	PrioQueue :: prioqueue(),
	Queue :: queue:queue().
queue(Prio, #pq{q=Queues}) when is_map_key(Prio, Queues) ->
	maps:get(Prio, Queues);
queue(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Check if the given item is a member of a subqueue of a priority queue.
-spec member(Priority, Item, PrioQueue) -> boolean() when
	Priority :: priority(),
	Item :: item(),
	PrioQueue :: prioqueue().
member(Prio, Item, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_member([Prio], Item, PQ);
member(Prio, Item, PQ) ->
	error(badarg, [Prio, Item, PQ]).

-spec member(MaxPriority, MinPriority, Item, PrioQueue) -> boolean() when
	MaxPriority :: priority(),
	MinPriority :: priority(),
	Item :: item(),
	PrioQueue :: prioqueue().
member(Prio, Prio, Item, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_member([Prio], Item, PQ);
member(MaxPrio, MinPrio, Item, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_member(Prios, Item, PQ);
member(MaxPrio, MinPrio, Item, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_member(Range, Item, PQ);
member(MaxPrio, MinPrio, Item, PQ) ->
	error(badarg, [MaxPrio, MinPrio, Item, PQ]).

%% @doc Check if the given item is a member of a priority queue.
-spec member(Item, PrioQueue) -> boolean() when
	Item :: item(),
	PrioQueue :: prioqueue().
member(Item, PQ=#pq{p=Prios}) ->
	do_member(Prios, Item, PQ);
member(Item, PQ) ->
	error(badarg, [Item, PQ]).

do_member([], _, _) ->
	false;
do_member(Range, Item, #pq{q=Queues}) ->
	lists:any(
		fun (Prio) ->
			queue:member(Item, maps:get(Prio, Queues))
		end,
		Range
	).

%% @doc Insert the given item into a subqueue of a priority queue, at the rear.
-spec in(Priority, Item, PrioQueue0) -> PrioQueue1 when
	Priority :: priority(),
	Item :: item(),
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
in(Prio, Item, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_in(Prio, Item, PQ);
in(Prio, Item, PQ) ->
	error(badarg, [Prio, Item, PQ]).

%% @doc Insert the given item into a priority queue, at the rear.
%%
%% The item will be inserted at the rear of the subqueue with the lowest priority.
-spec in(Item, PrioQueue0) -> PrioQueue1 when
	Item :: item(),
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
in(Item, PQ=#pq{l=Prio}) ->
	do_in(Prio, Item, PQ);
in(Item, PQ) ->
	error(badarg, [Item, PQ]).
 
do_in(Prio, Item, PQ=#pq{q=Queues0}) ->
	Queues1=maps:update_with(Prio, fun (Queue) -> queue:in(Item, Queue) end, Queues0),
	PQ#pq{q=Queues1}.

%% @doc Insert the given item into a subqueue of a priority queue, at the front.
-spec in_r(Priority, Item, PrioQueue0) -> PrioQueue1 when
	Priority :: priority(),
	Item :: item(),
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
in_r(Prio, Item, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_in_r(Prio, Item, PQ);
in_r(Prio, Item, PQ) ->
	error(badarg, [Prio, Item, PQ]).

%% @doc Insert the given item into a priority queue, at the front.
%%
%% The item will be inserted at the front of the subqueue with the highest priority.
-spec in_r(Item, PrioQueue0) -> PrioQueue1 when
	Item :: item(),
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
in_r(Item, PQ=#pq{p=[Prio|_]}) ->
	do_in_r(Prio, Item, PQ);
in_r(Item, PQ) ->
	error(badarg, [Item, PQ]).

do_in_r(Prio, Item, PQ=#pq{q=Queues0}) ->
	Queues1=maps:update_with(Prio, fun (Queue) -> queue:in_r(Item, Queue) end, Queues0),
	PQ#pq{q=Queues1}.

%% @doc Retrieve and remove the next item from a subqueue of a priority queue, from the front.
%%
%% The item will be taken from the front of the specified subqueue. If the subqueue is empty,
%% the atom `empty' is returned.
-spec out(Priority, PrioQueue0) -> empty | {Item, PrioQueue1} when
	Priority :: priority(),
	PrioQueue0 :: prioqueue(),
	Item :: item(),
	PrioQueue1 :: prioqueue().
out(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_out([Prio], PQ);
out(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Retrieve and remove the next item from a range of subqueues of a priority queue,
%% from the front.
%%
%% The item will be taken from the front of the subqueue with the highest priority
%% in the given range that is not empty. If all the subqueues are empty, the atom `empty'
%% is returned.
-spec out(MaxPriority, MinPriority, PrioQueue0) -> empty | {Item, PrioQueue1} when
	MaxPriority :: priority(),
	MinPriority :: priority(),
	PrioQueue0 :: prioqueue(),
	Item :: item(),
	PrioQueue1 :: prioqueue().
out(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_out([Prio], PQ);
out(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_out(Prios, PQ);
out(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_out(Range, PQ);
out(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% @doc Retrieve and remove the next item from a priority queue, from the front.
%%
%% The item will be taken from the front of the subqueue with the highest priority
%% that is not empty. If all subqueues are empty, the atom `empty' is returned.
-spec out(PrioQueue0) -> empty | {Item, PrioQueue1} when
	PrioQueue0 :: prioqueue(),
	Item :: item(),
	PrioQueue1 :: prioqueue().
out(PQ=#pq{p=Prios}) ->
	do_out(Prios, PQ);
out(PQ) ->
	error(badarg, [PQ]).

do_out([], _) ->
	empty;
do_out([Prio|Range], PQ=#pq{q=Queues}) ->
	case queue:out(maps:get(Prio, Queues)) of
		{empty, _} ->
			do_out(Range, PQ);
		{{value, Item}, Queue1} ->
			{Item, PQ#pq{q=maps:update(Prio, Queue1, Queues)}}
	end.
	
%% @doc Retrieve and remove the next item from a subqueue of a priority queue, from the rear.
%%
%% The item will be taken from the rear of the specified subqueue. If the subqueue is empty,
%% the atom `empty' is returned.
-spec out_r(Priority, PrioQueue0) -> empty | {Item, PrioQueue1} when
	Priority :: priority(),
	PrioQueue0 :: prioqueue(),
	Item :: item(),
	PrioQueue1 :: prioqueue().
out_r(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_out_r([Prio], PQ);
out_r(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Retrieve and remove the next item from a range of subqueues of a priority queue,
%% from the rear.
%%
%% The item will be taken from the rear of the subqueue with the lowest priority
%% in the given range that is not empty. If all the subqueues are empty, the atom `empty'
%% is returned.
-spec out_r(MaxPriority, MinPriority, PrioQueue0) -> empty | {Item, PrioQueue1} when
	MaxPriority :: priority(),
	MinPriority :: priority(),
	PrioQueue0 :: prioqueue(),
	Item :: item(),
	PrioQueue1 :: prioqueue().
out_r(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_out_r([Prio], PQ);
out_r(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_out_r(lists:reverse(Prios), PQ);
out_r(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_out_r(lists:reverse(Range), PQ);
out_r(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% @doc Retrieve and remove the next item from a priority queue, from the rear.
%%
%% The item will be taken from the rear of the subqueue with the lowest priority
%% that is not empty. If all subqueues are empty, the atom `empty'  is returned.
-spec out_r(PrioQueue0) -> empty | {Item, PrioQueue1} when
	PrioQueue0 :: prioqueue(),
	Item :: item(),
	PrioQueue1 :: prioqueue().
out_r(PQ=#pq{p=Prios}) ->
	do_out_r(lists:reverse(Prios), PQ);
out_r(PQ) ->
	error(badarg, [PQ]).

do_out_r([], _) ->
	empty;
do_out_r([Prio|Range], PQ=#pq{q=Queues}) ->
	case queue:out_r(maps:get(Prio, Queues)) of
		{empty, _} ->
			do_out_r(Range, PQ);
		{{value, Item}, Queue1} ->
			{Item, PQ#pq{q=maps:update(Prio, Queue1, Queues)}}
	end.

%% @doc Check if a subqueue of a priority queue is empty.
-spec is_empty(Priority, PrioQueue) -> boolean() when
	Priority :: priority(),
	PrioQueue :: prioqueue().
is_empty(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_is_empty([Prio], PQ);
is_empty(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Check if a range of subqueues of a priority queue is empty.
-spec is_empty(MaxPriority, MinPriority, PrioQueue) -> boolean() when
	MaxPriority :: priority(),
	MinPriority :: priority(),
	PrioQueue :: prioqueue().
is_empty(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_is_empty([Prio], PQ);
is_empty(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_is_empty(Prios, PQ);
is_empty(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_is_empty(Range, PQ);
is_empty(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% @doc Check if a priority queue is empty.
-spec is_empty(PrioQueue) -> boolean() when
	PrioQueue :: prioqueue().
is_empty(PQ=#pq{p=Prios}) ->
	do_is_empty(Prios, PQ);
is_empty(PQ) ->
	error(badarg, [PQ]).

do_is_empty([], _) ->
	true;
do_is_empty([Prio|Range], PQ=#pq{q=Queues}) ->
	case queue:is_empty(maps:get(Prio, Queues)) of
		true ->
			do_is_empty(Range, PQ);
		false ->
			false
	end.

%% @doc Retrieve the number of items in a subqueue of a priority queue.
-spec length(Priority, PrioQueue) -> Length when
	Priority :: priority(),
	PrioQueue :: prioqueue(),
	Length :: non_neg_integer().
length(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_length([Prio], PQ);
length(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Retrieve the number of items in a range of subqueues of a priority queue.
-spec length(MaxPriority, MinPriority, PrioQueue) -> Length when
	MaxPriority :: priority(),
	MinPriority :: priority(),
	PrioQueue :: prioqueue(),
	Length :: non_neg_integer().
length(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_length([Prio], PQ);
length(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_length(Prios, PQ);
length(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_length(Range, PQ);
length(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% @doc Retrieve the number of items in a priority queue.
-spec length(PrioQueue) -> Length when
	PrioQueue :: prioqueue(),
	Length :: non_neg_integer().
length(PQ=#pq{p=Prios}) ->
	do_length(Prios, PQ);
length(PQ) ->
	error(badarg, [PQ]).

do_length([], _) ->
	0;
do_length([Prio], #pq{q=Queues}) ->
	queue:len(maps:get(Prio, Queues));
do_length(Range, #pq{q=Queues}) ->
	maps:fold(
		fun (_, Queue, Acc) ->
			Acc+queue:len(Queue)
		end,
		0,
		maps:with(Range, Queues)
	).

%% @doc Append or prepend a queue to a subqueue of a priority queue.
%%
%% If the second argument is a queue and the third argument is a priority
%% queue, the queue is prepended to the specified subqueue of the priority
%% queue.
%% If the second argument is a priority queue and the third argument is a
%% queue, the queue is appended to the specified subqueue of the priority
%% queue.
-spec join(Priority, PrioQueue0, JoinQueue) -> PrioQueue1 when
	Priority :: priority(),
	PrioQueue0 :: prioqueue(),
	JoinQueue :: queue:queue(),
	PrioQueue1 :: prioqueue();
          (Priority, JoinQueue, PrioQueue0) -> PrioQueue1 when
	Priority :: priority(),
	JoinQueue :: queue:queue(),
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
join(Prio, PQ=#pq{q=Queues0}, JoinQueue) when is_map_key(Prio, Queues0) ->
	Queues1=case queue:is_queue(JoinQueue) of
		true ->
			maps:update_with(Prio, fun (Queue) -> queue:join(Queue, JoinQueue) end, Queues0);
		false ->
			error(badarg, [Prio, PQ, JoinQueue])
	end,
	PQ#pq{q=Queues1};
join(Prio, JoinQueue, PQ=#pq{q=Queues0}) when is_map_key(Prio, Queues0) ->
	Queues1=case queue:is_queue(JoinQueue) of
		true ->
			maps:update_with(Prio, fun (Queue) -> queue:join(JoinQueue, Queue) end, Queues0);
		false ->
			error(badarg, [Prio, PQ, JoinQueue])
	end,
	PQ#pq{q=Queues1};
join(Prio, PQ, JoinQueue) ->
	error(badarg, [Prio, PQ, JoinQueue]).

%% @doc Join two priority queues.
%%
%% Both priority queues must have the same priorities in the same order.
%%
%% The subqueues of the second priority queue will be appended to
%% their respective counterparts in of the first priority queue.
-spec join(PrioQueue0, PrioQueue1) -> PrioQueue2 when
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue(),
	PrioQueue2 :: prioqueue().
join(#pq{p=Prios, q=Queues1}, #pq{p=Prios, q=Queues2}) ->
	Queues3=maps:map(
		fun (Prio, Queue1) ->
			Queue2=maps:get(Prio, Queues2),
			queue:join(Queue1, Queue2)
		end,
		Queues1
	),
	#pq{p=Prios, q=Queues3};
join(PQ1, PQ2) ->
	error(badarg, [PQ1, PQ2]).

%% @doc Filter items of a subqueue of a priority queue.
%%
%% The filter function takes two arguments: the priority of the
%% subqueue being filtered, and each item in turn. It must return either
%% `true' to keep the current item, `false' to drop it, or a list
%% of items that will be inserted in that place instead of the current
%% item.
%%
%% The given subqueue is filtered from front to rear.
-spec filter(Priority, FilterFun, PrioQueue0) -> PrioQueue1 when
	Priority :: priority(),
	FilterFun :: fun((Priority, QueueItem) -> boolean() | list(InsertItem)),
	QueueItem :: item(),
	InsertItem :: item(),
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
filter(Prio, Fun, PQ=#pq{q=Queues}) when is_function(Fun, 2), is_map_key(Prio, Queues) ->
	do_filter([Prio], Fun, PQ);
filter(Prio, Fun, PQ) ->
	error(badarg, [Prio, Fun, PQ]).

%% @doc Filter items of a range of subqueues of a priority queue.
%%
%% The filter function takes two arguments: the priority of each subqueue
%% being filtered in turn, and each item in turn. It must return either
%% `true' to keep the current item, `false' to drop it, or a list
%% of items that will be inserted in that place instead of the current
%% item.
%%
%% The range is filtered from highest to lowest priority,
%% and each subqueue is filtered from front to rear.
-spec filter(MaxPriority, MinPriority, FilterFun, PrioQueue0) -> PrioQueue1 when
	MaxPriority :: priority(),
	MinPriority :: priority(),
	FilterFun :: fun((Priority, QueueItem) -> boolean() | list(InsertItem)),
	Priority :: priority(),
	QueueItem :: item(),
	InsertItem :: item(),
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
filter(Prio, Prio, Fun, PQ=#pq{q=Queues}) when is_function(Fun, 2), is_map_key(Prio, Queues) ->
	do_filter([Prio], Fun, PQ);
filter(MaxPrio, MinPrio, Fun, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) when is_function(Fun, 2) ->
	do_filter(Prios, Fun, PQ);
filter(MaxPrio, MinPrio, Fun, PQ=#pq{p=Prios, q=Queues}) when is_function(Fun, 2), is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_filter(Range, Fun, PQ);
filter(MaxPrio, MinPrio, Fun, PQ) ->
	error(badarg, [MaxPrio, MinPrio, Fun, PQ]).

%% @doc Filter items of a priority queue.
%%
%% The filter function takes two arguments: the priority of each
%% subqueue being filtered in turn, and each item in turn. It must
%% return either `true' to keep the current item, `false' to drop it,
%% or a list of items that will be inserted in that place instead
%% of the current item.
%%
%% The priority queue is filtered from highest to lowest priority,
%% and each subqueue is filtered from front to rear.
-spec filter(FilterFun, PrioQueue0) -> PrioQueue1 when
	FilterFun :: fun((Priority, QueueItem) -> boolean() | list(InsertItem)),
	Priority :: priority(),
	QueueItem :: item(),
	InsertItem :: item(),
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
filter(Fun, PQ=#pq{p=Prios}) when is_function(Fun, 2) ->
	do_filter(Prios, Fun, PQ);
filter(Fun, PQ) ->
	error(badarg, [Fun, PQ]).

do_filter([], _, PQ) ->
	PQ;
do_filter([Prio|Range], Fun, PQ=#pq{q=Queues}) ->
	Queues1=maps:update_with(
		Prio,
		fun (Queue) ->
			queue:filter(
				fun (Item) ->
					Fun(Prio, Item)
				end,
				Queue
			)
		end,
		Queues
	),
	do_filter(Range, Fun, PQ#pq{q=Queues1}).

%% @doc Fold a function over a subqueue of a priority queue.
%%
%% The fold function takes three arguments: the priority of the
%% subqueue being folded over, each item in turn, and an accumulator.
%% It must return a new accumulator which is then passed to the next
%% call of the fold function for the next item.
%%
%% The given subqueue is folded from front to rear.
-spec fold(Priority, FoldFun, Acc0, PrioQueue) -> Acc1 when
	Priority :: priority(),
	FoldFun :: fun((Priority, QueueItem, AccIn) -> AccOut),
	QueueItem :: item(),
	AccIn :: term(),
	AccOut :: term(),
	Acc0 :: term(),
	PrioQueue :: prioqueue(),
	Acc1 :: term().
fold(Prio, Fun, Acc0, PQ=#pq{q=Queues}) when is_function(Fun, 3), is_map_key(Prio, Queues) ->
	do_fold([Prio], Fun, Acc0, PQ);
fold(Prio, Fun, Acc0, PQ) ->
	error(badarg, [Prio, Fun, Acc0, PQ]).

%% @doc Fold a function over a range of subqueues of a priority queue.
%%
%% The fold function takes three arguments: the priority of each
%% subqueue being folded over in turn, each item in turn, and an accumulator.
%% It must return a new accumulator which is then passed to the next
%% call of the fold function for the next item.
%%
%% The range is folded over from highest to lowest priority,
%% and each subqueue is folded over from front to rear.
fold(Prio, Prio, Fun, Acc0, PQ=#pq{q=Queues}) when is_function(Fun, 3), is_map_key(Prio, Queues) ->
	do_fold([Prio], Fun, Acc0, PQ);
fold(MaxPrio, MinPrio, Fun, Acc0, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) when is_function(Fun, 3) ->
	do_fold(Prios, Fun, Acc0, PQ);
fold(MaxPrio, MinPrio, Fun, Acc0, PQ=#pq{p=Prios, q=Queues}) when is_function(Fun, 3), is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_fold(Range, Fun, Acc0, PQ);
fold(MaxPrio, MinPrio, Fun, Acc0, PQ) ->
	error(badarg, [MaxPrio, MinPrio, Fun, Acc0, PQ]).

%% @doc Fold a function over a priority queue.
%%
%% The fold function takes three arguments: the priority of each
%% subqueue being folded over in turn, each item in turn, and
%% an accumulator.
%% It must return a new accumulator which is then passed to the next
%% call of the fold function for the next item.
%%
%% The priority queue is folded over from highest to lowest priority,
%% and each subqueue is folded over from front to rear.
-spec fold(FoldFun, Acc0, PrioQueue) -> Acc1 when
	FoldFun :: fun((Priority, QueueItem, AccIn) -> AccOut),
	Priority :: priority(),
	QueueItem :: item(),
	AccIn :: term(),
	AccOut :: term(),
	Acc0 :: term(),
	PrioQueue :: prioqueue(),
	Acc1 :: term().
fold(Fun, Acc0, PQ=#pq{p=Prios}) when is_function(Fun, 3) ->
	do_fold(Prios, Fun, Acc0, PQ);
fold(Fun, Acc0, PQ) ->
	error(badarg, [Fun, Acc0, PQ]).

do_fold([], _, Acc, _) ->
	Acc;
do_fold([Prio|Range], Fun, Acc0, PQ=#pq{q=Queues}) ->
	%% TODO: Exchange for queue:fold/3 when
	%% https://github.com/erlang/otp/pull/2791
	%% is merged and released.
	Acc1=lists:foldl(
		fun (Item, AccIn) ->
			Fun(Prio, Item, AccIn)
		end,
		Acc0,
		queue:to_list(maps:get(Prio, Queues))
	),
	do_fold(Range, Fun, Acc1, PQ).

%% @doc Check if at least one item of a subqueue of a
%% priority queue satisfies a condition.
%%
%% The predicate function takes two arguments: the priority
%% of the suqueue being checked, and each item in turn.
%% It must return `true' if the priority and item satisfies
%% the condition, otherwise `false'.
%%
%% If the predicate function returns `true' for any item,
%% this function returns `true', otherwise `false' (including
%% the case of the subqueue being empty).
-spec any(Priority, PredicateFun, PrioQueue) -> boolean() when
	Priority :: priority(),
	PredicateFun :: fun((Priority, QueueItem) -> boolean()),
	QueueItem :: item(),
	PrioQueue :: prioqueue().
any(Prio, Fun, PQ=#pq{q=Queues}) when is_function(Fun, 2), is_map_key(Prio, Queues) ->
	do_any([Prio], Fun, PQ);
any(Prio, Fun, PQ) ->
	error(badarg, [Prio, Fun, PQ]).

%% @doc Check if at least one item of a range of subqueues of a
%% priority queue satisfies a condition.
%%
%% The predicate function takes two arguments: the priority
%% of each suqueue being checked in turn, and each item in turn.
%% It must return `true' if the priority and item satisfies
%% the condition, otherwise `false'.
%%
%% If the predicate function returns `true' for any item,
%% this function returns `true', otherwise `false' (including
%% the case of all the subqueues being empty).
any(Prio, Prio, Fun, PQ=#pq{q=Queues}) when is_function(Fun, 2), is_map_key(Prio, Queues) ->
	do_any([Prio], Fun, PQ);
any(MaxPrio, MinPrio, Fun, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) when is_function(Fun, 2) ->
	do_any(Prios, Fun, PQ);
any(MaxPrio, MinPrio, Fun, PQ=#pq{p=Prios, q=Queues}) when is_function(Fun, 2), is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_any(Range, Fun, PQ);
any(MaxPrio, MinPrio, Fun, PQ) ->
	error(badarg, [MaxPrio, MinPrio, Fun, PQ]).

%% @doc Check if at least one item of a priority queue
%% satisfies a condition.
%%
%% The predicate function takes two arguments: the priority
%% of each suqueue being checked in turn, and each item in turn.
%% It must return `true' if the priority and item satisfies
%% the condition, otherwise `false'.
%%
%% If the predicate function returns `true' for any item,
%% this function returns `true', otherwise `false' (including
%% the case of the priority queue being empty).
-spec any(PredicateFun, PrioQueue) -> boolean() when
	PredicateFun :: fun((Priority, QueueItem) -> boolean()),
	Priority :: priority(),
	QueueItem :: item(),
	PrioQueue :: prioqueue().
any(Fun, PQ=#pq{p=Prios}) when is_function(Fun, 2) ->
	do_any(Prios, Fun, PQ);
any(Fun, PQ) ->
	error(badarg, [Fun, PQ]).

do_any([], _, _) ->
	false;
do_any([Prio|Range], Fun, PQ=#pq{q=Queues}) ->
	case
		%% TODO: Replace when queue:any/2 is introduced.
		lists:any(
			fun (Item) ->
				Fun(Prio, Item)
			end,
			queue:to_list(maps:get(Prio, Queues))
		)
	of
		true ->
			true;
		false ->
			do_any(Range, Fun, PQ)
	end.

%% @doc Check if all items of a subqueue of a
%% priority queue satisfy a condition.
%%
%% The predicate function takes two arguments: the priority
%% of the suqueue being checked, and each item in turn.
%% It must return `true' if the priority and item satisfies
%% the condition, otherwise `false'.
%%
%% If the predicate function returns `true' for all items,
%% this function returns `true' (including the case of the
%% subqueue being empty), otherwise `false'.
-spec all(Priority, PredicateFun, PrioQueue) -> boolean() when
	Priority :: priority(),
	PredicateFun :: fun((Priority, QueueItem) -> boolean()),
	QueueItem :: item(),
	PrioQueue :: prioqueue().
all(Prio, Fun, PQ=#pq{q=Queues}) when is_function(Fun, 2), is_map_key(Prio, Queues) ->
	do_all([Prio], Fun, PQ);
all(Prio, Fun, PQ) ->
	error(badarg, [Prio, Fun, PQ]).

%% @doc Check if all items of a range of subqueues of a
%% priority queue satisfy a condition.
%%
%% The predicate function takes two arguments: the priority
%% of each suqueue being checked in turn, and each item in turn.
%% It must return `true' if the priority and item satisfies
%% the condition, otherwise `false'.
%%
%% If the predicate function returns `true' for all items,
%% this function returns `true' (including the case of all the
%% subqueues being empty), otherwise `false'.
-spec all(MaxPriority, MinPriority, PredicateFun, PrioQueue) -> boolean() when
	MaxPriority :: priority(),
	MinPriority :: priority(),
	PredicateFun :: fun((Priority, QueueItem) -> boolean()),
	Priority :: priority(),
	QueueItem :: item(),
	PrioQueue :: prioqueue().
all(Prio, Prio, Fun, PQ=#pq{q=Queues}) when is_function(Fun, 2), is_map_key(Prio, Queues) ->
	do_all([Prio], Fun, PQ);
all(MaxPrio, MinPrio, Fun, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) when is_function(Fun, 2) ->
	do_all(Prios, Fun, PQ);
all(MaxPrio, MinPrio, Fun, PQ=#pq{p=Prios, q=Queues}) when is_function(Fun, 2), is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_all(Range, Fun, PQ);
all(MaxPrio, MinPrio, Fun, PQ) ->
	error(badarg, [MaxPrio, MinPrio, Fun, PQ]).

%% @doc Check if all items of a priority queue
%% satisfy a condition.
%%
%% The predicate function takes two arguments: the priority
%% of each suqueue being checked in turn, and each item in turn.
%% It must return `true' if the priority and item satisfies
%% the condition, otherwise `false'.
%%
%% If the predicate function returns `true' for all items,
%% this function returns `true' (including the case of the
%% priority queue being empty), otherwise `false'.
-spec all(PredicateFun, PrioQueue) -> boolean() when
	PredicateFun :: fun((Priority, QueueItem) -> boolean()),
	Priority :: priority(),
	QueueItem :: item(),
	PrioQueue :: prioqueue().
all(Fun, PQ=#pq{p=Prios}) when is_function(Fun, 2) ->
	do_all(Prios, Fun, PQ);
all(Fun, PQ) ->
	error(badarg, [Fun, PQ]).

do_all([], _, _) ->
	true;
do_all([Prio|Range], Fun, PQ=#pq{q=Queues}) ->
	case
		%% TODO: Replace when queue:all/2 is introduced.
		lists:all(
			fun (Item) ->
				Fun(Prio, Item)
			end,
			queue:to_list(maps:get(Prio, Queues))
		)
	of
		false ->
			false;
		true ->
			do_all(Range, Fun, PQ)
	end.

select_prio_range(Max, Min, Prios0) ->
	Prios1=lists:dropwhile(fun (Prio) -> Prio=/=Min end, lists:reverse(Prios0)),
	lists:dropwhile(fun (Prio) -> Prio=/=Max end, lists:reverse(Prios1)).

%% Copyright (c) 2020, Maria Scott <maria-12648430@hnc-agency.org>
%% Copyright (c) 2020, Jan Uhlig <juhlig@hnc-agency.org>
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

%% @author Maria Scott <maria-12648430@hnc-agency.org>
%% @author Jan Uhlig <juhlig@hnc-agency.org>
%% @copyright 2020 Maria Scott, Jan Uhlig
%% @version 0.1.2

%% @doc Erlang priority queues.
-module(hnc_pq).

-dialyzer(underspecs).
-dialyzer(unknown).
-dialyzer(unmatched_returns).

-export([all/2, all/3, all/4]).
-export([any/2, any/3, any/4]).
-export([append/3]).
-export([filter/2, filter/3, filter/4]).
-export([fold/3, fold/4, fold/5]).
-export([from_list/1]).
-export([in/2, in/3]).
-export([in_r/2, in_r/3]).
-export([is_empty/1, is_empty/2, is_empty/3]).
-export([is_priority/2]).
-export([join/2]).
-export([length/1, length/2, length/3]).
-export([member/2, member/3, member/4]).
-export([new/1]).
-export([out/1, out/2, out/3]).
-export([out_r/1, out_r/2, out_r/3]).
-export([prepend/3]).
-export([priorities/1]).
-export([queue/2]).
-export([reprioritize/2]).
-export([reverse/1, reverse/2, reverse/3]).
-export([reverse_priorities/1, reverse_priorities/3]).
-export([to_flatlist/1, to_flatlist/2, to_flatlist/3]).
-export([to_flatqueue/1, to_flatqueue/2, to_flatqueue/3]).
-export([to_list/1, to_list/2, to_list/3]).

-export_type([prioqueue/0, prioqueue/2]).

-record(pq, {
	p=[]        :: list(Prio),
	l=undefined :: Prio,
	q=#{}       :: #{Prio => queue:queue()}
}).

-opaque prioqueue(Priority, Item) :: #pq{p :: list(Priority), l :: Priority, q :: #{Priority => queue:queue(Item)}}.

-type prioqueue() :: prioqueue(_, _).

%% ============================================================================
%% API
%% ============================================================================

%% ----------------------------------------------------------------------------
%% new/1
%% ----------------------------------------------------------------------------

%% @doc Create a new priority queue.
%% 
%% @param Priorities A list of unique priorities the priority queue
%%	should have, in descending order. A priority can be any term. 
%%
%% @returns A new, empty priority queue with the given priorities.
-spec new(Priorities) -> PrioQueue when
	Priorities :: nonempty_list(Priority),
	PrioQueue :: prioqueue(Priority, _).
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


%% ----------------------------------------------------------------------------
%% to_flatqueue/1,2,3
%% ----------------------------------------------------------------------------

%% @doc Convert a priority queue to a flat queue of items.
%%
%% @param PrioQueue The priority queue to convert.
%%
%% @returns A queue containing all the items of the priority
%%	queue, in priority queue order.
%%
%% @see to_flatqueue/2
%% @see to_flatqueue/3
-spec to_flatqueue(PrioQueue) -> FlatQueue when
	PrioQueue :: prioqueue(_, Item),
	FlatQueue :: queue:queue(Item).
to_flatqueue(PQ=#pq{p=Prios}) ->
	do_to_flatqueue(Prios, PQ);
to_flatqueue(PQ) ->
	error(badarg, [PQ]).

%% @doc Convert a subqueue of a priority queue to a flat queue of items.
%%
%% @param Priority The priority subqueue to convert.
%% @param PrioQueue The priority queue whose subqueue to convert.
%%
%% @returns A queue containing the items of the given subqueue.
%%
%% @see to_flatqueue/1
%% @see to_flatqueue/3
-spec to_flatqueue(Priority, PrioQueue) -> FlatQueue when
	PrioQueue :: prioqueue(Priority, Item),
	FlatQueue :: queue:queue(Item).
to_flatqueue(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_to_flatqueue([Prio], PQ);
to_flatqueue(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Convert a range of subqueues of a priority queue to a flat queue of items.
%%
%% @param MaxPriority The maximum priority subqueue to convert.
%% @param MinPriority The minimum priority subqueue to convert.
%% @param PrioQueue The priority queue whose subqueues to convert.
%%
%% @returns A queue containing the items of the priority subqueues between
%%	`MaxPriority' and `MinPriority', inclusive, in priority queue order.
%%
%% @see to_flatqueue/1
%% @see to_flatqueue/2
-spec to_flatqueue(MaxPriority, MinPriority, PrioQueue) -> FlatQueue when
	PrioQueue :: prioqueue(Priority, Item),
	MaxPriority :: Priority,
	MinPriority :: Priority,
	FlatQueue :: queue:queue(Item).
to_flatqueue(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_to_flatqueue([Prio], PQ);
to_flatqueue(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_to_flatqueue(Prios, PQ);
to_flatqueue(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_to_flatqueue(Range, PQ);
to_flatqueue(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% helper

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


%% ----------------------------------------------------------------------------
%% to_list/1,2,3
%% ----------------------------------------------------------------------------

%% @doc Convert a priority queue to a list of priorities and items.
%%
%% @param PrioQueue The priority queue to convert.
%%
%% @returns A list of tuples of the form `{Priority, Items}', in priority
%%	queue order, where `Priority' is the priority of a subqueue and
%%	`Items' is a list of the items in that subqueue.
%%
%% @see to_list/2
%% @see to_list/3
-spec to_list(PrioQueue) -> PrioList when
	PrioQueue :: prioqueue(Priority, Item),
	PrioList :: list({Priority, list(Item)}).
to_list(PQ=#pq{p=Prios}) ->
	do_to_list(Prios, PQ);
to_list(PQ) ->
	error(badarg, [PQ]).

%% @doc Convert a specific subqueue of the priority queue to a list of priorities and items.
%%
%% @param Priority The priority subqueue to convert.
%% @param PrioQueue The priority queue whose subqueue to convert.
%%
%% @returns A singleton list, with the only element being a tuple of the
%%	form `{Priority, Items}', where `Priority' is the priority of a
%%	subqueue and `Items' is a list of the items in that subqueue.
%%
%% @see to_list/1
%% @see to_list/3
-spec to_list(Priority, PrioQueue) -> PrioList when
	PrioQueue :: prioqueue(Priority, Item),
	PrioList :: list({Priority, list(Item)}).
to_list(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_to_list([Prio], PQ);
to_list(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Convert a range of subqueues of the priority queue to a list of items.
%%
%% @param MaxPriority The maximum priority subqueue to convert.
%% @param MinPriority The minimum priority subqueue to convert.
%% @param PrioQueue The priority queue whose subqueues to convert.
%%
%% @returns A list of tuples of the form `{Priority, Items}', in priority
%%	queue order, where `Priority' is the priority of a subqueue and
%%	`Items' is a list of the items in that subqueue.
%%
%% @see to_list/1
%% @see to_list/2
-spec to_list(MaxPriority, MinPriority, PrioQueue) -> PrioList when
	PrioQueue :: prioqueue(Priority, Item),
	MaxPriority :: Priority,
	MinPriority :: Priority,
	PrioList :: list({Priority, list(Item)}).
to_list(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_to_list([Prio], PQ);
to_list(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_to_list(Prios, PQ);
to_list(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_to_list(Range, PQ);
to_list(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% helper

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


%% ----------------------------------------------------------------------------
%% to_flatlist/1,2,3
%% ----------------------------------------------------------------------------

%% @doc Convert a priority queue to a flat list of items.
%%
%% @param PrioQueue The priority queue to convert.
%%
%% @returns A list containing all the items of the priority
%%	queue, in priority queue order.
%%
%% @see to_flatlist/2
%% @see to_flatlist/3
-spec to_flatlist(PrioQueue) -> ItemList when
	PrioQueue :: prioqueue(_, Item),
	ItemList :: list(Item).
to_flatlist(PQ=#pq{p=Prios}) ->
	do_to_flatlist(Prios, PQ);
to_flatlist(PQ) ->
	error(badarg, [PQ]).

%% @doc Convert a subqueue of a priority queue to a flat list of items.
%%
%% @param Priority The priority subqueue to convert.
%% @param PrioQueue The priority queue whose subqueue to convert.
%%
%% @returns A list of items in the given subqueue.
%%
%% @see to_flatlist/1
%% @see to_flatlist/3
-spec to_flatlist(Priority, PrioQueue) -> ItemList when
	PrioQueue :: prioqueue(Priority, Item),
	ItemList :: list(Item).
to_flatlist(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_to_flatlist([Prio], PQ);
to_flatlist(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Convert a range of subqueues of a priority queue to a flat list of items.
%%
%% @param MaxPriority The maximum priority subqueue to convert.
%% @param MaxPriority The minimum priority subqueue to convert.
%% @param PrioQueue The priority queue whose subqueues to convert.
%%
%% @returns A list containing the items of the priority subqueues between
%%	`MaxPriority' and `MinPriority', inclusive, in priority queue order.
%%
%% @see to_flatlist/1
%% @see to_flatlist/2
-spec to_flatlist(MaxPriority, MinPriority, PrioQueue) -> ItemList when
	PrioQueue :: prioqueue(Priority, Item),
	MaxPriority :: Priority,
	MinPriority :: Priority,
	ItemList :: list(Item).
to_flatlist(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_to_flatlist([Prio], PQ);
to_flatlist(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_to_flatlist(Prios, PQ);
to_flatlist(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_to_flatlist(Range, PQ);
to_flatlist(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% helper

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


%% ----------------------------------------------------------------------------
%% from_list/1
%% ----------------------------------------------------------------------------

%% @doc Create a priority queue from a list.
%%
%% @param PrioList A list of tuples of the form `{Priority, Items}',
%%	in priority queue order, where `Priority' is a priority
%%	label and `Items' is a list of items in this priority
%%	subqueue. The priorities must be unique among the list.
%%
%% @returns A new priority queue with subqueues according to
%%	the given priorities, containing the respective items.
-spec from_list(PrioList) -> PrioQueue when
	PrioList :: list({Priority, list(Item)}),
	PrioQueue :: prioqueue(Priority, Item).
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


%% ----------------------------------------------------------------------------
%% reprioritize/2
%% ----------------------------------------------------------------------------

%% @doc Change the priorities of a priority queue.
%%
%% This function can be used to add, remove, or rearrange priorities
%% of a priority queue.
%%
%% @param Priorities A list of unique new priorities to impose on the
%%	priority queue.
%% @param PrioQueue0 The priority queue whose priorities to change.
%%
%% @returns A new priority queue with the priorities as specified by
%%	the `Priorities' parameter, in that order. Priorities that
%%	exist in both the given priority queue and the new priorities
%%	list are present as before in the resulting priority queue.
%%	Priorities that exist in the new priorities but not in the
%%	given priority queue will be created empty in the resulting
%%	priority queue. Priorities that exist in the given priority
%%	queue but not in the new priorities will not be present in the
%%	resulting priority queue.
-spec reprioritize(Priorities, PrioQueue0) -> PrioQueue1 when
	Priorities :: nonempty_list(NewPriority),
	PrioQueue0 :: prioqueue(OldPriority, Item),
	PrioQueue1 :: prioqueue(OldPriority | NewPriority, Item).
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


%% ----------------------------------------------------------------------------
%% reverse/1,2,3
%% ----------------------------------------------------------------------------

%% @doc Reverse the priority queue order of a priority queue.
%%
%% @param PrioQueue0 The priority queue to reverse.
%%
%% @returns A new priority queue where the priorities of the subqueues
%%	has been reversed, and the order of items in each subqueue
%%	has been reversed, also.
%%
%% @see reverse/2
%% @see reverse/3
-spec reverse(PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(Priority, Item),
	PrioQueue1 :: prioqueue(Priority, Item).
reverse(PQ=#pq{p=Prios}) ->
	do_reverse(Prios, PQ);
reverse(PQ) ->
	error(badarg, [PQ]).

%% @doc Reverse the order of items in a subqueue of a priority queue.
%%
%% @param Priority The priority of the subqueue to reverse.
%% @param PrioQueue0 The priority whose subqueue to reverse,
%%
%% @returns A new priority queue with the items in the given
%%	subqueue reversed.
%%
%% @see reverse/1
%% @see reverse/3
-spec reverse(Priority, PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(Priority, Item),
	PrioQueue1 :: prioqueue(Priority, Item).
reverse(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_reverse([Prio], PQ);
reverse(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Reverse the priority queue order of a range of
%%	 subqueues of a priority queue.
%%
%% @param MaxPriority The maximum priority subqueue to reverse.
%% @param MaxPriority The minimum priority subqueue to reverse.
%% @param PrioQueue0 The priority queue whose subqueues to reverse.
%%
%% @returns A new priority queue where the priority queue order of
%%	the subqueues between `MaxPriority' and `MinPriority', inclusive,
%%	has been reversed. Specifically, this means that the priorities
%%	of the priority subqueues has been reversed, and the order of the
%%	items in each of those subqueues has been reversed, also.
%%
%% @see reverse/1
%% @see reverse/2
-spec reverse(MaxPriority, MinPriority, PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(Priority, Item),
	MaxPriority :: Priority,
	MinPriority :: Priority,
	PrioQueue1 :: prioqueue(Priority, Item).
reverse(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_reverse([Prio], PQ);
reverse(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_reverse(Prios, PQ);
reverse(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_reverse(Range, PQ);
reverse(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% reverse helper

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


%% ----------------------------------------------------------------------------
%% reverse_priorities/2,3
%% ----------------------------------------------------------------------------

%% @doc Reverse the priorities of a priority queue.
%%
%% @param PrioQueue0 The priority queue whose priorities to reverse.
%%
%% @returns A new priority queue where the priorities of all subqueues
%%	has been reversed. The order of items in the respective subqueues
%%	remains unchanged.
%%
%% @see reverse_priorities/3
-spec reverse_priorities(PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(Priority, Item),
	PrioQueue1 :: prioqueue(Priority, Item).
reverse_priorities(PQ=#pq{p=Prios}) ->
	PQ#pq{p=lists:reverse(Prios), l=hd(Prios)};
reverse_priorities(PQ) ->
	error(badarg, [PQ]).

%% @doc Reverse the priorities of a range of subqueues of a priority queue.
%%
%% @param MaxPriority The maximum priority subqueue whose priority to change.
%% @param MinPriority The minimum priority subqueue whose priority to change.
%% @param PrioQueue0 The priority queue whose subqueue priorities to reverse.
%%
%% @returns A new priority queue where the priorities of the subqueues between
%%	`MaxPriority' and `MinPriority', inclusive, has been reversed. The order
%%	of items in the respective subqueues remains unchanged.
%%
%% @see reverse_priorities/1
-spec reverse_priorities(MaxPriority, MinPriority, PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(Priority, Item),
	MaxPriority :: Priority,
	MinPriority :: Priority,
	PrioQueue1 :: prioqueue(Priority, Item).
reverse_priorities(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	PQ;
reverse_priorities(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	PQ#pq{p=lists:reverse(Prios), l=hd(Prios)};
reverse_priorities(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_reverse_priorities(Range, PQ);
reverse_priorities(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% helper

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


%% ----------------------------------------------------------------------------
%% is_priority/2
%% ----------------------------------------------------------------------------

%% @doc Check if the given priority exists in a priority queue.
%%
%% @param Priority The priority to check for existence.
%% @param PrioQueue The priority queue in which to check.
%%
%% @returns `true' if the given priority exists in the
%%	given priority queue, otherwise `false'.
-spec is_priority(Priority, PrioQueue) -> boolean() when
	Priority :: term(),
	PrioQueue :: prioqueue().
is_priority(Prio, #pq{q=Queues}) ->
	is_map_key(Prio, Queues);
is_priority(Prio, PQ) ->
	error(badarg, [Prio, PQ]).


%% ----------------------------------------------------------------------------
%% priorities/1
%% ----------------------------------------------------------------------------

%% @doc Retrieve the priorities of a priority queue.
%%
%% @param PrioQueue The priority queue whose priorities to retrieve.
%%
%% @returns A list of the priorities of the given priority queue,
%%	in descending order.
-spec priorities(PrioQueue) -> Priorities when
	PrioQueue :: prioqueue(Priority, _),
	Priorities :: list(Priority).
priorities(#pq{p=Prios}) ->
	Prios;
priorities(PQ) ->
	error(badarg, [PQ]).


%% ----------------------------------------------------------------------------
%% queue/2
%% ----------------------------------------------------------------------------

%% @doc Retrieve a subqueue of a priority queue.
%%
%% @param Priority The priority subqueue to retrieve.
%% @param PrioQueue The priority queue whose subqueue to retrieve.
%%
%% @returns The subqueue with the given priority in the given
%%	priority queue.
-spec queue(Priority, PrioQueue) -> Queue when
	PrioQueue :: prioqueue(Priority, Item),
	Queue :: queue:queue(Item).
queue(Prio, #pq{q=Queues}) when is_map_key(Prio, Queues) ->
	maps:get(Prio, Queues);
queue(Prio, PQ) ->
	error(badarg, [Prio, PQ]).


%% ----------------------------------------------------------------------------
%% member/2,3,4
%% ----------------------------------------------------------------------------

%% @doc Check if an item is a member of a priority queue.
%%
%% @param Item The item whose existence to check.
%% @param PrioQueue The priority queue whose subqueue to check.
%%
%% @returns `true' if the given `Item' is a member of the given
%%	priority queue, otherwise `false'.
%%
%% @see member/3
%% @see member/4
-spec member(Item, PrioQueue) -> boolean() when
	PrioQueue :: prioqueue(_, Item).
member(Item, PQ=#pq{p=Prios}) ->
	do_member(Prios, Item, PQ);
member(Item, PQ) ->
	error(badarg, [Item, PQ]).

%% @doc Check if an item is a member of a subqueue of a priority queue.
%%
%% @param Priority The priority of the subqueue to check.
%% @param Item The item whose existence to check.
%% @param PrioQueue The priority queue whose subqueue to check.
%%
%% @returns `true' if the given `Item' is a member of the given subqueue
%%	of the given priority queue, otherwise `false'.
%%
%% @see member/2
%% @see member/4
-spec member(Priority, Item, PrioQueue) -> boolean() when
	PrioQueue :: prioqueue(Priority, Item).
member(Prio, Item, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_member([Prio], Item, PQ);
member(Prio, Item, PQ) ->
	error(badarg, [Prio, Item, PQ]).

%% @doc Check if an item is a member of a range of subqueues of a priority queue.
%%
%% @param MaxPriority The maximum priority subqueue to check.
%% @param MinPriority The minimum priority subqueue to check.
%% @param Item The item whose existence to check.
%% @param PrioQueue The priority queue whose subqueue to check.
%%
%% @returns `true' if the given `Item' is a member of the priority subqueues between
%%	`MaxPriority' and `MinPriority', inclusive, of the given priority queue,
%%	otherwise `false'.
%%
%% @see member/2
%% @see member/3
-spec member(MaxPriority, MinPriority, Item, PrioQueue) -> boolean() when
	PrioQueue :: prioqueue(Priority, Item),
	MaxPriority :: Priority,
	MinPriority :: Priority.
member(Prio, Prio, Item, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_member([Prio], Item, PQ);
member(MaxPrio, MinPrio, Item, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_member(Prios, Item, PQ);
member(MaxPrio, MinPrio, Item, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_member(Range, Item, PQ);
member(MaxPrio, MinPrio, Item, PQ) ->
	error(badarg, [MaxPrio, MinPrio, Item, PQ]).

%% member helper

do_member([], _, _) ->
	false;
do_member(Range, Item, #pq{q=Queues}) ->
	lists:any(
		fun (Prio) ->
			queue:member(Item, maps:get(Prio, Queues))
		end,
		Range
	).


%% ----------------------------------------------------------------------------
%% in/2,3
%% ----------------------------------------------------------------------------

%% @doc Insert the given item into a priority queue, at the rear.
%%
%% @param Item The item to insert.
%% @param PrioQueue0 The priority queue into which to insert.
%%
%% @returns A new priority queue where the given item has been inserted
%%	at the rear of the lowest-priority subqueue of the given priority
%%	queue.
%%
%% @see in/3
%% @see in_r/2
-spec in(Item, PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(Priority, Item0),
	PrioQueue1 :: prioqueue(Priority, Item0 | Item).
in(Item, PQ=#pq{l=Prio}) ->
	do_in(Prio, Item, PQ);
in(Item, PQ) ->
	error(badarg, [Item, PQ]).
 
%% @doc Insert an item into a subqueue of a priority queue, at the rear.
%%
%% @param Priority The priority subqueue into which to insert.
%% @param Item The item to insert.
%% @param PrioQueue0 The priority queue into whose subqueue to insert.
%%
%% @returns A new priority queue where the given item has been inserted
%%	at the rear of the given subqueue of the given priority queue.
%%
%% @see in/2
%% @see in_r/3
-spec in(Priority, Item, PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(Priority, Item0),
	PrioQueue1 :: prioqueue(Priority, Item0 | Item).
in(Prio, Item, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_in(Prio, Item, PQ);
in(Prio, Item, PQ) ->
	error(badarg, [Prio, Item, PQ]).

%% helper
 
do_in(Prio, Item, PQ=#pq{q=Queues0}) ->
	Queues1=maps:update_with(Prio, fun (Queue) -> queue:in(Item, Queue) end, Queues0),
	PQ#pq{q=Queues1}.


%% ----------------------------------------------------------------------------
%% in_r/2,3
%% ----------------------------------------------------------------------------

%% @doc Insert the given item into a priority queue, at the front.
%%
%% @param Item The item to insert.
%% @param PrioQueue0 The priority queue into which to insert.
%%
%% @returns A new priority queue where the given item has been inserted
%%	at the front of the highest-priority subqueue of the given priority
%%	queue.
%%
%% @see in_r/3
%% @see in/2
-spec in_r(Item, PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(Priority, Item0),
	PrioQueue1 :: prioqueue(Priority, Item0 | Item).
in_r(Item, PQ=#pq{p=[Prio|_]}) ->
	do_in_r(Prio, Item, PQ);
in_r(Item, PQ) ->
	error(badarg, [Item, PQ]).

%% @doc Insert the given item into a subqueue of a priority queue, at the front.
%%
%% @param Priority The priority subqueue into which to insert.
%% @param Item The item to insert.
%% @param PrioQueue0 The priority queue into whose subqueue to insert.
%%
%% @returns A new priority queue where the given item has been inserted
%%	at the front of the given subqueue of the given priority queue.
%%
%% @see in_r/2
%% @see in/3
-spec in_r(Priority, Item, PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(Priority, Item0),
	PrioQueue1 :: prioqueue(Priority, Item0 | Item).
in_r(Prio, Item, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_in_r(Prio, Item, PQ);
in_r(Prio, Item, PQ) ->
	error(badarg, [Prio, Item, PQ]).

%% helper

do_in_r(Prio, Item, PQ=#pq{q=Queues0}) ->
	Queues1=maps:update_with(Prio, fun (Queue) -> queue:in_r(Item, Queue) end, Queues0),
	PQ#pq{q=Queues1}.


%% ----------------------------------------------------------------------------
%% out/1,2,3
%% ----------------------------------------------------------------------------

%% @doc Retrieve and remove the next item from a priority queue, from the front.
%%
%% @param PrioQueue0 The priority queue from which to retrieve an item.
%%
%% @returns Either the atom `empty' if all subqueues of the given priority queue are empty,
%%	or a tuple of the form `{Item, NewPrioQueue}', where `Item' is the item that was
%%	retrieved from the front of the highest-priority non-empty subqueue and `NewPrioQueue'
%%	is a new priority queue with that item accordingly removed.
%%
%% @see out/2
%% @see out/3
%% @see out_r/1
-spec out(PrioQueue0) -> empty | {Item, PrioQueue1} when
	PrioQueue0 :: prioqueue(Priority, Item),
	PrioQueue1 :: prioqueue(Priority, Item).
out(PQ=#pq{p=Prios}) ->
	do_out(Prios, PQ);
out(PQ) ->
	error(badarg, [PQ]).

%% @doc Retrieve and remove the next item from a subqueue of a priority queue, from the front.
%%
%% @param Priority The priority subqueue from which to retrieve an item.
%% @param PrioQueue0 The priority queue from whose subqueue to retrieve.
%%
%% @returns Either the atom `empty' if the given subqueue is empty, or a tuple of the form
%%	`{Item, NewPrioQueue}', where `Item' is the item that was retrieved from the front of
%%	the given subqueue and `NewPrioQueue' is a new priority queue with that item accordingly
%%	removed.
%%
%% @see out/1
%% @see out/3
%% @see out_r/2
-spec out(Priority, PrioQueue0) -> empty | {Item, PrioQueue1} when
	PrioQueue0 :: prioqueue(Priority, Item),
	PrioQueue1 :: prioqueue(Priority, Item).
out(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_out([Prio], PQ);
out(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Retrieve and remove the next item from a range of subqueues of a priority queue,
%% from the front.
%%
%% @param MaxPriority The maximum priority subqueue from which to retrieve an item.
%% @param MinPriority The minimum priority subqueue from which to retrieve an item.
%% @param PrioQueue0 The priority queue from whose subqueues to retrieve an item.
%%
%% @returns Either the atom `empty' if all subqueues between `MaxPriority'  and
%%	`MinPriority', inclusive, are empty, or a tuple of the form `{Item, NewPrioQueue}',
%%	where `Item' is the item that was retrieved from the front of the highest-priority
%%	non-empty subqueue and `NewPrioQueue' is a new priority queue with that item
%%	accordingly removed.
%%
%% @see out/1
%% @see out/2
%% @see out_r/3
-spec out(MaxPriority, MinPriority, PrioQueue0) -> empty | {Item, PrioQueue1} when
	PrioQueue0 :: prioqueue(Priority, Item),
	MaxPriority :: Priority,
	MinPriority :: Priority,
	PrioQueue1 :: prioqueue(Priority, Item).
out(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_out([Prio], PQ);
out(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_out(Prios, PQ);
out(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_out(Range, PQ);
out(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% helper

do_out([], _) ->
	empty;
do_out([Prio|Range], PQ=#pq{q=Queues}) ->
	case queue:out(maps:get(Prio, Queues)) of
		{empty, _} ->
			do_out(Range, PQ);
		{{value, Item}, Queue1} ->
			{Item, PQ#pq{q=maps:update(Prio, Queue1, Queues)}}
	end.


%% ----------------------------------------------------------------------------
%% out_r/1,2,3
%% ----------------------------------------------------------------------------

%% @doc Retrieve and remove the next item from a priority queue, from the rear.
%%
%% @param PrioQueue0 The priority queue from which to retrieve an item.
%%
%% @returns Either the atom `empty' if all subqueues of the given priority queue are empty,
%%	or a tuple of the form `{Item, NewPrioQueue}', where `Item' is the item that was
%%	retrieved from the rear of the lowest-priority non-empty subqueue and `NewPrioQueue'
%%	is a new priority queue with that item accordingly removed.
%%
%% @see out_r/2
%% @see out_r/3
%% @see out_1
-spec out_r(PrioQueue0) -> empty | {Item, PrioQueue1} when
	PrioQueue0 :: prioqueue(Priority, Item),
	PrioQueue1 :: prioqueue(Priority, Item).
out_r(PQ=#pq{p=Prios}) ->
	do_out_r(lists:reverse(Prios), PQ);
out_r(PQ) ->
	error(badarg, [PQ]).

%% @doc Retrieve and remove the next item from a subqueue of a priority queue, from the rear.
%%
%% @param Priority The priority subqueue from which to retrieve an item.
%% @param PrioQueue0 The priority queue from whose subqueue to retrieve.
%%
%% @returns Either the atom `empty' if the given subqueue is empty, or a tuple of the form
%%	`{Item, NewPrioQueue}', where `Item' is the item that was retrieved from the rear of
%%	the given subqueue and `NewPrioQueue' is a new priority queue with that item accordingly
%%	removed.
%%
%% @see out_r/1
%% @see out_r/3
%% @see out/2
-spec out_r(Priority, PrioQueue0) -> empty | {Item, PrioQueue1} when
	PrioQueue0 :: prioqueue(Priority, Item),
	PrioQueue1 :: prioqueue(Priority, Item).
out_r(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_out_r([Prio], PQ);
out_r(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Retrieve and remove the next item from a range of subqueues of a priority queue,
%% from the rear.
%%
%% @param MaxPriority The maximum priority subqueue from which to retrieve an item.
%% @param MinPriority The minimum priority subqueue from which to retrieve an item.
%% @param PrioQueue0 The priority queue from whose subqueues to retrieve an item.
%%
%% @returns Either the atom `empty' if all subqueues between `MaxPriority'  and
%%	`MinPriority', inclusive, are empty, or a tuple of the form `{Item, NewPrioQueue}',
%%	where `Item' is the item that was retrieved from the rear of the lowest-priority
%%	non-empty subqueue and `NewPrioQueue' is a new priority queue with that item
%%	accordingly removed.
%%
%% @see out_r/1
%% @see out_r/2
%% @see out/3
-spec out_r(MaxPriority, MinPriority, PrioQueue0) -> empty | {Item, PrioQueue1} when
	PrioQueue0 :: prioqueue(Priority, Item),
	MaxPriority :: Priority,
	MinPriority :: Priority,
	PrioQueue1 :: prioqueue(Priority, Item).
out_r(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_out_r([Prio], PQ);
out_r(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_out_r(lists:reverse(Prios), PQ);
out_r(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_out_r(lists:reverse(Range), PQ);
out_r(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% helper

do_out_r([], _) ->
	empty;
do_out_r([Prio|Range], PQ=#pq{q=Queues}) ->
	case queue:out_r(maps:get(Prio, Queues)) of
		{empty, _} ->
			do_out_r(Range, PQ);
		{{value, Item}, Queue1} ->
			{Item, PQ#pq{q=maps:update(Prio, Queue1, Queues)}}
	end.


%% ----------------------------------------------------------------------------
%% is_empty/1,2,3
%% ----------------------------------------------------------------------------

%% @doc Check if a priority queue is empty.
%%
%% @param PrioQueue The priority queue to check.
%%
%% @returns `true' if the entire priority queue is empty,
%%	otherwise `false'.
%%
%% @see is_empty/2
%% @see is_empty/3
-spec is_empty(PrioQueue) -> boolean() when
	PrioQueue :: prioqueue().
is_empty(PQ=#pq{p=Prios}) ->
	do_is_empty(Prios, PQ);
is_empty(PQ) ->
	error(badarg, [PQ]).

%% @doc Check if a subqueue of a priority queue is empty.
%%
%% @param Priority The priority subqueue to check.
%% @param PrioQueue The priority queue whose subqueue to check.
%%
%% @returns `true' if the given subqueue is empty, otherwise
%%	`false'.
%%
%% @see is_empty/1
%% @see is_empty/3
-spec is_empty(Priority, PrioQueue) -> boolean() when
	PrioQueue :: prioqueue(Priority, _).
is_empty(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_is_empty([Prio], PQ);
is_empty(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Check if a range of subqueues of a priority queue is empty.
%%
%% @param MaxPriority The maximum priority subqueue to check.
%% @param MinPriority The minimum priority subqueue to check.
%% @param PrioQueue The priority queue whose subqueues to check.
%%
%% @returns `true' if all subqueues between `MaxPriority' and `MinPriority',
%%	inclusive, are empty, otherwise `false'.
%%
%% @see is_empty/1
%% @see is_empty/2
-spec is_empty(MaxPriority, MinPriority, PrioQueue) -> boolean() when
	PrioQueue :: prioqueue(Priority, _),
	MaxPriority :: Priority,
	MinPriority :: Priority.
is_empty(Prio, Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_is_empty([Prio], PQ);
is_empty(MaxPrio, MinPrio, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) ->
	do_is_empty(Prios, PQ);
is_empty(MaxPrio, MinPrio, PQ=#pq{p=Prios, q=Queues}) when is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_is_empty(Range, PQ);
is_empty(MaxPrio, MinPrio, PQ) ->
	error(badarg, [MaxPrio, MinPrio, PQ]).

%% helper

do_is_empty([], _) ->
	true;
do_is_empty([Prio|Range], PQ=#pq{q=Queues}) ->
	case queue:is_empty(maps:get(Prio, Queues)) of
		true ->
			do_is_empty(Range, PQ);
		false ->
			false
	end.


%% ----------------------------------------------------------------------------
%% length/1,2,3
%% ----------------------------------------------------------------------------

%% @doc Retrieve the number of items in a priority queue.
%%
%% @param PrioQueue The priority queue whose length to retrieve.
%%
%% @returns The number of items in the entire priority queue.
%%
%% @see length/2
%% @see length/3
-spec length(PrioQueue) -> Length when
	PrioQueue :: prioqueue(),
	Length :: non_neg_integer().
length(PQ=#pq{p=Prios}) ->
	do_length(Prios, PQ);
length(PQ) ->
	error(badarg, [PQ]).

%% @doc Retrieve the number of items in a subqueue of a priority queue.
%%
%% @param Priority The priority subqueue whose length to retrieve.
%% @param PrioQueue The priority queue whose subqueue's length to retrieve.
%%
%% @returns The number of items in the given priority subqueue of the given
%%	priority queue.
%%
%% @see length/1
%% @see length/3
-spec length(Priority, PrioQueue) -> Length when
	PrioQueue :: prioqueue(Priority, _),
	Length :: non_neg_integer().
length(Prio, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_length([Prio], PQ);
length(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Retrieve the number of items in a range of subqueues of a priority queue.
%%
%% @param MaxPriority The maximum priority subqueue whose length to retrieve.
%% @param MinPriority The minimum priority subqueue whose length to retrieve.
%% @param PrioQueue The priority queue whose subqueues length to retrieve.
%%
%% @returns The number of items in the subqueues between `MaxPriority' and `MinPriority',
%%	inclusive, of the given priority queue.
%%
%% @see length/1
%% @see length/2
-spec length(MaxPriority, MinPriority, PrioQueue) -> Length when
	PrioQueue :: prioqueue(Priority, _),
	MaxPriority :: Priority,
	MinPriority :: Priority,
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

%% helper

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


%% ----------------------------------------------------------------------------
%% append/3
%% ----------------------------------------------------------------------------

%% @doc Append to an existing subqueue or create a new subqueue in a priority queue.
%%
%% @param Priority The priority of the subqueue to append to or create.
%% @param AppendQueue A queue to append or create in the given priority queue.
%%	If a priority subqueue with the given `Priority' exists in the priority queue,
%%	the queue is appended to the existing queue, otherwise the queue is created as
%%	the new lowest-priority subqueue.
%% @param PrioQueue0 The priority queue to whose subqueue to append to or in
%%	which to create a new subqueue.
%%
%% @returns A new priority queue with the given `AppendQueue' appended to
%%	an existing priority subqueue, or created as a new lowest-priority subqueue.
%%
%% @see prepend/3
%% @see join/2
-spec append(Priority, PrioQueue0, AppendQueue) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(Priority0, Item0),
	AppendQueue :: queue:queue(Item1),
	PrioQueue1 :: prioqueue(Priority0 | Priority, Item0 | Item1).
append(Prio, PQ=#pq{q=Queues0}, AppendQueue) when is_map_key(Prio, Queues0) ->
	Queues1=case queue:is_queue(AppendQueue) of
		true ->
			maps:update_with(Prio, fun (Queue) -> queue:join(Queue, AppendQueue) end, Queues0);
		false ->
			error(badarg, [Prio, PQ, AppendQueue])
	end,
	PQ#pq{q=Queues1};
append(Prio, PQ=#pq{p=Prios0, q=Queues0}, AppendQueue) ->
	Queues1=case queue:is_queue(AppendQueue) of
		true ->
			maps:put(Prio, AppendQueue, Queues0);
		false ->
			error(badarg, [Prio, PQ, AppendQueue])
	end,
	#pq{p=Prios0++[Prio], l=Prio, q=Queues1};
append(Prio, PQ, AppendQueue) ->
	error(badarg, [Prio, PQ, AppendQueue]).


%% ----------------------------------------------------------------------------
%% prepend/3
%% ----------------------------------------------------------------------------

%% @doc Prepend to an existing subqueue or create a new subqueue in a priority queue.
%%
%% @param Priority The priority of the subqueue to prepend to or create.
%% @param PrependQueue A queue to prepend or create in the given priority queue.
%%	If a priority subqueue with the given `Priority' exists in the priority queue,
%%	the queue is prepended to the existing queue, otherwise the queue is created as
%%	the new highest-priority subqueue.
%% @param PrioQueue0 The priority queue to whose subqueue to prepend to or in
%%	which to create a new subqueue.
%%
%% @returns A new priority queue with the given `PrependQueue' prepended to
%%	an existing priority subqueue, or created as a new highest-priority subqueue.
%%
%% @see append/3
%% @see join/2
-spec prepend(Priority, PrioQueue0, PrependQueue) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(Priority0, Item0),
	PrependQueue :: queue:queue(Item1),
	PrioQueue1 :: prioqueue(Priority0 | Priority, Item0 | Item1).
prepend(Prio, PQ=#pq{q=Queues0}, PrependQueue) when is_map_key(Prio, Queues0) ->
	Queues1=case queue:is_queue(PrependQueue) of
		true ->
			maps:update_with(Prio, fun (Queue) -> queue:join(PrependQueue, Queue) end, Queues0);
		false ->
			error(badarg, [Prio, PQ, PrependQueue])
	end,
	PQ#pq{q=Queues1};
prepend(Prio, PQ=#pq{p=Prios0, q=Queues0}, PrependQueue) ->
	Queues1=case queue:is_queue(PrependQueue) of
		true ->
			maps:put(Prio, PrependQueue, Queues0);
		false ->
			error(badarg, [Prio, PQ, PrependQueue])
	end,
	PQ#pq{p=[Prio|Prios0], q=Queues1};
prepend(Prio, PQ, PrependQueue) ->
	error(badarg, [Prio, PQ, PrependQueue]).


%% ----------------------------------------------------------------------------
%% join/2
%% ----------------------------------------------------------------------------

%% @doc Join two priority queues.
%%
%% @param PrioQueue0 The priority queue to join with `PrioQueue1'.
%% @param PrioQueue1 The priority queue to join with `PrioQueue0'.
%%
%% @returns A new priority queue where all subqueues in `PrioQueue1'
%%	which exist in `PrioQueue0' have been appended, and all
%%	non-existing subqueues have been inserted as new lowest-priority
%%	subqueues, in the order in which they appear in `PrioQueue1'.
-spec join(PrioQueue0, PrioQueue1) -> PrioQueue2 when
	PrioQueue0 :: prioqueue(Priority0, Item0),
	PrioQueue1 :: prioqueue(Priority1, Item1),
	PrioQueue2 :: prioqueue(Priority0 | Priority1, Item0 | Item1).
join(#pq{p=Prios0, q=Queues0}, #pq{p=Prios1, q=Queues1}) ->
	Queues2=maps:map(
		fun
			(Prio, Queue0) when is_map_key(Prio, Queues1) ->
				Queue1=maps:get(Prio, Queues1),
				queue:join(Queue0, Queue1);
			(_, Queue0) ->
				Queue0
		end,
		Queues0
	),
	Queues3=maps:merge(Queues1, Queues2),
	Prios2=Prios1--Prios0,
	Prios3=Prios0++Prios2,
	#pq{p=Prios3, l=lists:last(Prios3), q=Queues3};
join(PQ0, PQ1) ->
	error(badarg, [PQ0, PQ1]).


%% ----------------------------------------------------------------------------
%% filter/2,3,4
%% ----------------------------------------------------------------------------

%% @doc Filter items of a priority queue.
%%
%% @param FilterFun The filter function, which takes two arguments,
%%	the priority of the subqueue being filtered, and an item.
%%	It must either return `true' to keep, `false' to discard,
%%	or a list of items to insert in place of the current item.
%% @param PrioQueue0 The priority queue whose subqueue to filter.
%%
%% @return A new priority queue, filtered via `FilterFun'.
%%
%% @see filter/3
%% @see filter/4
-spec filter(FilterFun, PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(Priority, Item0),
	FilterFun :: fun((Priority, Item0) -> boolean() | list(Item1)),
	PrioQueue1 :: prioqueue(Priority, Item0 | Item1).
filter(Fun, PQ=#pq{p=Prios}) when is_function(Fun, 2) ->
	do_filter(Prios, Fun, PQ);
filter(Fun, PQ) ->
	error(badarg, [Fun, PQ]).

%% @doc Filter items of a subqueue of a priority queue.
%%
%% @param Priority The priority subqueue to filter.
%% @param FilterFun The filter function, which takes two arguments,
%%	the priority of the subqueue being filtered, and an item.
%%	It must either return `true' to keep, `false' to discard,
%%	or a list of items to insert in place of the current item.
%% @param PrioQueue0 The priority queue whose subqueue to filter.
%%
%% @return A new priority queue with the given subqueue filtered
%%	via `FilterFun'.
%%
%% @see filter/2
%% @see filter/4
-spec filter(Priority, FilterFun, PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(Priority, Item0),
	FilterFun :: fun((Priority, Item0) -> boolean() | list(Item1)),
	PrioQueue1 :: prioqueue(Priority, Item0 | Item1).
filter(Prio, Fun, PQ=#pq{q=Queues}) when is_function(Fun, 2), is_map_key(Prio, Queues) ->
	do_filter([Prio], Fun, PQ);
filter(Prio, Fun, PQ) ->
	error(badarg, [Prio, Fun, PQ]).

%% @doc Filter items of a range of subqueues of a priority queue.
%%
%% @param MaxPriority The maximum priority subqueue to filter.
%% @param MinPriority The minimum priority subqueue to filter.
%% @param FilterFun The filter function, which takes two arguments,
%%	the priority of the subqueue being filtered, and an item.
%%	It must either return `true' to keep, `false' to discard,
%%	or a list of items to insert in place of the current item.
%% @param PrioQueue0 The priority queue whose subqueue to filter.
%%
%% @return A new priority queue with the subqueues between `MaxPriority'
%%	and `MinPriority', inclusive, filtered via `FilterFun'.
%%
%% @see filter/2
%% @see filter/3
-spec filter(MaxPriority, MinPriority, FilterFun, PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(Priority, Item0),
	MaxPriority :: Priority,
	MinPriority :: Priority,
	FilterFun :: fun((Priority, Item0) -> boolean() | list(Item1)),
	PrioQueue1 :: prioqueue(Priority, Item0 | Item1).
filter(Prio, Prio, Fun, PQ=#pq{q=Queues}) when is_function(Fun, 2), is_map_key(Prio, Queues) ->
	do_filter([Prio], Fun, PQ);
filter(MaxPrio, MinPrio, Fun, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) when is_function(Fun, 2) ->
	do_filter(Prios, Fun, PQ);
filter(MaxPrio, MinPrio, Fun, PQ=#pq{p=Prios, q=Queues}) when is_function(Fun, 2), is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_filter(Range, Fun, PQ);
filter(MaxPrio, MinPrio, Fun, PQ) ->
	error(badarg, [MaxPrio, MinPrio, Fun, PQ]).

%% helper

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


%% ----------------------------------------------------------------------------
%% fold/3,4,5
%% ----------------------------------------------------------------------------

%% @doc Fold a function over a priority queue, in priority queue order.
%%
%% @param FoldFun The fold function, which takes 3 arguments, the
%%	priority of the subqueue being filtered, an item, and an
%%	accumulator. It must return a new accumulator, which is then
%%	passed along in the next call to the fold function.
%% @param Acc0 The initial value of the accumulator.
%% @param PrioQueue The priority queue over which to fold.
%%
%% @returns The last value of the accumulator returned by the fold
%%	function after being run on successive elements of the
%%	entire priority queue, or `Acc0' if the priority queue was empty.
%%
%% @see fold/4
%% @see fold/5
-spec fold(FoldFun, Acc0, PrioQueue) -> Acc1 when
	PrioQueue :: prioqueue(Priority, Item),
	FoldFun :: fun((Priority, Item, AccIn) -> AccOut),
	AccIn :: term(),
	AccOut :: term(),
	Acc0 :: term(),
	Acc1 :: term().
fold(Fun, Acc0, PQ=#pq{p=Prios}) when is_function(Fun, 3) ->
	do_fold(Prios, Fun, Acc0, PQ);
fold(Fun, Acc0, PQ) ->
	error(badarg, [Fun, Acc0, PQ]).

%% @doc Fold a function over a subqueue of a priority queue, in priority
%%	queue order.
%%
%% @param Priority The priority subqueue to be folded over.
%% @param FoldFun The fold function, which takes 3 arguments, the
%%	priority of the subqueue being filtered, an item, and an
%%	accumulator. It must return a new accumulator, which is then
%%	passed along in the next call to the fold function.
%% @param Acc0 The initial value of the accumulator.
%% @param PrioQueue The priority queue over whose subqueue to fold.
%%
%% @returns The last value of the accumulator returned by the fold
%%	function after being run on successive elements of the given
%%	subqueue, or `Acc0' if the subqueue was empty.
%%
%% @see fold/3
%% @see fold/5
-spec fold(Priority, FoldFun, Acc0, PrioQueue) -> Acc1 when
	PrioQueue :: prioqueue(Priority, Item),
	FoldFun :: fun((Priority, Item, AccIn) -> AccOut),
	AccIn :: term(),
	AccOut :: term(),
	Acc0 :: term(),
	Acc1 :: term().
fold(Prio, Fun, Acc0, PQ=#pq{q=Queues}) when is_function(Fun, 3), is_map_key(Prio, Queues) ->
	do_fold([Prio], Fun, Acc0, PQ);
fold(Prio, Fun, Acc0, PQ) ->
	error(badarg, [Prio, Fun, Acc0, PQ]).

%% @doc Fold a function over a range of subqueues of a priority queue,
%%	in priority queue order.
%%
%% @param MaxPriority The maximum priority subqueue to be folded over.
%% @param MinPriority The minimum priority subqueue to be folded over.
%% @param FoldFun The fold function, which takes 3 arguments, the
%%	priority of the subqueue being filtered, an item, and an
%%	accumulator. It must return a new accumulator, which is then
%%	passed along in the next call to the fold function.
%% @param Acc0 The initial value of the accumulator.
%% @param PrioQueue The priority queue over whose subqueues to fold.
%%
%% @returns The last value of the accumulator returned by the fold
%%	function after being run on successive elements of the
%%	subqueues between `MaxPriority' and `MinPriority', inclusive,
%%	or `Acc0' if all the subqueues were empty.
%%
%% @see fold/3
%% @see fold/4
-spec fold(MaxPriority, MinPriority, FoldFun, Acc0, PrioQueue) -> Acc1 when
	PrioQueue :: prioqueue(Priority, Item),
	MaxPriority :: Priority,
	MinPriority :: Priority,
	FoldFun :: fun((Priority, Item, AccIn) -> AccOut),
	AccIn :: term(),
	AccOut :: term(),
	Acc0 :: term(),
	Acc1 :: term().
fold(Prio, Prio, Fun, Acc0, PQ=#pq{q=Queues}) when is_function(Fun, 3), is_map_key(Prio, Queues) ->
	do_fold([Prio], Fun, Acc0, PQ);
fold(MaxPrio, MinPrio, Fun, Acc0, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) when is_function(Fun, 3) ->
	do_fold(Prios, Fun, Acc0, PQ);
fold(MaxPrio, MinPrio, Fun, Acc0, PQ=#pq{p=Prios, q=Queues}) when is_function(Fun, 3), is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_fold(Range, Fun, Acc0, PQ);
fold(MaxPrio, MinPrio, Fun, Acc0, PQ) ->
	error(badarg, [MaxPrio, MinPrio, Fun, Acc0, PQ]).

%% helper

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


%% ----------------------------------------------------------------------------
%% any/2,3,4
%% ----------------------------------------------------------------------------

%% @doc Check if at least one item of a priority queue
%% satisfies a predicate.
%%
%% @param PredicateFun The predicate function, which takes two arguments,
%%	the priority of the subqueue being checked, and an item. It must
%%	return either `true' if the predicate condition is met, otherwise
%%	`false'.
%% @param PrioQueue The priority queue to check.
%%
%% @returns `true' any item in the priority queue satisfies the predicate
%%	condition, otherwise `false'.
%%
%% @see any/3
%% @see any/4
-spec any(PredicateFun, PrioQueue) -> boolean() when
	PrioQueue :: prioqueue(Priority, Item),
	PredicateFun :: fun((Priority, Item) -> boolean()).
any(Fun, PQ=#pq{p=Prios}) when is_function(Fun, 2) ->
	do_any(Prios, Fun, PQ);
any(Fun, PQ) ->
	error(badarg, [Fun, PQ]).

%% @doc Check if at least one item of a subqueue of a
%% priority queue satisfies a predicate.
%%
%% @param Priority The priority subqueue to check.
%% @param PredicateFun The predicate function, which takes two arguments,
%%	the priority of the subqueue being checked, and an item. It must
%%	return either `true' if the predicate condition is met, otherwise
%%	`false'.
%% @param PrioQueue The priority queue whose subqueue to check.
%%
%% @returns `true' if any item in the given subqueue satisfies the predicate
%%	condition, otherwise `false'.
%%
%% @see any/2
%% @see any/4
-spec any(Priority, PredicateFun, PrioQueue) -> boolean() when
	PrioQueue :: prioqueue(Priority, Item),
	PredicateFun :: fun((Priority, Item) -> boolean()).
any(Prio, Fun, PQ=#pq{q=Queues}) when is_function(Fun, 2), is_map_key(Prio, Queues) ->
	do_any([Prio], Fun, PQ);
any(Prio, Fun, PQ) ->
	error(badarg, [Prio, Fun, PQ]).

%% @doc Check if at least one item of a range of subqueues of a
%% priority queue satisfies a predicate.
%%
%% @param MaxPriority The maximum priority subqueue to check.
%% @param MinPriority The minimum priority subqueue to check.
%% @param PredicateFun The predicate function, which takes two arguments,
%%	the priority of the subqueue being checked, and an item. It must
%%	return either `true' if the predicate condition is met, otherwise
%%	`false'.
%% @param PrioQueue The priority queue whose subqueues to check.
%%
%% @returns `true' if any item in the subqueues between `MaxPriority' and
%%	`MinPriority' satisfies the predicate condition, otherwise `false'.
%%
%% @see any/2
%% @see any/3
-spec any(MaxPriority, MinPriority, PredicateFun, PrioQueue) -> boolean() when
	PrioQueue :: prioqueue(Priority, Item),
	MaxPriority :: Priority,
	MinPriority :: Priority,
	PredicateFun :: fun((Priority, Item) -> boolean()).
any(Prio, Prio, Fun, PQ=#pq{q=Queues}) when is_function(Fun, 2), is_map_key(Prio, Queues) ->
	do_any([Prio], Fun, PQ);
any(MaxPrio, MinPrio, Fun, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) when is_function(Fun, 2) ->
	do_any(Prios, Fun, PQ);
any(MaxPrio, MinPrio, Fun, PQ=#pq{p=Prios, q=Queues}) when is_function(Fun, 2), is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_any(Range, Fun, PQ);
any(MaxPrio, MinPrio, Fun, PQ) ->
	error(badarg, [MaxPrio, MinPrio, Fun, PQ]).

%% helper

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


%% ----------------------------------------------------------------------------
%% all/2,3,4
%% ----------------------------------------------------------------------------

%% @doc Check if all items of a priority queue
%% satisfy a predicate.
%%
%% @param PredicateFun The predicate function, which takes two arguments,
%%	the priority of the subqueue being checked, and an item. It must
%%	return either `true' if the predicate condition is met, otherwise
%%	`false'.
%% @param PrioQueue The priority queue to check.
%%
%% @returns `true' if all items in the priority queue satisfy the predicate
%%	condition, otherwise `false'.
%%
%% @see all/3
%% @see all/4
-spec all(PredicateFun, PrioQueue) -> boolean() when
	PrioQueue :: prioqueue(Priority, Item),
	PredicateFun :: fun((Priority, Item) -> boolean()).
all(Fun, PQ=#pq{p=Prios}) when is_function(Fun, 2) ->
	do_all(Prios, Fun, PQ);
all(Fun, PQ) ->
	error(badarg, [Fun, PQ]).

%% @doc Check if all items of a subqueue of a
%% priority queue satisfy a predicate.
%%
%% @param Priority The priority subqueue to check.
%% @param PredicateFun The predicate function, which takes two arguments,
%%	the priority of the subqueue being checked, and an item. It must
%%	return either `true' if the predicate condition is met, otherwise
%%	`false'.
%% @param PrioQueue The priority queue whose subqueue to check.
%%
%% @returns `true' if all items in the given subqueue satisfy the predicate
%%	condition, otherwise `false'.
%%
%% @see all/2
%% @see all/4
-spec all(Priority, PredicateFun, PrioQueue) -> boolean() when
	PrioQueue :: prioqueue(Priority, Item),
	PredicateFun :: fun((Priority, Item) -> boolean()).
all(Prio, Fun, PQ=#pq{q=Queues}) when is_function(Fun, 2), is_map_key(Prio, Queues) ->
	do_all([Prio], Fun, PQ);
all(Prio, Fun, PQ) ->
	error(badarg, [Prio, Fun, PQ]).

%% @doc Check if all items of a range of subqueues of a
%% priority queue satisfy a predicate.
%%
%% @param MaxPriority The maximum priority subqueue to check.
%% @param MinPriority The minimum priority subqueue to check.
%% @param PredicateFun The predicate function, which takes two arguments,
%%	the priority of the subqueue being checked, and an item. It must
%%	return either `true' if the predicate condition is met, otherwise
%%	`false'.
%% @param PrioQueue The priority queue whose subqueues to check.
%%
%% @returns `true' if all items in the subqueues between `MaxPriority' and
%%	`MinPriority' satisfy the predicate condition, otherwise `false'.
%%
%% @see all/2
%% @see all/3
-spec all(MaxPriority, MinPriority, PredicateFun, PrioQueue) -> boolean() when
	PrioQueue :: prioqueue(Priority, Item),
	MaxPriority :: Priority,
	MinPriority :: Priority,
	PredicateFun :: fun((Priority, Item) -> boolean()).
all(Prio, Prio, Fun, PQ=#pq{q=Queues}) when is_function(Fun, 2), is_map_key(Prio, Queues) ->
	do_all([Prio], Fun, PQ);
all(MaxPrio, MinPrio, Fun, PQ=#pq{p=Prios=[MaxPrio|_], l=MinPrio}) when is_function(Fun, 2) ->
	do_all(Prios, Fun, PQ);
all(MaxPrio, MinPrio, Fun, PQ=#pq{p=Prios, q=Queues}) when is_function(Fun, 2), is_map_key(MaxPrio, Queues), is_map_key(MinPrio, Queues) ->
	Range=select_prio_range(MaxPrio, MinPrio, Prios),
	do_all(Range, Fun, PQ);
all(MaxPrio, MinPrio, Fun, PQ) ->
	error(badarg, [MaxPrio, MinPrio, Fun, PQ]).

%% helper

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

%% ============================================================================
%% INTERNAL
%% ============================================================================

select_prio_range(Max, Min, Prios0) ->
	Prios1=lists:dropwhile(fun (Prio) -> Prio=/=Min end, lists:reverse(Prios0)),
	lists:dropwhile(fun (Prio) -> Prio=/=Max end, lists:reverse(Prios1)).

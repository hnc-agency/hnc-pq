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

-module(hnc_pq).

-export([filter/2, filter/3]).
-export([fold/3, fold/4]).
-export([from_list/1]).
-export([in/2, in/3]).
-export([in_r/2, in_r/3]).
-export([is_empty/1, is_empty/2]).
-export([is_priority/2]).
-export([join/2, join/3]).
-export([length/1, length/2]).
-export([member/2, member/3]).
-export([new/1]).
-export([out/1, out/2]).
-export([out_r/1, out_r/2]).
-export([priorities/1]).
-export([queue/2]).
-export([reprioritize/2]).
-export([reverse/1, reverse/2]).
-export([reverse_priorities/1]).
-export([to_flatlist/1]).
-export([to_flatqueue/1]).
-export([to_list/1, to_list/2]).

-record(pq, {
	p=[]        :: list(Prio),
	l=undefined :: Prio,
	q=#{}       :: #{Prio => queue:queue()}
}).

-opaque prioqueue() :: #pq{}.
-export_type([prioqueue/0]).

-type priority() :: term().
-type item() :: term().

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

%% @doc Convert a priority queue to a flat queue of items.
%%
%% The items in the result queue are ordered from the front
%% element of the highest priority to the rear element of
%% the lowest priority, which is the order in which they would
%% be returned by successive calls to `out/1'.
-spec to_flatqueue(PrioQueue) -> FlatQueue when
	PrioQueue :: prioqueue(),
	FlatQueue :: queue:queue().
to_flatqueue(#pq{p=Prios, q=Queues}) ->
	lists:foldl(
		fun (Prio, Acc) ->
			queue:join(Acc, maps:get(Prio, Queues))
		end,
		queue:new(),
		Prios
	);
to_flatqueue(PQ) ->
	error(badarg, [PQ]).

%% @doc Convert a specific subqueue of the priority queue to a list of items.
%%
%% The items in the result list are ordered from front to rear of the queried
%% subqueue, which is the order in which they would be returned by successive calls
%% to `out/2'.
-spec to_list(Priority, PrioQueue) -> ItemList when
	Priority :: priority(),
	PrioQueue :: prioqueue(),
	ItemList :: list(Item),
	Item :: item().
to_list(Prio, #pq{q=Queues}) when is_map_key(Prio, Queues) ->
	queue:to_list(maps:get(Prio, Queues));
to_list(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

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
	lists:foldr(
		fun (Prio, Acc) ->
			[{Prio, to_list(Prio, PQ)}|Acc]
		end,
		[],
		Prios
	);
to_list(PQ) ->
	error(badarg, [PQ]).

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
	lists:foldr(
		fun (Prio, Acc) ->
			to_list(Prio, PQ)++Acc
		end,
		[],
		Prios
	);
to_flatlist(PQ) ->
	error(badarg, [PQ]).

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
reverse(Prio, PQ=#pq{q=Queues0}) when is_map_key(Prio, Queues0) ->
	Queues1=maps:update_with(Prio, fun (Queue) -> queue:reverse(Queue) end, Queues0),
	PQ#pq{q=Queues1};
reverse(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Reverse a priority queue.
%%
%% The resulting priority queue will have the priorities in reverse
%% order, and the items in each subqueue will be in reverse order, also.
-spec reverse(PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
reverse(#pq{p=Prios0, q=Queues0}) ->
	Prios1=lists:reverse(Prios0),
	Queues1=maps:map(
		fun (_, Queue) ->
			queue:reverse(Queue)
		end,
		Queues0
	),
	#pq{p=Prios1, l=hd(Prios0), q=Queues1};
reverse(PQ) ->
	error(badarg, [PQ]).

%% @doc Reverse the priorities of a priority queue.
%%
%% The resulting priority queue will have the priorities in reverse order,
%% but the items in each subqueue will remain in the current order.
-spec reverse_priorities(PrioQueue0) -> PrioQueue1 when
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
reverse_priorities(PQ=#pq{p=Prios0}) ->
	Prios1=lists:reverse(Prios0),
	PQ#pq{p=Prios1, l=hd(Prios0)};
reverse_priorities(PQ) ->
	error(badarg, [PQ]).

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
member(Prio, Item, #pq{q=Queues}) when is_map_key(Prio, Queues) ->
	queue:member(Item, maps:get(Prio, Queues));
member(Prio, Item, PQ) ->
	error(badarg, [Prio, Item, PQ]).

%% @doc Check if the given item is a member of a priority queue.
-spec member(Item, PrioQueue) -> boolean() when
	Item :: item(),
	PrioQueue :: prioqueue().
member(Item, #pq{q=Queues}) ->
	lists:any(
		fun (Queue) ->
			queue:member(Item, Queue)
		end,
		maps:values(Queues)
	);
member(Item, PQ) ->
	error(badarg, [Item, PQ]).

%% @doc Insert the given item into a subqueue of a priority queue, at the rear.
-spec in(Priority, Item, PrioQueue0) -> PrioQueue1 when
	Priority :: priority(),
	Item :: item(),
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
in(Prio, Item, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_in(in, Prio, Item, PQ);
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
	do_in(in, Prio, Item, PQ);
in(Item, PQ) ->
	error(badarg, [Item, PQ]).
 
%% @doc Insert the given item into a subqueue of a priority queue, at the front.
-spec in_r(Priority, Item, PrioQueue0) -> PrioQueue1 when
	Priority :: priority(),
	Item :: item(),
	PrioQueue0 :: prioqueue(),
	PrioQueue1 :: prioqueue().
in_r(Prio, Item, PQ=#pq{q=Queues}) when is_map_key(Prio, Queues) ->
	do_in(in_r, Prio, Item, PQ);
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
	do_in(in_r, Prio, Item, PQ);
in_r(Item, PQ) ->
	error(badarg, [Item, PQ]).

do_in(QOp, Prio, Item, PQ=#pq{q=Queues0}) ->
	Queues1=maps:update_with(Prio, fun (Queue) -> queue:QOp(Item, Queue) end, Queues0),
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
	do_out_one(out, Prio, PQ);
out(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Retrieve and remove the next item from a priority queue, from the front.
%%
%% The item will be taken from the front of the subqueue with the highest priority
%% that is not empty. If all subqueues are empty, the atom `empty'  is returned.
-spec out(PrioQueue0) -> empty | {Item, PrioQueue1} when
	PrioQueue0 :: prioqueue(),
	Item :: item(),
	PrioQueue1 :: prioqueue().
out(PQ=#pq{p=Prios}) ->
	do_out_all(out, Prios, PQ);
out(PQ) ->
	error(badarg, [PQ]).

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
	do_out_one(out_r, Prio, PQ);
out_r(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Retrieve and remove the next item from a priority queue, from the rear.
%%
%% The item will be taken from the rear of the subqueue with the lowest priority
%% that is not empty. If all subqueues are empty, the atom `empty'  is returned.
-spec out_r(PrioQueue0) -> empty | {Item, PrioQueue1} when
	PrioQueue0 :: prioqueue(),
	Item :: item(),
	PrioQueue1 :: prioqueue().
out_r(PQ=#pq{p=Prios}) ->
	do_out_all(out_r, lists:reverse(Prios), PQ);
out_r(PQ) ->
	error(badarg, [PQ]).

do_out_one(QOp, Prio, PQ=#pq{q=Queues}) ->
	Queue0=maps:get(Prio, Queues),
	case do_out(QOp, Queue0) of
		empty ->
			empty;
		{Value, Queue1} ->
			{Value, PQ#pq{q=maps:update(Prio, Queue1, Queues)}}
	end.

do_out_all(_, [], _) ->
	empty;
do_out_all(QOp, [Prio|Prios], PQ) ->
	case do_out_one(QOp, Prio, PQ) of
		empty ->
			do_out_all(QOp, Prios, PQ);
		Result ->
			Result
	end.

do_out(QOp, Queue0) ->
	case queue:QOp(Queue0) of
		{empty, _} -> empty;
		{{value, Value}, Queue1} -> {Value, Queue1}
	end.

%% @doc Check if a subqueue of a priority queue is empty.
-spec is_empty(Priority, PrioQueue) -> boolean() when
	Priority :: priority(),
	PrioQueue :: prioqueue().
is_empty(Prio, #pq{q=Queues}) when is_map_key(Prio, Queues) ->
	queue:is_empty(maps:get(Prio, Queues));
is_empty(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Check if a priority queue is empty.
-spec is_empty(PrioQueue) -> boolean() when
	PrioQueue :: prioqueue().
is_empty(#pq{q=Queues}) ->
	lists:all(fun queue:is_empty/1, maps:values(Queues));
is_empty(PQ) ->
	error(badarg, [PQ]).

%% @doc Retrieve the number of items in a subqueue of a priority queue.
-spec length(Priority, PrioQueue) -> Length when
	Priority :: priority(),
	PrioQueue :: prioqueue(),
	Length :: non_neg_integer().
length(Prio, #pq{q=Queues}) when is_map_key(Prio, Queues) ->
	queue:len(maps:get(Prio, Queues));
length(Prio, PQ) ->
	error(badarg, [Prio, PQ]).

%% @doc Retrieve the number of items in a priority queue.
-spec length(PrioQueue) -> Length when
	PrioQueue :: prioqueue(),
	Length :: non_neg_integer().
length(#pq{q=Queues}) ->
	maps:fold(
		fun (_, Queue, Acc) ->
			Acc+queue:len(Queue)
		end,
		0,
		Queues
	);
length(PQ) ->
	error(badarg, [PQ]).

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

%% @doc Join to priority queues.
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
%% The filter function takes two arguments, the priority of the
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
	do_filter_one(Prio, Fun, PQ);
filter(Prio, Fun, PQ) ->
	error(badarg, [Prio, Fun, PQ]).

%% @doc Filter items of a priority queue.
%%
%% The filter function takes two arguments, the priority of each
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
	do_filter_all(Prios, Fun, PQ);
filter(Fun, PQ) ->
	error(badarg, [Fun, PQ]).

do_filter_one(Prio, Fun, PQ=#pq{q=Queues0}) ->
	Queue0=maps:get(Prio, Queues0),
	Queue1=do_filter(Prio, Fun, Queue0),
	Queues1=maps:update(Prio, Queue1, Queues0),
	PQ#pq{q=Queues1}.

do_filter_all([], _, PQ) ->
	PQ;
do_filter_all([Prio|Prios], Fun, PQ) ->
	do_filter_all(Prios, Fun, do_filter_one(Prio, Fun, PQ)).

do_filter(Prio, Fun, Queue) ->
	queue:filter(fun (Item) -> Fun(Prio, Item) end, Queue).

%% @doc Fold a function over a subqueue of a priority queue.
%%
%% The fold function takes three elements, the priority of the
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
	do_fold_one(Prio, Fun, Acc0, PQ);
fold(Prio, Fun, Acc0, PQ) ->
	error(badarg, [Prio, Fun, Acc0, PQ]).

%% @doc Fold a function over a priority queue.
%%
%% The fold function takes three elements, the priority of each
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
	do_fold_all(Prios, Fun, Acc0, PQ);
fold(Fun, Acc0, PQ) ->
	error(badarg, [Fun, Acc0, PQ]).

do_fold_one(Prio, Fun, Acc0, #pq{q=Queues}) ->
	Queue=maps:get(Prio, Queues),
	lists:foldl(
		fun (Item, AccIn) ->
			Fun(Prio, Item, AccIn)
		end,
		Acc0,
		queue:to_list(Queue)
	).

do_fold_all([], _, Acc, _) ->
	Acc;
do_fold_all([Prio|Prios], Fun, Acc0, PQ) ->
	Acc1=do_fold_one(Prio, Fun, Acc0, PQ),
	do_fold_all(Prios, Fun, Acc1, PQ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

do_in_test() ->
	Prios=[a, b],

	Q0=#pq{p=Prios, l=lists:last(Prios), q=#{a => queue:new(), b => queue:new()}},
	lists:foreach(
		fun ({QOp, Prio}) ->
			Q=do_in(QOp, Prio, foo, Q0),
			?assertEqual(Q0#pq.p, Q#pq.p),
			?assertEqual(Q0#pq.l, Q#pq.l),
			_=maps:map(
				fun
					(P, QQ) when P=:=Prio ->
						?assertEqual([foo], queue:to_list(QQ));
					(_, QQ) ->
						?assertEqual([], queue:to_list(QQ))
				end,
				Q#pq.q
			)
		end,
		[{QOp, Prio} || QOp <- [in, in_r], Prio <- Prios]
	),

	Q1=#pq{p=Prios, l=lists:last(Prios), q=#{a => queue:from_list([{a, foo}]), b => queue:from_list([{b, foo}])}},
	lists:foreach(
		fun ({QOp, Prio}) ->
			Q=do_in(QOp, Prio, bar, Q1),
			?assertEqual(Q0#pq.p, Q#pq.p),
			?assertEqual(Q0#pq.l, Q#pq.l),
			_=maps:map(
				fun
					(P, QQ) when P=:=Prio, QOp=:=in ->
						?assertEqual([{P, foo}, bar], queue:to_list(QQ));
					(P, QQ) when P=:=Prio, QOp=:=in_r ->
						?assertEqual([bar, {P, foo}], queue:to_list(QQ));
					(P, QQ) ->
						?assertEqual([{P, foo}], queue:to_list(QQ))
				end,
				Q#pq.q
			)
		end,
		[{QOp, Prio} || QOp <- [in, in_r], Prio <- Prios]
	),

	ok.

do_out_test() ->
	?assertEqual(empty, do_out(out, queue:new())),
	?assertEqual(empty, do_out(out_r, queue:new())),
	Q0=queue:from_list([foo, bar]),
	{V1, Q1}=do_out(out, Q0),
	?assertEqual(foo, V1),
	?assertEqual([bar], queue:to_list(Q1)),
	{V2, Q2}=do_out(out_r, Q0),
	?assertEqual(bar, V2),
	?assertEqual([foo], queue:to_list(Q2)),
	ok.

do_out_one_test() ->
	Prios=[a, b],

	Q0=#pq{p=Prios, l=lists:last(Prios), q=#{a => queue:new(), b => queue:new()}},
	lists:foreach(
		fun ({QOp, Prio}) ->
			?assertEqual(empty, do_out_one(QOp, Prio, Q0))
		end,
		[{QOp, Prio} || QOp <- [out, out_r], Prio <- Prios]
	),

	Q1=#pq{p=Prios, l=lists:last(Prios), q=#{a => queue:from_list([foo, bar]), b => queue:from_list([foo, bar])}},
	{V11, Q11}=do_out_one(out, a, Q1),
	?assertEqual(Q1#pq.p, Q11#pq.p),
	?assertEqual(Q1#pq.l, Q11#pq.l),
	?assertEqual(foo, V11),
	?assertEqual([bar], queue:to_list(maps:get(a, Q11#pq.q))),
	?assertEqual([foo, bar], queue:to_list(maps:get(b, Q11#pq.q))),
	{V12, Q12}=do_out_one(out, b, Q1),
	?assertEqual(Q1#pq.p, Q12#pq.p),
	?assertEqual(Q1#pq.l, Q12#pq.l),
	?assertEqual(foo, V12),
	?assertEqual([foo, bar], queue:to_list(maps:get(a, Q12#pq.q))),
	?assertEqual([bar], queue:to_list(maps:get(b, Q12#pq.q))),
	{V13, Q13}=do_out_one(out_r, a, Q1),
	?assertEqual(Q1#pq.p, Q13#pq.p),
	?assertEqual(Q1#pq.l, Q13#pq.l),
	?assertEqual(bar, V13),
	?assertEqual([foo], queue:to_list(maps:get(a, Q13#pq.q))),
	?assertEqual([foo, bar], queue:to_list(maps:get(b, Q13#pq.q))),
	{V14, Q14}=do_out_one(out_r, b, Q1),
	?assertEqual(Q1#pq.p, Q14#pq.p),
	?assertEqual(Q1#pq.l, Q14#pq.l),
	?assertEqual(bar, V14),
	?assertEqual([foo, bar], queue:to_list(maps:get(a, Q14#pq.q))),
	?assertEqual([foo], queue:to_list(maps:get(b, Q14#pq.q))),

	ok.

do_out_all_test() ->
	Prios=[a, b],

	Q0=#pq{p=Prios, l=lists:last(Prios), q=#{a => queue:new(), b => queue:new()}},
	?assertEqual(empty, do_out_all(out, Prios, Q0)),
	?assertEqual(empty, do_out_all(out, Prios, Q0)),

	Q1=#pq{p=Prios, l=lists:last(Prios), q=#{a => queue:from_list([foo, bar]), b => queue:from_list([foo, bar])}},
	{V11, Q11}=do_out_all(out, Prios, Q1),
	?assertEqual(Q1#pq.p, Q11#pq.p),
	?assertEqual(Q1#pq.l, Q11#pq.l),
	?assertEqual(foo, V11),
	?assertEqual([bar], queue:to_list(maps:get(a, Q11#pq.q))),
	?assertEqual([foo, bar], queue:to_list(maps:get(b, Q11#pq.q))),
	{V12, Q12}=do_out_all(out_r, lists:reverse(Prios), Q1),
	?assertEqual(Q1#pq.p, Q12#pq.p),
	?assertEqual(Q1#pq.l, Q12#pq.l),
	?assertEqual(bar, V12),
	?assertEqual([foo, bar], queue:to_list(maps:get(a, Q12#pq.q))),
	?assertEqual([foo], queue:to_list(maps:get(b, Q12#pq.q))),

	ok.

do_filter_test() ->
	?assertEqual([], queue:to_list(do_filter(a, fun (_, _) -> true end, queue:new()))),
	?assertEqual([], queue:to_list(do_filter(a, fun (_, _) -> false end, queue:new()))),
	?assertEqual([], queue:to_list(do_filter(a, fun (_, I) -> [{I, I}] end, queue:new()))),

	Fun1=fun
		(a, foo) -> false;
		(b, bar) -> false;
		(c, baz) -> [{baz, baz}];
		(d, I) -> [I, I];
		(_, _) -> true
	end,
	Items1=[foo, bar, baz],
	?assertEqual([bar, baz], queue:to_list(do_filter(a, Fun1, queue:from_list(Items1)))),
	?assertEqual([foo, baz], queue:to_list(do_filter(b, Fun1, queue:from_list(Items1)))),
	?assertEqual([foo, bar, {baz, baz}], queue:to_list(do_filter(c, Fun1, queue:from_list(Items1)))),
	?assertEqual([foo, foo, bar, bar, baz, baz], queue:to_list(do_filter(d, Fun1, queue:from_list(Items1)))),
	?assertEqual([foo, bar, baz], queue:to_list(do_filter(e, Fun1, queue:from_list(Items1)))),

	ok.

do_filter_one_test() ->
	Prios=[a, b],

	Q0=#pq{p=Prios, l=lists:last(Prios), q=#{a => queue:new(), b => queue:new()}},
	?assertEqual(Q0, do_filter_one(a, fun (_, _) -> true end, Q0)),
	?assertEqual(Q0, do_filter_one(a, fun (_, _) -> false end, Q0)),
	?assertEqual(Q0, do_filter_one(a, fun (_, I) -> [{I, I}] end, Q0)),

	Q1=#pq{p=Prios, l=lists:last(Prios), q=#{a => queue:from_list([foo, bar]), b => queue:from_list([foo, bar])}},
	Fun1=fun
		(a, foo) -> false;
		(b, bar) -> false;
		(_, _) -> true
	end,
	Q11=do_filter_one(a, Fun1, Q1),
	?assertEqual(Q1#pq.p, Q11#pq.p),
	?assertEqual(Q1#pq.l, Q11#pq.l),
	?assertEqual([bar], queue:to_list(maps:get(a, Q11#pq.q))),
	?assertEqual([foo, bar], queue:to_list(maps:get(b, Q11#pq.q))),
	Q12=do_filter_one(b, Fun1, Q1),
	?assertEqual(Q1#pq.p, Q12#pq.p),
	?assertEqual(Q1#pq.l, Q12#pq.l),
	?assertEqual([foo, bar], queue:to_list(maps:get(a, Q12#pq.q))),
	?assertEqual([foo], queue:to_list(maps:get(b, Q12#pq.q))),
	Fun2=fun
		(a, foo) -> [foo, foo];
		(b, bar) -> [bar, bar];
		(_, _) -> false
	end,
	Q13=do_filter_one(a, Fun2, Q1),
	?assertEqual(Q1#pq.p, Q13#pq.p),
	?assertEqual(Q1#pq.l, Q13#pq.l),
	?assertEqual([foo, foo], queue:to_list(maps:get(a, Q13#pq.q))),
	?assertEqual([foo, bar], queue:to_list(maps:get(b, Q13#pq.q))),
	Q14=do_filter_one(b, Fun2, Q1),
	?assertEqual(Q1#pq.p, Q14#pq.p),
	?assertEqual(Q1#pq.l, Q14#pq.l),
	?assertEqual([foo, bar], queue:to_list(maps:get(a, Q14#pq.q))),
	?assertEqual([bar, bar], queue:to_list(maps:get(b, Q14#pq.q))),

	ok.
	
do_filter_all_test() ->
	Prios=[a, b],

	Q0=#pq{p=Prios, l=lists:last(Prios), q=#{a => queue:new(), b => queue:new()}},
	?assertEqual(Q0, do_filter_all(Prios, fun (_, _) -> true end, Q0)),
	?assertEqual(Q0, do_filter_all(Prios, fun (_, _) -> false end, Q0)),
	?assertEqual(Q0, do_filter_all(Prios, fun (_, I) -> [{I, I}] end, Q0)),

	Q1=#pq{p=Prios, l=lists:last(Prios), q=#{a => queue:from_list([foo, bar]), b => queue:from_list([foo, bar])}},
	Fun1=fun
		(a, foo) -> false;
		(b, bar) -> false;
		(_, _) -> true
	end,
	Q11=do_filter_all(Prios, Fun1, Q1),
	?assertEqual(Q1#pq.p, Q11#pq.p),
	?assertEqual(Q1#pq.l, Q11#pq.l),
	?assertEqual([bar], queue:to_list(maps:get(a, Q11#pq.q))),
	?assertEqual([foo], queue:to_list(maps:get(b, Q11#pq.q))),
	Fun2=fun
		(a, foo) -> [foo, foo];
		(b, bar) -> [bar, bar];
		(_, _) -> false
	end,
	Q12=do_filter_all(Prios, Fun2, Q1),
	?assertEqual(Q1#pq.p, Q12#pq.p),
	?assertEqual(Q1#pq.l, Q12#pq.l),
	?assertEqual([foo, foo], queue:to_list(maps:get(a, Q12#pq.q))),
	?assertEqual([bar, bar], queue:to_list(maps:get(b, Q12#pq.q))),

	ok.

do_fold_one_test() ->
	Prios=[a, b],

	Q0=#pq{p=Prios, l=lists:last(Prios), q=#{a => queue:new(), b => queue:new()}},
	?assertEqual(foo, do_fold_one(a, fun (_, _, _) -> bar end, foo, Q0)),

	Q1=#pq{p=Prios, l=lists:last(Prios), q=#{a => queue:from_list([foo]), b => queue:from_list([bar, baz])}},
	Fun1=fun
		(a, _, {AAcc, BAcc}) -> {AAcc+1, BAcc};
		(b, _, {AAcc, BAcc}) -> {AAcc, BAcc+1}
	end,
	Acc01={0, 0},
	?assertEqual({1, 0}, do_fold_one(a, Fun1, Acc01, Q1)),
	?assertEqual({0, 2}, do_fold_one(b, Fun1, Acc01, Q1)),

	ok.

do_fold_all_test() ->
	Prios=[a, b],

	Q0=#pq{p=Prios, l=lists:last(Prios), q=#{a => queue:new(), b => queue:new()}},
	?assertEqual(foo, do_fold_all(Prios, fun (_, _, _) -> bar end, foo, Q0)),

	Q1=#pq{p=Prios, l=lists:last(Prios), q=#{a => queue:from_list([foo]), b => queue:from_list([bar, baz])}},
	Fun1=fun
		(a, _, {AAcc, BAcc}) -> {AAcc+1, BAcc};
		(b, _, {AAcc, BAcc}) -> {AAcc, BAcc+1}
	end,
	?assertEqual({1, 2}, do_fold_all(Prios, Fun1, {0, 0}, Q1)),

	ok.

-endif.

# hnc-pq - Erlang priority queues

```erlang
%% Create a new hnc_pq instance with
%% 3 priorities: high, normal, and low
PQ0=hnc_pq:new([high, normal, low]).

%% Enqueue an item with high priority.
PQ1=hnc_pq:in(high, item_high, PQ0).

%% Enqueue another item with normal priority.
PQ2=hnc_pq:in(normal, item_normal, PQ1).

%% Enqueue yet another item with low priority.
PQ3=hnc_pq:in(low, item_low, PQ2).

%% Dequeue an item. Highest-priority items get
%% dequeued first.
{item_high, PQ4}=hnc_pq:out(PQ3).

%% Dequeue another item. As the high-priority
%% queue is empty by now, it is dequeued by
%% the next-highest priority queue (normal).
{item_normal, PQ5}=hnc_pq:out(PQ4).

%% Dequeue yet another item. As both the
%% high- and normal-priority queues are now
%% empty, it is dequeued from the next-highest
%% priority queue (low).
{item_low, PQ6}=hnc_pq:out(PQ5).

%% All queues are now empty.
empty=hnc_pq:out(PQ6).
```

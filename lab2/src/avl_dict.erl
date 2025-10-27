-module(avl_dict).


-export([new/0, insert/3, delete/2, find/2]).
-export([map/2, filter/2, fold_left/3, fold_right/3]).
-export([empty/0, concat/2, merge/3]).
-export([from_list/1, to_list/1, size_custom/1, equal/2]).

-record(node, {key, value, left = nil, right = nil, height = 1}).
-type tree() :: nil | #node{}.
-export_type([tree/0]).

%% Создание
new() -> nil.
empty() -> nil.

%% Вставка
insert(nil, Key, Value) ->
  #node{key = Key, value = Value};
insert(#node{key = K, left = L} = Node, Key, Value) when Key < K ->
  NewLeft = insert(L, Key, Value),
  rebalance(Node#node{left = NewLeft});
insert(#node{key = K, right = R} = Node, Key, Value) when Key > K ->
  NewRight = insert(R, Key, Value),
  rebalance(Node#node{right = NewRight});
insert(#node{} = Node, _Key, Value) ->
  Node#node{value = Value}.

%% Удаление
delete(nil, _Key) ->
  nil;
delete(#node{key = K, left = L} = Node, Key) when Key < K ->
  NewLeft = delete(L, Key),
  rebalance(Node#node{left = NewLeft});
delete(#node{key = K, right = R} = Node, Key) when Key > K ->
  NewRight = delete(R, Key),
  rebalance(Node#node{right = NewRight});
delete(#node{left = nil, right = R}, _Key) ->
  R;
delete(#node{left = L, right = nil}, _Key) ->
  L;
delete(#node{left = L, right = R}, _Key) ->
  {MinKey, MinValue, NewRight} = extract_min(R),
  rebalance(#node{key = MinKey, value = MinValue, left = L, right = NewRight}).

extract_min(#node{key = K, value = V, left = nil, right = R}) ->
  {K, V, R};
extract_min(#node{key = K, value = V, left = L, right = R}) ->
  {MinKey, MinValue, NewLeft} = extract_min(L),
  {MinKey, MinValue, rebalance(#node{key = K, value = V, left = NewLeft, right = R})}.

%% Поиск
find(nil, _Key) ->
  not_found;
find(#node{key = K, left = L}, Key) when Key < K ->
  find(L, Key);
find(#node{key = K, right = R}, Key) when Key > K ->
  find(R, Key);
find(#node{value = V}, _Key) ->
  {ok, V}.

%% Высота и баланс
tree_height(nil) -> 0;
tree_height(#node{height = H}) -> H.

update_height(#node{left = L, right = R} = Node) ->
  HL = tree_height(L),
  HR = tree_height(R),
  Node#node{height = 1 + max(HL, HR)}.

balance_factor(nil) -> 0;
balance_factor(#node{left = L, right = R}) ->
  tree_height(L) - tree_height(R).

%% Повороты
rotate_right(#node{left = nil} = Node) ->
  Node;
rotate_right(#node{key = K, value = V, left = LeftNode, right = R}) ->
  #node{key = LK, value = LV, left = LL, right = LR} = LeftNode,
  NewRight = #node{key = K, value = V, left = LR, right = R, height = 1},
  UpdatedRight = update_height(NewRight),
  NewRoot = #node{key = LK, value = LV, left = LL, right = UpdatedRight, height = 1},
  update_height(NewRoot).

rotate_left(#node{right = nil} = Node) ->
  Node;
rotate_left(#node{key = K, value = V, left = L, right = RightNode}) ->
  #node{key = RK, value = RV, left = RL, right = RR} = RightNode,
  NewLeft = #node{key = K, value = V, left = L, right = RL, height = 1},
  UpdatedLeft = update_height(NewLeft),
  NewRoot = #node{key = RK, value = RV, left = UpdatedLeft, right = RR, height = 1},
  update_height(NewRoot).

%% Балансировка
rebalance(nil) ->
  nil;
rebalance(Node) ->
  UpdatedNode = update_height(Node),
  BF = balance_factor(UpdatedNode),
  case BF > 1 of
    true ->
      #node{left = LeftChild} = UpdatedNode,
      LeftBF = balance_factor(LeftChild),
      case LeftBF < 0 of
        true ->
          NewLeft = rotate_left(LeftChild),
          rotate_right(UpdatedNode#node{left = NewLeft});
        false ->
          rotate_right(UpdatedNode)
      end;
    false ->
      case BF < -1 of
        true ->
          #node{right = RightChild} = UpdatedNode,
          RightBF = balance_factor(RightChild),
          case RightBF > 0 of
            true ->
              NewRight = rotate_right(RightChild),
              rotate_left(UpdatedNode#node{right = NewRight});
            false ->
              rotate_left(UpdatedNode)
          end;
        false ->
          UpdatedNode
      end
  end.

%% Map
map(_Fun, nil) ->
  nil;
map(Fun, #node{key = K, value = V, left = L, right = R, height = H}) ->
  NewValue = Fun(K, V),
  NewLeft = map(Fun, L),
  NewRight = map(Fun, R),
  #node{key = K, value = NewValue, left = NewLeft, right = NewRight, height = H}.

%% Filter
filter(_Pred, nil) ->
  nil;
filter(Pred, Tree) ->
  List = to_list(Tree),
  FilteredList = lists:filter(fun({K, V}) -> Pred(K, V) end, List),
  from_list(FilteredList).

%% Свертки
fold_left(_Fun, Acc, nil) ->
  Acc;
fold_left(Fun, Acc, #node{key = K, value = V, left = L, right = R}) ->
  Acc1 = fold_left(Fun, Acc, L),
  Acc2 = Fun(K, V, Acc1),
  fold_left(Fun, Acc2, R).

fold_right(_Fun, Acc, nil) ->
  Acc;
fold_right(Fun, Acc, #node{key = K, value = V, left = L, right = R}) ->
  Acc1 = fold_right(Fun, Acc, R),
  Acc2 = Fun(K, V, Acc1),
  fold_right(Fun, Acc2, L).

%% Моноид
concat(Tree1, nil) ->
  Tree1;
concat(Tree1, Tree2) ->
  fold_left(fun(K, V, Acc) -> insert(Acc, K, V) end, Tree1, Tree2).

merge(_ResolveFun, Tree1, nil) ->
  Tree1;
merge(_ResolveFun, nil, Tree2) ->
  Tree2;
merge(ResolveFun, Tree1, Tree2) ->
  fold_left(
    fun(K, V2, Acc) ->
      case find(Acc, K) of
        {ok, V1} ->
          NewValue = ResolveFun(K, V1, V2),
          insert(Acc, K, NewValue);
        not_found ->
          insert(Acc, K, V2)
      end
    end,
    Tree1,
    Tree2
  ).

%% Конвертация
to_list(Tree) ->
  fold_left(fun(K, V, Acc) -> Acc ++ [{K, V}] end, [], Tree).

from_list(List) ->
  lists:foldl(fun({K, V}, Acc) -> insert(Acc, K, V) end, nil, List).

%% Размер
size_custom(nil) ->
  0;
size_custom(#node{left = L, right = R}) ->
  1 + size_custom(L) + size_custom(R).

%% Сравнение
equal(nil, nil) ->
  true;
equal(nil, _) ->
  false;
equal(_, nil) ->
  false;
equal(#node{key = K1, value = V1, left = L1, right = R1},
      #node{key = K2, value = V2, left = L2, right = R2}) ->
  (K1 =:= K2) andalso (V1 =:= V2) andalso equal(L1, L2) andalso equal(R1, R2).

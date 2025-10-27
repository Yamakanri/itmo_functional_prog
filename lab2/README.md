Студент: ```Васильев Александр Дмитриевич```

Группа: ```P3325```

ИСУ: ```367962```

Язык: ```Erlang```

# Лабораторная по функциональному программированию №2
### **Вариант: ```avl-dict```**

**Требования:**

1) Функции:
– добавление и удаление элементов;
– фильтрация;
– отображение (map);
– свертки (левая и правая);
– структура должна быть моноидом.


2) Структуры данных должны быть неизменяемыми.


3) Библиотека должна быть протестирована в рамках unit testing.


4) Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).


5) Структура должна быть полиморфной.


6) Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.
Обратите внимание:

7) API должно быть реализовано для заданного интерфейса и оно не должно "протекать". На уровне тестов -- в первую очередь нужно протестировать именно API (dict, set, bag).
Должна быть эффективная реализация функции сравнения (не наивное приведение к спискам, их сортировка с последующим сравнением), реализованная на уровне API, а не внутреннего представления.



### Реализация

```erlang
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
  if
    BF > 1 ->
      #node{left = LeftChild} = UpdatedNode,
      LeftBF = balance_factor(LeftChild),
      if
        LeftBF < 0 ->
          NewLeft = rotate_left(LeftChild),
          rotate_right(UpdatedNode#node{left = NewLeft});
        true ->
          rotate_right(UpdatedNode)
      end;
    BF < -1 ->
      #node{right = RightChild} = UpdatedNode,
      RightBF = balance_factor(RightChild),
      if
        RightBF > 0 ->
          NewRight = rotate_right(RightChild),
          rotate_left(UpdatedNode#node{right = NewRight});
        true ->
          rotate_left(UpdatedNode)
      end;
    true ->
      UpdatedNode
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

```

### Тестрирование


```Unit-tests``` выполнены с помощью EUnit.

Покрытие тестов:
1) базовые операции – поиск, вставка, удаление
2) Функциональные операции – отображение, свертка, фильрация
3) Свойства моноида
4) Неизменяемость структуры
5) Полиморфность

```Property based tests``` выполнены выполнены с помощью PropEr.

 Покрытие тестов:
 1) Ассоцативность
 2) Нейтральный элемент
 3) Корректность поиска и вставки
